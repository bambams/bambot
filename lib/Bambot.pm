# 
# bambot is a simple^Wsarcastic IRC bot.
# Copyright (C) 2011-2013 Brandon McCaig
# 
# This file is part of bambot.
# 
# bambot is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
# 
# bambot is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with bambot.  If not, see <http://www.gnu.org/licenses/>.
# 
use v5.016;
use strict;
use warnings;
use version;
use utf8;

use constant {
    DEFAULT_NICK => 'bambot',
    DEFAULT_REALNAME => 'Unknown',
    DEFAULT_USERNAME => 'unknown',
};

package Bambot;

our ($EST, $VERSION);
my @submodules;

BEGIN {
    @submodules = qw(
        Bambot::Random
        Bambot::Reminder
        Bambot::Strings
        Bambot::Version
    );

    sub load_ {
        for (@submodules) {
            eval "require $_";
            die $@ if $@;
        }
    }

    sub unload_ {
        for (reverse @submodules) {
            Class::Unload->unload($_);
        }
    }

    load_();
}

use Bambot::Version;

use Class::Unload;
use Data::Dumper;
use DateTime;
use DateTime::Format::Duration;
use DateTime::Format::Natural;
use Encode;
use Errno::AnyString qw/custom_errstr/;
use File::Slurp qw(edit_file slurp);
use IO::Handle;
use IO::Select;
use IO::Socket::INET;
use List::Util qw(max);

sub _is_substitution {
    my ($self, $msg, $substitution_ref) = @_;
    my %pairs = qw/{ } ( ) [ ] < >/;

    undef $$substitution_ref;

    for my $opener (keys %pairs)
    {
        my $closer = $pairs{$opener};

        if($msg =~ /
                ^
                s
                \Q$opener\E
                ([^\Q$closer\E]+)
                \Q$closer\E
                \Q$opener\E
                ([^\Q$closer\E]*)
                \Q$closer\E
                (g)?
                $
                /x &&
                $1 ne $2) {
            $$substitution_ref = {
                opener => $opener,
                closer => $closer,
                pattern => $1,
                replacement => $2,
                global => defined $3,
            };

            return 1;
        }
    }

    if($msg =~ m{^s([^a-zA-Z0-9])([^\1]+)\1([^\1]*)\1(g)?$} &&
            $2 ne $3) {
        $$substitution_ref = {
            opener => $1,
            pattern => $2,
            replacement => $3,
            global => defined $4
        };

        return 1;
    }

    return;
}

sub add_urls {
    my ($self, $msg) = @_;
    my @urls = $msg =~ m{\b(https?://[-A-Za-z0-9_\.:/\?=%\&]+)}gi;

    if(@urls) {
        # File::Slurp::edit_file is resetting permissions. Need to
        # temporarily change umask so read permissions are not removed
        # from group and others.
        my $orig_umask = umask 033 or warn "Failed to modify umask";

        edit_file {
            my @lines = grep { /^http/ } split /^/m;

            push @lines, map { "$_\n" } @urls;
            shift @lines while @lines > 5;

            $_ = <<'EOF' . join '', @lines;
# This file is automatically written by Bambot, an IRC bot. The following
# lines are things that looked like HTTP and HTTPS URIs in an IRC channel
# that Bambot was in. It writes them to this file as a convenience for
# users that are working from a virtual console (or other interface)
# without copy+paste or open-URI functionality.
#
# Please note that there is no sensible way for Bambot to know the
# intentions of the users posting the URIs, nor the legitimacy of the URIs
# posted. Please use these URIs at your own risk. I am not responsible for
# what other people post in IRC channels and while I will make every
# effort to promptly remove URIs that I don't approve of, I make no
# guarantees to do so (I imagine I won't even know most of the URIs that
# get written here).
#
# Bambot limits the number of URIs that are recorded here so stale URIs
# will automatically be flushed as new ones are posted.

EOF
        } $self->{url_file};

        umask $orig_umask or warn "Failed to restore umask";
    }
}

sub auto_response {
    my ($self, @responses) = @_;

    $self->send(@responses);

    for (@responses) {
        $self->log($_, handle => \*STDOUT, level => 'AUTO');
    }

    return $self;
}

sub connect {
    my ($self) = @_;

    $self->log("Connecting to $self->{host}:$self->{port}...");

    my $sock = IO::Socket::INET->new(
            PeerAddr => $self->{host},
            PeerPort => $self->{port},
            Proto => 'tcp',
            ) or warn "IO::Socket::INET::new: $!";

    if($sock) {
        binmode $sock;

        $self->{on_} = 1;
        $self->{sock_} = $sock;

        $self->{selector_}->add($sock);

        push @{$self->{connected_date}}, DateTime->now();
    }

    return $self;
}

sub close {
    my ($self) = @_;

    $self->log("Closing connection...");

    my $sock = $self->{sock_};

    if(defined $sock) {
        $self->{selector_}->remove($sock);
        $sock->close();

        delete $self->{sock_};
        push @{$self->{disconnected_date}}, DateTime->now();
    }

    $self->{on_} = 0;

    return $self;
}

sub ctcp {
    my ($self, $msg) = @_;

    return "\001$msg\001";
}

sub exec_reminders {
    my ($self) = @_;

    my $now = DateTime->now();
    my $reminders = $self->{reminders_};
    my $count = @$reminders;
    my $quota = 3;
    my @due = grep $_->when <= $now && $quota-- > 0, reverse @$reminders;

    for (@due) {
        my ($target, $nick, $msg, $when) = (
                    $_->target, $_->nick, $_->msg, $_->when);

        $self->privmsg($target, $self->personalize($target, $nick,
                "Reminder that it's $when: $msg"));
    }

    $self->log("<<<<<<    Due reminders    ------", verbose=>1);
    $self->log("$_", verbose=>1) for @due;
    $self->log("---------------------------------", verbose=>1);

    @$reminders = grep !($_ ~~ \@due), @$reminders;

    $self->log("$_", verbose=>1) for @$reminders;
    $self->log(">>>>>> Remaining reminders ------", verbose=>1);

    $self->log(sprintf("Executed %d/%d reminders. %d remaining...",
            scalar @due, $count, scalar @$reminders), verbose=>1);

    return @due;
}

sub fstring {
    my ($self, $fmt, $key, %opts) = @_;

    return sprintf $fmt, $self->string($key, %opts);
}

sub get_nicks {
    my ($self, $channel) = @_;

    if($channel !~ /^[#&]/) {
        $channel = "#$channel";
    }

    my $nicks = $self->{nicks}{$channel};

    unless(defined $nicks) {
        $! = custom_errstr "No nicks found for $channel.";
        return;
    }

    $nicks = "Nicks in $channel: " .
            join ' ', grep $nicks->{$_},  keys %$nicks;

    return $nicks;
}

sub get_reminders {
    my ($self, $nick) = @_;

    return grep $_->nick eq $nick, @{$self->{reminders_}};
}

sub get_uptime_str {
    my ($self) = @_;

    my $now = DateTime->now();
    my $formatter = DateTime::Format::Duration->new(
            base => $now,
            normalize => 1,
            pattern => "%e days %H hours %M minutes %S seconds");

    my $uptime = $now - $self->{creation_date};
    my $str = sprintf 'Up for %s.', $formatter->format_duration($uptime);

    my $last_connected_date = $self->{connected_date}[-1];

    return $str unless defined $last_connected_date;

    my $connected_duration = $now - $last_connected_date;

    return $str if DateTime::Duration->compare(
            $uptime, $connected_duration, $now) == 0;

    $str = $str . sprintf ' Last connected %s ago.',
            $formatter->format_duration($connected_duration);

    return $str;
}

sub identify {
    my ($self) = @_;
    my $pwd = $self->load_pwd();

    if(defined $pwd && length $pwd > 0) {
        $self->privmsg('NickServ', "identify $pwd");
    }

    return $self;
}

sub init {
    my ($self) = @_;

    if(defined $self->{sock_}) {
        $self->log('Initializing...');
        $self->register();
        $self->join_channel(@{$self->{channels}});
    } else {
        $self->log('Cannot init... Socket undefined.');
    }

    return $self;
}

sub join_channel {
    my ($self, @channels) = @_;

    @channels = map $_ =~ s/^([^&#])/#$1/r, @channels;

    # Note: Refactor: this should be returned instead of auto_response.
    $self->auto_response("JOIN $_") for @channels;

    return $self;
}

sub load {
    my ($self) = @_;

    open my $fh, '<', $self->{config_file} or return 0;

    my $mode = (stat $fh)[2];

    die sprintf 'Insecure config permissions: %04o', $mode & 0777
            if ($mode & 0177) != 0;

    while(my $line = decode('UTF-8', <$fh>)) {
        next if $line =~ /^\s*(#|$)/;

        chomp $line;

        if($line =~ /^(\w+)\s*=\s*(.*)/) {
            my $type = ref $self->{$1};
            my @values = split ' ', $2;
            my $append = @values && substr($values[0], 0, 1) eq '+';

            if($append) {
                $values[0] = substr($values[0], 1);
            }

            if($type eq 'ARRAY') {
                if($append) {
                    push @{$self->{$1}}, @values;
                } else {
                    $self->{$1} = \@values;
                }
            } elsif($type eq 'HASH') {
                if($append) {
                    my %values = @values;

                    @{$self->{$1}}{keys %values} = values %values;
                } else {
                    $self->{$1} = {@values};
                }
            } else {
                $self->{$1} = $2;
            }

            next;
        }

        warn "invalid config: $line";
    }

    $fh->close or warn "close: $!";

    delete $self->{password};

    return $self;
}

sub load_pwd {
    my ($self) = @_;

    (map { /[^=]+=\s*(.*)/ and $1 or () } grep /^password\s*=/,
            slurp($self->{config_file}))[-1];
}

sub log {
    my ($self, $message, %opts) = @_;

    return if $opts{verbose} && !$self->{verbose};

    $opts{handle} //= \*STDERR;
    $opts{level} //= 'DIAGNOSTIC';

    my $now = DateTime->now();

    $opts{handle}->say("$now $opts{level}: $message");

    return $self;
}

sub ls {
    my ($self, $target, $what, $params) = @_;

    for ($what) {
        when('nicks') {
            if($params =~ /^([#&]\w+)/) {
                return $self->get_nicks($1);
            } else {
                $! = custom_errstr "Invalid arguments: $params";

                return;
            }
        }

        when('reminders') {
            my ($nick) = $params =~ /^(\S+)/;

            unless(defined $nick) {
                $! = custom_errstr "Invalid arguments: $params";

                return;
            };

            my $privates = 0;

            my @reminders = grep {
                        if($_->target eq $nick && $target ne $nick) {
                            $privates++;
                            0;
                        } else {
                            1;
                        }
                    } $self->get_reminders($nick);

            if(@reminders || $privates) {
                unshift @reminders,
                        "$privates private reminders." if $privates;

                unshift @reminders, "Remaining reminders for $nick:";

                return @reminders;
            } else {
                $! = custom_errstr "No reminders found for $nick.";

                return;
            }
        }
    }
}

sub new {
    my ($class, $config) = @_;
    my $selector = IO::Select->new(\*STDIN);

    my $self = {
        %$config,
        channels => [],
        creation_date => DateTime->now(),
        friendly_idents => [],
        on_ => 0,
        master_nicks => [],
        random_ => Bambot::Random->new(),
        reminders_ => [],
        selector_ => $selector,
        strings_ => Bambot::Strings->new(),
    };

    bless $self, $class;

    $self->load;

    return $self;
}

sub nickserv_ghost {
    my ($self) = @_;
    my $pwd = $self->load_pwd();

    if(defined $pwd && length $pwd > 0) {
        $self->privmsg('NickServ', "ghost $self->{nick} $pwd");
    }
}

sub notice {
    my ($self, $target, $msg) = @_;

    $self->auto_response("NOTICE $target :$msg");
}

sub personalize {
    my ($self, $target, $nick, $msg) = @_;

    return ($target eq $nick ? "" : "$nick: ") . "$msg";
}

sub privmsg {
    my ($self, $target, $msg) = @_;

    $self->auto_response("PRIVMSG $target :$msg");
}

sub pong {
    my ($self, @servers) = @_;

    $self->auto_response("PONG @servers");

    return $self;
}

sub process_client_command {
    my ($self, $command, $time) = @_;

    if($command =~ m{^/ctcp (\S+) (.+)}) {
        $self->privmsg($1, $self->ctcp($2));
    } elsif($command =~ m{^/eval (.+)}) {
        my @results = eval $1 or warn $@;

        print encode('UTF-8', Dumper \@results);
    } elsif($command =~ m{^/(exit|x)(?:\s+(.+))?}) {
        $self->quit($2);

        return 0;
    } elsif($command =~ m{^/identify$}) {
        $self->identify();
    } elsif($command =~ m{^/irc\s+(.+)}) {
        $self->send($1);
    } elsif($command =~ m{^/j(?:oin)? ([#&]?\w+)}) {
        $self->join_channel($1);
    } elsif($command =~ m{^/load$}) {
        $self->load;
    } elsif($command =~ m{^/ls\s+(nicks|reminders)\s*(.*)}) {
        my @lines = $self->ls(undef, $1, $2);

        if(@lines) {
            $self->log($_, handle => \*STDOUT) for @lines;
        } else {
            $self->log($!);
        }
    } elsif($command =~ m{^/me ([#&]?\w+) (.+)}) {
        $self->privmsg($1, $self->ctcp("ACTION $2"));
    } elsif($command =~ m{^/msg ([#&]?\w+) (.+)}) {
        $self->privmsg($1, $2);
    } elsif($command =~ m{^/nick (\w+)}) {
        $self->set_nick($1);
    } elsif($command =~ m{^/p(?:art)? ([#&]?\w+) (.*)}) {
        $self->auto_response("PART $1 :$2");
    } elsif($command =~ m{^/quit\s*(.*)}) {
        $self->quit($1);
    } elsif($command =~ m{^/register}) {
        $self->register();
    } elsif($command =~ m{^/reload$}) {
        $self->log(($self->reload)[1]);
    } elsif($command =~ m{^/remind
            \s+(\S+)
            \s+([#&]\w+|private)
            \s+(\S+)
            \s+(\S+)
            \s+(.+)
            }x) {
        my $remind_target = $2 eq 'private' ? $1 : $2;

        $self->remind($remind_target, $2, $3, $4, $5) or $self->log($!);
    } elsif($command =~ m{^/restart$}) {
        my $msg = 'Restarting ...';

        $self->log($msg);
        $self->quit($msg);

        exec("$0 @{$self->{ARGV}}");
    } elsif($command =~ m{^/uptime$}) {
        $self->log($self->get_uptime_str());
    } elsif($command =~
            m{^/verbose(?:\s+(on|off|1|0|yes|no|true|false))?}) {
        $self->{verbose} = $1 =~ /^(?:on|1|yes|true)$/ if defined $1;

        $self->log(sprintf "verbose: %s",
                $self->{verbose} ? "yes" : "no");
    } elsif($command =~ m{^/version$}) {
        $self->log(Bambot::version_str(), handle => \*STDOUT);
    } else {
        $self->log("Unknown command: $command");
        $self->log('Use /quit to quit. Use /irc for raw IRC.');
    }

    return $self;
}

sub process_server_message {
    my ($self, $msg, $time) = @_;

    $self->log($msg, handle => \*STDOUT, level => 'SERVER');

    if($msg =~ /^PING :?([\w\.]+)/) {
        $self->pong($1);
    } elsif($msg =~ /
            ^
            :\S+\s+353
            \s+$self->{nick}
            \s+=
            \s+([&#]\S+)
            \s+:?(.+)
            /x) {
        my ($channel, @nicks) = ($1, map s/[@+]*(.+)/$1/r, split / /, $2);

        $self->log(Dumper {channel=>$channel, nicks=>\@nicks},
                verbose=>1);

        $self->{nicks}{$channel}{$_} = 1 for @nicks;
    } elsif($msg =~ /^:\S+\s+433\s+\*\s+$self->{nick}
            \s+:Nickname is already in use./x) {
        $self->nickserv_ghost();
    } elsif($msg =~ /^:(\S+)\s+(JOIN|PART)\s+([#&]\S+)/) {
        my ($ident, $command, $channel) = ($1, lc($2), $3);
        my ($nick) = $ident =~ /([^!]+)/;

        $self->log("$nick ($ident) ${command}ed $channel.", verbose=>1);

        $self->{nicks}{$channel}{$nick} = $command eq 'join' ? 1 : 0;
    } elsif($msg =~ /^:(\S+) PRIVMSG (\S+) :?(.*)/) {
        my ($sender, $target, $msg) = ($1, $2, $3);

        $self->add_urls($msg);

        my ($nick, $ident) = $sender =~ /(\S+)!~?(\S+)/;
        my $is_master = $ident =~ /^\Q$self->{master}\E$/;
        my $is_friendly = $self->{friendly_idents} ~~ /^\Q$ident\E$/;

        $target = $target eq $self->{nick} ? $nick : $target;

        my $log = $self->{log_}{$target}{$ident} //= [];
        my $is_ctcp = $msg =~ /^\001(.*)\001/;
        my $ctcp = $1;
        my ($personalized_for_me, $for_other_instance) = (0, 0);

        if($msg =~ /^([^\s:]+):\s+(.*)/) {
            $msg = $2;
            $personalized_for_me = $1 eq $self->{nick};
            $for_other_instance = !$personalized_for_me;

            if($for_other_instance) {
                $self->log("Looks like that message was intended for $1" .
                        ", not me ($self->{nick}).");
            }
        }

        my $is_substitution = $self->_is_substitution($msg,
                \(my $substitution));

        if($for_other_instance) {
            if($msg =~ /^~/) {
                return $self;
            } else {
                goto LOG_PRIVMSG;
            }
        }

        if($is_ctcp) {
            $self->log("CTCP: $ctcp", verbose => 1);

            if($ctcp eq 'VERSION') {
                $self->notice($nick,
                        $self->ctcp("VERSION bambot:$VERSION:perl $]"));
            } elsif($ctcp =~ /^PING\b/) {
                $self->notice($nick, $self->ctcp("PONG"));
            }
        } elsif($is_friendly &&
                $msg eq "$self->{nick}: help") {
            $self->privmsg($target, $self->personalize(
                    $target, $nick,
                    $self->string('help_stub')));
        } elsif($is_friendly &&
                $msg =~ /^\s*are\s+you\s+still\s+there\s*\?\s*$/i) {
             $self->privmsg($target, $self->string('its_me'));
        } elsif($is_friendly &&
                $msg =~
                /^(?:
                        (?:$self->{nick})[:,\s]\s*)?
                        say\s+my\s+name[,\s]\s*
                        say\s+my\s+name\s*[\.!1]*\s*$/ix) {
            $self->privmsg($target, $self->personalize(
                    $target, $nick,
                    $self->string('say_my_name')));
        }
        elsif($is_friendly &&
                $msg =~ m[^.*\\o/\s*$] &&
                ($target eq $nick || $personalized_for_me)) {
            $self->privmsg($target,
                    $self->personalize($target, $nick, "\\o/"));
        } elsif($is_friendly && $is_substitution) {
            if(my ($old_msg_ref) = map { \$_; }
                    (grep { /\Q$substitution->{pattern}\E/ } @$log)[-1]) {
                my ($pat, $rep, $glob) = @$substitution{
                        qw(pattern replacement global)};

                if($glob) {
                    $$old_msg_ref =~ s/$pat/\x02$rep\x0F/g;
                } else {
                    $$old_msg_ref =~ s/$pat/\x02$rep\x0F/;
                }

                $self->privmsg($target,
                        "$nick meant to say: $$old_msg_ref");
            }
        } elsif(0 && $is_master && $msg =~ /
                drunk|intoxicated|
                beer|carling|budweiser|steam whistle|
                amaretto|rum|scotch|vodka|whiskey/ix) {
            $self->privmsg($target, $self->personalize(
                    $target, $nick,
                    $self->string('alcholic')));
        } elsif($is_friendly && $msg =~ /^~\s+\S/) {
            $self->privmsg($target, $self->string('query_stub'));
        } elsif($is_master && $msg eq '~activate') {
            $self->privmsg($target, 'Sentry mode activated..');
        } elsif($is_master && $msg eq '~deactivate') {
            $self->privmsg($target, 'Sleep mode activated..');
        } elsif($is_master && $msg =~ /^~eval (.*)/) {
            my $result = "eval: $1";

            $self->privmsg($target,
                    $self->personalize($target, $nick, $result));
        } elsif($is_master && $msg eq '~load') {
            $self->log('Master issued ~load...');

            $self->privmsg($target,
                    $self->string('loaded_config')) if $self->load;
        } elsif($is_friendly && $msg =~ /^~ls\s+(reminders)\b/) {
            my @lines = $self->ls($target, $1, $nick);

            if(@lines) {
                if(@lines > 4) {
                    $target = $nick;
                }

                $self->privmsg($target,
                        $self->personalize($target, $nick, $_))
                        for @lines;
            } else {
                $self->privmsg($target,
                        $self->personalize($target, $nick, $!));
            }
        } elsif($is_master && $msg eq '~reload') {
            $self->log('Master issued ~reload...');

            $self->privmsg($target, ($self->reload)[1]);
        } elsif($is_friendly && $msg =~ /^~remind
                (?:\s+(\S+))?
                (?:\s+([#&]\w+|private))?
                \s+(\S+)
                \s+(\S+)
                \s+(.+)
                /x) {
            my ($your_nick, $scope, $date, $time, $msg) =
                    ($1, $2, $3, $4, $5);

            $your_nick = $nick if ($your_nick // 'me') eq 'me';

            $scope = $target =~ /^[#&]/ ? 'public' : 'private'
                    unless defined $scope;

            if($is_master || $your_nick eq $nick) {
                my $msg = $self->remind($target, $your_nick, $scope,
                        $date, $time, $msg);

                $self->privmsg($target,
                        $self->personalize($target, $nick, $msg// $!));
            } else {
                $self->privmsg($target, $self->personalize(
                        $target, $nick,
                        "You can only set reminders for yourself."));
            }
        } elsif($is_master && $msg =~ /^~shutdown\s*(.*?)\s*$/) {
            $self->privmsg($target, $self->personalize(
                    $target, $nick,
                    $self->string('shutdown')));

            $self->quit($1 || ());
        } elsif($is_friendly && $msg eq '~sing') {
            $self->sing($target);
        } elsif($is_friendly && $msg eq '~sleep') {
            $self->privmsg($target, $self->string('sleep'));
        } elsif($is_friendly && $msg eq '~uptime') {
            $self->privmsg($target, $self->get_uptime_str());
        } elsif($is_friendly && $msg eq '~version') {
            $self->privmsg($target, $self->personalize($target, $nick,
                    Bambot::version_str()));
        } elsif($msg =~ /\bmadness\b/i && $target =~ /^[#&]/) {
            if(int rand 4 == 0) {
                $self->privmsg($target,
                        $self->string('spartaaa', target => $target));
            } else {
                $self->log('Spartan utterance...miss.');
            }
        } else {
            $self->log("Unrecognized user input from $nick: {{{$msg}}}",
                    verbose => 1);
        }

        if(!$is_ctcp && $msg eq '\\o/') {
            $self->{'\\o/'}++;

            if($self->{'\\o/'} > 1 && !$self->{'\\o/ed'}) {
                $self->privmsg($target, '\\o/');
                $self->{'\\o/ed'} = 1;
            }
        } else {
            $self->{'\\o/'} = 0;
            $self->{'\\o/ed'} = 0;
        }

LOG_PRIVMSG:
        unless($is_ctcp || $is_substitution) {
            push @$log, $msg;
            shift @$log while @$log > 5;
        }
    } elsif($msg =~ /^:(\S+)\s+QUIT/) {
        my ($ident, $nick) = ($1, $1 =~ /([^!]+)/);

        $self->log("$nick ($ident) quit.", verbose=>1);

        my $channels = $self->{nicks} or return $self;

        for my $channel (keys %$channels) {
            my $nicks = $channels->{$channel} or next;

            $nicks->{$nick} = 0 if $nicks->{$nick};
        }
    }

    return $self;
}

sub quit {
    my ($self, $msg) = @_;

    $msg //= 'Shutting down...';

    $self->auto_response("QUIT :$msg");
    $self->close();
}

sub random {
    my ($self) = @_;

    return $self->{random_};
}

sub reconnect {
    my ($self) = @_;

    return $self unless $self->{on_};

    $self->log('Reconnecting...');
    $self->close();
    $self->connect();
    $self->init();

    if(defined $self->{sock_} and @{$self->{master_nicks}}) {
        $self->privmsg($self->{master_nicks}[0], 'Reconnected...');
    }

    return $self;
}

sub register {
    my ($self) = @_;
    my $i = int rand 99;
    my $nick = $self->{nick} // DEFAULT_NICK() . $i;
    my $user = $self->{username} // $ENV{USER} // DEFAULT_USERNAME() . $i;
    my $real_name = $self->{real_name} // DEFAULT_REALNAME();

    $self->set_nick($self->{nick});

    $self->auto_response("USER $user 0 0 :$real_name");

    $self->identify();

    return $self;
}

sub reload {
    my ($self) = @_;

    $self->log('Reloading module...');

    my $pkg = __PACKAGE__;
    my $status;

    if(eval "require $pkg" && !$@) {
        unload_();
        Class::Unload->unload($pkg);

        eval "require $pkg";

        $status = !$@;
    } else {
        $self->log("Can't reload: $@");
        $status = 0;
    };

    return ($status, $self->string(
            "reload/" . (qw/failure success/)[$status]));
}

sub remind {
    my ($self, $target, $nick, $scope, $date, $time, $msg) = @_;
    my $formatter = DateTime::Format::Natural->new();
    my $when = $formatter->parse_datetime("$date $time");

    unless($formatter->success) {
        $! = custom_errstr "Invalid reminder date/time: $date $time";

        return;
    }

    if(DateTime->now() >= $when) {
        $! = custom_errstr
                "Can't remind you about past or current events.";

        return;
    }

    $target = $nick if $scope eq 'private';

    my $reminders = $self->{reminders_};

    push @$reminders, Bambot::Reminder->new($target, $nick, $when, $msg);

    @$reminders = sort { $a <=> $b } @$reminders;

    return "Reminder set for $nick at $when in $scope.";
}

sub run {
    my ($self) = @_;

    STDOUT->autoflush(1);

    $self->log(Bambot::version_str(), handle => \*STDOUT);

MAIN:
    while(1) {
        my ($sock, $selector) = @$self{qw/sock_ selector_/};

        my $now = DateTime->now();
        my $timeout;
        my $next_reminder = $self->{reminders_}[-1];

        if(defined $next_reminder && $next_reminder->when > $now) {
            $timeout = $next_reminder->when
                    ->subtract_datetime_absolute($now)->seconds() + 1;

            $self->log("Sleeping for $timeout seconds...", verbose=>1);
        }

        my @handles = $selector->can_read($timeout);

        for my $rh (@handles) {
            my $msg = <$rh>;

            unless(defined $msg) {
                $self->log('We appear to have been disconnected...');
                $self->reconnect();

                next;
            }

            my $now = DateTime->now();

            $msg = decode('UTF-8', $msg);

            chomp $msg;

            $msg =~ tr/\r//d;

            next if $msg =~ /^\s*$/;

            if($rh == $sock) {
                $self->log('Reading from socket...', verbose => 1);

                $self->process_server_message($msg, $now);
            } elsif($rh == \*STDIN) {
                $self->log('Reading from stdin...', verbose => 1);
                $self->log($msg, handle => \*STDOUT, level => 'STDIN');

                $self->process_client_command($msg, $now) or last MAIN;
            } else {
                $self->log('Unknown handle...', verbose => 1);

                print encode('UTF-8', Data::Dumper->Dump(
                        [\*STDIN, $sock, $rh],
                        [qw(STDIN sock rh)]));
            }
        }

        $self->exec_reminders();
    }

    $self->close();

    return $self;
}

sub send {
    my ($self, @messages) = @_;

    @messages = map encode('UTF-8', "$_\n"), @messages;

    my $sock = $self->{sock_};

    unless(defined $sock) {
        $self->log('Cannot send: socket undefined.');

        return $self;
    }

    if($sock->error) {
        $self->log('The socket appears to be dead.');

        if($self->clearerr == -1) {
            $self->log('Yep, definitely dead. -_-' .
                    ' Attempting to reconnect...');

            $self->reconnect();

            return $self;
        } else {
            $self->log('Apparently not dead. Just angry.');
        }
    } else {
        $sock->print(@messages);
    }

    return $self;
}

sub set_nick {
    my ($self, $nick) = @_;

    $self->auto_response("NICK $nick");
    $self->{nick} = $nick;

    return $self;
}

sub sing {
    my ($self, $target, $nick) = @_;

    my @wannabee_lyrics = map { chomp;$_ }
            slurp $self->{lyrics_file} or do {
        warn "Failed to load lyrics file: $!";

        $self->privmsg($target, $self->personalize(
                $target, $nick,
                "I can't think of any songs..."));

        return;
    };

    $self->privmsg($target,
            $self->random->pick_random(\@wannabee_lyrics));
}

sub string {
    my ($self, $key, %opts) = @_;

    return $self->{strings_}->get_string($key, %opts);
}

sub strings {
    my ($self, $key, %opts) = @_;

    return $self->{strings_}->get_strings($key, %opts);
}

sub version_str {
    return "This is Bambot v$Bambot::VERSION.";
}

1;
