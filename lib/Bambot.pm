# 
# bambot is a simple^Wsarcastic IRC bot.
# Copyright (C) 2011-2015 Brandon McCaig
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
package Bambot;

use v5.016;
use strict;
use warnings;
use version;
use utf8;

use constant {
    DEFAULT_SELECT_TIMEOUT => (60 * 5),
    DEFAULT_MAX_URLS => 5,
    DEFAULT_NICK => 'bambot',
    DEFAULT_REALNAME => 'Unknown',
    DEFAULT_USERNAME => 'unknown',
    TRUNCATE_LENGTH => 512,
};

our ($EST, $VERSION);

use Carp;
use Class::Unload;
use Data::Dumper;
use DateTime;
use DateTime::Format::Duration;
use DateTime::Format::Natural;
use Encode;
use English qw/$PERL_VERSION/;
use Errno::AnyString qw/custom_errstr/;
use File::Write::Rotate;
use File::Slurp qw(edit_file slurp);
use IO::Handle;
use IO::Select;
use IO::Socket::INET;
use List::Util qw(min);

my @submodules = qw(
    Bambot::Ident
    Bambot::Random
    Bambot::ReloadSignal
    Bambot::Reminder
    Bambot::Strings
    Bambot::Version
);

load_submodules();

sub _is_substitution {
    my ($self, $msg, $substitution_ref) = @_;
    my %pairs = qw/{ } ( ) [ ] < >/;

    undef $$substitution_ref;

    while(my ($opener, $closer) = each %pairs) {
        # This regex attempts to parse things like s{foo}{bar} or
        # s<foo><bar> as an s/// expression.
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

    # This regex attempts to parse a normal s/// expression, though it
    # supports custom delimiters. That is, 's', almost anything, anything
    # else, that first thing, anything else, that first thing again, and
    # optionally a 'g'.
    #
    # Delimiters of alphanumerics were disallowed when it was discovered
    # that things resembling an s/// that weren't occur regularly in the
    # wild. For a contrived example, "something innocent to" (which is far
    # from proper grammar, but this is a contrived example...) looks like
    # s/mething inn/cent t/.
    if($msg =~ m{
            ^
            s
            ([^a-zA-Z0-9])      # <-- The delimiter character.
            ([^\1]+)
            \1
            ([^\1]*)
            \1
            (g)?
            $
            }x &&
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

sub about {
    my ($self, $target) = @_;

    my $about_file = $self->{about_file};

    my $ok = 1;

    my $path = $INC{__PACKAGE__ . '.pm'};

    # Read the copyright notice from the module file.
    my @lines = map s/^#\s*//r, grep { $ok = $ok && /^#/ } slurp($path);

    # Check for an additional user-specified about file.
    if(defined $about_file) {
        unless($about_file =~ m{^/}) {
            $about_file = "$ENV{HOME}/.bambot/$about_file";
        }

        my @about_lines = eval {
            slurp($about_file);
        };

        if($@) {
            $self->log("Failed to open about file: $about_file: $@");
        } else {
            push @lines, "\n", @about_lines;
        }
    }

    chomp(@lines);

    $_ = ($_ eq '' ? ' ' : $_) for @lines;

    return wantarray ? @lines : \@lines;
}

sub add_urls {
    my ($self, $msg) = @_;
    my @urls = $msg =~ m{\b(https?://[-A-Za-z0-9_\.:/~\?=%\&#]+)}gi;

    if(@urls) {
        # File::Slurp::edit_file is resetting permissions. Need to
        # temporarily change umask so read permissions are not removed
        # from group and others.
        my $orig_umask = umask 033 or carp "Failed to modify umask";

        edit_file {
            my @lines = grep { /^http/ } split /^/m;

            push @lines, map { "$_\n" } @urls;

            my $max_urls = $self->{max_urls};

            # Limit number of URLs to max_urls. Purge old URLs.
            splice @lines, 0, @lines - $max_urls if @lines > $max_urls;

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

        umask $orig_umask or carp "Failed to restore umask";
    }
}

sub auto_response {
    my ($self, @responses) = @_;

    @responses = map
            substr($_, 0, min(TRUNCATE_LENGTH, length)),
            @responses;

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
            ) or carp "IO::Socket::INET::new: $!";

    if($sock) {
        binmode $sock, ':encoding(UTF-8)';

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

sub do_magic {
    my ($self, $msg, $ident) = @_;

    my $new_friendly_ident = $self->is_magic($msg, $ident);
    my $nick = $ident->nick;

    $self->log("WARNING: Adding '$new_friendly_ident' to " .
            "friendly_idents because $nick spoke the magic " .
            "words!");

    push @{$self->{friendly_idents}}, $new_friendly_ident;
}

sub exec_reminders {
    my ($self) = @_;

    my $now = DateTime->now();
    my $reminders = $self->{reminders_};
    my @reminders = sort { $b <=> $a } values %$reminders;
    my $count = @reminders;
    my $quota = 3;
    my @due = grep $_->when <= $now && $quota-- > 0, @reminders;

    for (@due) {
        my ($target, $nick, $msg, $when) = (
                    $_->target, $_->nick, $_->msg, $_->when);

        $self->privmsg($target, $self->personalize($target, $nick,
                "Reminder that it's $when: $msg"));

        delete $reminders->{$_->id};
    }

    if($self->{verbose})
    {
        my @reminders = values %$reminders;

        $self->log("<<<<<<    Due reminders    ------", verbose=>1);
        $self->log("$_", verbose=>1) for @due;
        $self->log("---------------------------------", verbose=>1);
        $self->log("$_", verbose=>1) for @reminders;
        $self->log(">>>>>> Remaining reminders ------", verbose=>1);

        $self->log(sprintf("Executed %d/%d reminders. %d remaining...",
                scalar @due, $count, scalar @reminders), verbose=>1);
    }

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

    $self->log(Dumper $nicks, verbose=>1);

    $nicks = "Nicks in $channel: " .
            join ' ', grep $nicks->{$_}{joined}, keys %$nicks;

    return $nicks;
}

sub get_reminders {
    my ($self, $nick) = @_;

    return sort grep $_->nick eq $nick, values %{$self->{reminders_}};
}

sub get_uptime_str {
    my ($self) = @_;

    my $now = DateTime->now();

    my $uptime = $now - $self->{creation_date};

    my $pattern = sub {
        my ($duration) = @_;

        my ($years, $months, $days, $hours, $minutes) =
                $duration->in_units(qw/years months days hours minutes/);

        my $toggle = 0;

        my $pattern = join ' ', (reverse
                map $_->[0],
                grep $_->[1],
                reverse
                ['%1y years', $toggle += $years],
                ['%1m months', $toggle += $months],
                ['%1e days', $toggle += $days],
                ['%1H hours', $toggle += $hours],
                ['%1M minutes', $toggle += $minutes]),
                '%1s seconds';

        return $pattern;
    };

    my $formatter = DateTime::Format::Duration->new(
            base => $now,
            normalize => 1,
            pattern => $pattern->($uptime));

    my $str = sprintf 'Up for %s.', $formatter->format_duration($uptime);

    my $last_connected_date = $self->{connected_date}[-1];

    return $str unless defined $last_connected_date;

    my $connected_duration = $now - $last_connected_date;

    return $str if DateTime::Duration->compare(
            $uptime, $connected_duration, $now) == 0;

    $formatter = DateTime::Format::Duration->new(
            base => $now,
            normalize => 1,
            pattern => $pattern->($connected_duration));

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

sub is_friendly {
    my ($self, $user) = @_;
    my $friendlies = $self->{friendly_idents};

    return grep $user eq $_, @$friendlies;
}

sub is_magic {
    my ($self, $msg, $ident) = @_;

    my $magic_words = $self->{magic_words};
    my $magic_len = length($magic_words);

    my $is_magic = $msg =~ /^\Q$magic_words\E(.*)/;

    $self->log("<<<$msg>>> =~ /^\Q$magic_words(.*)/");

    return $is_magic unless $is_magic;

    my ($new_friendly_ident) = $1 =~ /^(?:\s+\(([^\s]+)\))?\s*$/;

    $new_friendly_ident ||= $ident->user;

    return $new_friendly_ident;
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

    open my $fh, '< :encoding(UTF-8)', $self->{config_file} or return 0;

    my $mode = (stat $fh)[2];

    croak sprintf 'Insecure config permissions: %04o', $mode & 0777
            if ($mode & 0177) != 0;

    while(my $line = <$fh>) {
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

        carp "invalid config: $line";
    }

    $fh->close or carp "close: $!";

    delete $self->{password};

    return $self;
}

sub load_submodules {
    for (@submodules) {
        eval "require $_";
        croak $@ if $@;
    }
}

sub load_pwd {
    my ($self) = @_;

    (map { /[^=]+=\s*(.*)/ and $1 or () } grep /^password\s*=/,
            slurp($self->{config_file}))[-1];
}

sub log {
    my ($self, $message, %opts) = @_;

    my $handle = $opts{handle} // \*STDERR;
    my $level = $opts{level} // 'DIAGNOSTIC';
    my $verbose = $opts{verbose} // 0;

    my $now = DateTime->now();

    if($verbose) {
        $level .= '|VERBOSE';
    }

    $message = "$now $$ $level: $message\n";

    $self->{logger_}->write(encode('UTF-8', $message));

    unless ($verbose && !$self->{verbose}) {
        $handle->print($message);
    }

    return $self;
}

sub ls {
    my ($self, $target, $what, $params) = @_;

    if($what eq 'nicks') {
        if($params =~ /^([#&]\w+)/) {
            return $self->get_nicks($1);
        } else {
            $! = custom_errstr "Invalid arguments: $params";

            return;
        }
    } elsif($what eq 'reminders') {
        my ($nick) = $params =~ /^(\S+)/;

        unless(defined $nick) {
            $! = custom_errstr "Invalid arguments: $params";

            return;
        };

        my $privates = 0;

        my @reminders =
                map { my $id = substr($_->id, 0, 6); "$id: $_" }
                grep {
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

sub master($) {
    my ($self) = @_;
    my $nick = $self->{master_nicks}[0]; # h4x.
    my $ident = "$nick!~$self->{master}";

    return Bambot::Ident->new($ident);
}

sub new {
    my ($class, $config) = @_;

    my $rotator = File::Write::Rotate->new(
            dir => $config->{log_dir},
            histories => 500,
            prefix => 'bambot',
            size => 1024 ** 2,
            suffix => '.log',
            );

    my $selector = IO::Select->new(\*STDIN);

    my $self = {
        %$config,
        channels => [],
        creation_date => DateTime->now(),
        friendly_idents => [],
        initial_version => $Bambot::VERSION,
        logger_ => $rotator,
        master_nicks => [],
        max_urls => DEFAULT_MAX_URLS,
        on_ => 0,
        random_ => Bambot::Random->new(),
        reminders_ => {},
        run_depth_ => 0,
        select_timeout => DEFAULT_SELECT_TIMEOUT,
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

sub ping {
    my ($self) = @_;

    my $nick = $self->master->nick;
    my $server = $self->{server};

    $self->auto_response("PING $nick :$server");

    return $self;
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

    if($command =~ m{^/about$}) {
        $self->log($_) for $self->about();
    } elsif($command =~ m{^/ctcp (\S+) (.+)}) {
        $self->privmsg($1, $self->ctcp($2));
    } elsif($command =~ m{^/eval (.+)}) {
        my @results = eval $1 or carp $@;

        print Dumper \@results;
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
        $self->reload('console');
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
    } elsif($command =~ m{^/rm\s+(reminder)\s+(.+)}) {
        my $user = $self->master->user;

        $self->log($self->rm($1, "$user $2"));
    } elsif($command =~ m{^/uptime$}) {
        $self->log($self->get_uptime_str());
    } elsif($command =~
            m{^/verbose(?:\s+(on|off|1|0|yes|no|true|false))?}) {
        $self->{verbose} = $1 =~ /^(?:on|1|yes|true)$/ if defined $1;

        $self->log(sprintf "verbose: %s",
                $self->{verbose} ? "yes" : "no");
    } elsif($command =~ m{^/version$}) {
        $self->log($self->version_str(), handle => \*STDOUT);
    } elsif($command =~ m{^/who ([#&]\S+)}) {
        my $channel_name = $1;
        my $nicks = $self->{nicks};
        my $channel = $nicks->{$channel_name};

        if(ref $channel eq 'HASH') {
            $self->log(Dumper $channel, verbose=>1);

            $self->log("Believed to be in $channel_name: " .
                    join ', ',
                    sort { $a cmp $b }
                    grep $channel->{$_}{joined}, keys %$channel);
        } else {
            $self->log("Not in $channel_name?");
        }
    } else {
        $self->log("Unknown command: $command");
        $self->log('Use /quit to quit. Use /irc for raw IRC.');
    }

    return $self;
}

sub process_server_message {
    my ($self, $msg, $time) = @_;

    $self->log($msg, handle => \*STDOUT, level => 'SERVER');

    my ($server) = $msg =~ /^:([a-z1-9.]+)\s+(NOTICE|[0-9]+)\b/;

    if(defined $server) {
        $self->{servers}{$server} = 1;
        $self->{server} = $server;
    }

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

        for my $nick (@nicks) {
            $self->{nicks}{$channel}{$nick} = {
                joined => 1,
                seen => DateTime->now(),
            };
        }
    } elsif($msg =~ /^:\S+\s+433\s+\*\s+$self->{nick}
            \s+:Nickname is already in use./x) {
        $self->nickserv_ghost();
    } elsif($msg =~ /^:(\S+)\s+(JOIN|PART)\s+([#&]\S+)/) {
        my ($ident, $command, $channel) = ($1, lc($2), $3);
        my $nick = Bambot::Ident->new($ident)->nick;

        $self->log("$nick ($ident) ${command}ed $channel.", verbose=>1);

        $self->{nicks}{$channel}{$nick} = {
            joined => ($command eq 'join' ? 1 : 0),
            seen => DateTime->now(),
        };
    } elsif($msg =~ /^:(\S+) PRIVMSG (\S+) :?(.*)/) {
        my ($sender, $target, $msg) = ($1, $2, $3);

        $self->add_urls($msg);

        my $ident = Bambot::Ident->new($sender);
        my $nick = $ident->nick;
        my $is_master = $ident->user eq $self->master->user;
        my $is_friendly = $self->is_friendly($ident->user);
        my $is_private = $target eq $self->{nick};

        $target = $nick if $is_private;

        my $log = $self->{log_}{$target}{$ident->user} //= [];
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

        my $re_fuuuuuu = qr/f{1,}u{4,}/i;

        if($is_ctcp) {
            $self->log("CTCP: $ctcp", verbose => 1);

            if($ctcp eq 'VERSION') {
                my $initial = $self->{initial_version};
                my $version = $Bambot::VERSION;

                my $str = "Bambot v$initial";

                if($version ne $initial) {
                    $str .= " (hotswapped v$version)";
                }

                $str .= " / Perl $PERL_VERSION";

                $self->notice($nick,
                        $self->ctcp("VERSION $str"));
            } elsif($ctcp =~ /^PING ([0-9]+)\b/) {
                $self->notice($nick, $self->ctcp("PING $1"));
            } elsif($ctcp =~ /^TIME\b/) {
                my $zone = $self->{timezone};
                my $tz = DateTime::TimeZone->new(name => $zone);
                my $localtime = $time->clone();

                $localtime->set_time_zone('UTC');
                $localtime->set_time_zone($tz);

                $self->notice($nick, $self->ctcp("TIME $localtime"));
            }
        } elsif($is_private && $self->is_magic($msg, $ident)) {
            $self->do_magic($msg, $ident);
        } elsif($is_friendly && ($personalized_for_me ?
                $msg =~ /$re_fuuuuuu/i :
                $msg =~ /$re_fuuuuuu \Q$self->{nick}/)) {
            my $result = $self->personalize(
                    $target,
                    $nick,
                    "Yeah, I'm sorry, I'm stupid.");

            $self->privmsg($target, $result);
        } elsif($is_friendly &&
                $personalized_for_me) {
            if($msg eq "help") {
                $self->privmsg($target, $self->personalize(
                        $target, $nick,
                        $self->string('help_stub')));
            } elsif($msg =~ /\?\s*$/) {
                $self->privmsg($target, $self->personalize(
                        $target, $nick,
                        $self->string('dont_know')));
            }
        } elsif($is_friendly &&
                $msg =~ /^\s*are\s+you\s+still\s+there\s*\?\s*$/i) {
             $self->privmsg($target, $self->string('its_me'));
        } elsif($is_friendly && $msg =~ /
                        ^
                        say\s+my\s+name[,\s]\s*
                        say\s+my\s+name\s*[\.!1]*\s*
                        $
                        /ix) {
            $self->privmsg($target, $self->personalize(
                    $target, $nick,
                    $self->string('say_my_name')));
        }
        elsif($is_friendly &&
                $msg =~ m[^.*\\o/\s*$] &&
                ($is_private || $personalized_for_me)) {
            $self->privmsg($target,
                    $self->personalize($target, $nick, "\\o/"));
        } elsif($is_friendly && $is_substitution) {
            my ($pat, $rep, $glob) = @$substitution{
                    qw(pattern replacement global)};

            eval {
                if(my ($old_msg_ref) = map \$_,
                        (grep /$pat/, @$log)[-1]) {
                    my $msg = $$old_msg_ref;

                    if($glob) {
                        $$old_msg_ref =~ s/$pat/$rep/g;
                        $msg =~ s/$pat/\x02$rep\x0F/g;
                    } else {
                        $$old_msg_ref =~ s/$pat/$rep/;
                        $msg =~ s/$pat/\x02$rep\x0F/;
                    }

                    $self->privmsg($target,
                            $is_private ?
                            "(I think he meant to say: $msg)" :
                            "$nick meant to say: $msg");
                }
            };

            if($@) {
                $self->privmsg($target, $@);
            }
        } elsif(0 && $is_master && $msg =~ /
                drunk|intoxicated|
                beer|carling|budweiser|steam whistle|
                amaretto|rum|scotch|vodka|whiskey/ix) {
            $self->privmsg($target, $self->personalize(
                    $target, $nick,
                    $self->string('alcholic')));
        } elsif($is_friendly && $msg =~ /^~\s+(.*)/) {
            $msg = $1;

            # h4x: Calculate mileage from distance and fuel consumption...
            # Hard-coded because this is a quick hack and I don't have
            # time to properly refactor Bambot right now.
            if($msg =~ m{
                    ^
                    (
                      [0-9]*\.[0-9]+
                      |
                      [0-9]+
                    )
                    \s*
                    (
                      l(?:it(?:re|er)(?:s)?)?
                      |
                      ga(?:llon(?:s)?)?
                    )
                    \s+
                    (
                      [0-9]*\.[0-9]+
                      |
                      [0-9]+
                    )
                    \s*
                    (
                      kilometer(?:s)?
                      |
                      km
                      |
                      mi(?:le(?:s)?)?
                    )
                    (?:
                      \s*
                      (
                        l(?:/100km)?
                        |
                        m(?:pg)?
                      )
                    )?
                    \s*
                    $
                    }x) {
                my %default_output_units = (k => 'l/100km', m => 'mpg');

                my ($v, $vu, $d, $du, $ou) = ($1, $2, $3, $4, $5);

                my $ndu = substr($du, 0, 1);
                my $nvu = substr($vu, 0, 1);

                $ou //= $default_output_units{$ndu};
                my $nou = substr($ou, 0, 1);

                my $ga2l = sub { $_[0] * 3.785411784 };
                my $mi2km = sub { $_[0] * 1.6093472 };

                my $nd = ($ndu eq 'k') ? $d : $mi2km->($d);
                my $nv = ($nvu eq 'l') ? $v : $ga2l->($v);

                my $vd2l100km = sub { $_[0] / $_[1] * 100 };

                my $no = $vd2l100km->($nv, $nd);

                my $l100km2mpg = sub { 235.214583333333 / $_[0] };

                my $o = ($nou eq 'l') ? $no : $l100km2mpg->($no);

                $self->privmsg(
                        $target,
                        $self->string(
                                'mileage_result',
                                distance => $d,
                                distance_unit => $du,
                                output => $o,
                                output_unit => $ou,
                                volume => $v,
                                volume_unit => $vu,
                                ));
            } else {
                $self->privmsg($target, $self->string('query_stub'));
            }
        } elsif($is_master && $msg eq '~about') {
            $self->privmsg($nick, $_) for $self->about();
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

            $self->reload($target);
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
        } elsif($msg =~ /^~rm\s+(reminder)\s+(.+)/) {
            $self->privmsg($target, $self->personalize(
                    $target, $nick, $self->rm($1, "$sender $2")));
        } elsif($is_friendly &&
                (
                    $msg =~ /^~(?:rr|shoot)(?:\s*$|\s+(\S+))/ ||
                    $msg =~ /Do\s+you\s+feel\s+lucky,?\s+punk?\?/i)
                ) {
            $self->russian_roulette($target, $1 // $nick);
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
                    $self->version_str()));
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
        my $ident = Bambot::Ident->new($1);
        my $nick = $ident->nick;
        my $user = $ident->user;

        $self->log("$nick ($user) quit.", verbose=>1);

        my $channels = $self->{nicks} or return $self;

        for my $channel (keys %$channels) {
            my $nicks = $channels->{$channel} or next;

            $nicks->{$nick}{joined} = 0 if $nicks->{$nick};
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

    if(defined $self->{sock_} and defined $self->master->nick) {
        $self->privmsg($self->master->nick, 'Reconnected...');
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
    my ($self, $source) = @_;

    $self->log('Reloading module...');

    my $pkg = __PACKAGE__;
    my $status;

    if(eval "require $pkg" && !$@) {
        unload_submodules();
        Class::Unload->unload($pkg);

        eval "require $pkg";

        $status = !$@;
    } else {
        $self->log("Can't reload: $@");
        $status = 0;
    };

    my $message = $self->string(
            "reload/" . (qw/failure success/)[$status]);

    my $signal = Bambot::ReloadSignal->new($source, $status, $message);

    die $signal;
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

    my $reminder = Bambot::Reminder->new($target, $nick, $when, $msg);
    my $id = $reminder->id;

    my $reminders = $self->{reminders_};

    if(exists $reminders->{$id})
    {
        $self->log('Reminder SHA1 collision:' .
                " {{{$reminder}}} vs. {{{$reminders->{$id}}}}");
        $! = custom_errstr "Reminder already exists.";
        return;
    }

    $reminders->{$reminder->id} = $reminder;

    return "Reminder set for $nick at $when in $scope.";
}

sub rm {
    my ($self, $what, $params) = @_;

    if($what eq 'reminder') {
        my ($ident, $id) = $params =~ /^(\S+)\s+([A-Za-z0-9]{1,40})/;

        $ident = Bambot::Ident->new($ident);

        my $is_master = $ident->user eq $self->master->user;
        my $nick = $ident->nick;

        unless(defined $id) {
            $! = custom_errstr
                    "Invalid params: Reminder id was not specified.";

            return;
        }

        my $reminders = $self->{reminders_};
        my @ids = sort map $_->id, grep {
                    ($is_master || $_->nick eq $nick) &&
                    $id eq substr($_->id, 0, length $id)
                } values %$reminders;

        if(@ids > 1) {
            @ids = sort { $a cmp $b } @ids;

            $! = custom_errstr "Ambiguous id: @ids";

            return;
        }

        unless(@ids) {
            $! = custom_errstr "Reminder $id not found.";

            return;
        }

        $id = $ids[0];

        delete $reminders->{$id};

        return "Removed reminder $id.";
    }
}

sub run {
    my ($self) = @_;

    $self->{run_depth_}++;

    STDOUT->autoflush(1);

    $self->log($self->version_str(), handle => \*STDOUT);

    my $timeout_formatter = DateTime::Format::Duration->new(
            normalize => 1,
            pattern => "%r");
MAIN:
    while(1) {
        my ($sock, $selector) = @$self{qw/sock_ selector_/};

        my $now = DateTime->now();
        my $timeout;

        my $reminders = $self->{reminders_};
        my $next_reminder = (sort values %$reminders)[-1];

        if(defined $next_reminder && $next_reminder->when > $now) {
            $timeout = $next_reminder->when
                    ->subtract_datetime_absolute($now)->seconds() + 1;
        }

        # h4x: Automatically varify the health of IRC connection every 5
        # minutes.
        my $select_timeout = $self->{select_timeout};

        if(($timeout //= $select_timeout) > $select_timeout) {
            $timeout = $select_timeout;
        }

        my $friendly_timeout = $timeout_formatter->format_duration_from_deltas(
                seconds => $timeout);

        $self->log("Sleeping for $timeout seconds ($friendly_timeout)...",
                verbose=>1);

        my @handles = $selector->can_read($timeout);

        $self->log("I awake from my slumber to do my chores.",
                verbose => 1);

        if (@handles) {
            for my $rh (@handles) {
                my $isSocket = $rh == $sock;
                my $isStdin = $rh == \*STDIN;

                my $type = $isSocket ? 'socket' :
                           $isStdin ?  'stdin'  :
                                       'unknown';

                eval {
                    my $msg = <$rh>;

                    unless(defined $msg) {
                        $self->log('We appear to have been disconnected...');
                        $self->reconnect();

                        next;
                    }

                    my $now = DateTime->now();

                    chomp $msg;

                    $msg =~ tr/\r//d;

                    next if $msg =~ /^\s*$/;

                    if($isSocket) {
                        $self->log('Reading from socket...', verbose => 1);

                        $self->process_server_message($msg, $now);
                    } elsif($isStdin) {
                        $self->log('Reading from stdin...', verbose => 1);
                        $self->log($msg, handle => \*STDOUT, level => 'STDIN');

                        $self->process_client_command($msg, $now) or last MAIN;
                    } else {
                        $self->log('Unknown handle...', verbose => 1);

                        print Data::Dumper->Dump(
                                [\*STDIN, $sock, $rh],
                                [qw(STDIN sock rh)]);
                    }
                };

                if($@) {
                    my $type = ref $@;

                    # For now we'll use recursion to reload the run
                    # method.
                    if ($type eq 'Bambot::ReloadSignal') {
                        my $message = $@->message;
                        my $source = $@->source;
                        my $status = $@->status;

                        if($source eq 'console') {
                            $self->log($message);
                        } else {
                            $self->privmsg($source, $message);
                        }

                        if ($status) {
                            return $self->run();
                        }
                    } else {
                        $self->log(sprintf
                                'Unhandled exception caught for %s handle: %s',
                                $type,
                                $@);
                    }
                }
            }
        } else {
            # Timeout waiting for activity. Let's just make sure our
            # server connection is healthy.
            $self->ping();
        }

        $self->exec_reminders();
    }

    $self->close();

    return $self;
}

sub russian_roulette {
    my ($self, $target, $nick) = @_;
    my $personalized_for_me = $nick eq $self->{nick};
    my $msg = "";

    my $bullet = $self->{bullet_} //= do {
        $msg = "*brrrrrrrrrrrrzzzt* *chk* ... ";
        int rand 6;
    };

    $msg .= $bullet ?
            "*click* ... " .
                    ($personalized_for_me ?
                    "PHEW! Hahaha, that was close." :
                    "$nick got lucky, THIS time.") :
            "*BANG*" .
                    ($personalized_for_me ?
                    "" :
                    " I regret this immediately. Sorry, $nick...");

    $self->privmsg($target, $msg);

    if ($bullet) {
        $self->{bullet_}--;
    } else {
        $self->{bullet_} = undef;

        if ($personalized_for_me) {
            $self->close();
            sleep(60);
            $self->connect();
            $self->init();

            if(defined $self->{sock_} and defined $self->master->nick) {
                $self->privmsg($self->master->nick, 'Revived...');
            }
        }
    }
}

sub seen {
    my ($self, $target, $nick, $channel) = @_;
}

sub send {
    my ($self, @messages) = @_;

    @messages = map "$_\n", @messages;

    my $sock = $self->{sock_};

    unless(defined $sock) {
        $self->log('Cannot send: socket undefined.');

        $self->reconnect();

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
        carp "Failed to load lyrics file: $!";

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

sub unlink_pid_file {
    my ($self) = @_;
    my $file = $self->{pid_file};

    unlink $file or croak "unlink: $file: $!";

    return $self;
}

sub unload_submodules {
    for (reverse @submodules) {
        Class::Unload->unload($_);
    }
}

sub version_str {
    my ($self) = @_;
    my $initial = $self->{initial_version};
    my $version =$Bambot::VERSION;

    my $str = "This is Bambot v$initial";

    if ($version ne $initial) {
        $str .= " (hot-swapped with v$version)";
    }

    $str .= " running on Perl $PERL_VERSION.";

    return $str;
}

sub write_pid_file {
    my ($self) = @_;
    my $file = $self->{pid_file};

    open my $fh, '> :encoding(UTF-8)', $file or croak "open: $file: $!";

    print $fh $$ or croak "print: $file: $!";

    $fh->close or carp "close: $file: $!";

    return $self;
}

1;
