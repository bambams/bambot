# 
# bambot is a simple IRC bot.
# Copyright (C) 2011, 2012 Brandon McCaig
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

package Bambot;

our $VERSION;
our $EST;

BEGIN
{
    $VERSION = version->declare('0.0.1');
    $EST = '2011-12-19';
}

use Class::Unload;
use Data::Dumper;
use Encode;
use File::Slurp qw(edit_file slurp);
use IO::Handle;
use IO::Select;
use IO::Socket::INET;
use List::Util qw(max);

sub _is_substitution
{
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
                $1 ne $2)
        {
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
            $2 ne $3)
    {
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

sub add_urls
{
    my ($self, $msg) = @_;
    my @urls = $msg =~ m{\b(https?://\S+)}gi;
    if(@urls)
    {
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
    }
}

sub auto_response
{
    my ($self, @responses) = @_;
    my $response = join '', @responses;
    $response =~ s/^/AUTO: /gm; 
    print encode('UTF-8', $response);
    $self->send(@responses);
    return $self;
}

sub connect
{
    my ($self) = @_;
    my $sock = IO::Socket::INET->new(
            PeerAddr => $self->{host} // 'localhost',
            PeerPort => $self->{port} // 6667,
            Proto => 'tcp',
            ) or die "IO::Socket::INET::new: $!";

    binmode $sock;
    $self->{sock_} = $sock;
    $self->{selector_}->add($sock);
    #$self->{verbose_} = 1;
    return $self;
}

sub identify
{
    my ($self) = @_;
    my $pwd = (map { /[^=]+=\s*(.*)/; $1 } grep { /^password/ }
            slurp($self->{config_file}))[-1];
    if(defined $pwd && length $pwd > 0)
    {
        $self->auto_response('PRIVMSG NickServ :identify ', $pwd, "\n");
    }
    return $self;
}

sub join_channel
{
    my ($self, @channels) = @_;
    @channels = map {
        (my $channel = $_) =~ s/^([^&#])/#$1/;
        $channel
    } @channels;
    $self->auto_response(map { "JOIN $_\n" } @channels);
    return $self;
}

sub load
{
    my ($self) = @_;
    open my $fh, '<', $self->{config_file} or return 0;
    my $mode = (stat $fh)[2];
    die sprintf 'Insecure config permissions: %04o', $mode & 0777
            if ($mode & 0177) != 0;
    while(my $line = decode('UTF-8', <$fh>))
    {
        next if $line =~ /^\s*(#|$)/;
        chomp $line;
        if($line =~ /^(\w+)\s*=\s*(.*)/)
        {
            my $type = ref $self->{$1};
            my @values = split ' ', $2;
            my $append = @values && substr($values[0], 0, 1) eq '+';
            if($append)
            {
                $values[0] = substr($values[0], 1);
            }
            if($type eq 'ARRAY')
            {
                if($append)
                {
                    push @{$self->{$1}}, @values;
                }
                else
                {
                    $self->{$1} = \@values;
                }
            }
            elsif($type eq 'HASH')
            {
                if($append)
                {
                    my %values = @values;
                    @{$self->{$1}}{keys %values} = values %values;
                }
                else
                {
                    $self->{$1} = {@values};
                }
            }
            else
            {
                $self->{$1} = $2;
            }
            next;
        }
        warn "invalid config: $line";
    }
    close $fh or warn "close: $!";
    delete $self->{password};
    return $self;
}

sub log
{
    my ($self, $message, %opts) = @_;
    return if $opts{verbose} && !$self->{verbose};
    print STDERR 'DIAGNOSTIC: ', $message;
    return $self;
}

sub new
{
    my ($class, $config) = @_;
    my $selector = IO::Select->new(\*STDIN);
    my $self = {
        %$config,
        channels => [],
        friendly_idents => [],
        selector_ => $selector,
    };
    bless $self, $class;
    $self->load;
    return $self;
}

sub pong
{
    my ($self, @servers) = @_;
    $self->auto_response("PONG @servers\n");
    return $self;
}

sub process_client_command
{
    my ($self, $command) = @_;
    if($command =~ m{^/ctcp (\S+) (.+)})
    {
        $self->auto_response("PRIVMSG $1 :\001$2\001\n");
    }
    elsif($command =~ m{^/eval (.+)})
    {
        my @results = eval $1 or warn $@;
        print encode('UTF-8', Dumper \@results);
    }
    elsif($command =~ /^exit|q(?:uit)?|x$/)
    {
        $self->auto_response("QUIT :Shutting down...\n");
        return 0;
    }
    elsif($command =~ m{^/identify$})
    {
        $self->identify();
    }
    elsif($command =~ m{^/j(?:oin)? ([#&]?\w+)})
    {
        $self->join_channel($1);
    }
    elsif($command =~ m{^/load$})
    {
        $self->load;
    }
    elsif($command =~ m{^/me ([#&]?\w+) (.+)})
    {
        $self->auto_response('PRIVMSG ', $1, " :\001ACTION ", $2,
                "\001\n");
    }
    elsif($command =~ m{^/msg ([#&]?\w+) (.+)})
    {
        $self->auto_response('PRIVMSG ', $1, ' :', $2, "\n");
    }
    elsif($command =~ m{^/nick (\w+)})
    {
        $self->set_nick($1);
    }
    elsif($command =~ m{^/p(?:art)? ([#&]?\w+) (.*)})
    {
        $self->auto_response('PART ', $1, ' :', $2, "\n");
    }
    elsif($command =~ m{^/register})
    {
        $self->register();
    }
    elsif($command =~ m{^/reload$})
    {
        $self->reload;
    }
    elsif($command =~ m{^/restart$})
    {
        $self->log('Restarting ...');
        exec("$0 @{$self->{ARGV}}");
    }
    else
    {
        $self->send($command, "\n");
    }
    return $self;
}

sub process_server_message
{
    my ($self, $msg) = @_;
    print 'SERVER: ', encode('UTF-8', $msg), "\n";
    if($msg =~ /^PING :?([\w\.]+)/)
    {
        $self->pong($1);
    }
    elsif($msg =~ /:(\S+) PRIVMSG (\S+) :?(.*)/)
    {
        my ($sender, $target, $msg) = ($1, $2, $3);
        my ($nick, $ident) = $sender =~ /(\S+)!~?(\S+)/;
        my $is_master = $ident =~ /^\Q$self->{master}\E$/;
        my $is_friendly = $self->{friendly_idents} ~~ /^\Q$ident\E$/;
        $target = $target eq $self->{nick} ? $nick : $target;
        my $log = $self->{log_}{$target}{$ident} //= [];
        $self->add_urls($msg);
        my $is_ctcp = $msg =~ /^\001(.*)\001/;
        my $is_substitution = $self->_is_substitution($msg,
                \(my $substitution));
        if($is_ctcp)
        {
            say STDERR 'CTCP: ', encode('UTF-8', $1) if $self->{verbose};
            if($1 eq 'VERSION')
            {
                $self->auto_response(
                        'NOTICE ',
                        $nick,
                        " :\001VERSION bambot:$VERSION:perl $]\001\n",
                        );
            }
            elsif($1 =~ /^PING\b/)
            {
                $self->auto_response(
                        'NOTICE ',
                        $nick,
                        " :\001PONG\001\n",
                        );
            }
        }
        elsif($is_friendly &&
                $msg eq "$self->{nick}: help")
        {
            $self->auto_response('PRIVMSG ', $target,
                    " :You're gonna need it.\n");
        }
        elsif($is_friendly &&
                $msg =~ /^\s*are\s+you\s+still\s+there\s*\?\s*$/i)
        {
             $self->auto_response(
                    'PRIVMSG ',
                    $target,
                    " :Don't shoot, it's me!\n");
        }
        elsif($is_friendly &&
                $msg =~
                /^(?:
                        (?:$self->{nick})[:,\s]\s*)?
                        say\s+my\s+name[,\s+]\s*
                        say\s+my\s+name\s*[\.!1]*\s*$/ix)
        {
            $self->auto_response(
                    'PRIVMSG ',
                    $target,
                    " :$nick: Fine, I'll do it.\n");
        }
        elsif($is_friendly &&
                $msg =~ m{^bambot:?\s+.*\\o/\s*$})
        {
            $self->auto_response('PRIVMSG ', $target, ' :', $nick,
                    ": \\o/\n");
        }
        elsif($is_friendly && $is_substitution)
        {
            if(my ($old_msg_ref) = map { \$_; }
                    (grep { /\Q$substitution->{pattern}\E/ } @$log)[-1])
            {
                my ($pat, $rep, $glob) = @$substitution{
                        qw(pattern replacement global)};
                if($glob)
                {
                    $$old_msg_ref =~ s/\Q$pat\E/\x02$rep\x0F/g;
                }
                else
                {
                    $$old_msg_ref =~ s/\Q$pat\E/\x02$rep\x0F/;
                }
                $self->auto_response('PRIVMSG ', $target, ' :', $nick,
                        " meant to say: $$old_msg_ref\n",
                        );
            }
        }
        elsif(0 && $is_master && $msg =~ /
                drunk|intoxicated|
                beer|carling|budweiser|steam whistle|
                amaretto|rum|scotch|vodka|whiskey/ix)
        {
            my @responses = (
                    'Lush! >_>',
                    'Why ride the wagon when you can walk...?  ::)',
                    );
            my $response = $responses[int rand @responses];
            $self->auto_response('PRIVMSG ', $target, ' :', $nick,
                    ": $response\n");
        }
        elsif($is_friendly && $msg =~ /^~\s+\S/)
        {
            $self->auto_response('PRIVMSG ', $target,
                    " :I could tell you if I wasn't so busy. Sorry...\n");
        }
        elsif($is_master && $msg eq '~activate')
        {
            $self->auto_response(
                    "PRIVMSG $target :Sentry mode activated..\n");
        }
        elsif($is_master && $msg eq '~deactivate')
        {
            $self->auto_response(
                    "PRIVMSG $target :Sleep mode activated..\n");
        }
        elsif($is_master && $msg =~ /^~eval (.*)/)
        {
            my $result = "eval: $1";
            $self->auto_response('PRIVMSG ', $target, ' :', $nick, ': ',
                    $result, "\n");
        }
        elsif($is_master && $msg eq '~load')
        {
            $self->log('Master issued ~load...');
            $self->auto_response(
                    "PRIVMSG $target :Nom, nom, nom, ... ",
                    "that's some good config!\n")
                    if $self->load;
        }
        elsif($is_master && $msg eq '~reload')
        {
            $self->log('Master issued ~reload...');
            $self->auto_response("PRIVMSG $target :Upgrade complete ...",
                    q{ I hope you didn't disable "Linux" },
                    "(I'm looking at you, Sony)...\n")
                    if $self->reload;
        }
        elsif($is_master && $msg =~ /^~shutdown$/)
        {
            $self->auto_response(
                    "PRIVMSG $target :$nick: I don't blame you...\n");
            $self->auto_response("QUIT :Shutting down...\n");
        }
        elsif($is_friendly && $msg eq '~sing')
        {
            my @wannabee_lyrics = (
                " :It's Friday, Friday, gotta get down on Friday...",
                "Oh, oh, oh, it's Thanksgiving... " .
                        "We, we, we are gonna have a good time...",
            );
            my $wannabee_lyric =
                    $wannabee_lyrics[int rand @wannabee_lyrics];
            $self->auto_response(
                    'PRIVMSG ', $target, ' :', $wannabee_lyric, "\n");
        }
        if(!$is_ctcp && $msg eq '\\o/')
        {
            $self->{'\\o/'}++;
            if($self->{'\\o/'} > 1 && !$self->{'\\o/ed'})
            {
                $self->auto_response('PRIVMSG ', $target, ' :', "\\o/\n");
                $self->{'\\o/ed'} = 1;
            }
        }
        else
        {
            $self->{'\\o/'} = 0;
            $self->{'\\o/ed'} = 0;
        }
        unless($is_ctcp || $is_substitution)
        {
            push @$log, $msg;
            shift @$log while @$log > 5;
        }
    }
    return $self;
}

sub register
{
    my ($self) = @_;
    my $nick = $self->{nick} // 'bambot' . int rand 99;
    my $user = $self->{username} // $ENV{USER} // 'unknown' . rand(99);
    my $real_name = $self->{real_name} // 'Unknown';
    $self->set_nick($self->{nick});
    $self->auto_response('USER ', $user, ' 0 0 :', $real_name, "\n");
    $self->identify();

    return $self;
}

sub reload
{
    my $pkg = __PACKAGE__;
    unless(eval "require $pkg")
    {
        warn $@;
        return 0;
    };
    Class::Unload->unload($pkg);
    eval "require $pkg";
    return 0 if $@;
    return 1;
}

sub run
{
    my ($self) = @_;
    my ($sock, $selector) = @$self{qw/sock_ selector_/};
    STDOUT->autoflush(1);
    $self->register();
    $self->join_channel(@{$self->{channels}});
    MAIN: while(1)
    {
        my @handles = $selector->can_read;

        if(@handles > 0)
        {
            for my $rh (@handles)
            {
                my $msg = decode('UTF-8', <$rh>);

                next unless defined $msg;

                chomp $msg;
                $msg =~ tr/\r//d;

                if($rh == $sock)
                {
                    $self->log('Reading from socket...', verbose => 1);
                    $self->process_server_message($msg);
                }
                elsif($rh == \*STDIN)
                {
                    $self->log('Reading from stdin...', verbose => 1);
                    say STDERR 'STDIN: ', encode('UTF-8', $msg);
                    $self->process_client_command($msg) or last MAIN;
                }
                else
                {
                    $self->log('Unknown handle...', verbose => 1);
                    print encode('UTF-8', Data::Dumper->Dump(
                            [\*STDIN, $sock, $rh],
                            [qw(STDIN sock rh)]));
                }
            }
        }
    }

    close($sock);

    return $self;
}

sub send
{
    my ($self, @messages) = @_;
    @messages = map encode('UTF-8', $_), @messages;
    $self->{sock_}->print(@messages);
    return $self;
}

sub set_nick
{
    my ($self, $nick) = @_;
    $self->auto_response('NICK ', $nick, "\n");
    $self->{nick} = $nick;
    return $self;
}

1;

