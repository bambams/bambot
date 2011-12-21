# 
# bambot is a simple IRC bot.
# Copyright (C) 2011 Brandon McCaig
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

use v5.010;
use strict;
use warnings;
use version;

package Bambot;

our $VERSION;
our $EST;

BEGIN
{
    $VERSION = '0.0001';
    $EST = '2011-12-19';
}

use Data::Dumper;
use File::Slurp qw(slurp);
use IO::Handle;
use IO::Select;
use IO::Socket::INET;
use List::Util qw(max);

sub auto_response
{
    my ($self, @responses) = @_;
    print map { (my $res = $_) =~ s/^/AUTO: /gm; $res } @responses;
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

    $self->{sock_} = $sock;
    $self->{selector_}->add($sock);
    #$self->{verbose_} = 1;
    return $self;
}

sub identify
{
    my ($self) = @_;
    my $pwd = $self->{password};

    if(length $pwd > 0)
    {
        $self->auto_response('PRIVMSG NickServ :identify ', $pwd, "\n");
    }
}

sub join_channel
{
    my ($self, @channels) = @_;
    $self->send(map { "JOIN $_\n" } @channels);
    return $self;
}

sub log
{
    my ($self, @messages) = @_;
    my $opts = $messages[-1];
    if(ref $opts)
    {
        return if $opts->{verbose} && !$self->{verbose};
        delete $messages[-1];
    }

    my $messages = join ' ', map { "$_\n" } @messages;
    $messages =~ s/^/DIAGNOSTIC: /gm;

    print STDERR $messages;
}

sub new
{
    my ($class, $config) = @_;
    my $selector = IO::Select->new(\*STDIN);
    my %bot = (
        %$config,
        selector_ => $selector,
    );
    bless \%bot, $class;
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
    if($command =~ /^exit|q(?:uit)?|x$/)
    {
        $self->auto_response("PRIVMSG #allegro :I don't blame you...\n");
        $self->auto_response("QUIT :Shutting down...\n");
        return 0;
    }
    elsif($command =~ m{^/identify$})
    {
        $self->identify();
    }
    elsif($command =~ m{^/j(?:oin)? ([#&]?\w+)})
    {
        $self->auto_response('JOIN ', $1, "\n");
    }
    elsif($command =~ m{^/me ([#&]?\w+) (.+)})
    {
        $self->auto_response('PRIVMSG ', $1, " :\001ACTION ", $2, "\001\n");
    }
    elsif($command =~ m{^/msg ([#&]?\w+) (.+)})
    {
        $self->auto_response('PRIVMSG ', $1, ' :', $2, "\n");
    }
    elsif($command =~ m{^/p(?:art)? ([#&]?\w+) (.*)})
    {
        $self->auto_response('PART ', $1, ' :', $2, "\n");
    }
    elsif($command =~ m{^/register})
    {
        $self->register();
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
    print 'SERVER: ', $msg, "\n";
    if($msg =~ /PING :?([\w\.]+)/)
    {
        $self->pong($1);
    }
    elsif($msg =~ /:(\S+)!\S+ PRIVMSG bambot :?\001VERSION\001/)
    {
        $self->auto_response(
                'NOTICE ',
                $1,
                " :\001VERSION bambot:$VERSION:perl $]\001\n",
                );
    }
    return $self;
}

sub register
{
    my ($self) = @_;
    my $nick = $self->{nick} // 'bambot' . int rand 99;
    my $user = $self->{username} // $ENV{USER} // 'unknown' . rand(99);
    my $real_name = $self->{real_name} // 'Unknown';
    $self->auto_response(<<EOF);
NICK $nick
USER $user 0 0 :$real_name
EOF
    $self->identify();

    return $self;
}

sub run
{
    my ($self) = @_;
    my ($sock, $selector) = @$self{qw/sock_ selector_/};
    STDOUT->autoflush(1);
    $self->register();
    $self->join_channel('#allegro');
    MAIN: while(1)
    {
        my @handles = $selector->can_read;

        if(@handles > 0)
        {
            for my $rh (@handles)
            {
                my $line = <$rh>;

                next unless defined $line;

                chomp $line;

                if($rh == $sock)
                {
                    $self->log('Reading from socket...',
                            { verbose => 1 });
                    $self->process_server_message($line);
                }
                elsif($rh == \*STDIN)
                {
                    $self->log('Reading from stdin...',
                            { verbose => 1 });
                    $self->process_client_command($line) or last MAIN;
                }
                else
                {
                    $self->log('Unknown handle...',
                            {verbose => 1});
                    print Data::Dumper->Dump(
                            [\*STDIN, $sock, $rh],
                            [qw(STDIN sock rh)]);
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
    $self->{sock_}->print(@messages);
}

1;

