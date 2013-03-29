#!/usr/bin/perl

package Bambot::Config;

use v5.016;
use strict;
use utf8;
use warnings;

use Data::Dumper;
use Moose;

use constant {
    DEFAULT_CONFIG_FILE => "$ENV{HOME}/.bambot/config",
    DEFAULT_HOST => 'localhost',
    DEFAULT_LOG_DIR => "$ENV{HOME}/.bambot/logs",
    DEFAULT_LYRICS_FILE => "$ENV{HOME}/.bambot/lyrics",
    DEFAULT_MAX_URLS => 5,
    DEFAULT_NICK => 'bambot',
    DEFAULT_PID_FILE => "$ENV{HOME}/.bambot/pid",
    DEFAULT_PORT => 6667,
    DEFAULT_REALNAME => 'Unknown',
    DEFAULT_USERNAME => 'unknown',
};

my %bools = (
    noexec => { default => 0, },
    verbose => { default => 0, },
    view_log => { default => 0, },
);

my %ints = (
    max_urls => { default => DEFAULT_MAX_URLS, },
    port => { default => DEFAULT_PORT, },
);

my %string_arrays = (
    auto_channels => {},
    friendly_users => {},
    master_nicks => {},
);

my %strings = (
    config_file => { default => DEFAULT_CONFIG_FILE, },
    host => { default => DEFAULT_HOST, },
    ident => {},
    log_dir => { default => DEFAULT_LOG_DIR, },
    lyrics_file => { default => DEFAULT_LYRICS_FILE, },
    master_user => {},
    nick => { default => DEFAULT_NICK, },
    pid_file => { default => DEFAULT_PID_FILE, },
    real_name => { default => DEFAULT_REALNAME, },
    url_file => {},
    username => { default => DEFAULT_USERNAME, },
);

my @config_keys = grep { !($_ ~~ [qw/view_log/]) }
        keys %bools, keys %ints, keys %strings, keys %string_arrays;

my @ignored_config_keys = qw/password/;

while(my ($name, $opts) = each %bools) {
    has $name => ( is => 'rw', isa => 'Bool', %$opts );
}

while(my ($name, $opts) = each %ints) {
    has $name => ( is => 'rw', isa => 'Int', %$opts );
}

while(my ($name, $opts) = each %strings) {
    has $name => ( is => 'rw', isa => 'Str', %$opts );
}

while(my ($name, $opts) = each %string_arrays) {
    has $name => ( is => 'rw', isa => 'ArrayRef[Str]', %$opts );
}

sub load {
    my ($self) = @_;

    open my $fh, '< :encoding(UTF-8)', $self->{config_file} or return 0;

    my $mode = (stat $fh)[2];

    die sprintf 'Insecure config permissions: %04o', $mode & 0777
            if ($mode & 0177) != 0;

    while(my $line = <$fh>) {
        next if $line =~ /^\s*(#|$)/;

        chomp $line;

        if(my ($key, $value) = $line =~ /^(\w+)\s*=\s*(.*)/) {
            next if $key ~~ \@ignored_config_keys;

            unless($key ~~ \@config_keys) {
                warn "invalid config key: $key";
                next;
            }

            # h4x... Surely there's a better way...
            my $attr = __PACKAGE__->meta->get_attribute($key);
            my $attr_type = $attr->{isa};

            my @values = split ' ', $value;
            my $append = @values && substr($values[0], 0, 1) eq '+';

            if($append) {
                $values[0] = substr($values[0], 1);
            }

            eval {
                if($attr_type =~ /^ArrayRef\[/) {
                    if($append) {
                        $self->$key($self->$key(), @values);
                    } else {
                        $self->$key(\@values);
                    }
                } elsif($attr_type =~ /^HashRef\[/) {
                    if($append) {
                        $self->$key({%{$self->$key()}, @values});
                    } else {
                        $self->$key({@values});
                    }
                } else {
                    $self->$key($value);
                }
            };

            if($@) {
                say STDERR $@;
                return 0;
            }

            next;
        }

        warn "invalid config: $line";
    }

    $fh->close or warn "close: $!";

    delete $self->{password};

    return $self;
}

sub password($) {
    my ($self) = @_;

    (map { /[^=]+=\s*(.*)/ and $1 or () } grep /^password\s*=/,
            slurp($self->{config_file}))[-1];
}

__PACKAGE__->meta->make_immutable;
