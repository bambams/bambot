#!/usr/bin/perl
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

package Bambot::Strings;

use v5.016;
use strict;
use utf8;
use warnings;

use Data::Dumper;

$Data::Dumper::Sortkeys = 1;
$Data::Dumper::Indent = 1;

use Bambot::Random;

our %strings = (
    alcoholic => [
        'Lush! >_>',
        'Why ride the wagon when you can walk...? ::)',
    ],
    its_me => q/Don't shoot, it's me!/,
    help_stub => q/You're gonna need it./,
    loaded_config => q/Nom, nom, nom, ... that's some good config!/,
    query_stub => q/I could tell you if I wasn't so busy. Sorry.../,
    reload => {
        failure => q/Upgrade failed... I can't even do that right./,
        success => "Upgrade complete... \x{02}\x{02}*click* \x{02}\x{02}*click* \x{02}\x{02}*click*  ...Still defective.",
    },
    say_my_name => "Fine, I'll do it.",
    sleep => 'Sleep mode activated...',
    spartaaa => "Madness? \x{02}THIS. IS. %{target}!!!!!11\x{0F}",
    shutdown => q/I don't blame you.../,
);

sub get_string {
    my ($self, $key) = @_;

    return scalar $self->get_strings($key);
}

sub get_strings {
    my ($self, $key, %opts) = @_;
    my $value = \%strings;
    my @keys = split m{\W+}, $key;

    for (@keys) {
        if(ref $value ne 'HASH' || ! defined $value->{$_}) {
            warn "Invalid string: $key";
            return;
        }

        $value = $value->{$_};
    }

    if(ref $value eq 'ARRAY') {
        if(wantarray) {
            return @$value;
        } else {
            $value = $self->{random}->random_pick($value);
        }
    }

    for (keys %opts) {
        $value =~ s/\Q%{$_}/$opts{$_}/;
    }

    my @missing_params = $value =~ /%{(\w+)}/g;

    if(@missing_params) {
        die "The string '$key' requires the following parameters: " .
                join ', ', @missing_params;
    }

    return $value;
}

sub new {
    my ($class) = @_;
    my $self = {
        random => Bambot::Random->new(),
    };

    bless $self, $class;

    return $self;
}

1;
