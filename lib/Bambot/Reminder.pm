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

package Bambot::Reminder;

use v5.016;
use strict;
use utf8;
use warnings;

use overload (
    '==' => \&equals,
    '~~' => \&smart_match_,
    '<=>' => \&compare_,
    '""' => \&to_string,
);

use Data::Dumper;
use Encode;

$Data::Dumper::Sortkeys = 1;
$Data::Dumper::Indent = 1;

sub compare_ {
    my ($lhs, $rhs) = @_;

    return $lhs->when <=> $rhs->when;
}

sub smart_match_ {
    my ($lhs, $rhs) = @_;

    return $lhs == $rhs;
}

sub equals($$) {
    my ($lhs, $rhs) = @_;

    return $lhs->msg eq $rhs->msg &&
            $lhs->nick eq $rhs->nick &&
            $lhs->target eq $rhs->target &&
            $lhs->when == $rhs->when;
}

sub new($$$$$) {
    my ($class, $target, $nick, $when, $msg) = @_;
    my $self = [$msg, $nick, $target, $when];

    bless $self, $class;

    return $self;
}

sub to_string($) {
    my ($self) = @_;

    my ($t, $n) = ($self->target, $self->nick);

    return sprintf "In %s to %s on %s about '%s'",
            ($t eq $n ? 'private' : $t),
            $n,
            $self->when,
            $self->msg;
}

sub msg($) { return shift->[0]; }
sub nick($) { return shift->[1]; }
sub target($) { return shift->[2]; }
sub when($) { return shift->[3]; }

1;
