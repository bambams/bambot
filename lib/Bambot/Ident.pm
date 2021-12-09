#!/usr/bin/perl
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

package Bambot::Ident;

use v5.016;
use strict;
use utf8;
use warnings;

use Data::Dumper ();

$Data::Dumper::Sortkeys = 1;
$Data::Dumper::Indent = 1;

sub query_($$) {
    return (${$_[0]} =~ /$_[1]/)[0];
}

sub host($) {
    return shift->query_(qr/@(.+)/);
}

sub ident($) {
    return $$_[0];
}

sub new($) {
    my ($class, $ident) = @_;
    return bless \$ident, $class;
}

sub nick($) {
    return shift->query_(qr/^(.+)!/);
}

sub user($) {
    my ($self) = @_;

    return sprintf "%s@%s", $self->username, $self->host;
}

sub username($) {
    return shift->query_(qr/!~?(.+)@/);
}

1;
