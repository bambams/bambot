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

package Bambot::Random;

use v5.016;
use strict;
use utf8;
use warnings;

use Data::Dumper;
use Encode;

$Data::Dumper::Sortkeys = 1;
$Data::Dumper::Indent = 1;

sub new
{
    my ($class) = @_;
    my $self = bless {}, $class;
    return $self;
}

sub pick_random
{
    my ($self, $list) = @_;

    return $list->[int rand @$list];
}

1;
