#!/usr/bin/perl

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

sub compare_
{
    my ($lhs, $rhs) = @_;

    return $lhs->when <=> $rhs->when;
}

sub smart_match_
{
    my ($lhs, $rhs) = @_;
    return $lhs == $rhs;
}

sub equals($$)
{
    my ($lhs, $rhs) = @_;

    return $lhs->msg eq $rhs->msg &&
            $lhs->nick eq $rhs->nick &&
            $lhs->target eq $rhs->target &&
            $lhs->when == $rhs->when;
}

sub new($$$$$)
{
    my ($class, $target, $nick, $when, $msg) = @_;
    my $self = [$msg, $nick, $target, $when];
    bless $self, $class;
    return $self;
}

sub to_string($)
{
    my ($self) = @_;
    return sprintf "In %s to %s on %s about '%s'",
            $self->target,
            $self->nick,
            $self->when,
            $self->msg;
}

sub msg($) { return shift->[0]; }
sub nick($) { return shift->[1]; }
sub target($) { return shift->[2]; }
sub when($) { return shift->[3]; }

1;
