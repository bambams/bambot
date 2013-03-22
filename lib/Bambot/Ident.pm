#!/usr/bin/perl

package Bambot::Ident;

use v5.016;
use strict;
use utf8;
use warnings;

use Data::Dumper;

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
