#!/usr/bin/perl

use v5.022;
use strict;
use utf8;
use warnings;

package Bambot::ReloadSignal;

sub message {
    return shift->{message};
}

sub new {
    my ($class, $source, $status, $message) = @_;

    my $self = {
        message => $message,
        source => $source,
        status => $status
    };

    return bless $self, $class;
}

sub source {
    return shift->{source};
}

sub status {
    return shift->{status};
}

1;
