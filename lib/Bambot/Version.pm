#!/usr/bin/perl

package Bambot;

use v5.016;
use strict;
use utf8;
use warnings;

our $VERSION;
our $EST;

BEGIN {
    $EST = '2011-12-19';
    $VERSION = version->declare('0.0.2');
}

1;
