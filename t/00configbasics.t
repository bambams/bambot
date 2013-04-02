#!/usr/bin/perl

use v5.016;
use strict;
use utf8;
use warnings;

use Data::Dumper;
use Test::More;

use lib "lib";

main();
exit;

sub main {
    require_ok('Bambot::Config');

    test_load_nonexist();
    test_load_empty();
    test_load_debug();

    done_testing;
}

sub test_load_debug {
    my $config = Bambot::Config->new(
            config_file => 't/config/debug',
            master_user => 'debugger@localhost',
            port => 27015,
            verbose => 1,
            );

    test_load_file('t/config/debug', $config);
}

sub test_load_empty {
    test_load_file('t/config/empty',
            Bambot::Config->new(config_file => 't/config/empty'));
}

sub test_load_file {
    my ($file, $expected) = @_;
    my $config = new_ok('Bambot::Config' =>
            [config_file => $file]);

    eval {
        ok($config->load());
    };

    diag($@) if $@;
    ok(!$@);

    is_deeply($config, $expected);
}

sub test_load_nonexist {
    my ($file) = @_;
    my $config = new_ok('Bambot::Config' =>
            [config_file => 'nonexist']);

    ok(!$config->load());
}
