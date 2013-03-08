use strict;
use warnings;

use ExtUtils::MakeMaker;

WriteMakefile(
        AUTHOR => 'Brandon McCaig <bamccaig@gmail.com>',
        EXE_FILES => [qw(scripts/bambot)],
        LICENSE => 'GNU GPLv2+',
        NAME => 'Bambot',
        PREREQ_PM => {
            'Class::Unload' => 0,
            'Data::Dumper' => 0,
            'DateTime' => 0,
            'Encode' => 0,
            'ExtUtils::MakeMaker' => 0,
            'File::Slurp' => 0,
            'Getopt::Long' => 0,
            'IO::Handle' => 0,
            'IO::Select' => 0,
            'IO::Socket::INET' => 0,
            'List::Util' => 0,
            'local::lib' => 0,
        },
        VERSION_FROM => 'lib/Bambot/Version.pm');
