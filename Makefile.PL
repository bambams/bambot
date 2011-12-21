use strict;
use warnings;

use ExtUtils::MakeMaker;

WriteMakefile(
        AUTHOR => 'Brandon McCaig <bamccaig@gmail.com>',
        EXE_FILES => [qw(scripts/bambot)],
        LICENSE => 'GNU GPLv2+',
        NAME => 'Bambot',
        VERSION_FROM => 'lib/Bambot.pm');

