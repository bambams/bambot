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
package Bambot::Eval;

use v5.016;
use strict;
use warnings;
use utf8;

use Data::Dumper;
use JSON qw/decode_json/;
use LWP::UserAgent;

use constant (
    URL_FORMAT => 'http://api.dan.co.jp/lleval.cgi?c=callback&l=%s&s=%s'
);

our %language_map = (
    bash => sh,
    commonlisp => lsp,
    ecmascript6 => es6,
    emacs => el,
    emacslisp => el,
    #ghostscript => ps,
    harmony => es6,
    lisp => lsp,
    node => js,
    ocaml => ml,
    perl => pl,
    perl6 => p6,
    #postscript => ps,
    python => py,
    python3 => py3,
    ruby20 => rb20,
    ruby21 => rb,
    ruby21 => rb21,
    scheme => scm,
);

sub eval {
    my ($self, $lang, $source) = @_;

    $lang = $self->map_language($lang);

    my $ua = LWP::UserAgent->new();
    my $url = sprintf URL_FORMAT(), $lang, $source;
    my $req = HTTP::Request->new(GET => $url);
    my $res = $ua->request($req);
    my $status = $res->code;

    if ($status != 200) {
        return $res;
    }

    my $content = $res->content;
    my ($json) = $content =~ /^callback\((.*)\)$/;

    my $result = decode_json($json);

    return $result;
}

sub map_language {
    my ($self, $lang) = @_;

    return exists $language_map{$lang} ? $language_map{$lang} : $lang;
}

sub new {
    my ($class) = @_;

    return bless {}, $class;
}

1;
