#!/usr/bin/perl
use strict;
use warnings FATAL => 'all';

my $isJustStarted = 1;
my $isPrevLineEmpty = 0;

while (<>) {
    s/<([^>]*)>//g;
    if (/^\s*$/) {
        if ($isJustStarted == 0) {
            $isPrevLineEmpty = 1;
        }
    } else {
        if ($isJustStarted == 0) {
            print "\n";
        }
        $isJustStarted = 0;
        s/^\s+|\s+$//g;
        s/(\s){2,}/ /g;
        if ($isPrevLineEmpty == 1) {
            $isPrevLineEmpty = 0;
            print "\n";
        }
        print;
    }
}
