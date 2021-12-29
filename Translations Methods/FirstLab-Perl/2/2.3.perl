#!/usr/bin/perl
use strict;
use warnings FATAL => 'all';

while (<>) {
    s/\ba+\b/argh/i;
    print;
}
