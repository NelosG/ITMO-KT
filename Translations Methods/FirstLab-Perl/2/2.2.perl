#!/usr/bin/perl
use strict;
use warnings FATAL => 'all';

while (<>) {
    s/\bhuman\b/computer/g;
    print;
}
