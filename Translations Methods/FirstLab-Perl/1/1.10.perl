#!/usr/bin/perl
use strict;
use warnings FATAL => 'all';

while (<>) {
    print if /\b(.+)\1\b/;
}
