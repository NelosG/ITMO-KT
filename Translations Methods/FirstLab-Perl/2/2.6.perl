#!/usr/bin/perl
use strict;
use warnings FATAL => 'all';

while (<>) {
    s/(\w)\1/$1/g;
    print;
}
