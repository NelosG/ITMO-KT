#!/usr/bin/perl
use strict;
use warnings FATAL => 'all';

while (<>) {
    s/(\w+)(\W+)(\w+)/$3$2$1/;
    print;
}
