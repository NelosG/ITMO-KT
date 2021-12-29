#!/usr/bin/perl
use strict;
use warnings FATAL => 'all';

while (<>) {
    s/(a(.)*?a)(a(.)*?a)(a(.)*?a)/bad/g;
    print;
}
