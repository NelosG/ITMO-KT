#!/usr/bin/perl
use strict;
use warnings FATAL => 'all';

while (<>) {
    print if /^(0|((0)*1(0(1)*0)*1(0)*)*)$/;
}
