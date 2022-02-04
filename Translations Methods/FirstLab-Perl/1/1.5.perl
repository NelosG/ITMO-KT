#!/usr/bin/perl
use strict;
use warnings FATAL => 'all';

while (<>) {
    print if /[xyz](.){5,17}[xyz]/;
}
