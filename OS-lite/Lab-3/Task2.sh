#!/bin/bash
file="Task1.sh"
at -f $file now + 1 minute
tail -n 0 -f ./report


