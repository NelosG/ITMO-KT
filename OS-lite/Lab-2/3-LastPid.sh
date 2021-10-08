#!/bin/bash
ps -A -o pid,lstart | sort -r -k 5 -k 4 -nk 3 -k 7 | head -n 1
