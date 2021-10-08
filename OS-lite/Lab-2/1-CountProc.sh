#!/bin/bash
ps | wc -l > CountProc.txt
ps | awk '{if ($1 != "PID") print $1 " : " $4}' >> CountProc.txt
