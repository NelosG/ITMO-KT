#!/bin/bash

dmesg -c
> CheckReport.log

for line in $(cat .pid)
do
    dmesg | grep -E -e "pid=$line" -e "Killed process $line" >> CheckReport.log
    echo -e "\n" >> CheckReport.log
done
