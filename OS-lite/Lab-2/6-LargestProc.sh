#!/bin/bash

MaxProc=0

for pid in $(ps -a -x -o pid --sort=ppid | grep -E [[:digit:]])
do
    statusFile="/proc/"$pid"/status"
    CurProc=$(grep -i VMSIZE $statusFile | awk '{print $2}')
    if [[ "$CurProc" -gt "$MaxProc" ]]
    then
        MaxProc=$CurProc
    fi
done
echo $MaxProc

echo $(top -b -n 1 -o +%MEM | head -n 10 | tail -n 3 | head -n 1| awk '{print $5}')
