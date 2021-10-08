#!/bin/bash

> CPUBurst.out
for pid in $(ps -a -x -o pid --sort=ppid | grep -E [[:digit:]])
do
    statusFile="/proc/"$pid"/status"
    schedFile="/proc/"$pid"/sched"
    ppid=$(grep -s "PPid" $statusFile | awk '{print $2}')
    if [ -z $ppid ]
    then
        continue
    fi
    sum_exec_runtime=$(grep -s "se.sum_exec_runtime" $schedFile | awk '{print $3}')
    nr_switches=$(grep -s "nr_switches" $schedFile | awk '{print $3}')
    Average_Running_Time=$(echo "$sum_exec_runtime/$nr_switches" | bc -l)
    echo ProcessID=$pid : Parent_ProcessID=$ppid : Average_Running_Time=$Average_Running_Time
done >> CPUBurst.out
