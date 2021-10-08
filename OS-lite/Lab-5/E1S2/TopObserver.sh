#!/bin/bash

> TopObserverReport.log
pids=$(top -b -n 2 | head -n 12 | tail -n 5 | awk '{print $1}')

pid1=$(echo $pids | awk '{print $1}')
pid2=$(echo $pids | awk '{print $2}')
pid3=$(echo $pids | awk '{print $3}')
pid4=$(echo $pids | awk '{print $4}')
pid5=$(echo $pids | awk '{print $5}')

pidMem1=$(pgrep "mem.bash")
pidMem2=$(pgrep "mem2.bash")
top -d 1 -b -p $pid1 -p $pid2 -p $pid3 -p $pid4 -p $pid4 -p $pid5 -p $pidMem1 -p $pidMem2 >> TopObserverReport.log
