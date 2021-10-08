#!/bin/bash

declare -A ArrayOfReadBytes

index=0
for pid in $(ps -a -x -o pid --sort=pid | grep -E [[:digit:]])
do
    statusFile="/proc/"$pid"/status"
    ioFile="/proc/"$pid"/io"
    
    ArrayOfReadBytes[$pid]=$(sudo grep -s "read_bytes:" $ioFile | awk '{print $2}')
    
    index=$(($index+1))
done
sleep 60

buffer=""
for pid in ${!ArrayOfReadBytes[@]}
do
    ioFile="/proc/"$pid"/io"
    
    ReadBytesEnd=$(sudo grep -s "read_bytes:" $ioFile | awk '{print $2}')
    if [ -z ${ArrayOfReadBytes[$pid]} ]
    then
        continue
    fi
    ArrayOfReadBytes[$pid]=$(($ReadBytesEnd-${ArrayOfReadBytes[$pid]}))
    pathCMD=$(cat /proc/"$pid"/cmdline)
    if [ -z $pathCMD ]
    then
        pathCMD="--EmptyCmdLine--"
    fi
    buffer="$buffer$pid ${ArrayOfReadBytes[$pid]} $pathCMD\n"
done
echo -e $buffer | sort -k 2 | tail -3 | awk '{print "PID="$1 " : CMD="$3" : ReadBytes="$2}'
