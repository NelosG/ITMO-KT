#!/bin/bash
inFile="/Users/michael/Desktop/OS-lite/TestFullLogFile.txt"
outFile="full.log"
data=$(grep -E "(WW)|(II)" $inFile | sed -e 's/(WW)/Warning:/; s/(II)/Information:/')
echo "$data" | grep "Warning:" > $outFile
echo "$data" | grep "Information:" >> $outFile
cat $outFile
