#!/bin/bash
./Task4-Process.sh &
pid1=$!
pid1=$($pid1 | tail -n 1)
echo $pid1
./Task4-Process.sh &
pid2=$!
echo $pid2
./Task4-Process.sh &
pid3=$!
echo $pid3

#echo $(top -b -n 1 -pid $pid1 -pid $pid2 -pid $pid3)
osascript -e '
tell application "Terminal"
    do script "top -pid $pid1"
end tell
'

cpulimit --pid=$pid1 --limit=10 &

sleep 15
kill $pid3
