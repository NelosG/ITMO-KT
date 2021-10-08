#!/bin/bash
K=$1
N=$2
echo "start"
> .pid
for ((i=0; i<K; i++))
do
    cat .pid | while true;
    do
        read line
        [ -z $line ] && break
        if [ -n "$(dmesg | egrep "$line" | egrep "newmem.bash")" ]
        then
            echo "fail:$(dmesg | grep -E "$line")"
        fi
    done
    [ -n "$flag" ] &&break
    ./newmem.bash $N &
    echo $! >> .pid
    echo $i
    sleep 1
done


