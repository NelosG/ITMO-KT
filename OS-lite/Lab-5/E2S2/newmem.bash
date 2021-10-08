#!/bin/bash
declare -a array
N=$1

while true; do
    array+=( 1 2 3 4 5 6 7 8 9 10 )
    [ ${#array[@]} -gt $N ] && exit 0
done


