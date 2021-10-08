#!/bin/bash

ppid=$$
echo "Handler: My ppid is $ppid"
operation="+"
answer=1

(tail -f my-pipe) |
while true
do
    read Line
    case $Line in
        "QUIT")
        echo "Handler: Line = $Line -> Finish work"
        pkill -P $ppid
        exit 0
        ;;
        "+"|"*")
        operation=$Line
        echo "Handler: Current operation is ($operation)"
        ;;
        *)
        echo "Handler: Current evaluation is $answer $operation $Line"
        case $operation in
            "+")
            answer=$(($answer + $Line))
            ;;
            "*")
            answer=$(($answer * $Line))
            ;;
        esac
        echo "Handler: Current answer is $answer"
        ;;
    esac
done

