#!/bin/bash

echo MENU:
echo "1) +  2) *  3) TERM"
Command=""
while [[ $Command != "TERM" ]]
do
    read Command
    case $Command in
        "+")
        echo "Generator: Token = $Command -> Passing signal = 'USR1' to the handler"
        kill -USR1 $(cat .pid)
        ;;
        "*")
        echo "Generator: Token = $Command -> Passing signal = 'USR2' to the handler"
        kill -USR2 $(cat .pid)
        ;;
        "TERM")
        echo "Generator: Command = $Command -> Finish work && Passing signal = 'SIGTERM' to the handler"
        kill -SIGTERM $(cat .pid)
        exit 0
        ;;
        *)
        continue
        ;;
    esac
done
