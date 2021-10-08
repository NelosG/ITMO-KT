#!/bin/bash
mkfifo my-pipe

echo MENU:
echo "1) +  2) *  3) Digits  4) QUIT"
Command=""
while [[ $Command != "QUIT" ]]
do
    read Command
    case $Command in
        "+"|"*"|[0-9]*)
        echo "Generator: Token = $Command -> Passing data to the handler"
        echo "$Command" >> my-pipe
        ;;
        "QUIT")
        echo "Generator: Command = $Command -> Finish work"
        echo "QUIT" >> my-pipe
        exit 0
        ;;
        *)
        echo "Generator: Unknown token = $Command -> Fatal Error"
        echo "QUIT" >> my-pipe
        exit 1
        ;;
    esac
done
