#!/bin/bash

echo MENU:
echo "1)'TERM' 2)'QUIT' 3)'KILL'"
Command=""
while [[ $Command != "FINISH" ]]
do
    read Command
    echo "Generator: Token = $Command"
    case $Command in
        "QUIT")
        kill -USR1 $(cat .pid)
        ;;
        "KILL")
        kill -USR2 $(cat .pid)
        ;;
        "TERM")
        kill -SIGTSTP $(cat .pid)
        ;;
        "FINISH")
        kill $(cat .pid)
        ;;
        *) 
        continue
        ;;
    esac
done

