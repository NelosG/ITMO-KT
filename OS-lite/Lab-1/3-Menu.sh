#!/bin/bash
echo MENU:
echo "1) nano  2) vi  3) links  4) exit"
Command=""
while [[ $Command != "4" ]]
do
read Command
case $Command in
    "1" )
    nano
    ;;
    "2" )
    vi
    ;;
    "3" )
    links
    ;;
    "4" )
    ;;
esac
done
