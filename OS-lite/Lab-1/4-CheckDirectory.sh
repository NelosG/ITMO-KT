#!/bin/bash

if [[ $PWD == $HOME ]]
then
    echo $PWD
    exit 0
else
    echo ERROR 404
    exit 1
fi
