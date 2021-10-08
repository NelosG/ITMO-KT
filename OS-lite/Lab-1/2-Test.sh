#!/bin/bash
Str=""
Ans=""
while [[ $Str != "q" ]]
do
    Ans=$Ans$Str
    read Str
done

echo $Ans
exit 0
