#!/bin/bash
> report
mkdir "test" && {
echo "catalog test was created successfully" >> report;
> ./"test"/$(date +"%F_%R")
}

ping -c 1 "www.net_nikogo.ru" || echo "$(date +"%F_%R") Unknown host" >> report
