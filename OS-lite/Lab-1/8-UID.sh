#!/bin/bash
awk -F: '{ print $1 " " $3 }' /etc/passwd | sort -n -k 2
