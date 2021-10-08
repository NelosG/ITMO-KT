#!/bin/bash
awk '{if ($2 == "INFO") print}' /var/log/anaconda/syslog > info.log
#/Users/michael/Desktop/OS-lite/TestLogFile.txt
#/var/log/anaconda/syslog
