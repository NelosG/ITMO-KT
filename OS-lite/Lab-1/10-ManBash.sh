#!/bin/bash
man bash | grep -o -i "[[:alpha:]]\{4,\}" | sort | uniq -c | sort -n | tail -3


