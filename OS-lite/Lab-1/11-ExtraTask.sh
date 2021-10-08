#!/bin/bash
# Задание: создать текстовый документ, в который поместить список файлов из директории /var/log, записав данные через двойное пространство между строками
ls -p /var/log | grep -E "[^/$]" | awk '{print $0; print "\n"}' > TextFile.txt
