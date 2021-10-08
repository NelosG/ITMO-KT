#!/bin/bash
MailRegular="[[:alnum:]._-]+@[[:alnum:].-]+\.[[:alpha:]]{2,6}"
grep -E -o -r -h $MailRegular "/etc/" | tr '\n' ',' > emails.lst
