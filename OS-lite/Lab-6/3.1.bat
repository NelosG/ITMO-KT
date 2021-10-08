chcp 1251
goto start2

-----------------
:start1


sc queryex type=service state=all | find /i "SERVICE_NAME:" > C:\lab6\net_1.txt
sc stop dnscache
timeout /t 10
sc queryex type=service state=all | find /i "SERVICE_NAME:" > C:\lab6\net_2.txt
fc C:\lab6\net_1.txt C:\lab6\net_2.txt > net_res.txt
sc start dnscache

goto end

неработает((((
-----------------



:start2
net start > C:\lab6\net_1.txt
net stop dnscache
timeout /t 10
net start > C:\lab6\net_2.txt
fc C:\lab6\net_1.txt C:\lab6\net_2.txt > net_res.txt
net start dnscache
:end