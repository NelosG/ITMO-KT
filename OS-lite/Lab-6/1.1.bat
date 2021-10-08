chcp 1251
mkdir c:\lab6
cd c:\lab6
ver > ver.txt
wmic os get Caption > wmicCaption.txt 
wmic os get FreePhysicalMemory,TotalVisibleMemorySize > wmicMemory.txt 
wmic LOGICALDISK GET DeviceId, VolumeName, Description, Size > wmicDisk.txt


md TEST
copy .\ .\TEST

cd c:\lab6\TEST
cd.>copy.txt

copy .\ .\copy.txt

