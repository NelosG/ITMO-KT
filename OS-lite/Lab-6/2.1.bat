chcp 1251

./copy2.2.bat
copy2.2.bat :
	xcopy /Z C:\Users\Gleb\Desktop\TestFile.txt \\%computername%\temp\CopyFile.txt

schtasks /Create /SC MINUTE /TN copy2.2 /TR copy2.2.bat

schtasks /Query | find "copy"
schtasks /Delete /TN copy2.2

fc C:\Users\Gleb\Desktop\TestFile.txt \\%computername%\temp\CopyFile.txt > result.txt

./copy2.2.bat