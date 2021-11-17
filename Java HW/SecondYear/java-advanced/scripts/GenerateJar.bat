SET source=..\..\java-advanced-2021
SET moduleName=info.kgeorgiy.ja.pushkarev.implementor
SET packagePath=info\kgeorgiy\ja\pushkarev\implementor


XCOPY ..\java-solutions\%packagePath% %moduleName%\%packagePath% /Y /C /I

COPY ..\java-solutions\module-info.txt %moduleName%\module-info.java

javac -p %source%\artifacts\;%source%\lib --module-source-path . -d TEMP -m %moduleName%

jar cmvf jar\Manifest.MF jar\Implementor.jar -C TEMP\%moduleName% .

rd /Q /S TEMP

rd /Q /S %moduleName%
