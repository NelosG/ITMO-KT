#!/bin/bash
source=../../java-advanced-2021
moduleName=info.kgeorgiy.ja.pushkarev.implementor
packagePath=info/kgeorgiy/ja/pushkarev/implementor


mkdir -p $moduleName/$packagePath/

cp ../java-solutions/$packagePath/*.java $moduleName/$packagePath/

cp ../java-solutions/module-info.txt $moduleName/module-info.java

javac -p $source/artifacts:$source/lib --module-source-path . -d TEMP -m $moduleName

jar cmvf jar/Manifest.MF jar/Implementor.jar -C TEMP/$moduleName .

rm -r TEMP
rm -r $moduleName
