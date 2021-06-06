#!/bin/bash
link=https://docs.oracle.com/en/java/javase/15/docs/api/

prefToSourcePath="../../java-advanced-2021"
sourceJarPath="$prefToSourcePath/artifacts/info.kgeorgiy.java.advanced.implementor.jar"
sourceFilesPath="$prefToSourcePath/modules/info.kgeorgiy.java.advanced.implementor/info/kgeorgiy/java/advanced/implementor"

package="info.kgeorgiy.ja.pushkarev.implementor"

javadoc -quiet -cp ../java-solutions -link $link -author -version -d javaDoc -private \
 $package $sourceFilesPath/Impler.java $sourceFilesPath/JarImpler.java $sourceFilesPath/ImplerException.java
