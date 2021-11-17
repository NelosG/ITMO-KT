#!/bin/bash
fileForTest="info.kgeorgiy.java.advanced.implementor.full.classes.standard.FileCacheImageInputStream"

java -jar Implementor.jar $fileForTest test
java -jar Implementor.jar -jar $fileForTest test/$fileForTest.jar
