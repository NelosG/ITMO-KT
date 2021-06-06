SET fileForTest=testing.pack.TestClass
SET otherModules=..\..\..\java-advanced-2021
SET packageName=info.kgeorgiy.ja.pushkarev.implementor

java -cp test -p %otherModules%\lib;%otherModules%\artifacts;. -m %packageName%/%packageName%.Implementor %fileForTest% test
