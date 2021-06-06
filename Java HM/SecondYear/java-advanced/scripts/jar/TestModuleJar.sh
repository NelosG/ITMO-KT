fileForTest=testing.pack.TestClass
otherModules=../../../java-advanced-2021
packageName=info.kgeorgiy.ja.pushkarev.implementor

java -cp test -p $otherModules/lib:$otherModules/artifacts:. -m $packageName/$packageName.Implementor $fileForTest test