chcp 1251
driverquery /NH /FO TABLE > drivers.txt
sort /R drivers.txt > reverse_drivers.txt

goto end

------------------
/NH - не отображать заголовки столбцов в выходных данных, представляемых в формате "TABLE" и "CSV".

/FO формат - формат выводимых данных. Допустимые значения - таблица (TABLE), список (LIST), значения с разделителями (CSV)

/R вроде логично
-----------------

:end