-- Используем hash потому что StudentId внешний ключ и по нему
-- будет часто происходить естаственное соединение.
-- ДЗ-5.7.1. Группы и предметы, такие что все
-- студенты группы сдали предмет.
-- ДЗ-5.9.1. Средний балл одного студента :StudentId.
-- ДЗ-5.9.3. Средний балл каждой группы.
CREATE UNIQUE INDEX SIdIndex ON Marks USING hash (StudentId);

-- Используем hash потому что CourseId внешний ключ и по нему
-- будет часто происходить естаственное соединение.
-- ДЗ-6.2.2. Полную информацию о студентах для всех студентов,
-- не имеющих оценки по предмету :CourseId.
-- ДЗ-6.2.3. Полную информацию о студентах для всех студентов,
-- не имеющих оценки по предмету :CourseName.
-- ДЗ-6.2.4. Полную информацию о студентах для всех студентов,
-- не имеющих оценки по предмету :CourseId
CREATE UNIQUE INDEX CIdIndex ON Marks USING hash (CourseId);

-- покрывающий индекс
-- ДЗ-5.3.1 Информация о студентах с :Mark по предмету :CourseId
-- ДЗ-5.3.2 Информация о студентах с :Mark по предмету :CourseName
-- ДЗ-5.3.3 Информация о студентах с :Mark по предмету, который
-- вёл :LecturerName
CREATE INDEX MCIndex ON Marks USING btree (Mark, CourseId);