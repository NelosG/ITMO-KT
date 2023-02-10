-- Используем hash потому что LecturerId внешний ключ и по нему
-- будет часто происходить естаственное соединение.
-- ДЗ-5.3.4.sql - Информацию о студентах с заданной оценкой
-- по дисциплине, которую у него вёл лектор, заданный ФИО
-- ДЗ-5.6.1 - Идентификаторы студентов по преподавателю,
-- имеющих хотя бы одну оценку у преподавателя
-- ДЗ-5.6.2 - Идентификаторы студентов по преподавателю,
-- не имеющих ни одной оценки у преподавателя
CREATE UNIQUE INDEX LIdIndex ON Lecturers USING hash (LecturerId);

-- ДЗ-5.3.4.sql Информация о студентах с :Mark по предмету, который
-- у него вёл :LecturerName
-- ДЗ-5.3.6 Информация о студентах с :Mark по предмету у :LName
-- Дз-5.6.1 Идентификаторы студентах по :LecturerName
CREATE INDEX LNIndex ON Lecturers USING btree (LecturerName, LecturerId);