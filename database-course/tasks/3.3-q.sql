-- Вывести курсы, которые по плану
-- преподает преподаватель :LecturerName
SELECT DISTINCT CourseId, CourseName
FROM Courses
         NATURAL JOIN Lecturers
         NATURAL JOIN Plan
WHERE LecturerName = :LecturerName;