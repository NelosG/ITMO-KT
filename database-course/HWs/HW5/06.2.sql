SELECT Students.StudentId
FROM Students
EXCEPT
SELECT Students.StudentId
FROM Students
         NATURAL JOIN Marks
         NATURAL JOIN Plan
         NATURAL JOIN Lecturers
WHERE Lecturers.LecturerName = :LecturerName;