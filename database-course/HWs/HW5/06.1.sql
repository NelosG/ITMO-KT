SELECT DISTINCT Students.StudentId
FROM Students
         NATURAL JOIN Marks
         NATURAL JOIN Plan
         NATURAL JOIN Lecturers
WHERE Lecturers.LecturerName = :LecturerName;