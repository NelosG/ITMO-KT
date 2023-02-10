SELECT Students.StudentId, Students.StudentName, Students.GroupId
FROM Students
         NATURAL JOIN Plan
         NATURAL JOIN Marks
         NATURAL JOIN Lecturers
WHERE Marks.Mark = :Mark
  AND Lecturers.LecturerName = :LecturerName;