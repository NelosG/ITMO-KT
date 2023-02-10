SELECT Students.StudentId, Students.StudentName, Students.GroupId
FROM Students
         NATURAL JOIN Plan
         NATURAL JOIN Marks
WHERE Marks.Mark = :Mark
  AND Plan.LecturerId = :LecturerId;