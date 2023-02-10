SELECT Students.StudentId, Students.StudentName, Students.GroupId
FROM Students
         NATURAL JOIN Marks
WHERE Marks.Mark = :Mark
  AND Marks.CourseId = :CourseId;