SELECT Students.StudentId, Students.StudentName, Students.GroupId
FROM Students
         NATURAL JOIN Marks
         NATURAL JOIN Courses
WHERE Marks.Mark = :Mark
  AND Courses.CourseName = :CourseName;