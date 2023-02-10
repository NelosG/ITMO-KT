SELECT Students.StudentName, Courses.CourseName
FROM (SELECT Students.StudentId, Plan.CourseId
      FROM Students
               NATURAL JOIN Plan
      EXCEPT
      SELECT Students.StudentId, Marks.CourseId
      FROM Students
               NATURAL JOIN Marks
      WHERE Marks.Mark = 4
         OR Marks.Mark = 5) S
         NATURAL JOIN Students
         NATURAL JOIN Courses;