SELECT Students.StudentName, Courses.CourseName
FROM (SELECT Students.StudentId, Plan.CourseId
      FROM Students
               NATURAL JOIN Plan
      EXCEPT
      SELECT Students.StudentId, Marks.CourseId
      FROM Students
               NATURAL JOIN Marks) S
               NATURAL JOIN Students
               NATURAL JOIN Courses;