SELECT StudentName, CourseName
FROM (SELECT DISTINCT StudentId, StudentName, CourseId
      FROM Students,
           Plan
      WHERE Students.GroupId = Plan.GroupId
        AND NOT EXISTS
          (SELECT StudentId, CourseId
           FROM Marks
           WHERE Marks.StudentId = Students.StudentId
             AND Marks.CourseId = Plan.CourseId
             AND Mark > 2)) D,
     Courses
WHERE D.CourseId = Courses.CourseId;