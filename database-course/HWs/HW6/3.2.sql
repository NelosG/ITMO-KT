SELECT StudentName, CourseName
FROM (SELECT StudentId, CourseId
      FROM Students,
           Plan
      WHERE Students.GroupId = Plan.GroupId
      UNION
      SELECT StudentId, CourseId
      FROM Marks) RES,
     Students,
     Courses
WHERE RES.StudentId = Students.StudentId
  AND RES.CourseId = Courses.CourseId
