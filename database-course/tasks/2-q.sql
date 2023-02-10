SELECT GroupName, CourseName, AVG(CAST(Mark AS FLOAT)) AS AvgMark
FROM Students
         NATURAL JOIN Marks
         NATURAL JOIN Groups
         NATURAL JOIN Courses
WHERE GroupName = :GroupName
  AND CourseName = :CourseName
GROUP BY Groups.GroupId, Courses.CourseId;
