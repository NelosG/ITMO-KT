SELECT DISTINCT StudentId, StudentName, GroupName
FROM Students,
     Groups
WHERE Students.GroupId = Groups.GroupId
  AND Students.GroupId IN
      (SELECT Plan.groupid
       FROM Plan
       WHERE Plan.CourseId IN
         (SELECT Courses.courseid
          FROM Courses
          WHERE Courses.CourseName = :CourseName))
  AND Students.StudentId NOT IN
      (SELECT Marks.StudentId
       FROM Marks
       WHERE Marks.CourseId IN
         (SELECT Courses.courseid
          FROM Courses
          WHERE Courses.CourseName = :CourseName));
