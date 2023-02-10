SELECT Students.StudentId, Students.StudentName, Students.GroupId
FROM Students
         NATURAL JOIN Plan
         NATURAL JOIN Courses
WHERE Courses.CourseName = :CourseName
EXCEPT
SELECT Students.StudentId, Students.StudentName, Students.GroupId
FROM Students
         NATURAL JOIN Marks
         NATURAL JOIN Courses
WHERE Courses.CourseName = :CourseName;
