SELECT Students.StudentId, Students.StudentName, Students.GroupId
FROM Students
EXCEPT
SELECT Students.StudentId, Students.StudentName, Students.GroupId
FROM Students
         NATURAL JOIN Marks
         NATURAL JOIN Courses
WHERE Courses.CourseName = :CourseName;