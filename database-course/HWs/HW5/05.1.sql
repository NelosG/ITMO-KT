SELECT DISTINCT Students.StudentName, Courses.CourseName
FROM Students
         NATURAL JOIN Plan
         NATURAL JOIN Courses;