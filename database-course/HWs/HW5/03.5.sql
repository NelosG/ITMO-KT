SELECT Students.StudentId, Students.StudentName, Students.GroupId
FROM Students
         NATURAL JOIN Marks
         NATURAL JOIN (SELECT Plan.CourseId FROM Plan WHERE Plan.LecturerId = :LecturerId) P
WHERE Marks.Mark = :Mark;