SELECT Students.StudentId, Students.StudentName, Students.GroupId
FROM Students
         NATURAL JOIN Marks
         NATURAL JOIN (SELECT Plan.CourseId
                       FROM Plan
                       NATURAL JOIN Lecturers
                       WHERE Lecturers.LecturerName = :LecturerName) P
WHERE Marks.Mark = :Mark;