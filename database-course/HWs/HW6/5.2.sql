SELECT StudentId
FROM Students
WHERE NOT EXISTS(SELECT Marks.StudentId
                 FROM Marks
                 WHERE Students.StudentId = Marks.StudentId
                   AND EXISTS(
                         SELECT Plan.GroupId
                         FROM plan
                         WHERE Plan.LecturerId IN (SELECT Lecturers.LecturerId
                                                   FROM Lecturers
                                                   WHERE Lecturers.LecturerName = :LecturerName)
                           AND Marks.CourseId = Plan.CourseId
                           AND Students.GroupId = Plan.GroupId
                     ));