SELECT StudentId
FROM Students
WHERE NOT EXISTS
    (SELECT CourseId
     FROM Plan
     WHERE Plan.LecturerId IN (SELECT Lecturers.LecturerId
                               FROM Lecturers
                               WHERE Lecturers.LecturerName = :LecturerName)
       AND NOT EXISTS
         (SELECT Marks.StudentId, Marks.CourseId
          FROM Marks
          WHERE Marks.StudentId = Students.StudentId
            AND Marks.CourseId = Plan.CourseId));

