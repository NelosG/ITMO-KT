UPDATE Students
SET Marks = (SELECT COUNT(DISTINCT CourseId)
             FROM Marks
             WHERE Students.StudentId = Marks.StudentId
               AND Marks.Mark IS NOT NULL);
