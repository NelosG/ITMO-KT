UPDATE Students
SET Marks = (SELECT COUNT(Mark)
             FROM Marks
             WHERE Students.StudentId = Marks.StudentId);
