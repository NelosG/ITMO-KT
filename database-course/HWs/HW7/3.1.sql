UPDATE Students
SET Marks = (SELECT COUNT(Mark)
             FROM Marks
             WHERE StudentId = :StudentId)
WHERE StudentId = :StudentId;
