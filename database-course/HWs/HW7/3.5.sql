UPDATE Students
SET Debts = (SELECT COUNT(DISTINCT CourseId)
             FROM Plan
                      NATURAL JOIN Students
                      NATURAL LEFT JOIN Marks
             WHERE Students.StudentId = :StudentId
               AND Marks.Mark IS NULL)
WHERE StudentId = :StudentId;
