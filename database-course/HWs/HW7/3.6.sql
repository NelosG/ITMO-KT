UPDATE Students
SET Debts = (SELECT COUNT(DISTINCT CourseId)
             FROM Plan
                      NATURAL JOIN Students s
                      NATURAL LEFT JOIN Marks
             WHERE Students.StudentId = s.StudentId
               AND Marks.Mark IS NULL);
