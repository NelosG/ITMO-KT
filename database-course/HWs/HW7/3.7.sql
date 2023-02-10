UPDATE Students
SET Debts = (SELECT COUNT(DISTINCT CourseId)
             FROM Plan
                      NATURAL JOIN Students s
                      NATURAL LEFT JOIN Marks
             WHERE Students.StudentId = s.StudentId
               AND Mark IS NULL)
WHERE GroupId = (SELECT GroupId
                 FROM Groups
                 WHERE GroupName = :GroupName);
