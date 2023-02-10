DELETE
FROM Students
WHERE StudentId IN (SELECT StudentId
                    FROM Plan
                             NATURAL JOIN Students
                             NATURAL LEFT JOIN Marks
                    WHERE Mark IS NULL);
