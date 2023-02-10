DELETE
FROM Students
WHERE StudentId IN (SELECT StudentId
                    FROM Students
                    EXCEPT
                    SELECT StudentId
                    FROM Plan
                             NATURAL JOIN Students
                             NATURAL LEFT JOIN Marks
                    WHERE Mark IS NULL
                    UNION
                    SELECT StudentId
                    FROM Plan
                             NATURAL JOIN Students
                             NATURAL LEFT JOIN Marks
                    WHERE Mark IS NULL
                    GROUP BY StudentId
                    HAVING COUNT(DISTINCT CourseId) <= 2);
