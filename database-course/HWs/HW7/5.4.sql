CREATE VIEW StudentDebts AS
SELECT StudentId, 0 AS Debts
FROM Students
WHERE StudentId NOT IN (SELECT StudentId
                    FROM Plan
                             NATURAL JOIN Students
                             NATURAL LEFT JOIN Marks
                    WHERE Mark IS NULL)
UNION
SELECT StudentId, COUNT(DISTINCT CourseId) AS Debts
FROM Plan
         NATURAL JOIN Students
         NATURAL LEFT JOIN Marks
WHERE Mark IS NULL
GROUP BY StudentId;
