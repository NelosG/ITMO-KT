CREATE VIEW Debts AS
SELECT StudentId, COUNT(DISTINCT CourseId) AS Debts
FROM Plan
         NATURAL JOIN Students
         NATURAL LEFT JOIN Marks
WHERE Mark IS NULL
GROUP BY StudentId;

