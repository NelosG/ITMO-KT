CREATE VIEW StudentMarks AS
SELECT StudentId, COUNT(Mark) AS Marks
FROM Students
         NATURAL LEFT JOIN Marks
GROUP BY StudentId;
