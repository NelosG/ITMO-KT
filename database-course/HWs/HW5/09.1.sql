SELECT CAST(SUM(Mark) AS FLOAT) / COUNT(*) as AvgMark
FROM Marks
WHERE Marks.StudentId = :StudentId;