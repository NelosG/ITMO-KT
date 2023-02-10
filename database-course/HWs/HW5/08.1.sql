SELECT SUM(Marks.Mark) as SumMark
FROM Marks
WHERE Marks.StudentId = :StudentId;