MERGE INTO Marks M
    USING NewMarks N
    ON M.StudentId = N.StudentId AND M.CourseId = N.CourseId
    WHEN matched AND N.Mark > M.Mark THEN
UPDATE SET M.Mark = N.Mark
    WHEN NOT matched THEN
INSERT (StudentId, CourseId, Mark)
VALUES (N.StudentId, N.CourseId, N.Mark);
