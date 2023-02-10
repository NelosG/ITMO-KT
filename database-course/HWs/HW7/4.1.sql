INSERT INTO Marks
    (StudentId, CourseId, Mark)
SELECT StudentId, CourseId, Mark
FROM NewMarks
WHERE NOT EXISTS(
        SELECT StudentId, CourseId, Mark
        FROM Marks
        WHERE Marks.StudentId = NewMarks.StudentId
          AND Marks.CourseId = NewMarks.CourseId
    );
