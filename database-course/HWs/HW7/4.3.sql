UPDATE Marks
SET Mark = (SELECT NewMarks.Mark
            FROM NewMarks
            WHERE NewMarks.StudentId = Marks.StudentId
              AND NewMarks.CourseId = Marks.CourseId)
WHERE EXISTS(
              SELECT StudentId, CourseId, Mark
              FROM NewMarks
              WHERE Marks.StudentId = NewMarks.StudentId
                AND Marks.CourseId = NewMarks.CourseId
                AND Marks.Mark < NewMarks.Mark
          );
