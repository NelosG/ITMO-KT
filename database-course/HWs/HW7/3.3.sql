UPDATE Students
SET Marks = Marks + (SELECT COUNT(Mark)
                     FROM NewMarks
                     WHERE Students.StudentId = NewMarks.StudentId);
