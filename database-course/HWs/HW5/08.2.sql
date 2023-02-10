SELECT studentname, SumMark AS SumMark
FROM Students
         LEFT JOIN (SELECT Marks.StudentId, SUM(Marks.Mark) AS SumMark
                    FROM Marks
                    GROUP BY Marks.StudentId) MS ON Students.studentid = MS.studentid;


