SELECT T.StudentId, COALESCE(Total, 0) AS Total, COALESCE(Passed, 0) as Passed, COALESCE(Total - Passed, Total) AS Failed
FROM (SELECT COUNT(Plan.courseid) AS Total, Students.StudentId
      FROM Plan
               LEFT JOIN Students ON Students.GroupId = Plan.GroupId
      GROUP BY Students.StudentId) T

         LEFT JOIN (SELECT COUNT(Marks.Mark) AS Passed, Students.StudentId
                    FROM Marks
                             LEFT JOIN Students ON Students.StudentId = Marks.StudentId
                    GROUP BY Students.StudentId) P
                   ON P.StudentId = T.StudentId;