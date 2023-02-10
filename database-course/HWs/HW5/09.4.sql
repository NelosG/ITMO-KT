SELECT GroupName, AvgAvgMark
FROM groups
         LEFT JOIN (SELECT GroupId, CAST(SUM(AvgMark) AS FLOAT) / COUNT(*) AS AvgAvgMark

                    FROM (SELECT Marks.StudentId, CAST(SUM(Mark) AS FLOAT) / COUNT(*) AS AvgMark
                          FROM Marks
                          GROUP BY Marks.StudentId) MS
                             LEFT JOIN Students ON Students.studentid = MS.studentid
                    GROUP BY GroupId) G ON G.groupid = Groups.groupid;


