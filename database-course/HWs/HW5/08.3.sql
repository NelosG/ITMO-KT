SELECT groupname, SumMark AS SumMark
FROM groups
         LEFT JOIN (SELECT groupid, SUM(Marks.Mark) AS SumMark
                    FROM Marks LEFT JOIN students s ON Marks.studentid = s.studentid
                    GROUP BY groupid) MS ON groups.groupid = MS.groupid;


