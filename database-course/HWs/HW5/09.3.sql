SELECT groupname, AvgMark
FROM groups
         LEFT JOIN (SELECT groupid, CAST(SUM(Mark) AS FLOAT) / COUNT(*) AS AvgMark
                    FROM Marks LEFT JOIN students s ON Marks.studentid = s.studentid
                    GROUP BY groupid) MS ON groups.groupid = MS.groupid;


