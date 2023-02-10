SELECT DISTINCT StudentId
FROM Marks
         NATURAL JOIN students
WHERE Studentid NOT IN (SELECT Studentid
                        FROM (SELECT Studentid, CourseId
                              FROM (SELECT CourseId
                                    FROM Lecturers
                                             NATURAL JOIN Plan
                                    WHERE LecturerName = :LecturerName) C
                                       CROSS JOIN
                                   (SELECT StudentId
                                    FROM Marks) MC
                              EXCEPT
                              SELECT StudentId, CourseId
                              FROM Marks) DM)
  AND groupid IN (SELECT plan.groupid
                  FROM Lecturers
                           NATURAL JOIN Plan
                  WHERE LecturerName = :LecturerName);