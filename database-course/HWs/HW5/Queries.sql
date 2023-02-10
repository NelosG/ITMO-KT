--  1. Информацию о студентах
--      1. С заданным идентификатором (StudentId, StudentName, GroupId по :StudentId).
--      pi{StudentId, StudentName, GroupId}(sigma{StudentId = :StudentId}(Students))
SELECT Students.StudentId, Students.StudentName, Students.GroupId
FROM Students
WHERE Students.StudentId = :StudentId;

--      2. С заданным ФИО (StudentId, StudentName, GroupId по :StudentName).
--      pi{StudentId, StudentName, GroupId}(sigma{StudentName = :StudentName}(Students))
SELECT Students.StudentId, Students.StudentName, Students.GroupId
FROM Students
WHERE Students.StudentName = :StudentName;



--  2.Полную информацию о студентах
--      1. С заданным идентификатором (StudentId, StudentName, GroupName по :StudentId).
--      pi{StudentId, StudentName, GroupName}(sigma{StudentId = :StudentId}(Students njoin Groups))
--      pi{StudentId, StudentName, GroupName}(sigma{StudentId = :StudentId}(Students ⋈ Groups))
SELECT Students.StudentId, Students.StudentName, Groups.GroupName
FROM Students
         NATURAL JOIN Groups
WHERE Students.StudentId = :StudentId;
--      2. С заданным ФИО (StudentId, StudentName, GroupName по :StudentName).
--      pi{StudentId, StudentName, GroupName}(sigma{StudentName = :StudentName}(Students njoin Groups))
--      pi{StudentId, StudentName, GroupName}(sigma{StudentName = :StudentName}(Students ⋈ Groups))
SELECT Students.StudentId, Students.StudentName, Groups.GroupName
FROM Students
         NATURAL JOIN Groups
WHERE Students.StudentName = :StudentName;



--  3. Информацию о студентах с заданной оценкой по дисциплине
--      1. С заданным идентификатором (StudentId, StudentName, GroupId по :Mark, :CourseId).
--      pi{StudentId, StudentName, GroupId}(sigma{Mark = :Mark && CourseId = :CourseId}(Students njoin Marks))
--      pi{StudentId, StudentName, GroupId}(sigma{Mark = :Mark && CourseId = :CourseId}(Students ⋈ Marks))
SELECT Students.StudentId, Students.StudentName, Students.GroupId
FROM Students
         NATURAL JOIN Marks
WHERE Marks.Mark = :Mark
  AND Marks.CourseId = :CourseId;
--      2. С заданным названием (StudentId, StudentName, GroupId по :Mark, :CourseName).
--      pi{StudentId, StudentName, GroupId}(sigma{Mark = :Mark && CourseName = :CourseName}(Students njoin Marks njoin Courses))
SELECT Students.StudentId, Students.StudentName, Students.GroupId
FROM Students
         NATURAL JOIN Marks
         NATURAL JOIN Courses
WHERE Marks.Mark = :Mark
  AND Courses.CourseName = :CourseName;
--      3. Которую у него вёл лектор заданный идентификатором (StudentId, StudentName, GroupId по :Mark, :LecturerId).
--      pi{StudentId, StudentName, GroupId}(sigma{Mark = :Mark && LecturerId = :LecturerId}(Students njoin Plan njoin Marks))
SELECT Students.StudentId, Students.StudentName, Students.GroupId
FROM Students
         NATURAL JOIN Plan
         NATURAL JOIN Marks
WHERE Marks.Mark = :Mark
  AND Plan.LecturerId = :LecturerId;
--      4. Которую у него вёл лектор, заданный ФИО (StudentId, StudentName, GroupId по :Mark, :LecturerName).
--      pi{StudentId, StudentName, GroupId}(sigma{Mark = :Mark && LecturerName = :LecturerName}(Students njoin Plan njoin Marks njoin Lecturers))
SELECT Students.StudentId, Students.StudentName, Students.GroupId
FROM Students
         NATURAL JOIN Plan
         NATURAL JOIN Marks
         NATURAL JOIN Lecturers
WHERE Marks.Mark = :Mark
  AND Lecturers.LecturerName = :LecturerName;
--      5. Которую вёл лектор, заданный идентификатором (StudentId, StudentName, GroupId по :Mark, :LecturerId).
--      pi{StudentId, StudentName, GroupId}(sigma{Mark = :Mark && LecturerId = :LecturerId}(Students njoin Plan njoin Marks))
SELECT Students.StudentId, Students.StudentName, Students.GroupId
FROM Students
         NATURAL JOIN Marks
         INNER JOIN Plan ON Marks.CourseId = Plan.CourseId
WHERE Marks.Mark = :Mark
  AND Plan.LecturerId = :LecturerId;
--      6. Которую вёл лектор, заданный ФИО (StudentId, StudentName, GroupId по :Mark, :LecturerName).


--  4. Информацию о студентах не имеющих оценки по дисциплине
--      1. Среди всех студентов (StudentId, StudentName, GroupId по :CourseName).
--      Students diff pi{StudentId, StudentName, GroupId}(sigma{CourseName = :CourseName}(Students ljoin Marks njoin Courses))
--      pi{StudentId, StudentName, GroupId} (Students) diff pi{StudentId, StudentName, GroupId} (sigma{CourseName = :CourseName} (Students njoin Marks njoin Courses))
SELECT Students.StudentId, Students.StudentName, Students.GroupId
FROM Students
EXCEPT
SELECT Students.StudentId, Students.StudentName, Students.GroupId
FROM Students
         NATURAL JOIN Marks
         NATURAL JOIN Courses
WHERE Courses.CourseName = :CourseName;

--      2. Cреди студентов, у которых есть эта дисциплина (StudentId, StudentName, GroupId по :CourseName).
--      pi{StudentId, StudentName, GroupId}(Students njoin Plan njoin sigma{CourseName = :CourseName}(Courses)) ∖ pi{StudentId, StudentName, GroupId}(Students njoin Marks njoin sigma{CourseName = :CourseName}(Courses))
--      pi{StudentId, StudentName, GroupId} (sigma{CourseName = :CourseName} (Students njoin Plan njoin Courses)) diff pi{StudentId, StudentName, GroupId} (sigma{CourseName = :CourseName} (Students njoin Marks njoin Courses))
SELECT Students.StudentId, Students.StudentName, Students.GroupId
FROM Students
         NATURAL JOIN Plan
         NATURAL JOIN Courses
WHERE Courses.CourseName = :CourseName
EXCEPT
SELECT Students.StudentId, Students.StudentName, Students.GroupId
FROM Students
         NATURAL JOIN Marks
         NATURAL JOIN Courses
WHERE Courses.CourseName = :CourseName;



--  5. Для каждого студента ФИО и названия дисциплин
--      1. Которые у него есть по плану (StudentName, CourseName).
--      pi{StudentName, CourseName}(Students njoin Plan njoin Courses)
SELECT Students.StudentName, Courses.CourseName
FROM Students
         NATURAL JOIN Plan
         NATURAL JOIN Courses;

--      2. Есть, но у него нет оценки (StudentName, CourseName).
--      pi {StudentName, CourseName} ((pi{StudentId, CourseId} (Students njoin Plan) diff pi{StudentId, CourseId} (Students njoin Marks)) njoin Students njoin Courses)
SELECT Students.StudentName, Courses.CourseName
FROM (SELECT Students.StudentId, Plan.CourseId
      FROM Students
               NATURAL JOIN Plan
      EXCEPT
      SELECT Students.StudentId, Marks.CourseId
      FROM Students
               NATURAL JOIN Marks) S
         NATURAL JOIN Students
         NATURAL JOIN Courses;

--      3. Есть, но у него не 4 или 5 (StudentName, CourseName).
--      pi {StudentName, CourseName} ((pi{StudentId, CourseId} (Students njoin Plan) diff pi{StudentId, CourseId} (sigma{Mark = 4 || Mark = 5} (Students njoin Marks))) njoin Students njoin Courses)
SELECT Students.StudentName, Courses.CourseName
FROM (SELECT Students.StudentId, Plan.CourseId
      FROM Students
               NATURAL JOIN Plan
      EXCEPT
      SELECT Students.StudentId, Marks.CourseId
      FROM Students
               NATURAL JOIN Marks
      WHERE Marks.Mark = 4
         OR Marks.Mark = 5) S
         NATURAL JOIN Students
         NATURAL JOIN Courses;



--  6. Идентификаторы студентов по преподавателю
--      1. Имеющих хотя бы одну оценку у преподавателя (StudentId по :LecturerName).
--      pi{StudentId} (sigma{LecturerName=:LecturerName} (Students njoin Marks njoin Plan njoin Lecturers))
SELECT Students.StudentId
FROM Students
         NATURAL JOIN Marks
         NATURAL JOIN Plan
         NATURAL JOIN Lecturers
WHERE Lecturers.LecturerName = :LecturerName;

--      2. Не имеющих ни одной оценки у преподавателя (StudentId по :LecturerName).
--      pi{StudentId} (Students) diff pi{StudentId} (sigma{LecturerName=:LecturerName} (Students njoin Marks njoin Plan njoin Lecturers))
SELECT Students.StudentId
FROM Students
EXCEPT
SELECT Students.StudentId
FROM Students
         NATURAL JOIN Marks
         NATURAL JOIN Plan
         NATURAL JOIN Lecturers
WHERE Lecturers.LecturerName = :LecturerName;

--      3. Имеющих оценки по всем дисциплинам преподавателя (StudentId по :LecturerName).
--      pi{StudentId, CourseId} (Marks) div pi{CourseId} (sigma{LecturerName=:LecturerName} (Lecturers njoin Plan))

SELECT DISTINCT StudentId
FROM Marks
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
                              FROM Marks) DM);
--      4. Имеющих оценки по всем дисциплинам преподавателя, которые он вёл у этого студента (StudentId по :LecturerName).
--      pi{StudentId} ( sigma{GroupId1=GroupId} ( rho{GroupId1=GroupId} ( pi{StudentId, GroupId} ( pi{StudentId, CourseId} (Marks) gdiv pi{CourseId, GroupId} ( sigma{LecturerName=:LecturerName} (Lecturers ⋈ Plan) ) ) ) ⋈ Students ) )
SELECT StudentId
FROM (SELECT StudentId, GroupId GroupId1
      FROM (SELECT StudentId, GroupId
            FROM (SELECT StudentId FROM (SELECT StudentId, CourseId FROM Marks) q1) pixq1
                     CROSS JOIN (SELECT GroupId
                                 FROM (SELECT CourseId, GroupId
                                       FROM Lecturers
                                                NATURAL JOIN Plan
                                       WHERE LecturerName = :LecturerName) s1) pizs) lhs
      EXCEPT
      SELECT StudentId, GroupId GroupId1
      FROM (SELECT StudentId, GroupId
            FROM (SELECT StudentId, CourseId, GroupId
                  FROM (SELECT StudentId FROM (SELECT StudentId, CourseId FROM Marks) q2) pixq2
                           CROSS JOIN (SELECT CourseId, GroupId
                                       FROM Lecturers
                                                NATURAL JOIN Plan
                                       WHERE LecturerName = :LecturerName) s2
                  EXCEPT
                  SELECT StudentId, CourseId, GroupId
                  FROM (SELECT StudentId, CourseId FROM Marks) q3
                           NATURAL JOIN (SELECT CourseId, GroupId
                                         FROM Lecturers
                                                  NATURAL JOIN Plan
                                         WHERE LecturerName = :LecturerName) s3) t1) rhs) kek
         NATURAL JOIN Students
WHERE GroupId1 = GroupId;


SELECT s.*
FROM Plan t
         INNER JOIN Students s ON s.GroupId = t.GroupId
         LEFT JOIN Marks m ON m.CourseId = t.CourseId AND s.StudentId = m.StudentId
WHERE t.LecturerId = :LecturerId
GROUP BY s.StudentId
HAVING BOOL_AND(m.Mark IS NOT NULL);

--  7. Группы и дисциплины, такие что все студенты группы сдали эту дисциплину
--      1. Идентификаторы (GroupId, CourseId).
--      pi{GroupId, CourseId} ( pi{CourseId, StudentId} (sigma{Mark >= 3} (Students ⋈ Marks)) gdiv pi{StudentId, GroupId} (Students) )
SELECT CourseId, GroupId
FROM (SELECT CourseId, GroupId
      FROM (SELECT CourseId
            FROM (SELECT CourseId, StudentId
                  FROM Students
                           NATURAL JOIN Marks
                  WHERE Mark >= 3) q1) piXQ1
               CROSS JOIN (SELECT GroupId
                           FROM (SELECT StudentId, GroupId
                                 FROM Students) S) piZS) lhs
EXCEPT
SELECT CourseId, GroupId
FROM (SELECT CourseId, GroupId
      FROM (SELECT CourseId, StudentId, GroupId
            FROM (SELECT CourseId
                  FROM (SELECT CourseId, StudentId
                        FROM Students
                                 NATURAL JOIN Marks
                        WHERE Mark >= 3) Q) piXQ
                     CROSS JOIN (SELECT StudentId, GroupId
                                 FROM Students) S
            EXCEPT
            SELECT CourseId, StudentId, GroupId
            FROM (SELECT CourseId, StudentId
                  FROM Students
                           NATURAL JOIN Marks
                  WHERE Mark >= 3) Q
                     NATURAL JOIN (SELECT StudentId, GroupId
                                   FROM Students) S) t1) rhs;


SELECT g.GroupId, c.CourseId
FROM Plan t
         INNER JOIN Groups g ON g.GroupId = t.GroupId
         INNER JOIN Courses c ON c.CourseId = t.CourseId
         INNER JOIN Students s ON s.GroupId = g.GroupId
         LEFT JOIN Marks m ON m.StudentId = s.StudentId AND m.CourseId = c.CourseId
GROUP BY g.GroupId, c.CourseId
HAVING BOOL_AND(m.Mark IS NOT NULL);

--      2. Названия (GroupName, CourseName).
--      pi{GroupName, CourseName} ( pi{GroupId, CourseId} ( pi{CourseId, StudentId} (sigma{Mark >= 3} (Students ⋈ Marks)) gdiv pi{StudentId, GroupId} (Students) ) ⋈ Groups ⋈ Courses )
SELECT GroupName, CourseName
FROM (SELECT CourseId, GroupId
      FROM (SELECT CourseId, GroupId
            FROM (SELECT CourseId
                  FROM (SELECT CourseId, StudentId
                        FROM Students
                                 NATURAL JOIN Marks
                        WHERE Mark >= 3) q1) piXQ1
                     CROSS JOIN (SELECT GroupId
                                 FROM (SELECT StudentId, GroupId
                                       FROM Students) S) piZS) lhs
      EXCEPT
      SELECT CourseId, GroupId
      FROM (SELECT CourseId, GroupId
            FROM (SELECT CourseId, StudentId, GroupId
                  FROM (SELECT CourseId
                        FROM (SELECT CourseId, StudentId
                              FROM Students
                                       NATURAL JOIN Marks
                              WHERE Mark >= 3) Q) piXQ
                           CROSS JOIN (SELECT StudentId, GroupId
                                       FROM Students) S
                  EXCEPT
                  SELECT CourseId, StudentId, GroupId
                  FROM (SELECT CourseId, StudentId
                        FROM Students
                                 NATURAL JOIN Marks
                        WHERE Mark >= 3) Q
                           NATURAL JOIN (SELECT StudentId, GroupId
                                         FROM Students) S) t1) rhs) t2
         NATURAL JOIN Groups
         NATURAL JOIN Courses;


SELECT g.GroupName, c.CourseName
FROM Plan t
         INNER JOIN Groups g ON g.GroupId = t.GroupId
         INNER JOIN Courses c ON c.CourseId = t.CourseId
         INNER JOIN Students s ON s.GroupId = g.GroupId
         LEFT JOIN Marks m ON m.StudentId = s.StudentId AND m.CourseId = c.CourseId
GROUP BY g.GroupId, c.CourseId
HAVING BOOL_AND(m.Mark IS NOT NULL);


-- Составьте SQL-запросы, позволяющие получать

--  8. Суммарный балл
--      1. Одного студента (SumMark по :StudentId).
--      select AVG(cast(Mark as real)) as AvgMark from Marks where StudentId = :StudentId


--      2. Каждого студента (StudentName, SumMark).

--      3. Каждой группы (GroupName, SumMark).
--

--  9. Средний балл
--      1. Одного студента (AvgMark по :StudentId).
SELECT AVG(CAST(Marks.Mark AS REAL)) AS AvgMark
FROM Marks
WHERE Marks.StudentId = :StudentId;


SELECT CAST(SUM(Mark) AS FLOAT) / COUNT(Mark) AS AvgMark
FROM Marks
WHERE StudentId = :StudentId;

SELECT AVG(m.Mark)
FROM Marks m
WHERE m.StudentId = 1;


--      2. Каждого студента (StudentName, AvgMark).
SELECT StudentName, AvgMark
FROM (SELECT S.StudentId, CAST(SUM(Mark) AS FLOAT) / COUNT(*) AS AvgMark
      FROM Students S
               LEFT JOIN Marks M ON S.StudentId = M.StudentId
      GROUP BY S.StudentId) t
         NATURAL JOIN Students;

--      3. Каждой группы (GroupName, AvgMark).


--      4. Средний балл средних баллов студентов каждой группы (GroupName, AvgAvgMark).
SELECT g.*, AVG(m.mark)
FROM Groups g
         JOIN Students s ON s.GroupId = g.GroupId
         JOIN (SELECT s.StudentId, AVG(m.Mark) AS mark
               FROM Marks m
                        JOIN Students s ON s.StudentId = m.StudentId
               GROUP BY s.StudentId) m ON m.StudentId = s.StudentId
GROUP BY g.GroupId;

--  10. Для каждого студента: число дисциплин, которые у него были, число сданных дисциплин и число несданных дисциплин (StudentId, Total, Passed, Failed).
SELECT s.StudentId, t1.c AS Total, t2.c AS Passed, t1.c - t2.c AS Failed
FROM Students s
         JOIN (SELECT COUNT(*) AS c, t.GroupId AS gid FROM Plan t GROUP BY t.GroupId) AS t1 ON t1.gid = s.GroupId
         JOIN (SELECT COUNT(m.Mark) AS c, s.StudentId AS sid
               FROM Plan t
                        JOIN Students s ON t.GroupId = s.GroupId
                        LEFT JOIN Marks m ON s.StudentId = m.StudentId AND m.CourseId = t.CourseId
               GROUP BY s.StudentId) AS t2 ON t2.sid = s.StudentId;

