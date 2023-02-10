-- Лучшие студенты по курсу :CourseId
SELECT DISTINCT StudentId, StudentName
FROM Students
         NATURAL JOIN Marks
WHERE CourseId = :CourseId
  AND Mark >= ALL (SELECT Mark
                   FROM Marks
                   WHERE CourseId = :CourseId);

