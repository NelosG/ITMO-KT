DELETE
FROM Students
WHERE StudentId IN (SELECT StudentId
                    FROM Students
                             NATURAL LEFT JOIN Marks
                    GROUP BY StudentId
                    HAVING COUNT(StudentId) <= 3);
