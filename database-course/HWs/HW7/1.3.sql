DELETE
FROM Students
WHERE StudentId NOT IN (SELECT StudentId
                        FROM Marks);
