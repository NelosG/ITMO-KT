UPDATE Runs
SET Accepted = 1
WHERE RunId IN (SELECT RunId
                FROM (SELECT RunId, MAX(CAST(SubmitTime AS int))
                      FROM Runs
                      GROUP BY SessionId, RunId) r
                WHERE r.max IN (SELECT MAX(CAST(SubmitTime AS int))
                                FROM Runs
                                GROUP BY SessionId));


