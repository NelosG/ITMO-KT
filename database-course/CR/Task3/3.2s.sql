DELETE
FROM Runs
WHERE Runs.SessionId IN (SELECT SessionId
                         FROM Sessions
                                  NATURAL JOIN Teams
                         WHERE TeamName = :TeamName);