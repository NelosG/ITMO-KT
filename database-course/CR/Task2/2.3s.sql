SELECT DISTINCT TeamId
FROM Sessions
         NATURAL JOIN Runs
WHERE Accepted = 1
  AND ContestId = :ContestId;