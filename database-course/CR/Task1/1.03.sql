SELECT RunId, SessionId, Letter, SubmitTime
FROM Sessions
         NATURAL JOIN Runs
WHERE ContestId = :ContestId
  AND Accepted = 1;