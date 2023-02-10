SELECT DISTINCT RunId, SessionId, Letter, SubmitTime, Accepted
FROM Runs
         NATURAL JOIN Sessions
WHERE ContestId = :ContestId
  AND TeamId = :TeamId;