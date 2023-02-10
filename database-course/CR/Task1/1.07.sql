SELECT SessionId
FROM Sessions
EXCEPT
SELECT SessionId
FROM (SELECT ContestId, SessionId, Letter
      FROM Problems
               NATURAL JOIN Sessions
      EXCEPT
      SELECT ContestId, SessionId, Letter
      FROM Runs
               NATURAL JOIN Sessions) sub;