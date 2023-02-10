SELECT SessionId
FROM Sessions
EXCEPT
SELECT SessionId
FROM (SELECT SessionId, Letter
      FROM Problems
               NATURAL JOIN Sessions
      EXCEPT
      SELECT SessionId, Letter
      FROM Runs
      WHERE Accepted = 1) sub;