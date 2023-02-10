SELECT ContestId, Letter
FROM Problems
EXCEPT
SELECT ContestId, Letter
FROM Sessions
         NATURAL JOIN Runs
WHERE Accepted = 1;