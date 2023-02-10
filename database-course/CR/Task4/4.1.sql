SELECT SessionId, COUNT(Letter) AS Solved
FROM (SELECT DISTINCT r.SessionId, r.Letter
      FROM Runs r WHERE accepted = 1) runs
GROUP BY SessionId;
