SELECT TeamId, COUNT(Letter) AS Solved
FROM (SELECT DISTINCT s.TeamId, s.ContestId, r.Letter
      FROM (SELECT * from Runs WHERE accepted = 1) r
               NATURAL JOIN Sessions s) runs
GROUP BY TeamId;
