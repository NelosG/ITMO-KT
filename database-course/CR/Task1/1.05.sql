SELECT TeamName
FROM Teams
         NATURAL JOIN
     (SELECT DISTINCT TeamId
      FROM (SELECT TeamId, ContestId
            FROM Contests,
                 Teams
            EXCEPT
            SELECT TeamId, ContestId
            FROM Sessions
                     NATURAL JOIN
                 Runs) NotSolvedTeam) SolvedTeam;