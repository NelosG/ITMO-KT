SELECT TeamName
FROM (SELECT DISTINCT TeamId, TeamName
      FROM (SELECT TeamId, TeamName, ContestId
            FROM Teams
                     NATURAL JOIN Sessions
            EXCEPT
            SELECT TeamId, TeamName, ContestId
            FROM Teams
                     NATURAL JOIN Sessions
                     NATURAL JOIN Runs) tt) Teams;