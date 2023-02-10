INSERT INTO Sessions (TeamId, ContestId, Start)
SELECT TeamId, :ContestId, CURRENT_TIMESTAMP
FROM (SELECT T.TeamId
      FROM Teams T
      WHERE T.TeamId NOT IN (SELECT TeamId
                             FROM Sessions
                             WHERE ContestId = :ContestId)) sub;