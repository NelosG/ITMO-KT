SELECT p.ProblemName
FROM Problems p
WHERE NOT EXISTS(
        SELECT s.SessionId, s.TeamId, s.ContestId, r.Letter
        FROM Sessions s,
             Runs r
        WHERE r.SessionId = s.SessionId
          AND p.Letter = r.Letter
          AND p.ContestId = s.ContestId
          AND r.Accepted = 1
    );
