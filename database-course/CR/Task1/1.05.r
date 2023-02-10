proj{TeamName}(
    Teams
    nj
    (
        (proj{ContestId}(Contests) cj proj{TeamId}(Teams))
        diff
        proj{ContestId, TeamId}(Sessions nj sel{Accepted=1}(Runs))
    )
)