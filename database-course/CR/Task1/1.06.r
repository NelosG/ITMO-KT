proj{TeamName}(
    Teams
    nj
    (
        proj{ContestId, TeamId}(Sessions)
        diff
        proj{ContestId, TeamId}(Sessions nj sel{Accepted=1}(Runs))
    )
)