pi{TeamName} (
    Teams njoin 
    (
        pi{TeamId}(Teams) ∖ pi{TeamId} (
            sigma{Accepted = 1}(Sessions njoin Runs)
        )
    )
)