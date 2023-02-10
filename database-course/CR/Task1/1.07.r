pi{SessionId}(
    Sessions
)  ∖  pi{SessionId} (
    pi{ContestId, SessionId, Letter}(
        Problems njoin Sessions
    )  ∖  pi{ContestId, SessionId, Letter}(
        Runs njoin Sessions
    )
)