pi{SessionId}(
    Sessions
)  ∖  pi{SessionId} (
    pi{SessionId, Letter}(
        Problems njoin Sessions
    )  ∖  pi{SessionId, Letter}(
        sigma{Accepted = 1}(Runs) 
    )
)