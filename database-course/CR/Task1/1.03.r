pi{RunId, SessionId, Letter, SubmitTime} (
    sigma{ContestId = :ContestId && Accepted = 1} (Sessions njoin Runs)
)