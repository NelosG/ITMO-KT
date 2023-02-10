select distinct TeamName
from Teams T, Runs R, Sessions S
where T.TeamId = S.TeamId and R.SessionId = S.SessionId and Accepted = 1 and Letter = :Letter and ContestId = :ContestId
