select distinct TeamId
from Runs R, Sessions S
where R.SessionId = S.SessionId and Accepted = 1 and Letter = :Letter and ContestId = :ContestId