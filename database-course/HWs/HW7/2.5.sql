UPDATE Students
SET GroupId = (SELECT GroupId
               FROM Groups
               WHERE GroupName = :GroupName)
WHERE GroupId = (SELECT GroupId
                 FROM Groups
                 WHERE GroupName = :FromGroupName)
  AND EXISTS(
        SELECT GroupName
        FROM Groups
        WHERE GroupName = :GroupName
    );
