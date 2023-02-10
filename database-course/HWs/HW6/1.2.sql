SELECT StudentId, StudentName, Students.GroupId
FROM Students
WHERE Students.GroupId IN
      (SELECT Groups.groupid
       FROM Groups
       WHERE Groups.GroupName = :GroupName);
