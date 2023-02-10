SELECT Students.StudentId, Students.StudentName, Groups.GroupName
FROM Students
         NATURAL JOIN Groups
WHERE Students.StudentId = :StudentId;