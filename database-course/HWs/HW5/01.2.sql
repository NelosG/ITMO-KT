SELECT Students.StudentId, Students.StudentName, Students.GroupId
FROM Students
WHERE Students.StudentName = :StudentName;