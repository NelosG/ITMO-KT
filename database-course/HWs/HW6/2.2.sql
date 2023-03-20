SELECT DISTINCT StudentId, StudentName, GroupName
FROM Students,
     Groups
WHERE Students.GroupId = Groups.GroupId
  AND StudentId NOT IN
      (SELECT Marks.StudentId
       FROM Marks
       WHERE Marks.CourseId = :CourseId);