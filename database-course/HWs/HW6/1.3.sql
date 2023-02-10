SELECT StudentId, StudentName, GroupId
FROM Students
WHERE Students.StudentId IN
      (SELECT Marks.StudentId
       FROM Marks
       WHERE Marks.CourseId = :CourseId
         AND Marks.Mark = :Mark);
