SELECT StudentId, StudentName, GroupId
FROM Students
WHERE Students.StudentId IN
      (SELECT Marks.StudentId
       FROM Marks
       WHERE Marks.Mark = :Mark
         AND Marks.CourseId IN
         (SELECT Courses.courseid
          FROM Courses
          WHERE Courses.CourseName = :CourseName));
