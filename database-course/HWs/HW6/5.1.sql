SELECT DISTINCT Students.StudentId
FROM Students,
     Plan,
     Marks
WHERE Plan.LecturerId IN (SELECT Lecturers.LecturerId
                          FROM Lecturers
                          WHERE Lecturers.LecturerName = :LecturerName)
  AND Marks.CourseId = Plan.CourseId
  AND Students.GroupId = Plan.GroupId
  AND Students.StudentId = Marks.StudentId;
