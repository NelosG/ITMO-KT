INSERT INTO Groups(group_id, group_name)
VALUES (1, 'M34341'),
       (2, 'M34351');

INSERT INTO Students (student_id, student_name, group_id)
VALUES (1, 'Пушкарев Глеб Андреевич', 2),
       (2, 'Корнеев Георгий Александрович', 2),
       (3, 'Перцев Павел', 1);

INSERT INTO Courses(course_id, course_name)
VALUES (1, 'Базы данных'),
       (2, 'Алгоритмы и структуры данных'),
       (3, 'Дискретная математика');

INSERT INTO Lecturers (lecturer_id, lecturer_name)
VALUES (1, 'Корнеев Георгий Александрович'),
       (2, 'Станкевич Андрей Сергеевич');

INSERT INTO Plans (course_id, group_id, lecturer_id)
VALUES (1, 1, 1),
       (1, 2, 1),
       (2, 1, 2),
       (2, 2, 2),
       (3, 1, 2),
       (3, 2, 2);


INSERT INTO Marks (student_id, course_id, mark)
VALUES (1, 1, 100),
       (2, 1, 101),
       (3, 1, 70),
       (2, 2, 80),
       (3, 2, 60),
       (2, 3, 69),
       (3, 3, 43);


SELECT Lecturers.lecturer_name, Courses.course_name, Groups.group_name
FROM Plans
         INNER JOIN Lecturers ON Plans.lecturer_id = Lecturers.lecturer_id
         INNER JOIN Courses ON Plans.course_id = Courses.course_id
         INNER JOIN Groups ON Plans.group_id = Groups.group_id;


SELECT Students.student_name, Groups.group_name, Courses.course_name, Marks.mark
FROM Marks
         INNER JOIN Students ON Marks.student_id = Students.student_id
         INNER JOIN Groups ON Students.group_id = Groups.group_id
         INNER JOIN Courses ON Marks.course_id = Courses.course_id;