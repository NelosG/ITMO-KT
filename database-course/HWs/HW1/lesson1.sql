CREATE TABLE groups
(
    group_id INT CONSTRAINT group_id_unique PRIMARY KEY UNIQUE,
    group_no CHAR(6)
);

CREATE TABLE students
(
    student_id INT,
    name       VARCHAR(30),
    group_id   INT REFERENCES groups
);

INSERT INTO groups
    (group_id, group_no)
VALUES (1, 'M34371'),
       (2, 'M34391');

INSERT INTO students
    (student_id, name, group_id)
VALUES (1, 'Ilona Bozhe', 1),
       (2, 'Alex Slastin', 2),
       (3, 'Ivan Uss', 1);

-- Список групп
SELECT group_id, group_no
FROM groups;

-- Список студентов
SELECT student_id, name, group_id
FROM students;

-- Список студентов и групп 1
SELECT name, group_no
FROM students
         NATURAL JOIN groups;

-- Список студентов и групп 1
SELECT students.name, groups.group_no
FROM students
         INNER JOIN groups
                    ON students.group_id = groups.group_id;