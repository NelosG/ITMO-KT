CREATE TABLE groups
(
    group_id NUMERIC(30) PRIMARY KEY,
    name     CHAR(6)
);

CREATE TABLE disciplines
(
    discipline_id NUMERIC(30) PRIMARY KEY,
    name          VARCHAR(100)
);

CREATE TABLE teachers
(
    teacher_id NUMERIC(30) PRIMARY KEY,
    passport   CHAR(10) UNIQUE,
    name       VARCHAR(50)
);

CREATE TABLE students
(
    student_id NUMERIC(30) PRIMARY KEY,
    passport   CHAR(10) UNIQUE,
    name       VARCHAR(50),
    group_id   NUMERIC(30)
        CONSTRAINT fk_student__group REFERENCES groups (group_id)
);

CREATE TABLE discipline_by_group
(
    group_id      NUMERIC(30)
        CONSTRAINT fk_discipline_by_group__group REFERENCES groups (group_id),
    discipline_id NUMERIC(30)
        CONSTRAINT fk_discipline_by_group__discipline REFERENCES disciplines (discipline_id),
    teacher_id    NUMERIC(30)
        CONSTRAINT fk_discipline_by_group__teacher REFERENCES teachers (teacher_id),
    PRIMARY KEY (group_id, discipline_id)
);

CREATE TABLE marks
(
    mark          NUMERIC(3),
    student_id    NUMERIC(30)
        CONSTRAINT fk_mark__student REFERENCES students (student_id),
    discipline_id NUMERIC(30)
        CONSTRAINT fk_mark__discipline REFERENCES disciplines (discipline_id),
    PRIMARY KEY (student_id, discipline_id)
);

INSERT INTO groups(group_id, name)
VALUES (1, 'M34341'),
       (2, 'M34351');

INSERT INTO students(student_id, passport, name, group_id)
VALUES (1, '0000111222', 'Корнеев Георгий Александрович', 2),
       (2, '1234567890', 'Пушкарев Глеб Андреевич', 2),
       (3, '0987654321', 'Перцев Павел', 1);

INSERT INTO disciplines(discipline_id, name)
VALUES (1, 'Введение в базы данных (семестр 1)'),
       (2, 'Введение в базы данных (семестр 2)');

INSERT INTO teachers(teacher_id, passport, name)
VALUES (1, '0000111222', 'Корнеев Георгий Александрович'),
       (2, '1111222333', 'Станкевич Андрей Сергеевич');

INSERT INTO discipline_by_group(group_id, discipline_id, teacher_id)
VALUES (1, 1, 1),
       (2, 1, 2),
       (1, 2, 2),
       (2, 2, 1);

INSERT INTO marks(mark, student_id, discipline_id)
VALUES (61, 1, 1),
       (80, 2, 1),
       (87, 3, 1),
       (70, 1, 2),
       (130, 2, 2),
       (97, 3, 2);
