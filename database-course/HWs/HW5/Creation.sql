CREATE TABLE Groups
(
    GroupId   BIGINT     NOT NULL PRIMARY KEY,
    GroupName VARCHAR(6) NOT NULL UNIQUE
);

CREATE TABLE Students
(
    StudentId   BIGINT      NOT NULL PRIMARY KEY,
    StudentName VARCHAR(50) NOT NULL,
    GroupId     BIGINT      NOT NULL
        CONSTRAINT fk_student__group REFERENCES Groups (GroupId)
);

CREATE TABLE Courses
(
    CourseId   BIGINT      NOT NULL PRIMARY KEY,
    CourseName VARCHAR(50) NOT NULL
);

CREATE TABLE Lecturers
(
    LecturerId   BIGINT      NOT NULL PRIMARY KEY,
    LecturerName VARCHAR(50) NOT NULL
);

CREATE TABLE Plan
(
    GroupId    BIGINT NOT NULL
        CONSTRAINT fk_plan__group REFERENCES Groups (GroupId),
    CourseId   BIGINT NOT NULL
        CONSTRAINT fk_plan__course REFERENCES Courses (CourseId),
    LecturerId BIGINT NOT NULL
        CONSTRAINT fk_plan__lecturer REFERENCES Lecturers (LecturerId),
    PRIMARY KEY (GroupId, CourseId)
);

CREATE TABLE Marks
(
    StudentId BIGINT     NOT NULL
        CONSTRAINT fk_mark__student REFERENCES Students (StudentId),
    CourseId  BIGINT     NOT NULL
        CONSTRAINT fk_mark__course REFERENCES Courses (CourseId),
    Mark      NUMERIC(1) NOT NULL,
    PRIMARY KEY (StudentId, CourseId)
);

INSERT INTO Groups(GroupId, GroupName)
VALUES (1, 'M34341'),
       (2, 'M34351');

INSERT INTO Students (StudentId, StudentName, GroupId)
VALUES (1, 'Пушкарев Глеб Андреевич', 2),
       (2, 'Корнеев Георгий Александрович', 2),
       (3, 'Перцев Павел', 1);

INSERT INTO Courses(CourseId, CourseName)
VALUES (1, 'Базы данных'),
       (2, 'Алгоритмы и структуры данных'),
       (3, 'Дискретная математика');

INSERT INTO Lecturers (LecturerId, LecturerName)
VALUES (1, 'Корнеев Георгий Александрович'),
       (2, 'Станкевич Андрей Сергеевич');

INSERT INTO Plan (CourseId, GroupId, LecturerId)
VALUES (1, 1, 1),
       (1, 2, 1),
       (2, 1, 2),
       (2, 2, 2),
       (3, 1, 2),
       (3, 2, 2);


INSERT INTO Marks (StudentId, CourseId, Mark)
VALUES (1, 1, 5),
       (2, 1, 5),
       (3, 1, 4),
       (2, 2, 4),
       (3, 2, 3),
       (2, 3, 3),
       (3, 3, 2);
