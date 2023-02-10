CREATE TABLE Groups
(
    group_id   BIGINT     NOT NULL PRIMARY KEY,
    group_name VARCHAR(6) NOT NULL UNIQUE
);

CREATE TABLE Students
(
    student_id   BIGINT      NOT NULL PRIMARY KEY,
    student_name VARCHAR(50) NOT NULL,
    group_id     BIGINT      NOT NULL
        CONSTRAINT fk_student__group REFERENCES Groups (group_id)
);

CREATE TABLE Courses
(
    course_id   BIGINT      NOT NULL PRIMARY KEY,
    course_name VARCHAR(50) NOT NULL
);

CREATE TABLE Lecturers
(
    lecturer_id   BIGINT      NOT NULL PRIMARY KEY,
    lecturer_name VARCHAR(50) NOT NULL
);

CREATE TABLE Plans
(
    group_id    BIGINT NOT NULL
        CONSTRAINT fk_plan__group REFERENCES Groups (group_id),
    course_id   BIGINT NOT NULL
        CONSTRAINT fk_plan__course REFERENCES Courses (course_id),
    lecturer_id BIGINT NOT NULL
        CONSTRAINT fk_plan__lecturer REFERENCES Lecturers (lecturer_id),
    PRIMARY KEY (group_id, course_id)
);

CREATE TABLE Marks
(
    mark       NUMERIC(3) NOT NULL,
    student_id BIGINT     NOT NULL
        CONSTRAINT fk_mark__student REFERENCES Students (student_id),
    course_id  BIGINT     NOT NULL
        CONSTRAINT fk_mark__course REFERENCES Courses (course_id),
    PRIMARY KEY (student_id, course_id)
);
