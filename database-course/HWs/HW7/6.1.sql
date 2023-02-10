-- PostgreSQL 14.5
CREATE OR REPLACE FUNCTION check_marks() RETURNS TRIGGER AS
$check_marks$
BEGIN
    ASSERT new.studentid IN (SELECT StudentId
                             FROM (SELECT * FROM Plan WHERE Plan.courseid = new.CourseId) AS P
                                      NATURAL JOIN Students), 'Оценка студента должна быть по курсу который есть у студента';
    RETURN new;
END;
$check_marks$ LANGUAGE plpgsql;

CREATE OR REPLACE TRIGGER NoExtraMarks
    BEFORE INSERT OR UPDATE
    ON Marks
    FOR EACH ROW
EXECUTE PROCEDURE check_marks();

CREATE OR REPLACE FUNCTION check_plan() RETURNS TRIGGER AS
$check_plan$
BEGIN
    IF (tg_op = 'DELETE') THEN
        ASSERT NOT EXISTS(SELECT *
                          FROM (SELECT studentid FROM Marks WHERE Marks.courseid = OLD.courseid) M
                          WHERE studentid IN (SELECT StudentId FROM Students WHERE Students.StudentId = OLD.groupid)),
            'Нельзя удалить из плана дисциплину по которой у студентов группы есть оценки';
        RETURN OLD;
    ELSE
        IF (NEW.courseid != OLD.courseid OR NEW.groupid != OLD.groupid)  then
                    ASSERT NOT EXISTS(SELECT *
                          FROM (SELECT studentid FROM Marks WHERE Marks.courseid = NEW.courseid) M
                          WHERE studentid IN (SELECT StudentId FROM Students WHERE Students.StudentId = NEW.groupid)),
            'Нельзя изменить в плане дисциплину по которой у студентов группы есть оценки';
        END IF;
        RETURN NEW;
    END IF;
END;
$check_plan$ LANGUAGE plpgsql;

CREATE OR REPLACE TRIGGER NoExtraMarks
    BEFORE DELETE OR UPDATE
    ON Plan
    FOR EACH ROW
EXECUTE PROCEDURE check_plan();
