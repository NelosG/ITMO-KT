-- PostgreSQL 14.5
CREATE OR REPLACE FUNCTION check_marks() RETURNS TRIGGER AS
$check_marks$
BEGIN
    ASSERT old.mark <= new.mark, 'Нельзя уменьшить оценку студента';
    RETURN new;
END;
$check_marks$ LANGUAGE plpgsql;

CREATE OR REPLACE TRIGGER PreserveMarks
    BEFORE UPDATE
    ON Marks
    FOR EACH ROW
EXECUTE PROCEDURE check_marks();


