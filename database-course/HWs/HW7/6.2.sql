-- PostgreSQL 14.5
CREATE OR REPLACE FUNCTION check_marks() RETURNS TRIGGER AS
$check_marks$
BEGIN
    IF (tg_op = 'DELETE') THEN
        ASSERT NOT EXISTS
            (
                WITH SG AS (SELECT *
                            FROM students
                            WHERE groupid IN (SELECT groupid FROM students WHERE students.studentid = old.studentid)
                            AND students.studentid != old.studentid),
                     M AS (SELECT * FROM marks WHERE marks.courseid = new.courseid)
                SELECT *
                FROM SG
                WHERE EXISTS(
                        SELECT *
                        FROM M
                        WHERE marks.studentid = SG.studentid)
            );
        RETURN old;
    ELSE
        ASSERT NOT EXISTS
            (
                WITH SG AS (SELECT *
                            FROM students
                            WHERE groupid IN (SELECT groupid FROM students WHERE students.studentid = new.studentid)),
                     M AS (SELECT * FROM marks WHERE marks.courseid = new.courseid)
                SELECT *
                FROM SG
                WHERE NOT EXISTS(
                        SELECT *
                        FROM M
                        WHERE marks.studentid = SG.studentid)
            );
        RETURN new;
    END IF;
END;
$check_marks$ LANGUAGE plpgsql;

CREATE OR REPLACE TRIGGER SameMarks
    BEFORE INSERT OR UPDATE OR DELETE
    ON marks
    FOR EACH ROW
EXECUTE PROCEDURE check_marks();
