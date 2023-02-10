-- Хотим по Id лектора получать курса,
-- давайте заведем покрывающий индекс
CREATE INDEX LIdCId ON Plan USING btree (LecturerId, CourseId);

-- Хотим получать Id для лектора по LecturerName ,
-- заведем покрывающий индекс
CREATE INDEX LNmLId ON Lecturers
    USING btree (LecturerName, LecturerId);