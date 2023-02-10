-- Хотим по Name получать Mark, StudentId по курсу,
-- заведем покрывающий индекс
CREATE INDEX CMSIdIndex ON Marks USING btree (CourseId, Mark, StudentId)