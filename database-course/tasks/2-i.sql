-- не важен порядок, ускорение соединения с Students
CREATE INDEX ON Students USING hash (GroupId);

-- не важен порядок, ускорение соединения с Groups
CREATE UNIQUE INDEX ON Groups USING hash (GroupId);

-- покрывающий индекс, порядок не важен, уск получения
CREATE INDEX ON Groups USING hash (GroupName, GroupId);

-- не важен порядок, ускорение соединения с Courses
CREATE UNIQUE INDEX ON Courses USING hash (CourseId);

-- покрывающий индекс, порядок не важен
CREATE INDEX ON Courses USING btree (CourseName, CourseId);

-- покрывающий индекс, порядок не важен, ускорение соединения с Marks
create index on Marks using btree (CourseId, StudentId, Mark);
