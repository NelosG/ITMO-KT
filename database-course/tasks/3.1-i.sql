-- Хотим по Name получать Id для групп,
-- заведем покрывающий индекс
create index SNameGIdIndex on Students using btree (StudentName, GroupId)