INSERT INTO students
    (student_id, name, group_id)
VALUES (4, 'Михайлов Максим Николаевич', 1),
       (5, 'Назаров Георгий Дмитриевич', 1),
       (6, 'Самсикова Мария Денисовна', 1),
       (7, 'Стрельников Илья Денисович', 1);


SELECT COUNT(*)
FROM Students
WHERE group_id IN (SELECT group_id FROM Groups WHERE group_no = 'M34371');

SELECT COUNT(*)
FROM Students
         NATURAL JOIN Groups
WHERE name LIKE '%ов%'
   OR group_no LIKE '%9%';

SELECT COUNT(*)
FROM Students
         NATURAL JOIN Groups
WHERE name LIKE '%n'
  AND group_no LIKE '%9%';

DELETE
FROM groups
WHERE 1 = 1;

DELETE
FROM students
WHERE 1 = 1;

DELETE
FROM groups
WHERE 1 = 1;


INSERT INTO groups (group_id, group_no)
VALUES (1, 'M34341'),
       (2, 'M34351'),
       (3, 'M34361'),
       (4, 'M34371');

INSERT INTO students
    (student_id, name, group_id)
VALUES (4, '1', 1),
       (5, '2', 2),
       (6, '3', 2),
       (6, '4', 3),
       (6, '5', 3),
       (6, '6', 3),
       (6, '7', 4),
       (6, '8', 4),
       (6, '9', 4),
       (7, '10', 4);


select group_no, count(*)
from Groups g inner join Students s on g.group_id = s.group_id
group by group_no
order by group_no desc;

select group_no, count(*)
from Groups g inner join Students s on g.group_id <> s.group_id
where group_no like '%1'
group by group_no
order by group_no;