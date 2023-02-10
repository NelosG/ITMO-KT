-- вывести информацию обо всех Пушкаревых из определенной группы
select StudentId, StudentName, GroupId
    from Students
    where StudentName like 'Пушкарев %'
        and GroupId = :GroupId