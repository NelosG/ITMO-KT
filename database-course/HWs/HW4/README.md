# HW4 for DBMS course, 09 October 2015.

(StudentId, StudentName, GroupId, GroupName, CourseId, CourseName, LecturerId, LecturerName, Mark) => (StudentId,
StudentName, GroupId, GroupName, CourseId, CourseName, LecturerId, LecturerName, Mark).

### Functional dependencies

1. Student_Id -> Student_Name
2. Student_Id -> Group_Id
3. Group_Id -> Group_Name
4. Group_Name -> Group_Id
5. Course_Id -> Course_Name
6. Lecturer_Id -> Lecturer_Name
7. Group_Id Course_Id -> Lecturer_Id
8. Student_Id Course_Id -> Mark

# 1НФ
Исходное множество ФЗ:
StudentId -> StudentName, GroupId
GroupId -> GroupName
CourseId -> CourseName
LecturerId -> LecturerName
StudentId, CourseId -> Mark
GroupId, CourseId -> LecturerId, LecturerName
GroupName -> GroupId


Отношение уже находится в первой нормальной форме, так как в нём:
1. Нет повторяющихся групп
2. Все атрибуты атомарны
3. Есть ключ (StudentId, CourseId)

(StudentId, StudentName, GroupId, GroupName, CourseId, CourseName, LecturerId, LecturerName, Mark) => (StudentId, StudentName, GroupId, GroupName, CourseId, CourseName, LecturerId, LecturerName, Mark).

# 2НФ
Cуществуют атрибуты которые зависят от лишь от части составного ключа
Например, StudentId -> StudentName и CourseId -> CourseName. 

Декомпозируем по этим зависимостям исходное отношение:
(StudentId, StudentName)
(CourseId, CourseName)
(StudentId, GroupId, GroupName, CourseId, LecturerId, LecturerName, Mark)

Скомбинируем GroupId, CourseId -> GroupId с GroupId -> GroupName:
GroupId, CourseId -> GroupName

Далее из GroupId, CourseId -> LecturerId, LecturerName и GroupId, CourseId -> GroupName
получим GroupId, CourseId -> LecturerId, LecturerName, GroupName
Декомпозируем с помощью Хита

(StudentId, StudentName)
(CourseId, CourseName) 
(StudentId, CourseId, GroupId, Mark)
(GroupId, CourseId, GroupName, LecturerId, LecturerName)

GroupId -> GroupName и StudentId -> GroupId, проведем декомпозицию:

(StudentId, CourseId, Mark)
(GroupId, GroupName)
(StudentId, GroupId)
(StudentId, StudentName)
(CourseId, CourseName)
(GroupId, CourseId, LecturerId, LecturerName)

У каждого студента есть группа и имя, поэтому соединим (StudentId, GroupId) и (StudentId, StudentName): 
(StudentId, CourseId, Mark) 
(GroupId, GroupName) 
(StudentId, GroupId, StudentName) 
(CourseId, CourseName) 
(GroupId, CourseId, LecturerId, LecturerName)


(StudentId, StudentName, GroupId, GroupName, CourseId, CourseName, LecturerId, LecturerName, Mark) => (StudentId, CourseId, Mark); (StudentId, GroupId, StudentName); (GroupId, GroupName); (CourseId, CourseName); (GroupId, CourseId, LecturerId, LecturerName).

# 3НФ

LecturerName транзитивно зависит от ключа (GroupId, CourseId) из-за ФЗ GroupId, CourseId -> LecturerId и LecturerId -> LecturerName
Декомпозируем по последнему:
(GroupId, CourseId, LecturerId, LecturerName) => (GroupId, CourseId, LecturerId); (LecturerId, LecturerName).

(StudentId, CourseId, Mark) => (StudentId, CourseId, Mark).
(StudentId, GroupId, StudentName) => (StudentId, GroupId, StudentName).
(GroupId, GroupName) => (GroupId, GroupName).
(CourseId, CourseName) => (CourseId, CourseName).
(GroupId, CourseId, LecturerId, LecturerName) => (GroupId, CourseId, LecturerId); (LecturerId, LecturerName).

# НФБК
 отношение находится в 3НФ и не имеет пересекающихся ключей |=> уже в НФБК

(StudentId, CourseId, Mark) => (StudentId, CourseId, Mark).
(StudentId, GroupId, StudentName) => (StudentId, GroupId, StudentName).
(GroupId, GroupName) => (GroupId, GroupName).
(CourseId, CourseName) => (CourseId, CourseName).
(GroupId, CourseId, LecturerId) => (GroupId, CourseId, LecturerId).
(LecturerId, LecturerName) => (LecturerId, LecturerName).

# 4НФ
В отношениях нет нетривиальных МЗ, которые не являются ФЗ,
отношения находятся в НФБК,

Отношения уже находятся в 4НФ.

(StudentId, CourseId, Mark) => (StudentId, CourseId, Mark).
(StudentId, GroupId, StudentName) => (StudentId, GroupId, StudentName).
(GroupId, GroupName) => (GroupId, GroupName).
(CourseId, CourseName) => (CourseId, CourseName).
(GroupId, CourseId, LecturerId) => (GroupId, CourseId, LecturerId).
(LecturerId, LecturerName) => (LecturerId, LecturerName).
# 5НФ

Рассмотрим (LecturerId, LecturerName); LecturerId не входит в замыкание {LecturerName}+ -> LecturerId входит в любой надключ

LecturerId - ключ -> составного ключа не существует -> все ключи - простые

Аналогично для  (CourseId, CourseName) (GroupId, GroupName) (StudentId, GroupId, StudentName)

Рассмотрим (GroupId, CourseId, LecturerId)
Разобьем на 3 части по 2 атрибута и получим: (GroupId, CourseId) (GroupId, LecturerId) (CourseId, LecturerId)
для данного отношения нет нетривиальных ЗС

Аналогично для (StudentId, CourseId, Mark)

Отношения уже в 5НФ

(StudentId, CourseId, Mark) => (StudentId, CourseId, Mark).
(StudentId, GroupId, StudentName) => (StudentId, GroupId, StudentName).
(GroupId, GroupName) => (GroupId, GroupName).
(CourseId, CourseName) => (CourseId, CourseName).
(GroupId, CourseId, LecturerId) => (GroupId, CourseId, LecturerId).
(LecturerId, LecturerName) => (LecturerId, LecturerName).
