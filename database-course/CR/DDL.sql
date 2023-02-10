drop table if exists Teams cascade;
create table Teams
(
    TeamId   int          not null,
    TeamName varchar(255) not null,
    constraint pk_Teams_TeamId primary key (TeamId)
);

drop table if exists Contests cascade;
create table Contests
(
    ContestId   int          not null,
    ContestName varchar(255) not null,
    constraint pk_Contest_ContestId primary key (ContestId)
);

drop table if exists Sessions cascade;
create table Sessions
(
    SessionId int       not null,
    Start     timestamp not null,
    TeamId    int       not null,
    ContestId int       not null,
    constraint pk_Sessions_SessionId primary key (SessionId),
    constraint fk_Sessions_TeamId foreign key (TeamId)
        references Teams (TeamId),
    constraint fk_Sessions_ContestId foreign key (ContestId)
        references Contests (ContestId)
);


drop table if exists Problems cascade;
create table Problems
(
    ContestId   int          not null,
    Letter      char(1)      not null,
    ProblemName varchar(255) not null,
    constraint pk_Problems_ContestId_Letter primary key (ContestId, Letter),
    constraint fk_Problems_ContestId foreign key (ContestId)
        references Contests (ContestId)
);

drop table if exists Runs cascade;
create table Runs
(
    RunId      int     not null,
    SessionId  int     not null,
    Letter     char(1)     not null,
    SubmitTime int     not null,
    Accepted   boolean not null,
    constraint pk_Runs_RunId primary key (RunId),
    constraint fk_Runs_SessionId foreign key (SessionId)
        references Sessions (SessionId)
);