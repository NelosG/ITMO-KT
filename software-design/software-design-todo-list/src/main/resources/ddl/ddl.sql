CREATE TABLE task_list
(
    id          bigint       NOT NULL PRIMARY KEY,
    name        varchar(300) NOT NULL,
    description varchar(3000)
);

CREATE TABLE task
(
    id           bigint PRIMARY KEY,
    task_list_id bigint REFERENCES task_list (id) NOT NULL,
    status       char(1)                          NOT NULL,
    name         varchar(300)                     NOT NULL,
    description  varchar(3000)
);


CREATE SEQUENCE task_list_seq
    MINVALUE 0;

CREATE SEQUENCE task_seq
    MINVALUE 0;

ALTER TABLE task
    OWNER TO postgres;
ALTER TABLE task_list
    OWNER TO postgres;
ALTER SEQUENCE task_list_seq OWNER TO postgres;
ALTER SEQUENCE task_seq OWNER TO postgres;
