-- Users
CREATE TABLE users
(
    id bigint NOT NULL PRIMARY KEY
);

-- ADMIN USER
INSERT INTO users VALUES (0);

CREATE SEQUENCE users_seq MINVALUE 1;

ALTER TABLE users
    OWNER TO postgres;

ALTER SEQUENCE users_seq OWNER TO postgres;


-- (user's)Wallet
CREATE TABLE wallet
(
    id      bigint NOT NULL PRIMARY KEY,
    user_id bigint NOT NULL UNIQUE,
    amount  bigint NOT NULL,
    CONSTRAINT fk_wallet__user FOREIGN KEY (user_id) REFERENCES users (id)
);

-- ADMIN WALLET
INSERT INTO wallet VALUES (0, 0, 0);

CREATE SEQUENCE wallet_seq MINVALUE 1;

ALTER TABLE wallet
    OWNER TO postgres;

ALTER SEQUENCE wallet_seq OWNER TO postgres;

-- Company
CREATE TABLE company
(
    id          bigint       NOT NULL PRIMARY KEY,
    name        varchar(300) NOT NULL,
    description varchar(3000)
);

CREATE SEQUENCE company_seq MINVALUE 0;

ALTER TABLE company
    OWNER TO postgres;

ALTER SEQUENCE company_seq OWNER TO postgres;

-- Stock
CREATE TABLE stock
(
    id         bigint NOT NULL PRIMARY KEY,
    name        varchar(300) NOT NULL,
    company_id bigint NOT NULL,
    price      bigint NOT NULL,
    CONSTRAINT fk_stock__company FOREIGN KEY (company_id) REFERENCES company (id)
);

CREATE INDEX idx_stock__name_company_id ON stock (name, company_id);

CREATE SEQUENCE stock_seq MINVALUE 0;

ALTER TABLE stock
    OWNER TO postgres;

ALTER SEQUENCE stock_seq OWNER TO postgres;

-- Stocks_Wallet
CREATE TABLE stocks_wallet
(
    id        bigint NOT NULL PRIMARY KEY,
    stock_id  bigint NOT NULL,
    wallet_id bigint NOT NULL,
    count     bigint NOT NULL,
    CONSTRAINT fk_stocks_wallet__stock FOREIGN KEY (stock_id) REFERENCES stock (id),
    CONSTRAINT fk_stocks_wallet__wallet FOREIGN KEY (wallet_id) REFERENCES wallet (id)
);

CREATE INDEX idx_stocks_wallet__stock_id_wallet_id ON stocks_wallet (stock_id, wallet_id);

CREATE SEQUENCE stocks_wallet_seq MINVALUE 0;

ALTER TABLE stocks_wallet
    OWNER TO postgres;

ALTER SEQUENCE stocks_wallet_seq OWNER TO postgres;


