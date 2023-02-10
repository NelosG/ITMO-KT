CREATE TABLE Flights
(
    FlightId   int       NOT NULL PRIMARY KEY,
    FlightTime timestamp NOT NULL,
    PlaneId    int       NOT NULL
);

CREATE TABLE Seats
(
    PlaneId int        NOT NULL,
    SeatNo  varchar(4) NOT NULL
);

-- View for seats
CREATE VIEW AllSeats AS
SELECT FlightId, FlightTime, PlaneId, SeatNo
FROM Flights
         NATURAL JOIN Seats;

-- Table for users with passwords
CREATE TABLE Users
(
    UserId int  NOT NULL PRIMARY KEY,
    -- Password hash
    Passwd text NOT NULL
);

CREATE TYPE TicketState AS enum ('reserved', 'bought');

-- Table for reserved and bought seats
CREATE TABLE Tickets
(
    FlightId int         NOT NULL,
    SeatNo   varchar(4)  NOT NULL,
    State    TicketState NOT NULL,
    UserId   int,
    Expires  timestamp,
    PRIMARY KEY (FlightId, SeatNo),
    FOREIGN KEY (FlightId) REFERENCES Flights (FlightId),
    FOREIGN KEY (UserId) REFERENCES Users (UserId)
);
