CREATE OR REPLACE FUNCTION FindNotFreeSeats(IN FId int)
    RETURNS Table
            (
                SeatNo varchar(4)
            )
    LANGUAGE plpgsql
    SECURITY DEFINER
AS
$$
BEGIN
    RETURN QUERY
        SELECT Tickets.SeatNo
        FROM Tickets
        WHERE FlightId = FId
            AND State = 'reserved' AND Expires > NOW()
           OR State = 'bought'
        UNION
        SELECT AllSeats.SeatNo
        FROM AllSeats
        WHERE FlightId = FId
          AND FlightTime < NOW();
END
$$;

CREATE OR REPLACE FUNCTION FreeSeats(IN FId int)
    RETURNS Table
            (
                SeatNo varchar(4)
            )
    LANGUAGE plpgsql
    SECURITY DEFINER
AS
$$
DECLARE
    PId int;
BEGIN
    PId := (SELECT PlaneId
            FROM Flights
            WHERE FlightId = FId);
    RETURN QUERY
        SELECT s.SeatNo
        FROM Seats s
        WHERE PlaneId = PId
          AND s.SeatNo NOT IN (SELECT FindNotFreeSeats.SeatNo
                               FROM FindNotFreeSeats(FId));
END
$$;
