CREATE OR REPLACE FUNCTION BuyFree(IN FId int, IN SNo varchar(4))
    RETURNS boolean
    LANGUAGE plpgsql
    SECURITY DEFINER
AS
$$
BEGIN
    IF NOT EXISTS(
            SELECT FlightId FROM Flights WHERE FlightId = FId
        ) THEN
        RETURN FALSE;
    END IF;

    IF SNo NOT IN (SELECT SeatNo
                   FROM AllSeats
                   WHERE FlightId = FId) THEN
        RETURN FALSE;
    END IF;

    IF SNo IN (SELECT FindNotFreeSeats.SeatNo
               FROM FindNotFreeSeats(FId)) THEN
        RETURN FALSE;
    END IF;

    INSERT INTO Tickets
        (FlightId, SeatNo, State)
    VALUES (FId, SNo, 'bought');

    RETURN TRUE;
END
$$;
