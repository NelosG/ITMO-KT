CREATE OR REPLACE FUNCTION BuyReserved(IN UId int, IN Pass varchar(50), IN FId int, IN SNo varchar(4))
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

    IF NOT IsValidUser(UId, Pass) THEN
        RETURN FALSE;
    END IF;

    IF EXISTS(
            SELECT FlightId
            FROM Tickets
            WHERE FlightId = FId
              AND SeatNo = SNo
              AND State = 'reserved'
              AND UserId = UId
              AND Expires > NOW()
        ) THEN
        UPDATE Tickets
        SET State   = 'bought',
            UserId  = NULL,
            Expires = NULL
        WHERE FlightId = FId
          AND SeatNo = SNo;
        RETURN TRUE;
    END IF;

    RETURN FALSE;
END
$$;
