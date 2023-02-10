CREATE OR REPLACE FUNCTION FlightsStatistics(IN UId int, IN Pass varchar(50))
    RETURNS Table
            (
                Reservable int,
                Buyable    int,
                Free       int,
                Reserved   int,
                Bought     int
            )
    LANGUAGE plpgsql
    SECURITY DEFINER
AS
$$
DECLARE
    allAvailableSeats      int;
    DECLARE boughtSeats    int;
    DECLARE reservedSeats  int;
    DECLARE freeSeats      int;
    DECLARE reservedByUser int;
BEGIN
    IF NOT IsValidUser(UId, Pass) THEN
        RETURN;
    END IF;

    allAvailableSeats := (SELECT COUNT(*)
                          FROM AllSeats
                          WHERE FlightTime > NOW());
    boughtSeats := (SELECT COUNT(*)
                    FROM Tickets
                             NATURAL JOIN AllSeats
                    WHERE State = 'bought'
                      AND FlightTime > NOW());
    reservedSeats := (SELECT COUNT(*)
                      FROM Tickets
                               NATURAL JOIN AllSeats
                      WHERE State = 'reserved'
                        AND Expires > NOW()
                        AND FlightTime > NOW());
    freeSeats := allAvailableSeats - boughtSeats - reservedSeats;

    reservedByUser := (SELECT COUNT(*)
                       FROM Tickets
                                NATURAL JOIN AllSeats
                       WHERE State = 'reserved'
                         AND UserId = UId
                         AND Expires > NOW()
                         AND FlightTime > NOW());

    RETURN QUERY
        SELECT *
        FROM (VALUES (freeSeats, freeSeats + reservedByUser, freeSeats, reservedSeats, boughtSeats)) AS t
                 (Reservable, Buyable, Free, Reserved, Bought);
END
$$;
