CREATE OR REPLACE PROCEDURE CompressSeats(IN FId int)
    LANGUAGE plpgsql
    SECURITY DEFINER
AS
$$
DECLARE
    currentSeat varchar(4);
    DECLARE seatCursor CURSOR FOR
        SELECT SeatNo
        FROM AllSeats
        WHERE FlightId = FId
        ORDER BY SeatNo
            FOR READ ONLY;
    DECLARE boughtCursor CURSOR FOR
        SELECT SeatNo
        FROM Tickets
        WHERE FlightId = FId
          AND State = 'bought'
        ORDER BY SeatNo
            FOR UPDATE;
    DECLARE reservedCursor CURSOR FOR
        SELECT SeatNo
        FROM Tickets
        WHERE FlightId = FId
          AND State = 'reserved'
        ORDER BY SeatNo
            FOR UPDATE;
BEGIN
    IF NOT EXISTS(
            SELECT FlightId FROM Flights WHERE FlightId = FId
        ) THEN
        RETURN;
    END IF;

    OPEN seatCursor;
--     open boughtCursor;
    FOR SNo IN boughtCursor
        LOOP
            FETCH NEXT FROM seatCursor INTO currentSeat;
            UPDATE Tickets
            SET SeatNo = currentSeat
            WHERE CURRENT OF boughtCursor;
        END LOOP;
    --     close boughtCursor;
--     
--     open reservedCursor;
    FOR SNo IN reservedCursor
        LOOP
            FETCH NEXT FROM seatCursor INTO currentSeat;
            UPDATE Tickets
            SET SeatNo = currentSeat
            WHERE CURRENT OF reservedCursor;
        END LOOP;
--     close reservedCursor;
    CLOSE seatCursor;
END
$$;
