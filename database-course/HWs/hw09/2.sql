-- Help procedure for adding new users
CREATE OR REPLACE PROCEDURE CreateNewUser(IN UId int, IN Pass varchar(50))
    LANGUAGE plpgsql
    SECURITY DEFINER
AS
$$
BEGIN
    INSERT INTO Users
        (UserId, Passwd)
    VALUES (UId, crypt(Pass, gen_salt('bf')));
END
$$;

CREATE OR REPLACE FUNCTION IsValidUser(IN UId int, IN Pass varchar(50))
    RETURNS boolean
    LANGUAGE plpgsql
    SECURITY DEFINER
AS
$$
BEGIN
    RETURN EXISTS(
            SELECT UserId
            FROM Users
            WHERE UserId = UId
              AND Passwd = crypt(Pass, Passwd)
        );
END
$$;

CREATE OR REPLACE FUNCTION Reserve(IN UId int, IN Pass varchar(50), IN FId int, IN SNo varchar(4))
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

    -- check if Seat in this Plane actually exists
    IF SNo NOT IN (SELECT SeatNo
                   FROM AllSeats
                   WHERE FlightId = FId) THEN
        RETURN FALSE;
    END IF;

    IF SNo IN (SELECT FindNotFreeSeats.SeatNo
               FROM FindNotFreeSeats(FId)) THEN
        RETURN FALSE;
    END IF;

    -- if exist an old expired log update needed, else insert
    INSERT INTO Tickets
        (FlightId, SeatNo, State, UserId, Expires)
    VALUES (FId, SNo, 'reserved', UId, NOW() + INTERVAL '3 day')
    ON CONFLICT (FId, SNo)
        DO UPDATE SET UserId  = UId,
                      Expires = NOW() + INTERVAL '3 day';
    RETURN TRUE;
END
$$;
