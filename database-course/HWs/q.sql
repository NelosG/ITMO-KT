START TRANSACTION READ WRITE ISOLATION LEVEL SERIALIZABLE;
-- если бронирование
SELECT Reserve(:UserId, :Pass, :FlightId, :SeatNo);
-- если покупка места
SELECT BuyFree(:FlightId, :SeatNo);
COMMIT AND NO CHAIN;