PROGRAM Example1:
VAR INT64 a;
VAR INT64 b;
VAR INT64 pot;
PROCEDURE potenz;
VAR INT64 y;
BEGIN
  pot = 1;
  y = b;
  WHILE NOT y < 1 DO BEGIN
    pot = pot * a;
    y = y - 1
  END
END;
BEGIN
  a = 10;
  b = 3;
  CALL potenz;
pot = pot
END.