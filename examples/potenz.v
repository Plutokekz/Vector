PROGRAM Example1:
VAR INT32 a;
VAR INT32 b;
VAR INT32 pot;
PROCEDURE potenz;
VAR INT32 y;
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