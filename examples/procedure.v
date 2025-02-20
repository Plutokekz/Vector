PROGRAMM Example1:
    CONST INT32 b = 12345;
    VAR INT32 a;
    VAR INT32 c;
    PROCEDURE add;
        BEGIN
            a = b + 25;
        END;
    BEGIN
        CALL add;
        c = a;
    END.