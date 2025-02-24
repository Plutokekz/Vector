PROGRAM GCD:
    VAR INT64 a;
    VAR INT64 b;
    VAR INT64 result;
    PROCEDURE euclidean;
    VAR INT64 temp;
    BEGIN
        WHILE NOT b = 0 DO BEGIN
            temp = b;
            b = a - (a / b) * b;
            a = temp
        END;
        result = a
    END;
    BEGIN
        a = 48;
        b = 18;
        CALL euclidean;
        result = result
    END. 