PROGRAMM test2;
    VAR x = 5;
    VAR A: Sparse[INT32];
    VAR B: Sparse[INT32];
    BEGIN
        A = [[1, 0, 3]
            [0, 2, 0]
            [4, 0, 5]]
        B = [[1, 0, 0]
            [0, 1, 0]
            [0, 0, 1]]
        C = A .* B;
    END.