PROGRAM matrix:
  VAR INT64 value;
  VAR INT64 DIM(7, 3) A;
  BEGIN
    A = [[1, 0, 3, 6, 7, 5, 4],
         [0, 2, 0, 1, 0, 3, 6],
         [4, 9, 5, 1, 0, 3, 6]];
    value = A[1,2];
  END.