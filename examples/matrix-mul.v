PROGRAM matrix:
  VAR INT32 DIM(3,3) A Sparse;
  VAR INT32 DIM(3,3) B Identity;
  BEGIN
    A = [[1, 0, 3],
         [0, 2, 0],
         [4, 0, 5]];
    B = [[1, 0, 0],
         [0, 1, 0],
         [0, 0, 1]];
    WRITE A * B
  END.