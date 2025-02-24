PROGRAM matrix:
  VAR INT64 c;
  VAR INT64 i;
  VAR INT64 j;
  VAR INT64 DIM(3, 3) A;
  BEGIN
    i = 0;
    A = [[1, 0, 3],
         [0, 2, 0],
         [4, 0, 5]];
    WHILE i < 3 DO BEGIN
      WHILE j < 3 DO BEGIN
          value = A[i,j];
          j = j+1;
        END;
        j=0;
        i = i + 1;
    END;
  END.