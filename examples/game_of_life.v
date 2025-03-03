PROGRAM GameOfLife:
CONST INT8 size = 8;
VAR INT8 DIM(8, 8) grid;
VAR INT64 DIM(2) pos;
VAR INT64 i;
VAR INT64 j;
VAR INT64 neighbors;

PROCEDURE playFirstGeneration;
BEGIN
  i = 1;
  maxNeighbors = 0;
  grid = GenRandom DIM(8, 8) INT8;  -- Random initial state
  
  WHILE i < size - 1 DO BEGIN
    j = 1;
    WHILE j < size - 1 DO BEGIN
      neighbors = 0;
      -- Count neighbors (excluding edges for simplicity)
      IF grid[i-1][j-1] > 0 THEN BEGIN
        neighbors = neighbors + 1
      END;
      IF grid[i-1][j] > 0 THEN BEGIN
        neighbors = neighbors + 1
      END;
      IF grid[i-1][j+1] > 0 THEN BEGIN
        neighbors = neighbors + 1
      END;
      IF grid[i][j-1] > 0 THEN BEGIN
        neighbors = neighbors + 1
      END;
      IF grid[i][j+1] > 0 THEN BEGIN
        neighbors = neighbors + 1
      END;
      IF grid[i+1][j-1] > 0 THEN BEGIN
        neighbors = neighbors + 1
      END;
      IF grid[i+1][j] > 0 THEN BEGIN
        neighbors = neighbors + 1
      END;
      IF grid[i+1][j+1] > 0 THEN BEGIN
        neighbors = neighbors + 1
      END;
      j = j + 1
    END;
    i = i + 1
  END
  IF neighbors > 3 THEN BEGIN
    grid[i][j] = 0
  END;
  IF neighbors < 2 THEN BEGIN
    grid[i][j] = 0
  END;
  IF neighbors = 3 THEN BEGIN
    grid[i][j] = 1
  END;
  IF neighbors = 2 THEN BEGIN
    grid[i][j] = grid[i][j]
  END;
END;

BEGIN
  CALL playFirstGeneration;
END. 