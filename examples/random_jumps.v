PROGRAM RandomJumps:
CONST INT64 jumps = 20;
VAR INT64 DIM(20, 2) path;
VAR INT64 DIM(2) pos;
VAR INT64 i;
VAR INT64 maxDist;
VAR INT64 dist;

PROCEDURE computeMaxDistance;
BEGIN
  i = 0;
  maxDist = 0;
  pos[0] = 0;
  pos[1] = 0;
  path = GenRandom DIM(20, 2) INT64;
  
  WHILE i < jumps DO BEGIN
    pos[0] = pos[0] + path[i][0];
    pos[1] = pos[1] + path[i][1];
    
    dist = pos[0];
    IF dist < 0 THEN BEGIN
      dist = 0 - dist
    END;
    IF pos[1] < 0 THEN BEGIN
      dist = dist + (0 - pos[1])
    END;
    IF NOT pos[1] < 0 THEN BEGIN
      dist = dist + pos[1]
    END;
    
    IF dist > maxDist THEN BEGIN
      maxDist = dist
    END;
    i = i + 1
  END
END;

BEGIN
  CALL computeMaxDistance;
  maxDist = maxDist
END. 