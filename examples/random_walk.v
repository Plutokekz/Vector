PROGRAM RandomWalk:
    CONST INT64 steps = 20;
    VAR INT64 DIM(20, 2) walk;
    VAR INT64 DIM(2) pos;
    VAR INT64 i;
    VAR INT64 maxDist;
    VAR INT64 dist;
    VAR INT64 stepX;
    VAR INT64 stepY;

    PROCEDURE computeMaxDistance;
    BEGIN
        i = 0;
        maxDist = 0;
        pos[0] = 0;
        pos[1] = 0;
        
        walk = GenRandom DIM(20, 2) INT64;  -- Generate steps
        
        WHILE i < steps DO BEGIN
            -- Convert random numbers to -1 or 1 based on sign
            stepX = walk[i][0];
            IF stepX < 0 THEN BEGIN
                pos[0] = pos[0] - 1
            END;
            IF stepX >= 0 THEN BEGIN
                pos[0] = pos[0] + 1
            END;
            
            stepY = walk[i][1];
            IF stepY < 0 THEN BEGIN
                pos[1] = pos[1] - 1
            END;
            IF stepY >= 0 THEN BEGIN
                pos[1] = pos[1] + 1
            END;
            -- Manhattan distance calculation
            dist = pos[0];
            IF dist < 0 THEN BEGIN
                dist = 0 - dist
            END;
            
            IF pos[1] < 0 THEN BEGIN
                dist = dist + (0 - pos[1])
            END;
            IF pos[1] >= 0 THEN BEGIN
                dist = dist + pos[1]
            END;
            -- Update max distance if needed
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