import Data.List.Split

makeMove (x, y, z) "n"  = (x, y, z-1)
makeMove (x, y, z) "s"  = (x, y, z+1)
makeMove (x, y, z) "nw" = (x, y+1, z)
makeMove (x, y, z) "se" = (x, y-1, z)
makeMove (x, y, z) "ne" = (x+1, y, z)
makeMove (x, y, z) "sw" = (x-1, y, z)

distanceTo (x, y, z) = minimum [xx, yy, zz]
    where
        xx = abs(x-x) + abs(y-x) + abs(z-x)
        yy = abs(x-y) + abs(y-y) + abs(z-y)
        zz = abs(x-z) + abs(y-z) + abs(z-z)


solveA = fmap (distanceTo . foldl makeMove (0, 0, 0) . splitOn ",") . readFile

-- -- --

makeMoveWithAccum (coords, maxDistance) direction = (newCoords, newMaxDistance)
    where
        newCoords = makeMove coords direction
        newDistance = distanceTo newCoords
        newMaxDistance = if newDistance > maxDistance then newDistance else maxDistance

solveB = fmap (snd . foldl makeMoveWithAccum ((0, 0, 0), 0) . splitOn ",") . readFile