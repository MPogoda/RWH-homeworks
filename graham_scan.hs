import Data.List (sortBy, minimumBy)

data Direction = CounterClockwiseTurn | ClockwiseTurn | NoTurn
  deriving (Eq, Show)

data Point = Point Double Double
  deriving (Eq, Show)

toPoint :: (Double, Double) -> Point
toPoint (x, y) = Point x y

getDirection :: Point -> Point -> Point -> Direction
getDirection (Point p1x p1y) (Point p2x p2y) (Point p3x p3y) =
  case (compare r 0) of
    LT -> ClockwiseTurn
    EQ -> NoTurn
    GT -> CounterClockwiseTurn
  where r = (p2x - p1x) * (p3y - p1y) - (p3x - p1x) * (p2y - p1y)

comparePolar :: Point -> Point -> Ordering
comparePolar p1@(Point x1 y1) p2@(Point x2 y2) =
  if phi1 == phi2 then
    if phi1 == 0 then
      compare rho2 rho1
    else
      compare rho1 rho2
  else
    compare phi2 phi1
  where rho1 = sqrt (x1 ^ 2 + y1 ^ 2)
        rho2 = sqrt (x2 ^ 2 + y2 ^ 2)
        phi1 = if rho1 /= 0 then x1 / rho1 else 1
        phi2 = if rho2 /= 0 then x2 / rho2 else 1

centerPoint :: Point -> Point -> Point
centerPoint (Point lvlX lvlY) (Point x y) = Point (x - lvlX) (y - lvlY)

decenterPoint :: Point -> Point -> Point
decenterPoint (Point lvlX lvlY) p = centerPoint (Point (-lvlX) (-lvlY)) p

compareDecart :: Point -> Point -> Ordering
compareDecart (Point x1 y1) (Point x2 y2) = if y1 == y2 then
                                              compare x1 x2
                                            else
                                              compare y1 y2

step :: [Point] -> [Point]
step (x:y:rest@(z:_)) =
  if getDirection x y z == ClockwiseTurn then
    step (x:rest)
  else
    x:(step (y:rest))
step (x:xs) = [x]

grahamScan :: [Point] -> [Point]
grahamScan points = ((map (decenterPoint minXY)) . step) (sortedPoints ++ [x])
  where sortedPoints = sortBy comparePolar centerededPoints
        centerededPoints = map (centerPoint minXY) points
        minXY = minimumBy compareDecart points
        x = head sortedPoints
