import Data.List

data Direction =  CounterClockwiseTurn
                | ClockwiseTurn
                | NoTurn
    deriving (Eq, Show)

data Point = Point Double Double
  deriving (Eq, Show)

toPoint :: (Double, Double) -> Point
toPoint a = Point (fst a) (snd a)

getDirection :: Point -> Point -> Point -> Direction
getDirection (Point p1x p1y) (Point p2x p2y) (Point p3x p3y) =
  case (compare r 0) of
    LT -> ClockwiseTurn
    EQ -> NoTurn
    GT -> CounterClockwiseTurn
  where r = (p2x - p1x) * (p3y - p1y) - (p3x - p1x) * (p2y - p1y)

sortPoints :: [Point] -> [Point]
sortPoints points = sortBy sorting points
  where sorting (Point x1 y1) (Point x2 y2) = if phi1 == phi2 then
                                                compare rho1 rho2
                                              else
                                                compare phi1 phi2
                                              where rho1 = sqrt (x1 * x1 + y1 * y1)
                                                    rho2 = sqrt (x2 * x2 + y2 * y2)
                                                    phi1 = atan (x1 / y1)
                                                    phi2 = atan (x2 / y2)

makeDirections :: [Point] -> [Direction]
makeDirections points = if (length points) < 3 then
                          []
                        else
                          (getDirection x y z) : (makeDirections tailPoints)
                        where x = head points
                              tailPoints = tail points
                              y = head tailPoints
                              z = head (tail tailPoints)
