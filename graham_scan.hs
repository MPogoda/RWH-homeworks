data Direction =  CounterClockwiseTurn
                | ClockwiseTurn
                | NoTurn
    deriving (Eq, Show)

data Point = Point Double Double
  deriving (Eq, Show)

data Polar = Polar Double Double
  deriving (Eq, Show)

toPolar :: Point -> Polar
toPolar (Point x y) = Polar rho phi
                      where rho = sqrt (x * x + y * y)
                            phi = atan (y / x)

toPoint :: (Double, Double) -> Point
toPoint a = Point (fst a) (snd a)

getDirection :: Point -> Point -> Point -> Direction
getDirection (Point p1x p1y) (Point p2x p2y) (Point p3x p3y) =
  case (compare r 0) of
    LT -> ClockwiseTurn
    EQ -> NoTurn
    GT -> CounterClockwiseTurn
  where r = (p2x - p1x) * (p3y - p1y) - (p3x - p1x) * (p2y - p1y)

makePoints :: [(Double, Double)] -> [Point]
makePoints xs = map toPoint xs

sortPoints :: [Point] -> [Point]
sortPoints points = 

makeDirections :: [Point] -> [Direction]
makeDirections points = if (length points) < 3 then
                          []
                        else
                          (getDirection x y z) : (makeDirections tailPoints)
                        where x = head points
                              tailPoints = tail points
                              y = head tailPoints
                              z = head (tail tailPoints)
