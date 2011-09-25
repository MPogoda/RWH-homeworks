import Data.List (sortBy, minimumBy)

data Direction = CounterClockwiseTurn | ClockwiseTurn | NoTurn
  deriving (Eq, Show)

grahamScan :: [(Double, Double)] -> [(Double, Double)]
grahamScan points = map (decenterPoint minXY) gPoints
                    where gPoints         = grahamScanHelper sortedPoints
                          sortedPoints    = sortBy comparePolar centeredPoints
                          centeredPoints  = map (centerPoint minXY) points
                          minXY           = minimumBy compareDecart points

decenterPoint :: (Double, Double) -> (Double, Double) -> (Double, Double)
decenterPoint (lvlX, lvlY) = centerPoint (-lvlX, -lvlY)

grahamScanHelper :: [(Double, Double)] -> [(Double, Double)]
grahamScanHelper points = if (length gPoints) < (length points) then
                            grahamScanHelper gPoints
                          else
                            points
                          where gPoints = grahamScanStep (points ++ (take 1 points))

grahamScanStep :: [(Double, Double)] -> [(Double, Double)]
grahamScanStep (x:y:rest@(z:_)) =
  if getDirection x y z /= CounterClockwiseTurn then
    grahamScanStep ( x:rest )
  else
    x:( grahamScanStep ( y:rest ) )
grahamScanStep (x:xs) = [x]

getDirection :: (Double, Double)-> (Double, Double) -> (Double, Double) -> Direction
getDirection (p1x, p1y) (p2x, p2y) (p3x, p3y) =
  case (compare r 0) of
    LT -> ClockwiseTurn
    EQ -> NoTurn
    GT -> CounterClockwiseTurn
  where r = (p2x - p1x) * (p3y - p1y) - (p3x - p1x) * (p2y - p1y)

comparePolar :: (Double, Double) -> (Double, Double) -> Ordering
comparePolar (x1, y1) (x2, y2) =
  if phi1 == phi2 then
    compare rho1 rho2
  else
    compare phi2 phi1
  where rho1 = sqrt (x1 ^ 2 + y1 ^ 2)
        rho2 = sqrt (x2 ^ 2 + y2 ^ 2)
        phi1 = if rho1 /= 0 then x1 / rho1 else 1
        phi2 = if rho2 /= 0 then x2 / rho2 else 1

centerPoint :: (Double, Double) -> (Double, Double) -> (Double, Double)
centerPoint (lvlX, lvlY) (x, y) = (x - lvlX, y - lvlY)

compareDecart :: (Double, Double) -> (Double, Double) -> Ordering
compareDecart (x1, y1) (x2, y2) = if y1 == y2 then
                                    compare x1 x2
                                  else
                                    compare y1 y2
