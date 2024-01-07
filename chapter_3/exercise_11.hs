-- exercise 10

data TurnDirection =
    DirectionRight |
    DirectionLeft |
    DirectionStraight
    deriving (Show)

data Point = Point Double Double

data Quadrant = Quadrant Int

quadrantFrom :: Double -> Double -> Quadrant
quadrantFrom dx dy | dx >= 0 && dy >= 0 = Quadrant 1
quadrantFrom dx dy | dx <= 0 && dy >= 0 = Quadrant 2
quadrantFrom dx dy | dx <= 0 && dy <= 0 = Quadrant 3
quadrantFrom dx dy | dx >= 0 && dy <= 0 = Quadrant 4

quadrantDiff (Quadrant q1) (Quadrant q2) | abs (q1 - q2) == 3 = mod q1 4  - mod q2 4
quadrantDiff (Quadrant q1) (Quadrant q2) = q1 - q2


calculateTurn (Point xA yA) (Point xB yB) (Point xC yC) =
  case odiff of
    -1 -> DirectionLeft
    0 -> DirectionStraight
    1 -> DirectionRight

    where dx1 = xB - xA
          dy1 = yB - yA
          dx2 = xC - xA
          dy2 = yC - yA
          q1 = quadrantFrom dx1 dy1
          q2 = quadrantFrom dx2 dy2
          k1 = dy1 / dx1
          k2 = dy2 / dx2
          odiff = case abs (quadrantDiff q1 q2) of
            0 -> signum (k2 - k1) * coeff q1
            1 -> -1
            2 -> -signum (k2 - k1) * coeff q1
            3 -> 1
            where
              coeff (Quadrant q) | q == 1 || q == 3 = 1
              coeff (Quadrant q) | q == 2 || q == 4 = -1

-- exercise 11

tripletDirection :: [Point] -> [TurnDirection]
tripletDirection (p1:(p2:(p3:t))) =
    calculateTurn p1 p2 p3 : tripletDirection (p2:(p3:t))
tripletDirection l = [] 