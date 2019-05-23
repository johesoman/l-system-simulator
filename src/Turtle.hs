module Turtle where



import qualified Graphics.Gloss             as Gloss
import qualified Graphics.Gloss.Data.Point  as Gloss
import qualified Graphics.Gloss.Data.Vector as Gloss



data Turtle = Turtle
  { position    :: Gloss.Point
  , orientation :: Int
  , pictures    :: [Gloss.Picture]
  , states      :: [(Gloss.Point, Int)]
  } deriving (Show, Eq)



defaultTurtle :: Turtle
defaultTurtle = Turtle
  { position    = (0, 0)
  , orientation = 0
  , pictures    = []
  , states      = []
  }



makeTurtle :: (Int, Int) -> Int -> Turtle
makeTurtle (x, y) o = Turtle
  { position    = (fromIntegral x, fromIntegral y)
  , orientation = o
  , pictures    = []
  , states      = []
  }



takePics :: Int -> Turtle -> ([Gloss.Picture], Turtle)
takePics n t =
  let (xs, ys) = splitAt n (pictures t)
  in (xs, t {pictures = ys})



move :: Int -> Turtle -> Turtle
move dist t =
  let d        = fromIntegral dist
      (x1, y1) = position t 
      (x2, y2) = unitVecFromOrientation t
  in t {position = (x1 + x2 * d, y1 + y2 * d)}

  where
    unitVecFromOrientation = 
      Gloss.unitVectorAtAngle
      . toRad
      . fromIntegral
      . orientation

    toRad :: Float -> Float
    toRad = (*) (pi / 180.0)



draw :: Int -> Turtle -> Turtle
draw dist t1 =
  let p1 = position t1
      t2 = move dist t1
      p2 = position t2
  in t2 {pictures = pictures t2 ++ [Gloss.line [p1, p2]]}



turn :: Int -> Turtle -> Turtle
turn a t = t {orientation = orientation t + a `mod` 360}



push :: Turtle -> Turtle
push t = t {states = (position t, orientation t) : states t}



pop :: Turtle -> Turtle
pop t =
  case states t of
    []          -> t
    (p, o) : ss -> t
      { position    = p
      , orientation = o
      , states      = ss
      }



