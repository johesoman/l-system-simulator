{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}



module Main where



import qualified Turtle                           
import           Data.Char
import           Data.Maybe
import           GHC.Generics
import           System.Environment
import qualified Data.Text                        as T
import qualified Data.HashMap.Strict              as H
import qualified Data.ByteString.Char8            as B
import qualified Data.ByteString.Lazy.Char8       as BL
import qualified Graphics.Gloss                   as Gloss
import qualified Graphics.Gloss.Data.Vector       as Gloss
import qualified Graphics.Gloss.Interface.IO.Game as Gloss
import qualified Data.Aeson                       as Aeson
import qualified Data.Aeson.Types                 as Aeson
import qualified Data.Aeson.Encode.Pretty         as Aeson
import qualified Text.Parsec                      as Parsec
import qualified Text.Parsec.String               as Parsec
import qualified Text.Parsec.Number               as Parsec



-- +++++++++++++++++
-- + TurtleCommand +
-- +++++++++++++++++



data TurtleCommand
  = Pop
  | Push
  | Turn Int
  | Move Int
  | Draw Int
  deriving (Show, Eq)



-- +++++++++++++++++++++++++++++++++
-- + serialization/deserialization +
-- +++++++++++++++++++++++++++++++++



parseTurtleCommand :: Parsec.Parser TurtleCommand
parseTurtleCommand = Parsec.choice . map Parsec.try $
  [ Pop  <$ Parsec.string "pop"
  , Push <$ Parsec.string "push"
  , p Turn "turn"
  , p Move "move"
  , p Draw "draw"
  ]

  where
    p :: (Int -> TurtleCommand) -> String -> Parsec.Parser TurtleCommand
    p f s = do
      _ <- Parsec.string s
      _ <- Parsec.many1 (Parsec.char ' ')
      x <- Parsec.int
      pure (f x)



instance Aeson.FromJSON TurtleCommand where
  parseJSON (Aeson.String t) =
    case Parsec.parse parseTurtleCommand "" (T.unpack t) of
      Left _  -> fail $ T.unpack t ++ " is not a valid command."
      Right c -> pure c

    where
      parseJSON invalid = Aeson.typeMismatch "command" invalid



instance Aeson.ToJSON TurtleCommand where
  toJSON Pop      = Aeson.String (T.pack "pop")
  toJSON Push     = Aeson.String (T.pack "push")
  toJSON (Turn x) = Aeson.String (T.pack ("turn " ++ show x))
  toJSON (Move x) = Aeson.String (T.pack ("move " ++ show x))
  toJSON (Draw x) = Aeson.String (T.pack ("draw " ++ show x))



-- ++++++++++++
-- + L-system +
-- ++++++++++++



data LSystem = LSystem
  { commands   :: H.HashMap Char TurtleCommand
  , axiom      :: String
  , rules      :: H.HashMap Char String
  , iterations :: Int
  } deriving (Show, Eq, Generic, Aeson.FromJSON, Aeson.ToJSON)



expand :: LSystem -> String
expand sys = go (iterations sys) (axiom sys)
  where
    go n s
      | n < 1 = s
      | otherwise =
          go (n - 1) (concatMap f s)

    f c = fromMaybe [c] (H.lookup c (rules sys))



eval :: LSystem -> Turtle.Turtle -> Turtle.Turtle
eval sys t1 = foldl f t1 (expand sys)
  where
    f t2 c =
      case H.lookup c (commands sys) of
        Nothing       -> t2
        Just Pop      -> Turtle.pop t2
        Just Push     -> Turtle.push t2
        Just (Turn x) -> Turtle.turn x t2
        Just (Move x) -> Turtle.move x t2
        Just (Draw x) -> Turtle.draw x t2



-- +++++++++
-- + World +
-- +++++++++



data State
  = Paused
  | Running
  deriving (Show, Eq)



data World = World
  { turtle :: Turtle.Turtle
  , scene  :: [Gloss.Picture]
  , state  :: State
  , pencol :: Gloss.Color
  , fps    :: Int
  , cps    :: Int
  , ticks  :: Int
  } deriving (Show, Eq)



numberOfPics :: World -> (Int, World)
numberOfPics w
  | cps w < fps w && ticks w < 2 = (1, w {ticks = fps w `div` cps w})
  | cps w < fps w                = (0, w {ticks = ticks w - 1})
  | otherwise = (cps w `div` fps w, w)



mkWorld :: Turtle.Turtle -> Gloss.Color -> Int -> Int -> World
mkWorld t c x y = World
  { turtle = t
  , scene  = []
  , state  = Paused
  , pencol = c
  , fps    = x
  , cps    = y
  , ticks  = 0
  }



tick :: Float -> World -> World
tick _ w1
  | state w1 == Paused = w1
  | otherwise =
      let (n, w2) = numberOfPics w1
      in case Turtle.takePics n (turtle w2) of
           ([], _) -> w2
           (ps, t) -> w2 { turtle = t, scene = ps ++ scene w2 }



draw :: World -> Gloss.Picture
draw w = Gloss.Pictures . map (Gloss.color (pencol w)) . scene $ w



event :: Gloss.Event -> World -> World
event (Gloss.EventKey (Gloss.SpecialKey Gloss.KeySpace) Gloss.Up _ _) w =
  case state w of
    Paused  -> w {state = Running}
    Running -> w {state = Paused}

event _ w = w



-- ++++++++++
-- + Config +
-- ++++++++++



colors :: H.HashMap String Gloss.Color
colors = H.fromList
  [ ("black"     , Gloss.black)
  , ("red"       , Gloss.red)
  , ("green"     , Gloss.green)
  , ("blue"      , Gloss.blue)
  , ("yellow"    , Gloss.yellow)
  , ("cyan"      , Gloss.cyan)
  , ("magenta"   , Gloss.magenta)
  , ("rose"      , Gloss.rose)
  , ("violet"    , Gloss.violet)
  , ("azure"     , Gloss.azure)
  , ("aquamarine", Gloss.aquamarine)
  , ("chartreuse", Gloss.chartreuse)
  , ("orange"    , Gloss.orange)
  ]



validateColor :: String -> Either String Gloss.Color
validateColor s =
  case H.lookup (map toLower s) colors of
    Nothing -> Left $
      "Error: Unknown color " ++ s ++ "."

    Just c  -> Right c



data VideoConfig = VideoConfig
  { windowWidth       :: Int
  , windowHeight      :: Int
  , windowOpenX       :: Int
  , windowOpenY       :: Int
  , backgroundColor   :: String
  , penStartX         :: Int
  , penStartY         :: Int
  , penStartAngle     :: Int
  , penColor          :: String
  , commandsPerSecond :: Int
  , framesPerSecond   :: Int
  } deriving (Show, Eq, Generic, Aeson.FromJSON, Aeson.ToJSON)



data Config = Config
  { video  :: VideoConfig
  , system :: LSystem
  } deriving (Show, Eq, Generic, Aeson.FromJSON, Aeson.ToJSON)



-- +++++++++++++++++++
-- + default configs + 
-- +++++++++++++++++++



plant :: Config
plant = Config vid sys
  where
    sys = LSystem
      { commands   = H.fromList [ ('-', Turn 25), ('+', Turn (-25))
                                , ('[', Push)   , (']', Pop)
                                , ('F', Draw 8)
                                ]
      , axiom      = "X"
      , rules      = H.fromList [ ('X', "F+[[X]-X]-F[-FX]+X")
                                , ('F', "FF")]
      , iterations = 6
      }

    vid = VideoConfig
      { windowWidth = 800
      , windowHeight = 600
      , windowOpenX = 0
      , windowOpenY = 0
      , backgroundColor = "black"
      , framesPerSecond = 60
      , penStartX = 215
      , penStartY = -80
      , penStartAngle = 0
      , penColor = "green"
      , commandsPerSecond = 120
      }



dragoncurve :: Config
dragoncurve = Config vid sys
  where
    sys = LSystem
      { commands   = H.fromList [ ('-', Turn (-90)), ('+', Turn 90)
                                , ('F', Draw 16)
                                ]
      , axiom      = "FX"
      , rules      = H.fromList [('X', "X+YF+"), ('Y', "-FX-Y")]
      , iterations = 10
      }

    vid = VideoConfig
      { windowWidth = 800
      , windowHeight = 600
      , windowOpenX = 0
      , windowOpenY = 0
      , backgroundColor = "black"
      , framesPerSecond = 60
      , penStartX = 215
      , penStartY = -80
      , penStartAngle = 90
      , penColor = "cyan"
      , commandsPerSecond = 120
      }



sierpinski :: Config
sierpinski = Config vid sys
  where
    sys = LSystem
      { commands   = H.fromList [ ('+', Turn 120), ('-', Turn (-120))
                                , ('F', Draw 8)  , ('G', Draw 8)
                                ]
      , axiom      = "F-G-G"
      , rules      = H.fromList [ ('F', "F-G+F+G-F"), ('G', "GG")]
      , iterations = 6
      }

    vid = VideoConfig
      { windowWidth       = 800
      , windowHeight      = 600
      , windowOpenX       = 0
      , windowOpenY       = 0
      , backgroundColor   = "black"
      , penStartX         = -250
      , penStartY         = 225
      , penStartAngle     = 0
      , penColor          = "magenta"
      , commandsPerSecond = 120
      , framesPerSecond   = 60
      }


-- ++++++
-- + IO +
-- ++++++


readConfig :: String -> IO (Either String Config)
readConfig fp = do
  c <- B.readFile fp
  pure (Aeson.eitherDecodeStrict' c)



writeConfig :: String -> Config -> IO ()
writeConfig fp = BL.writeFile fp . Aeson.encodePretty' (indent 2)
  where
    indent x = Aeson.defConfig {Aeson.confIndent = Aeson.Spaces x}



-- ++++++++
-- + main +
-- ++++++++



runSimulation :: Gloss.Color -> Gloss.Color -> VideoConfig -> LSystem -> IO ()
runSimulation bkg pen vid sys = 
  Gloss.play win bkg (fps world) world draw event tick

  where
    win = Gloss.InWindow
      "L-system"
      (windowWidth vid, windowHeight vid)
      (windowOpenX vid, windowOpenY vid)

    world = mkWorld
      (eval sys turtle)
      pen
      (framesPerSecond vid)
      (commandsPerSecond vid)

    turtle = Turtle.makeTurtle 
      (penStartX vid, penStartY vid) 
      (penStartAngle vid)



main :: IO ()
main = do
  as <- getArgs

  case as of
    [fp] -> do
      res <- readConfig fp

      case res of
        Left _ -> putStrLn "Error: unable to parse JSON file."

        Right cfg -> do
          case validateColor (backgroundColor (video cfg)) of
            Left err2 -> putStrLn err2

            Right c1  ->
              case validateColor (penColor (video cfg)) of
                Left err3 -> putStrLn err3

                Right c2  ->
                  runSimulation c1 c2 (video cfg) (system cfg)

    _ -> putStrLn "Error: expected 1 argument."
