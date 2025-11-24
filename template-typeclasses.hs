import Control.Monad.State

data Cell
    = Empty
    | Wall
    | Treasure Int       -- Waarde van de schat
    deriving (Show, Eq)

data Grid a = Grid [[a]]
    deriving (Show, Eq)

testGrid :: Grid Cell
testGrid = Grid
  [ [Empty, Wall, Empty, Treasure 10, Empty]
  , [Wall, Empty, Treasure 5, Empty, Wall]
  , [Empty, Empty, Wall, Empty, Empty]
  , [Treasure 20, Wall, Empty, Empty, Treasure 15]
  , [Empty, Empty, Empty, Wall, Empty]
  ]

invalidTestGrid :: Grid Cell
invalidTestGrid = Grid
  [ [Empty, Wall, Empty, Treasure 10, Empty]
  , [Wall, Empty, Treasure 5, Empty, Wall]
  , [Empty, Empty, Wall, Empty, Empty]
  , [Treasure 20, Wall, Empty, Empty, Treasure 15]
  , [Empty, Empty, Empty, Wall, Empty]
  ]
    
-- Functor

-- Voeg `Grid` toe aan de `Functor` class door instance te implementeren,
-- we willen dat fmap de functie op elke cel van de grid toepast:
instance Functor Grid where
    fmap = undefined

-- Definieer een helperfunctie `increaseTreasure` die de waarde van een `Treasure` verhoogt met een gegeven waarde:
-- increaseTreasure :: Cell -> Int -> Cell
-- increaseTreasure = undefined

-- Gebruik `fmap` en de hulperfunctie om alle schatten in de grid te verhogen met 5:
increaseAllTreasures :: Grid Cell -> Int -> Grid Cell
increaseAllTreasures = undefined


-- Foldable

-- Voeg `Grid` toe aan de `Foldable` class door instance te implementeren,
-- we willen dat er over alle elementen van het grid rij per rij wordt gefold:
instance Foldable Grid where
    foldMap = undefined -- Kies 1 van de 2 opties
    foldr   = undefined
    
-- Gebruik een functie die door de Foldable class wordt voorzien naar keuze #linebreak()
-- om `totalTreasure` te implementeren:
totalTreasure :: Grid Cell -> Int
totalTreasure = undefined


-- Traversable + Maybe

-- Voeg `Grid` toe aan de `Traversable` class door instance te implementeren, 
-- we willen dat alle elementen van het grid rij per rij worden overlopen:
instance Traversable Grid where
    traverse = undefined
    
-- Definieer een helperfunctie `validateCell` die nakijkt of een schat geen negatieve waarde bevat en anders Nothing terug geeft:
validateCell :: Cell -> Maybe Cell
validateCell = undefined

-- Gebruik een `traverse` om het hele bord te validaten:
validateGrid :: Grid Cell -> Maybe (Grid Cell)
validateGrid = undefined


-- Traversable + Either

-- De implementatie met `Maybe` laat ons niet weten waarom het hele grid faalt, dus we gaan nu 
-- hetzelfde doen maar met `Either` waarbij validate oftewel een `CellError` of een `Cell` terug geeft:
data CellError = TreasureTooSmall Int
    deriving (Show)

-- Definieer een nieuwe helperfunctie `validateCellE` die een `CellError` teruggeeft
-- (`Left` constructor) of de `Cell` zelf terug geeft (`Right` constructor):
validateCellE :: Cell -> Either CellError Cell
validateCellE = undefined

-- Gebruik een `traverse` om het hele bord te validaten:
validateGridE :: Grid Cell -> Either CellError (Grid Cell)
validateGridE = undefined


-- State monad

-- We gaan de `State` monad gebruiken om een speler door het doolhof te laten lopen en de verzamelde hoeveelheid treasure bij te houden.
data Move = MoveUp | MoveDown | MoveLeft | MoveRight
      deriving (Show, Eq)

type Pos         = (Int, Int)
type Score       = Int
type PlayerState = (Pos, Score)

-- Schrijf een hulperfunctie `getCell` die safely een cell opvraagt, return `Nothing` als de cel buiten het veld ligt:
getCell :: Grid a -> Pos -> Maybe a
getCell = undefined

-- Definieer een `step` functie die de speler verplaats:
step :: Grid Cell -> Move -> State PlayerState (Maybe Int)
step = undefined

-- Verwacht gedrag:
-- - Speler stapt buiten het veld `->` Return `Nothing`
-- - Speler stapt naar een muur `->` Speler beweegt niet
-- - Speler stapt naar een schat `->` Speler verplaatst en verzamelt de schat
-- - Speler stapt naar een leeg vakje `->` Speler verplaatst

-- Schrijf een functie die een een speler een lijst van stappen in het grid laat volgen:
runSteps :: Grid Cell -> [Move] -> State PlayerState [Maybe Int]
runSteps = undefined

