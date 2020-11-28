module UI.NCurses.HalfHeight
  ( ColorMap (),
    colorId,
    initHexColors,
    Buffer (),
    mkBuffer,
    setXY,
    drawBuffer,
  )
where

import Control.Applicative (ZipList (ZipList, getZipList))
import Data.Int (Int16)
import Data.List.Split as LS (chunksOf)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Vector.Split as VS (chunksOf)
import Numeric (readHex)
import UI.NCurses
  ( Color (Color),
    ColorID,
    Curses,
    Glyph (Glyph),
    Update,
    defineColor,
    drawGlyph,
    moveCursor,
    newColorID,
    setColor,
  )

toTuple3 :: [a] -> Maybe (a, a, a)
toTuple3 [a, b, c] = Just (a, b, c)
toTuple3 _ = Nothing

-- A pair of colors s.t. we can display half-characters using unicode blocks.
-- Represents the foreground and background color of a combined half-height cell.
-- Color must be one of Curses Color 1 to Color 15.
data ColorPair = ColorPair Color Color deriving (Show, Eq)

instance Ord ColorPair where
  -- Arbitrary comparison so that we end up with distinct map keys.
  (ColorPair (Color a1) (Color a2)) <= (ColorPair (Color b1) (Color b2)) = (a1, a2) <= (b1, b2)

-- | A map to store the registered color pairings.
type ColorMap = M.Map ColorPair ColorID

-- | Get the registered curses ID for a given foreground / background colour combination.
colorId :: ColorMap -> Color -> Color -> Maybe ColorID
colorId colors fg bg = M.lookup (ColorPair fg bg) colors

-- Takes a hex code and converts to a tuple of RGB 1000 values.
-- These are required by the curses color register.
parseColor :: String -> Maybe (Integer, Integer, Integer)
parseColor hex = toTuple3 scaled
  where
    rgbVals = fst <$> (head . readHex <$> LS.chunksOf 2 hex)
    scaled = (\x -> (x * 1000) `div` 255) <$> rgbVals

-- Defines as many custom colors as we have in hexColors.
-- Supports thousands but we can ultimately only use 255.
-- By doubling vertical resolution we can only use <16 since any color can
-- appear next to any color.
registerHexColors :: [String] -> Curses ()
registerHexColors hexes = sequence_ . getZipList $ ZipList mkColors <*> ZipList colorIDs
  where
    colorIDs = [1 ..] :: [Int16]
    mkColors =
      [ \i -> defineColor (Color i) r g b
        | (r, g, b) <- catMaybes $ parseColor <$> hexes
      ]

-- | Takes all custom colors and defines + persists them as an overlap map.
-- Registers the given list of 15 hex colors with Curses.
-- Stores each combination of colors (foreground and background) as a unique ColorID
-- Returns a map from (fg, bg) pairs to the corresponding registered ColorID for the pair.
initHexColors :: [String] -> Curses ColorMap
initHexColors hexes = do
  registerHexColors hexes
  sequenceA $ M.fromList (zip colorKeys newColors)
  where
    colors = Color <$> [1 .. 15]
    colorIDs = [1 .. length colors ^ 2]
    colorCombos = (,) <$> colors <*> colors
    colorKeys = uncurry ColorPair <$> colorCombos
    colorCreators = ZipList $ uncurry newColorID <$> colorCombos
    newColors = getZipList $ colorCreators <*> ZipList (fromIntegral <$> colorIDs)

-- Representation of a 2D grid of half-height coloured cells.
type Buffer = Vector (Vector Color)

-- Make an empty buffer.
mkBuffer :: Int -> Int -> Color -> Buffer
mkBuffer width height bgCol = V.replicate height (V.replicate width bgCol)

-- | Set a single block in the graphics buffer to the given color.
-- If the update is out of bounds, no action is performed.
setXY :: Int -> Int -> Color -> Buffer -> Buffer
setXY x y c b
  | x < 0 || x >= width || y < 0 || y >= height = b
  | otherwise = b V.// [(y, row)]
  where
    height = V.length b
    width = V.length $ b V.! 0
    row = (b V.! y) V.// [(x, c)]

-- Takes chunked vector rows and combines them into the paired
-- block-display halfrows.
combineRows :: Vector (Vector Color) -> Vector ColorPair
combineRows chunk = combineCells <$> V.zip r1 r2
  where
    [r1, r2] = V.toList chunk
    combineCells (fg, bg) = ColorPair fg bg

-- | Draws the graphics buffer to the screen at the given top-left position.
-- Must first have obtained a color map by registering colors with initHexColors.
drawBuffer :: ColorMap -> Int -> Int -> Buffer -> Update ()
drawBuffer colorMap topLeftX topLeftY gfxBuffer =
  do
    let height = V.length gfxBuffer `div` 2
        width = V.length (gfxBuffer V.! 0)
        pairedBuffer = V.fromList $ combineRows <$> VS.chunksOf 2 gfxBuffer
        mkSetter x y = do
          let (ColorPair fg bg) = pairedBuffer V.! y V.! x
          case colorId colorMap fg bg of
            Just cId -> do
              moveCursor (fromIntegral $ y + topLeftY) (fromIntegral $ x + topLeftX)
              setColor cId
              drawGlyph $ Glyph '▀' []
            Nothing -> return ()
    sequence_ [mkSetter x y | x <- [0 .. width - 1], y <- [0 .. height -1]]
