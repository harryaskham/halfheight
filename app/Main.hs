{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (forever)
import Data.Complex
import Data.List (foldl')
import HalfHeight
import UI.NCurses

-- Set up a 15-color greyscale pallette
greyScale :: [String]
greyScale =
  [ "000000",
    "111111",
    "222222",
    "333333",
    "444444",
    "666666",
    "777777",
    "888888",
    "999999",
    "aaaaaa",
    "bbbbbb",
    "cccccc",
    "dddddd",
    "eeeeee",
    "ffffff"
  ]

-- Calculate the Mandelbrot steps-to-divergence and plot as Color 1 to Color 15
mandelbrot :: (Fractional a, RealFloat a) => a -> a -> Color
mandelbrot x y = Color (16 - ((+) 1 $ steps (0 :+ 0) (x' :+ y') 0))
  where
    x' = (x - 100) * 0.025
    y' = (y - 50) * 0.025
    steps z c i
      | i > 13 || magnitude z' > 2 = i
      | otherwise = steps z' c (i + 1)
      where
        z' = z * z + c

-- Draw the Mandelbrot set to the buffer point by point
drawMandelbrot :: Buffer -> Buffer
drawMandelbrot buffer =
  foldl'
    (\b (x, y) -> setXY x y (mandelbrot (fromIntegral x) (fromIntegral y)) b)
    buffer
    [(x, y) | x <- [0 .. 149], y <- [0 .. 99]]

main :: IO ()
main = do
  -- Register the 15 greyscale hex colors as Color 1 through Color 15.
  -- Every combination of these colors (fg and bg) is registered with Curses
  -- with a unique ID, which we return in the colorMap.
  colorMap <- runCurses $ initHexColors greyScale

  -- Creates a new buffer and draws the Mandelbrot set in each cell using the
  -- greyscale colors 1 through 15
  let buffer = drawMandelbrot $ mkBuffer 150 100 (Color 1)

  runCurses $ do
    -- Hide the cursor
    setEcho False
    setCursorMode CursorInvisible

    -- Enter an infinite drawing loop displaying the buffer.
    forever $ do
      let drawOp = do
            -- First draw the buffer to the screen.
            drawBuffer colorMap 0 0 buffer
            -- Now draw some text in the centre.
            -- We set the color to curses ID corresponding to fg 15 (white) and bg 1 (black)
            setColor $ getCol colorMap (Color 15) (Color 1)
            moveCursor 25 50
            drawText "| Text demonstrating that each cell is half the row height |"
      w <- defaultWindow
      updateWindow w drawOp
      render
