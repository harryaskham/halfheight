{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (forever)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.State.Strict
  ( MonadState (get, put),
    execStateT,
    forever,
    lift,
    runStateT,
  )
import Data.Complex (Complex (..), magnitude)
import Data.List (foldl')
import qualified Data.Text as T
import System.Random (Random (randomR), newStdGen)
import UI.NCurses
  ( Color (..),
    CursorMode (..),
    defaultWindow,
    drawText,
    moveCursor,
    render,
    runCurses,
    setColor,
    setCursorMode,
    setEcho,
    updateWindow,
  )
import UI.NCurses.HalfHeight
  ( Buffer,
    colorId,
    drawBuffer,
    initHexColors,
    mkBuffer,
    setXY,
  )

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

mandelbrotMain :: IO ()
mandelbrotMain = do
  -- Register the 15 greyscale hex colors as Color 1 through Color 15.
  -- Every combination of these colors (fg and bg) is registered with Curses
  -- with a unique ID, which we return in the colorMap.
  colorMap <- runCurses $ initHexColors nord

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
            let (Just c) = colorId colorMap (Color 15) (Color 1)
            setColor c
            moveCursor 25 50
            drawText "| Text demonstrating that each cell is half the row height |"
      w <- defaultWindow
      updateWindow w drawOp
      render

nord :: [String]
nord =
  [ "3b4252",
    "bf616a",
    "a3be8c",
    --"ebcb8b",
    "81a1c1",
    "b48ead",
    "88c0d0",
    "e5e9f0",
    "4c566a",
    "bf616a",
    "a3be8c",
    "ebcb8b",
    "81a1c1",
    "b48ead",
    "8fbcbb",
    "eceff4"
  ]

performanceMain :: IO ()
performanceMain = do
  colorMap <- runCurses $ initHexColors greyScale
  -- Precompute the buffers
  let buffer = mkBuffer 100 100 (Color 1)
      col x y t =
        let x' = fromIntegral (x - 50)
            y' = fromIntegral (y - 50)
            t' = fromIntegral t
         in abs $ round $ (14 * ((sin $ 0.1 * x') * (cos $ 0.1 * y')) / (sin $ 0.1 * t')) + 1
      nextBuf b t = foldl' (\b (x, y) -> setXY x y (Color $ col x y t) b) b [(x, y) | x <- [0 .. 99], y <- [0 .. 99]]
      bufs = cycle $ scanl nextBuf buffer [0 .. 63]
  runCurses $ do
    _ <- (flip runStateT) (bufs, 0) $ do
      lift $ setEcho False
      lift $ setCursorMode CursorInvisible
      forever $ do
        (bufs, t) <- get
        let b = head bufs
            drawOp = do
              drawBuffer colorMap 0 0 b
              let (Just c) = colorId colorMap (Color 15) (Color 1)
              setColor c
              moveCursor 0 0
              drawText $ T.pack $ show t
        w <- lift defaultWindow
        lift $ updateWindow w drawOp
        lift render
        put (tail bufs, t + 1)
    return ()

main :: IO ()
main = do
  --mandelbrotMain
  performanceMain
