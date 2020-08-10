import           Control.Concurrent        (threadDelay)
import           Control.Monad             (unless)
import           Graphics.Gloss            (arcSolid, circle, circleSolid, color, lineLoop, pictures, polygon,
                                            rectangleSolid, rectangleWire, rotate, scale, text, thickArc, thickCircle,
                                            translate)
import           Graphics.Gloss.Data.Color (bright, chartreuse, cyan, dark, dim, green, light, magenta, makeColor, red,
                                            violet, white, yellow)
import           Graphics.Gloss.Rendering  (Picture, State, displayPicture, initState)
import           Graphics.UI.GLFW          (Key (Key'Escape), Window, pollEvents, swapBuffers)
import           System.Exit               (exitSuccess)
import           Window                    (keyIsPressed, withWindow)

windowWidth, windowHeight :: Int
windowWidth = 640
windowHeight = 480

main :: IO ()
main = do
  glossState <- initState
  withWindow windowWidth windowHeight "Haskell Shapes" $ \window -> do
    loop window glossState
    exitSuccess
 where
  loop window glossState = do
    threadDelay 2000
    pollEvents
    renderFrame window glossState
    exit <- keyIsPressed window Key'Escape
    unless exit $ loop window glossState

renderFrame :: Window -> State -> IO ()
renderFrame window glossState = do
  displayPicture (windowWidth, windowHeight) white glossState 1.0 $ pictures
    [ translate (-200) 100 $ color violet star
    , translate (-100) 100 $ color (makeColor 0 128 255 1) $ lineLoop [(-30, -30), (-40, 30), (30, 40), (50, -20)]
    , translate 0 100 $ color red $ circle 30
    , translate 100 100 $ color green $ thickCircle 30 10
    , translate 200 100 $ color yellow $ circleSolid 30
    , translate (-200) (-100) $ color chartreuse $ thickArc 0 180 30 30
    , translate (-100) (-100) $ color (dark magenta) $ arcSolid 0 90 30
    , translate 0 (-100) $ scale 0.2 0.2 $ color (bright magenta) $ text "Boo!"
    , translate 100 (-100) $ rotate 30 $ color (dim cyan) $ rectangleWire 20 50
    , translate 200 (-100) $ rotate 60 $ color (light cyan) $ rectangleSolid 20 50
    ]
  swapBuffers window

star :: Picture
star = lineLoop
  [(0, 50), (10, 20), (40, 20), (20, 0), (30, -30), (0, -10), (-30, -30), (-20, 0), (-40, 20), (-10, 20)]
