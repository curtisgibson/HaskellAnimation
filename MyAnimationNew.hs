module MyAnimationNew where

import Animation

-- Sesaw
vertices :: [Point]
vertices = [(500,645), (500, 653), (800,653), (800, 645)]

centre :: Point
centre = (average (map fst vertices), average (map snd vertices))

average :: [Length] -> Length
average xs = sum xs / fromIntegral (length xs)
-- End of seesaw variables

-- Makes a circle whos colour can change and the scale changes over time to oscillate
makeBigSquare :: Varying Colour -> Double -> Double -> Double -> Double -> Double -> Animation
makeBigSquare col interval rotateVal spinSpeed posX posY =
                translate (always (interval*posY, interval*posY))
                    (rotate (always rotateVal)
                        (rotate (spinner spinSpeed)
                            (scale (repeatSmooth (0,0) [(1, (0.2, 0.2)), (2, (interval/7, interval/7)), (3, (0.2, 0.2))])
                                (withBorder (col) (always (spinSpeed*2))
                                    (withoutPaint (rect (always 300) (always 300)))))))

makeSmallSquare :: Double -> Animation
makeSmallSquare speed = 
                translate (cycleSmooth speed [(0,0), (780,0), (780, 580), (0, 580)])
                    (rotate (spinner (speed*2))
                        (scale (repeatSmooth (0,0) [(1, (0, 0)), (2, (speed*2, speed*2)), (3, (0, 0))])
                            (withPaint (cycleSmooth speed [teal, black, red, yellow])
                                (rect (always 20) (always 20)))))

initBigSquares :: Int -> Double -> Double -> [Animation]
initBigSquares numOfSquares posX posY = 
            [makeBigSquare col (fromIntegral interval) (fromIntegral rotateVal) (fromIntegral spinSpeed) (posX) (posY)
                | col <- [cycleSmooth 0.5 [teal, green, red, yellow]],
                    interval <- [numOfSquares, numOfSquares-1 .. 1],
                        rotateVal <- [45, 225],
                            spinSpeed <- [-5..numOfSquares-5]]

initSmallSquares :: [Animation]
initSmallSquares = 
                [makeSmallSquare (fromIntegral speed)
                    | speed <- [1..3]]

seesaw :: Animation
seesaw =
        withPaint (cycleSteps 1 [red, blue]) -- Top of seesaw
            (translate (always (600, 500))
                (rotate (cycleSmooth 1 [15, -15])
                    (translate (always (negatePoint centre))
                        (polygon vertices))))
        `plus` -- Triangle
        withPaint (always yellow)
            (polygon [(600, 500), (560, 550), (640, 550)])
        `plus`
        withBorder (always red) (always 5)
            (translate (cycleSmooth 1 [(510, 455), (510, 330)])
                (withoutPaint (circle (always 20))))
        `plus`
        withPaint (always blue)
            (translate (cycleSmooth 1 [(690, 330), (690, 450)])
                (circle (always 20)))

negatePoint :: Point -> Point
negatePoint (x, y) = (-x, -y)

pic :: Animation
pic = withPaint (always black) -- Background colour
          (rect (always 800) (always 600))
      `plus`
      seesaw
      `plus`
       combine (initSmallSquares) -- Bordering square
      `plus`
      combine (initBigSquares 7 50 40) -- Star features

test :: IO ()
test = writeFile "newAnimation.svg" (svg 800 600 pic)