module MyAnimationNew where

import Animation

-- Sesaw Variables
vertices :: [Point]
vertices = [(500,645), (500, 653), (800,653), (800, 645)]

centre :: Point
centre = (average (map fst vertices), average (map snd vertices))

average :: [Length] -> Length
average xs = sum xs / fromIntegral (length xs)
-- End of seesaw variables

-- Makes a circle whos colour can change and the scale changes over time to oscillate
makeBigSquare :: Double -> Double -> Double -> Double -> Double -> Animation
makeBigSquare interval rotateVal spinSpeed posX posY =
                translate (always (interval*posY, interval*posY))
                    (rotate (always rotateVal)
                        (rotate (spinner spinSpeed)
                            (scale (cycleSmooth 0.8 [(0.1, 0.1), (interval/14, interval/14), (interval/7, interval/7)])
                                (withBorder (cycleSmooth 0.5 [teal, green, red, yellow]) (always (spinSpeed*2))
                                    (withoutPaint (rect (always 300) (always 300)))))))

-- Small squares around the outside, called from initSmallSquares
makeSmallSquare :: Double -> Animation
makeSmallSquare speed = 
                translate (cycleSmooth speed [(0,0), (780,0), (780, 580), (0, 580)])
                    (rotate (spinner (speed*2))
                        (scale (repeatSmooth (0,0) [(1, (0, 0)), (2, (speed*2, speed*2)), (3, (0, 0))])
                            (withPaint (cycleSmooth speed [teal, black, red, yellow])
                                (rect (always 20) (always 20)))))

-- Initialiser for the large squares
initBigSquares :: Int -> Double -> Double -> [Animation]
initBigSquares numOfSquares posX posY = 
            [makeBigSquare (fromIntegral interval) (fromIntegral rotateVal) (fromIntegral spinSpeed) (posX) (posY)
                | interval <- [numOfSquares, numOfSquares-1 .. 1],
                        rotateVal <- [45, 225],
                            spinSpeed <- [-5..numOfSquares-5]]

-- Initialiser for the small squares
initSmallSquares :: [Animation]
initSmallSquares = 
                [makeSmallSquare (fromIntegral speed)
                    | speed <- [1..3]]

-- Creates the seesaw animation
seesaw :: Animation
seesaw =
        withBorder (always white) (always 5) -- Trinagle base
            (withoutPaint (polygon [(600, 500), (560, 550), (640, 550)]))
        `plus`
        withPaint (cycleSteps 1 [red, blue]) -- Top of seesaw
            (translate (always (600, 500))
                (rotate (cycleSmooth 1 [15, -15])
                    (translate (always (negatePoint centre))
                        (polygon vertices))))
        `plus` -- Red ball
        withBorder (always red) (always 5)
            (translate (cycleSmooth 1 [(510, 455), (510, 330)])
                (withoutPaint (circle (always 20))))
        `plus` -- Blue ball
        withBorder (always blue) (always 5)
            (translate (cycleSmooth 1 [(690, 330), (690, 450)])
                (withoutPaint (circle (always 20))))

-- Take two numbers, and return the negative
negatePoint :: Point -> Point
negatePoint (x, y) = (-x, -y)

pic :: Animation
pic = withPaint (always black) -- Background colour
          (rect (always 800) (always 600))
      `plus`
      seesaw -- Seesaw animation
      `plus`
       combine (initSmallSquares) -- Bordering square
      `plus`
      combine (initBigSquares 7 50 40) -- Star features

test :: IO ()
test = writeFile "newAnimation.svg" (svg 800 600 pic)