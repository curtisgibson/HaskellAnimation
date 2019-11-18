module MyAnimationNew where

import Animation

-- Makes a circle whos colour can change and the scale changes over time to oscillate
makeBigSquare :: Varying Colour -> Double -> Double -> Double -> Double -> Double -> Animation
makeBigSquare col interval rotateVal spinSpeed posX posY =
                translate (always (interval*posY, interval*posY))
                    (rotate (always rotateVal)
                        (rotate (spinner spinSpeed)
                            (scale (repeatSmooth (0,0) [(1, (0,0)), (2, (interval/15, interval/15)), (3, (0, 0))])
                                (withBorder (col) (always 2)
                                    (withoutPaint (rect (always 300) (always 300)))))))

makeSmallSquare :: Animation
makeSmallSquare = 
                translate (cycleSmooth 1 [(0,0), (780,0), (780, 580), (800/2,600/2)])
                    (withPaint (cycleSmooth 0.8 [teal, black, red, yellow])
                        (rect (always 20) (always 20)))

-- rotateVal = rotate direction
-- interval = multiplier for transformation
-- spinSpeed = speed of the spinner
initBigSquares :: Int -> Double -> Double -> [Animation]
initBigSquares numOfSquares posX posY = 
            [makeBigSquare col (fromIntegral interval) (fromIntegral rotateVal) (fromIntegral spinSpeed) (posX) (posY)
                | col <- [cycleSmooth 0.5 [teal, black, red, yellow]],
                    interval <- [numOfSquares, numOfSquares-1 .. 1],
                        rotateVal <- [45, 225],
                            spinSpeed <- [-5..numOfSquares-5]]

--initSmallSquares :: Animation
--initSmallSquares = 
--                [makeSmallSquare (fromIntegral posStart) (fromIntegral posEnd)
--                    | posStart <- []]

pic :: Animation
pic = makeSmallSquare
      `plus`
      combine (initBigSquares 7 50 40)

test :: IO ()
test = writeFile "newAnimation.svg" (svg 800 600 pic)

-- Call pic 
-- Call initBigSquares
-- initBigSquares call makeBigSquare 

-- rotateVal = rotate direction
-- interval = ???
-- spinSpeed = speed of the spinner