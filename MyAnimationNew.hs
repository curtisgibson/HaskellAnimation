module MyAnimationNew where

import Animation

-- Makes a circle whos colour can change and the scale changes over time to oscillate
makeBigSquare :: Varying Colour -> Double -> Double -> Double -> Animation
makeBigSquare col sc r spin =
                translate (always (sc*50, sc*40))
                    (rotate (always r)
                        (rotate (spinner spin)
                            (scale (repeatSmooth (0,0) [(1, (0,0)), (2, (sc/10, sc/10)), (3, (0, 0))])
                                (withBorder (col) (always 2)
                                    (withoutPaint (rect (always 300) (always 300)))))))

-- r = rotate direction
-- sc = ???
-- spin = speed of the spinner
initBigSquares :: Int -> [Animation]
initBigSquares numOfSquares = 
            [makeBigSquare col (fromIntegral sc) (fromIntegral r) (fromIntegral spin)
                | col <- [cycleSteps 0.1 [teal, black]], 
                    sc <- [numOfSquares, numOfSquares-1 .. 1],
                        r <- [45, 225],
                            spin <- [-5..numOfSquares-5]]

--initSmallSquares :: Double -> Animation
--smallSquares spin = 
--            withPaint (always red)
--                (rect (always 20) (always 20))

pic :: Animation
pic = --combine (initSmallSquares 3)
      --`plus`
      combine (initBigSquares 10)

test :: IO ()
test = writeFile "newAnimation.svg" (svg 800 600 pic)

-- Call pic 
-- Call initBigSquares
-- initBigSquares call makeBigSquare 

-- r = rotate direction
-- sc = ???
-- spin = speed of the spinner
