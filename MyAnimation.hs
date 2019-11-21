module MyAnimation where

import Animation

-- Seesaw Variables
vertices :: [Point]
vertices = [(500,645), (500, 653), (800,653), (800, 645)]

centre :: Point
centre = (average (map fst vertices), average (map snd vertices))

average :: [Length] -> Length
average xs = sum xs / fromIntegral (length xs)

negatePoint :: Point -> Point
negatePoint (x, y) = (-x, -y)
-- End of seesaw variables

-- Creates multiple square, depending on the number passed in the init. Called from picture
initBigSquare :: Double -> Double -> Double -> Animation
initBigSquare numOfSquares posX posY =
                (combine
                    [translate (always (interval*posY, interval*posY))
                        (rotate (always rotateVal)
                            (rotate (spinner spinSpeed)
                                (scale (cycleSmooth 0.8 [(0.1, 0.1), (interval/10, interval/10), (interval/6, interval/6)])
                                    (withBorder (cycleSmooth 0.5 [teal, green, red, white, yellow]) (always (spinSpeed*2))
                                        (withoutPaint (rect (always 150) (always 150)))))))
                    | interval <- [numOfSquares, numOfSquares-1..1],
                        spinSpeed <- [-5..numOfSquares-5],
                            rotateVal <- [45, 225]])

-- Creates multiple small squares around the perimeter, called from picture
initSmallSquare :: Animation
initSmallSquare =
                (combine
                    [translate (cycleSmooth speed [(0,0), (780,0), (780, 580), (0, 580)])
                        (rotate (spinner (speed*2))
                            (scale (repeatSmooth (0,0) [(1, (0, 0)), (2, (speed*2, speed*2)), (3, (0, 0))])
                                (withPaint (cycleSmooth speed [teal, white, red, blue])
                                    (rect (always 20) (always 20)))))
                    | speed <- [1..3]])

-- Creates the seesaw animation
seesaw :: Animation
seesaw =
        withBorder (always white) (always 5) -- Triangle base
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

-- Called upon start
picture :: Animation
picture =
      withPaint (always black) -- Background colour
          (rect (always 800) (always 600))
      `plus`
      seesaw -- Seesaw animation
      `plus`
      initSmallSquare -- Bordering square
      `plus`
      initBigSquare 7 50 40 -- Star features
