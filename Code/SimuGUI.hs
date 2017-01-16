module SimuGUI(startGui) where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort

import SimulationFunction

width, heigth, offset:: Int
width = 800
heigth = 800


offset=200

coilPosition::Double
coilPosition = 0.1

--length::Double
--length = 7.43

window :: Display
window = InWindow "Simulation" (width, heigth) (offset,offset)

background :: Color
background = white

--called by start button
startGui :: Double ->Double ->Double ->Double -> Color-> IO ()
startGui b1 chi1 ma1 semdia1 col= simulate window background fps (initialState b1 chi1 ma1 semdia1 col) render update

-- | Date describing the state of the simulation
data PongGame = Game {
                ballLocM :: (Double, Double)
                , ballLocPix :: (Float, Float) -- ^ balls location
                , ballVel :: Double
                , binitial :: Double-- ^ ball velocity
                , force :: Double   -- ^ force acting on the ball
                , current :: Double
                , deltax :: Double
                , chi :: Double
                , mass :: Double
                , semidiameter :: Double
                , magForceSaved:: Double
                , bfieldz::Double
                , fremain::Double
                , stop::Bool
                , help::Bool
                , angle::Double
                , fstS::Bool
                , colOfElem::Color
              } deriving Show

              -- | The starting state for the game of Pong.
initialState :: Double -> Double -> Double -> Double -> Color-> PongGame
initialState b ch ma semdia col = Game
                { ballLocM = (0, 0)
                , ballLocPix = (0, 0)
                , ballVel = 0
                , binitial = b
                , force = 0.00002
                , current = calcI b
                , deltax = 0
                , chi = ch
                , mass = ma
                , semidiameter = semdia
                , magForceSaved = 0
                , bfieldz =0
                , fremain = 0
                , stop = False
                , help = False
                , angle = 0
                , fstS = False
                , colOfElem = col
                }

-- | Convert game into an Picture
render :: PongGame -> Picture
render game = pictures [printANG, printRemainForce, printForce, printBz, refenceLine, string, ball, coil, wire1, wire2, circle1, circle2 , printDx , referenceLength, beginLine, endLine, printLength, rec]
                        where
                        semi = semidiameter game
                        magFo = magForceSaved game
                        f = force game
                        bf = bfieldz game
                        hp = help game
                        an = angle game
                        dx = deltax game
                        -- ball
                        ball::Picture
                        ball = uncurry translate  (ballLocPix game) $ color ballColor $ circleSolid $  m2Pix semi
                        ballColor = colOfElem game

                        --coil
                        coil:: Picture
                        coil  = translate (-(m2Pix coilPosition)) 0 $ color black $ scale 0.25 1 $ circle $ m2Pix radiusOfCoil

                        wire1::Picture
                        wire1 =  color black $ line [((-(m2Pix coilPosition)+5), (-88)), ((-(m2Pix coilPosition)+5), (-138))]

                        wire2::Picture
                        wire2 = color black $ line [((-(m2Pix coilPosition)-5), (-88)), ((-(m2Pix coilPosition)-5), (-138))]

                        rec::Picture
                        rec = translate (-(m2Pix coilPosition)) (-88) $ color white $ rectangleSolid 9 9

                        circle1::Picture
                        circle1 = translate (-(m2Pix coilPosition)-5) (-138) $ color black $ circleSolid 3

                        circle2::Picture
                        circle2 = translate (-(m2Pix coilPosition)+5) (-138) $ color black $ circleSolid 3

                        (x, y) = ballLocPix game
                        string::Picture
                        string = color (green) $ line [(x, y+10), (0, m2Pix lengthOfString)]

                        refenceLine::Picture
                        refenceLine = color (dark red) $ line [(0, - (m2Pix lengthOfString)), (0, m2Pix lengthOfString)]

                        referenceLength::Picture
                        referenceLength = color black $ line [ ((-(m2Pix coilPosition)), (-200)) , ((-(m2Pix coilPosition) + m2Pix 0.05),(-200) )  ]

                        beginLine::Picture
                        beginLine= translate (-(m2Pix coilPosition)) (-200) $ color black $ circleSolid 3

                        endLine::Picture
                        endLine = translate (-(m2Pix coilPosition) + m2Pix 0.05) (-200) $ color black $ circleSolid 3

                        printLength::Picture
                        printLength = translate (-(m2Pix coilPosition)) (-250) $ scale 0.25 0.25 $ text $ "0.05 m"

                        printForce::Picture
                        printForce = translate (0) (-175) $ scale 0.25 0.25 $text $ "Fm: "++(show $ roundIntelegent 2 magFo)++" N"

                        printRemainForce::Picture
                        printRemainForce = translate (0) (-225) $ scale 0.25 0.25 $ text $ "Fr: "++(show $ roundIntelegent 2 f)++" N"

                        printBz::Picture
                        printBz = translate (0)(-275) $ scale 0.25 0.25 $text $  "Bz: "++(show $ roundIntelegent 2 bf)++" T"

                        printANG::Picture
                        printANG = translate (0)(-325) $ scale 0.25 0.25 $text $  "Winkel: "++(show $ roundIntelegent 2 $ degrees an)++" Grad"

                        printDx::Picture
                        printDx = translate (0)(-375) $ scale 0.25 0.25 $text $  "Auslenkung: "++(show $ roundIntelegent 2 $ dx)++" m"
-- Update the ball position using its current velocity
moveBall :: Float
         -- ^ The number of seconds since last update
         -> PongGame-- ^ The initial game state
         -> PongGame -- ^ A new game state with an updated ball position

moveBall seconds game = game {ballLocM = ( x' ,y'), ballLocPix = (xPix', yPix'),  ballVel = vx', force = f', deltax = dx', magForceSaved = mf', bfieldz = bf', fremain = fr', stop = st, help = hp', angle = a', fstS = fs'}
                        where
                        --old saved things
                        s = 1 * seconds
                        (x,y) = ballLocM game
                        vx = ballVel game
                        f = force game
                        i = current game
                        dx = deltax game
                        ma = mass game
                        ch = chi game
                        sedi = semidiameter game
                        hp = help game
                        fs = fstS game
                        --new saved things
                        mf' = magneticForce ch i sedi (coilPosition + x)
                        fr' = f


                        f' = if (hp)
                                then f
                                else  calcRemainForce (calcAngle dx) (groundForce ma) (magneticForce ch i sedi (coilPosition + x))



                        st = if(fs)
                              then differentSings f' fr'
                              else False

                        hp' = if (st)
                                then True
                                else hp

                        x' = if (st || hp')
                                then x
                                else  x + (realToFrac $ calcDeltaX ma (realToFrac s) vx f')

                        dx' = if (st || hp')
                                then dx
                                else dx + (calcDeltaX ma (realToFrac s) vx f')

                        y' = if (st || hp')
                                then y
                                else realToFrac $ calcDeltaY dx

                        vx' = if (st || hp')
                                then vx
                                else calcVel ma (realToFrac s) vx f'


                        fs' = True
                        a' = calcAngle dx
                        --dx' = dx + (calcDeltaX ma (realToFrac s) vx f')
                        xPix' = m2Pix x'
                        yPix' = m2Pix y'
                        bf' = calcBz i (coilPosition + x)





--number of frames to show per second
fps:: Int
fps = 60

differentSings:: Double -> Double -> Bool
differentSings a b
              | (a<0  && b > 0) = True
              | (a>0  && b < 0) = True
              | otherwise = False

{--decideCalcX:: Double -> Double -> Double -> Double -> Double -> Double
decideCalcX x ma s vx f'
                      |(f' == 0) = x
                      | otherwise = x + (realToFrac $ calcDeltaX ma (realToFrac s) vx f')

decideCalcY::
decideCalcY

decideCalcVX

decideCalcdx

--}
update :: ViewPort -> Float -> PongGame -> PongGame
update _ = moveBall

m2Pix :: Double->Float
m2Pix meter = realToFrac (3000 * meter)

roundIntelegent:: Int -> Double -> Double
roundIntelegent x num
                        | (r /= 0 ) = r
                        | otherwise = roundIntelegent (x+1) num

                        where r= (fromInteger $ round (num * (10^x)))/(10^x)

degrees:: Double -> Double
degrees x = (x * 180)/ pi
