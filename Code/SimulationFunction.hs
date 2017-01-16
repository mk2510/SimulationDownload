module SimulationFunction where


example:: Double -> Double -> Double
example a b = 2* a * b


--calculates the magneticForce
magneticForce::  Double ->Double ->Double ->Double -> Double
magneticForce chi i d x = (chi/(1+(chi)/3)) * ((4/3) * pi * d^3) * (1/mu0) * (((calcBz i (x+dz) )^2)-(((calcBz i x)^2)))/dz



groundForce:: Double -> Double
groundForce m = m * gravityacceleration


calcVel::Double -> Double -> Double -> Double -> Double
calcVel m sec vel f  = vel + (f/m) * sec

-- calculates the difference in X in a certan amount of time. Due to the small angle, the deltaY is to negligible
calcDeltaX:: Double -> Double -> Double -> Double -> Double
calcDeltaX m sec vel force = vel * sec + 0.5 * (force / m) * sec ^2


calcDeltaY:: Double -> Double
calcDeltaY dx = lengthOfString - senOfPyta lengthOfString dx

-- calculates the strength of the force, acting on the ball
calcRemainForce:: Double -> Double -> Double -> Double
calcRemainForce ang gforce magnforce = magnforce - (tan ang) * gforce

-- calculates the current from the Initial B - Field strength
calcI :: Double -> Double
calcI b = (b * radiusOfCoil * 2)/(mu0)

calcBz:: Double -> Double -> Double
calcBz i z = ((mu0/2) * i * (radiusOfCoil^2))/((nthRoot 2 ((radiusOfCoil^2) + z^2))^3)

--calculates the angle between the oreginal position and the new on
calcAngle:: Double -> Double
calcAngle dx = asin (dx / lengthOfString)

-- uses the sentence of the Pythagoras
senOfPytc:: Double -> Double -> Double
senOfPytc a b = sqrt((a^2) + (b^2))

-- takes two sides of a triagnel and calculates the third one, which  must be a leg of the triangel
senOfPyta:: Double -> Double -> Double
senOfPyta c b = sqrt((max c b)^2 - (min c b)^2)

--takes the nth root of a number
nthRoot::Double -> Double -> Double
nthRoot n x = fst $ until (uncurry(==)) (\(_,x0) -> (x0,((n-1)*x0+x/x0**(n-1))/n)) (x,x/n)

-- returns the length of the String in meters
lengthOfString:: Double
lengthOfString = 400

--retruns mu0 magnetic field constance
mu0 :: Double
mu0 = 4 * pi * (10^^(-7))

--returns the differencial of dz
dz::Double
dz = 1 * (10^^(-5))

--returns the g-Factor
gravityacceleration:: Double
gravityacceleration = 9.81

--returns the radius of the coil
radiusOfCoil:: Double
radiusOfCoil = 0.03
