module Main where

armMass :: Double
armMass = 0.143  -- z

cargoArmLength :: Double
cargoArmLength = 0.1  -- ml

missileArmLength :: Double
missileArmLength = 0.2  -- mk

missileInitialHeight :: Double
missileInitialHeight = 0.05  -- p1

missileMass :: Double
missileMass = 0.01  -- m

cargoMass :: Double
cargoMass = 0.177  -- m2

axisHeight :: Double
axisHeight = 0.07  -- h

g :: Double
g = 9.8

cargoHeight :: Double
cargoHeight = (cargoArmLength + missileArmLength + s) * axisHeight / (missileArmLength + s)  -- q
    where s = missileArmLength * missileInitialHeight / (axisHeight - missileInitialHeight)

missileHeightOnDetach :: Double
missileHeightOnDetach = (cargoArmLength + missileArmLength) * axisHeight / cargoArmLength  -- p2

cargoArmMass :: Double
cargoArmMass = armMass * cargoArmLength / (cargoArmLength + missileArmLength)  -- ml
missileArmMass :: Double
missileArmMass = armMass * missileArmLength / (cargoArmLength + missileArmLength)  -- mk

missileHeightDifference :: Double
missileHeightDifference = missileHeightOnDetach - missileInitialHeight  -- pd

potentialEnergyDifference :: Double
potentialEnergyDifference = cargoEnegry + cargoArmEnergy - missileEnergy - missileArmEnergy   -- Ep
    where
        cargoEnegry = cargoMass * g * cargoHeight
        cargoArmEnergy = cargoArmMass * g * cargoHeight / 2
        missileEnergy = missileMass * g * missileHeightDifference
        missileArmEnergy = missileArmMass * g * missileHeightDifference / 2

velocity :: Double
velocity = sqrt $ potentialEnergyDifference / coefSum  -- v
    where
        coefSum =  missileCoef + cargoCoef + missileArgCoef + cargoArmCoef
        missileCoef = missileMass / 2
        cargoCoef = cargoMass * cargoArmLength / missileArmLength
        missileArgCoef = missileArmMass / 4
        cargoArmCoef = cargoArmMass * cargoArmLength**2 / (4 * missileArmLength ** 2)

armToFloorAngle :: Double
armToFloorAngle = asin $ axisHeight / cargoArmLength  -- psi

velocityToFloorAngle :: Double
velocityToFloorAngle = pi / 2 - armToFloorAngle  -- alpha

flightTime :: Double
flightTime = (velocity * sin velocityToFloorAngle + sqrt d) / g  -- t
    where
        d = velocity**2 * sin velocityToFloorAngle ** 2 + 2 * g * missileHeightOnDetach


measurements :: Int
measurements = 10

times :: [Double]
times = [fromIntegral m * step | m <- [0..measurements]]
    where
        step = flightTime / fromIntegral measurements

distance :: Double -> Double
distance t = velocity * cos velocityToFloorAngle * t

height :: Double -> Double
height t = missileHeightOnDetach + velocity * sin velocityToFloorAngle * t - (g * t ** 2) / 2

main :: IO ()
main = do
    putStrLn "Heights:"
    putStrLn $ concatMap (((++"\n") . show) . height) times
    putStrLn "Distances:"
    putStrLn $ concatMap (((++"\n") . show) . distance) times