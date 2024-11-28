armMass = 0.2  -- z
cargoArmLength = 0.1  -- ml
missileArmLength = 0.2  -- mk
missileInitialHeight = 0.05  -- p1
missileMass = 0.2  -- m
cargoMass = 1.0  -- m2
axisHeight = 0.07  -- h
g = 9.8

cargoHeight = (cargoArmLength + missileArmLength + s) * axisHeight / (missileArmLength + s)  -- q
    where s = missileArmLength * missileInitialHeight / (axisHeight - missileInitialHeight)

missileHeightOnDetach = (cargoArmLength + missileArmLength) * axisHeight / cargoArmLength  -- p2

cargoArmMass = armMass * cargoArmLength / (cargoArmLength + missileArmLength)  -- ml
missileArmMass = armMass * missileArmLength / (cargoArmLength + missileArmLength)  -- mk

missileHeightDifference = missileHeightOnDetach - missileInitialHeight  -- pd

potentialEnergyDifference = cargoEnegry + cargoArmEnergy - missileEnergy - missileArmEnergy   -- Ep
    where
        cargoEnegry = cargoMass * g * cargoHeight
        cargoArmEnergy = cargoArmMass * g * cargoHeight / 2
        missileEnergy = missileMass * g * missileHeightDifference
        missileArmEnergy = missileArmMass * g * missileHeightDifference / 2

velocity = sqrt $ potentialEnergyDifference / coefSum  -- v
    where
        coefSum =  missileCoef + cargoCoef + missileArgCoef + cargoArmCoef
        missileCoef = missileMass / 2
        cargoCoef = cargoMass * cargoArmLength / missileArmLength
        missileArgCoef = missileArmMass / 4
        cargoArmCoef = cargoArmMass * cargoArmLength**2 / (4 * missileArmLength ** 2)

armToFloorAngle = asin $ axisHeight / cargoArmLength  -- psi

velocityToFloorAngle = pi / 2 - armToFloorAngle  -- alpha

flightTime = velocity * sin velocityToFloorAngle + sqrt d / g  -- t
    where
        d = velocity**2 * sin velocityToFloorAngle ** 2 + 2 * g * missileHeightOnDetach

distance = velocity * cos velocityToFloorAngle * flightTime