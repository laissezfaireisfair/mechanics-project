z = 0.1  -- Arm mass
l = 0.2  -- Cargo arm length
k = 0.3  -- Missile arm length
p1 = 0.05  -- Missile initial height
m = 0.1  -- Missile mass
m2 = 0.7  -- Cargo mass
h = 0.1  -- Axis height
g = 9.8

q = (l + k + s) * h / (k + s)  -- Cargo height
    where s = k * p1 / (h - p1)


p2 = (l + k) * h / l  -- Missile height on detach

ml = z * l / (l + k)  -- Cargo arm mass
mk = z * k / (l + k)  -- Missile arm mass

pd = p2 - p1  -- Missile height difference

ep = term1 + term2 + term3 + term4   -- Potential energy
    where
        term1 = m2 * g * q
        term2 = ml * q / 2
        term3 = m * g * pd
        term4 = mk * g * pd / 2

v = sqrt $ ep / denominator  -- Velocity
    where
        denominator = term1 + term2 + term3 + term4
        term1 = m / 2
        term2 = l**2 / (2 * m)
        term3 = mk * l**2 / (4 * m**2)
        term4 = ml / 4

phi = asin $ h / l  -- Arm to floor angle

alpha = pi / 2 - phi  -- Missile velocity to floor angle

d = v**2 * sin alpha ** 2 + 2 * g * p2  -- Discriminant from time equation

t = v * sin alpha + sqrt d / g  -- Flight time

distance = v * cos alpha * t  -- Throw distance