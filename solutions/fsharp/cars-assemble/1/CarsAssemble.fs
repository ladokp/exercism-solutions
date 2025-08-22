module CarsAssemble

let successRate (speed: int): float =
    if speed >= 1 && speed <= 4 then
        float(1)
    elif speed >= 5 && speed <= 8 then
        float(0.9)
    elif speed = 9 then
        float(0.8)
    elif speed = 10 then
        float(0.77)
    else
        float(0)

let productionRatePerHour (speed: int): float =
    float(221 * speed) * successRate(speed)

let workingItemsPerMinute (speed: int): int =
    int(productionRatePerHour(speed)/float(60))
