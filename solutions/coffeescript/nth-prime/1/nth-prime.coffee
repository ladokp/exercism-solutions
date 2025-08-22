module.exports = class NthPrime
  @nth: (index) ->
    throw 'Prime is not possible' if index < 1
    primes = [2]
    currentNumber = 3
    until index is primes.length
      if primes.every((p) -> currentNumber % p)
      then primes.push currentNumber
      else currentNumber += 2
    primes.pop()