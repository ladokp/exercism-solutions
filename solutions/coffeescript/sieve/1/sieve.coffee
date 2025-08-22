class Sieve
  @primes: (limit) ->
    results = []
    return results if limit == 1

    numbers = [2..limit]
    while numbers.length > 0
        results.push numbers[0]
        numbers = numbers.filter (current) -> current % numbers[0] != 0
    results

module.exports = Sieve