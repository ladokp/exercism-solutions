{
  number: .value,
  factor: 2,
  primes: []
}
| until(
    .factor * .factor > .number;
    if .number % .factor == 0 then
      .number /= .factor | .primes += [.factor]
    else
      .factor += 1
    end
  )
| .primes + [if .number > 1 then .number else empty end]
