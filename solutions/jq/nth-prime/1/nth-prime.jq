def is_prime(primes): primes[] as $prime
  | if (. % $prime) == 0 then false
    elif ($prime * $prime) > . then true
    else empty end;

def next_prime: .[1:] as $primes | last + 2 | until(first(is_prime($primes)); . + 2);

if $n == 0 then "there is no zeroth prime" | halt_error else . end
| [2, 3] | until(length >= $n; . + [next_prime]) | .[$n - 1]