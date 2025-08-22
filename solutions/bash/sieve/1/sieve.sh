#!/usr/bin/env bash

#!/usr/bin/env bash

# Sieve of Eratosthenes
# https://en.wikipedia.org/wiki/Sieve_of_Eratosthenes

upper_limit=$1

sieve=()
for ((index=2; index<=$upper_limit; index++)); do
    sieve[index]=true
done

for ((index=2; index*index<=$upper_limit; index++)); do
    if ${sieve[$index]}; then
        for ((index2=index*index; index2<=$upper_limit; index2+=index)); do
            sieve[$index2]=false
        done
    fi
done

primes=()
for ((index=2; index<=$upper_limit; index++)); do
    if ${sieve[$index]}; then
        primes+=( $index )
    fi
done
echo "${primes[*]}"
