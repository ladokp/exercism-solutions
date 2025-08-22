module sieve
  implicit none

contains

  function primes(limit) result(array)
    integer, intent(in) :: limit
    integer, allocatable :: array(:)
    logical :: composites(limit)
    integer :: temp(limit), n_primes, index, index2

    n_primes = 0
    composites = .false.

    do index = 2, limit
      if (composites(index)) cycle

      n_primes = n_primes + 1
      temp(n_primes) = index

      do index2 = index ** 2, limit, index
        composites(index2) = .true.
      end do
    end do

    array = temp(1:n_primes)
  end function primes

end module sieve