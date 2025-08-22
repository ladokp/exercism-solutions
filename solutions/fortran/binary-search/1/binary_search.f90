module binary_search
  implicit none
contains

  ! This function performs binary search on a sorted array.
  function find(array, val) result(idx)
    ! Input: 
    ! array - A sorted array of integers
    ! val - The integer value to search for in the array
    ! Output: 
    ! idx - The index of the found element, or -1 if not found

    integer, dimension(:), intent(in) :: array
    integer, intent(in) :: val
    integer :: idx
    integer :: start_index, end_index, middle_index

    ! Initialize start and end indices
    start_index = 1
    end_index = size(array)

    ! Perform binary search
    do while (end_index >= start_index)
      ! Calculate the middle index
      middle_index = (end_index + start_index) / 2

      ! Check if the middle element is the one we're searching for
      if (array(middle_index) == val) then
        idx = middle_index
        return
      end if

      ! Adjust search range based on comparison
      if (array(middle_index) > val) then
        end_index = middle_index - 1
      else
        start_index = middle_index + 1
      end if
    end do

    ! If the value is not found, return -1
    idx = -1
  end function find

end module binary_search
