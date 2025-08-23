[
  # Collect all the output from `foreach`
  foreach range(.count) as $n (
    # Initialize an empty list for each iteration
    [];
    # Reduce range to calculate values
    reduce range($n - 1; 0; -1) as $i (
      # Start with a list of ones
      . + [1];
      # Add the value from the previous index to the current index
      .[$i] += .[$i - 1]
    )
  )
]
