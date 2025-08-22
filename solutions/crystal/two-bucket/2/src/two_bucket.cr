module TwoBucket
  enum Bucket
    One
    Two
  end

  struct Result
    property moves, other_bucket, goal_bucket

    def initialize(@moves : UInt32, @other_bucket : UInt32, @goal_bucket : Bucket)
    end
  end

  def self.measure(bucket_one : UInt32, bucket_two : UInt32, goal : UInt32, start_bucket : Bucket)
    # Define the initial states of the buckets
    current_state = {bucket_one, 0_u32}
    initial_other_state = {0_u32, bucket_two}

    # Swap if the start bucket is Bucket::Two
    if start_bucket == Bucket::Two
      current_state, initial_other_state = initial_other_state, current_state
    end

    states = {current_state => 1_u32}
    queue = Deque{current_state}

    # Process states until we find the solution
    while state = queue.shift?
      moves_count = states[state]

      # Check if we've reached the goal in either bucket
      return Result.new(moves_count, state[1], Bucket::One) if state[0] == goal
      return Result.new(moves_count, state[0], Bucket::Two) if state[1] == goal

      new_moves_count = moves_count + 1

      # Calculate the amount of water to pour between the buckets
      pour_to_bucket_two = {state[0], bucket_two - state[1]}.min
      pour_to_bucket_one = {state[1], bucket_one - state[0]}.min

      # Possible new states after performing each operation
      new_states = {
        {0_u32, state[1]},               # Empty bucket one
        {bucket_one, state[1]}, # Fill bucket one
        {state[0], 0_u32},               # Empty bucket two
        {state[0], bucket_two}, # Fill bucket two
        {state[0] + pour_to_bucket_one, state[1] - pour_to_bucket_one}, # Pour from bucket two to bucket one
        {state[0] - pour_to_bucket_two, state[1] + pour_to_bucket_two}  # Pour from bucket one to bucket two
      }

      # Process each new state if it hasn't been visited and is not forbidden
      new_states.each do |new_state|
        if new_state != initial_other_state && !states.has_key?(new_state)
          states[new_state] = new_moves_count
          queue << new_state
        end
      end
    end

    raise ArgumentError.new("Can't reach goal")
  end
end
