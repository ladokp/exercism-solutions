class LargestSeriesProduct
  # This class method calculates the largest product of a series of digits with a given span.
  @largestProduct: (digits, span) ->
    # Check if the span is negative, which is invalid.
    if span < 0
      throw new Error 'span must not be negative'
    # Check if the span is longer than the length of the digit string, which is invalid.
    if span > digits.length
      throw new Error 'span must not exceed string length'    
    # Check if the input string contains non-digit characters, which is invalid.
    if digits.match(/[^0-9]/)
      throw new Error 'digits input must only contain digits'
    # Initialize the maximum product to zero.
    max = 0

    # Iterate through the digit string to calculate the product of each series of the given span.
    for i in [0...digits.length - span + 1]
      product = 1
      # Calculate the product of the current series.
      for j in [0...span]
        product *= Number digits[i + j]
      # Update the maximum product if the current product is greater.
      if product > max
        max = product    
    # Return the largest product found.
    max

# Export the LargestSeriesProduct class for use in other modules.
module.exports = LargestSeriesProduct
