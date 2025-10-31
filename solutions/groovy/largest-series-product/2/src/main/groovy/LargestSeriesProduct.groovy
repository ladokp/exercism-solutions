class LargestSeriesProduct {
    /**
     * Calculates the largest product of 'span' number of contiguous digits within 'digits'.
     *
     * @param digits The string containing the sequence of digits.
     * @param span The number of contiguous digits to multiply (the length of the series).
     * @return The largest product found.
     * @throws IllegalArgumentException if span is invalid or digits contains non-digit characters.
     */
    static int largestProduct(String digits, final int span) {
        // Validation check 1: The span must be a non-negative number.
        if (span < 0) throw new IllegalArgumentException("span must not be negative")
        
        // Validation check 2: The span cannot be longer than the input string.
        if (span > digits.length()) throw new IllegalArgumentException("span must not exceed string length")
        
        // Validation check 3: The input string must only contain numeric digits.
        if (!digits.chars().allMatch(Character::isDigit)) throw new IllegalArgumentException("digits input must only contain digits")
        
        // Initialize the variable to store the largest product found so far.
        int maximum = 0
        
        // Outer loop: Iterate through all possible starting positions (i) for a sub-string of length 'span'.
        // The loop runs from index 0 up to the last index where a full 'span' can still be taken.
        for (i in 0..digits.length() - span) {
            
            // Initialize the product for the current 'span' series.
            int product = 1
            
            // Inner loop: Iterate 'span' times to calculate the product of the current series.
            // 'j' is the offset from the starting position 'i'.
            for (j in 0..<span) {
                // Get the character at the current position (i + j).
                // Convert the character to its numeric integer value and multiply it into 'product'.
                product *= Character.getNumericValue(digits.charAt(i + j))
            }
            
            // Check if the product of the current series is larger than the maximum found.
            if (product > maximum) maximum = product
        }
        
        // Return the final largest product. (In Kotlin, the last expression in a function is the return value).
        maximum
    }
}
