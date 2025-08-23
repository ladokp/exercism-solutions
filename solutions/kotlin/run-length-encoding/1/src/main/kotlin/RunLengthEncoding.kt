object RunLengthEncoding {

    fun encode(input: String): String {
        if (input.isEmpty()) return ""
    
        val encoded = StringBuilder()
        var count = 1
    
        for (i in 1 until input.length) {
            if (input[i] == input[i - 1]) {
                count++ // Increment count if the current character is the same as the previous
            } else {
                // Append count and character only if count is greater than 1
                if (count > 1) encoded.append(count)
                encoded.append(input[i - 1]) // Append the character
                count = 1 // Reset count
            }
        }
    
        // Append the last character and count
        if (count > 1) encoded.append(count)
        encoded.append(input.last())
    
        return encoded.toString()
    }


    fun decode(encoded: String): String {
        val decoded = StringBuilder()
        var count = 0
    
        for (char in encoded) {
            if (char.isDigit()) {
                // Build the count from consecutive digits
                count = count * 10 + (char - '0')
            } else {
                // Append the character `count` times, or once if count is zero
                decoded.append(char.toString().repeat(if (count > 0) count else 1))
                count = 0 // Reset count after using it
            }
        }
    
        return decoded.toString()
    }

}
