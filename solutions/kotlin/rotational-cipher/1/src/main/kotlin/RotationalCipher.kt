class RotationalCipher(val shiftKey: Int) {

    companion object {
        private val UPPERS = ('A'..'Z').toList()
        private val LOWERS = ('a'..'z').toList()
    }

    fun encode(text: String): String {
        return text.map { char ->
            when (char) {
                in UPPERS -> UPPERS[(UPPERS.indexOf(char) + shiftKey) % UPPERS.size]
                in LOWERS -> LOWERS[(LOWERS.indexOf(char) + shiftKey) % LOWERS.size]
                else      -> char
            }
        }.joinToString("")
    }

}