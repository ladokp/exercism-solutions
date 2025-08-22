class Bob
  hey: (sentence) ->
        answer = "Whatever."
        answer_key = -1
        is_a_question = sentence.split("").reverse()[0] == "?"
        is_yelling = sentence == sentence.toUpperCase() and sentence.toLowerCase() != sentence.toUpperCase()
        if sentence.split(" ").join("") == ""
            return "Fine. Be that way!"
        else if is_yelling
            return "Whoa, chill out!"
        else if is_a_question
            return "Sure."
        else if is_a_question and is_yelling
            return "Calm down, I know what I'm doing!"
        answer

module.exports = Bob
