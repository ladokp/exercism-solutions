class Bob
  hey: (sentence) ->
        sentence = sentence.replace /^\s+|\s+$/g, ""
        answer = "Whatever."
        answer_key = -1
        is_a_question = sentence.match(/\?$/)
        is_yelling = sentence.match(/[A-Z]/) and !sentence.match(/[a-z]/)
        if sentence.match(/^\s*$/)
            return "Fine. Be that way!"
        if is_yelling and is_a_question
            return "Calm down, I know what I'm doing!"
        if is_yelling
            return "Whoa, chill out!"
        if is_a_question
            return "Sure."
        answer

module.exports = Bob