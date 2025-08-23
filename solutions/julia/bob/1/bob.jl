function bob(stimulus)
    statement = strip(stimulus)
    question = isquestion(statement)
    yelling = isyelling(statement)
    silent = isempty(statement)
    ( question && yelling ) ? "Calm down, I know what I'm doing!" : 
               ( question ) ? "Sure." :
                ( yelling ) ? "Whoa, chill out!" :
                 ( silent ) ? "Fine. Be that way!" :
                              "Whatever."
end

function isquestion(str)
    !isempty(str) && str[end] == '?'
end

function isyelling(str)
    any(isuppercase, str) && !any(islowercase,str)
end