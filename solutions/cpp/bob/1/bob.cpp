#include "bob.h"

namespace bob {
    std::string hey(const std::string& s)
    {
        bool question = false;
        bool upperCase = false;
        bool lowerCase = false;
        bool nothing = true;
        
        for (char c : s) {
            if (islower(c))
                lowerCase = true;
            if (isupper(c))
                upperCase = true;
            if (!isspace(c)) {
                question = false;
                nothing = false;
            }
            if (c == '?')
            question = true;
        }
        
        bool shouting = upperCase && !lowerCase;
        if (shouting && question)
            return "Calm down, I know what I'm doing!";
        if (shouting)
            return "Whoa, chill out!";
        if (question)
            return "Sure.";
        if (nothing)
            return "Fine. Be that way!";
        return "Whatever.";
    }
}  // namespace bob
