const letters = [...Array(26)].map((val, i) => String.fromCharCode(i + 97));

export function isPangram(sentence) {
    const lowercaseSentence = sentence.toLowerCase()
    for (const character of letters) {
        if (!lowercaseSentence.includes(character)) {
            return false;
        }
    };
    return true;
};