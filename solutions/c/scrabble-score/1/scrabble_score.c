#include "scrabble_score.h"
#include <ctype.h>

static unsigned int score_book[] = { 1, 3, 3,  2,  1, 4, 2, 4, 1, 8, 5, 1,  3,
                                     1, 1, 3, 10,  1, 1, 1, 1, 4, 4, 8, 4, 10 };

unsigned int score(const char *word)
{
   unsigned int word_score = 0;
   char character;

   while ((character = tolower(*word++)) != '\0') {
      word_score += score_book[character - 'a'];
   }

   return word_score;
}
