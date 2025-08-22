#include "reverse_string.h"
#include <stdlib.h>
#include <string.h>

char *reverse(const char *value)
{
   size_t value_length = strlen(value);
   char *result = malloc(value_length + 1);
   char *dest = result + value_length;

   *dest = '\0';
   while (*value) { *--dest = *value++; }
   return result;
}
