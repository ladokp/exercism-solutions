#include "pangram.h"

#include <cctype>
#include <cstdint>

bool pangram::is_pangram(const std::string& str)
{
  std::uint_fast32_t histogram = 0;
  for(const auto c : str) if(std::isalpha(c))
  {
    histogram |= 1 << (std::tolower(c) - 'a');
  }
  return histogram == (1 << 26) - 1;
}