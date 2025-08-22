#include "reverse_string.h"

#include <iterator>
#include <string>

namespace reverse_string {

std::string reverse_string(const std::string& source) {
  return std::string{source.rbegin(), source.rend()};
}

}  // namespace reverse_string
