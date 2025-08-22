#include <string>

namespace log_line {
    std::string message(std::string logline) {
        return logline.substr(logline.find(":") + 2);
    }

    std::string log_level(std::string logline) {
        return logline.substr(1, logline.find(":") - 2);
    }

    std::string reformat(std::string logline) {
        return message(logline) + " (" + log_level(logline) + ")";
    }
} // namespace log_line
