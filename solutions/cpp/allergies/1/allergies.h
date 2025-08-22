#if !defined(ALLERGIES_H)
#define ALLERGIES_H

#include <unordered_set>
#include <unordered_map>
#include <string>

namespace allergies {
    class allergy_test {
    private:
        std::unordered_set<std::string> list_of_items_allergic_to;
    public:
        allergy_test(int score);
        std::unordered_set<std::string> get_allergies();
        bool is_allergic_to(std::string str);
    };
}  // namespace allergies
#endif // ALLERGIES_H