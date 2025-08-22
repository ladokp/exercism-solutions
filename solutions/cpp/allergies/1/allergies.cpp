#include "allergies.h"

namespace allergies {
    allergy_test::allergy_test(int score) {
        const std::unordered_map<int, std::string> allergens({
            {1, "eggs"},
            {2, "peanuts"},
            {4, "shellfish"},
            {8, "strawberries"},
            {16, "tomatoes"},
            {32, "chocolate"},
            {64, "pollen"},
            {128, "cats"},
        });
        for (auto x : allergens) {
            if ((score & x.first) == x.first)
                list_of_items_allergic_to.insert(x.second);
        }
    }
    std::unordered_set<std::string> allergy_test::get_allergies() {return list_of_items_allergic_to;}
    bool allergy_test::is_allergic_to(std::string str) {
        return list_of_items_allergic_to.count(str);
    }
}  // namespace allergies
