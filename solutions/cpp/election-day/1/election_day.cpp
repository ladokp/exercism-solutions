#include <algorithm>
#include <string>
#include <vector>

namespace election {

// The election result struct is already created for you:

struct ElectionResult {
    // Name of the candidate
    std::string name{};
    // Number of votes the candidate has
    int votes{};
};

    int vote_count(const ElectionResult& election_result) {
        return election_result.votes;
    }

    void increment_vote_count(ElectionResult& election_result, const int votes) {
        election_result.votes += votes;
    }

    ElectionResult& determine_result(std::vector<ElectionResult>& candidates) {
        ElectionResult& president {
            *std::max_element(candidates.begin(),
                              candidates.end(),
                              [](auto a, auto b) { return a.votes < b.votes; })
        };
        president.name = "President " + president.name;
        return president;
    }

}  // namespace election