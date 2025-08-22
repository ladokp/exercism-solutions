#include "high_scores.h"
namespace arcade {
    vector<int> HighScores::list_scores() {
        return scores;
    }

    int HighScores::latest_score() {
        return scores.back();
    }

    int HighScores::personal_best() {
        return *max_element(scores.begin(), scores.end());
    }

    vector<int> HighScores::top_three() {
        vector<int> result(scores);
        sort(result.begin(), result.end(), greater<>());
        result.resize(min(result.size(), static_cast<size_t>(3)));
        return result;
    }
}