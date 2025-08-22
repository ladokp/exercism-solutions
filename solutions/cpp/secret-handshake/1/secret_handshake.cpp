#include "secret_handshake.h"
#include <vector>
#include <string>
#include <algorithm>

using namespace std;

vector<string> secret_handshake::commands(int value) {
    vector<string> answer;
    static const vector<string> actions = {"wink", "double blink", "close your eyes", "jump"};

    for (int i = 0; i < 4; ++i) {
        if (value & (1 << i)) {
            answer.push_back(actions[i]);
        }
    }

    if (value & (1 << 4)) {
        reverse(answer.begin(), answer.end());
    }

    return answer;
}
