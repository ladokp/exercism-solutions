#include <array>
#include <string>
#include <vector>
#include <cmath>

// Round down all provided student scores.
std::vector<int> round_down_scores(std::vector<double> student_scores) {
    std::vector<int> result;
    for(auto & element : student_scores) {
        result.push_back(int(floor(element)));
    }
    return result;
}


// Count the number of failing students out of the group provided.
int count_failed_students(std::vector<int> student_scores) {
    int failed_students = 0;
    for(auto & element : student_scores) {
        if(element <= 40) {
            failed_students++;
        }
    }
    return failed_students;
}

// Determine how many of the provided student scores were 'the best' based on the provided threshold.
std::vector<int> above_threshold(std::vector<int> student_scores, int threshold) {
    std::vector<int> result;
    for(auto & element : student_scores) {
        if(element >= threshold) {
            result.push_back(element);
        }
    }
    return result;
}

// Create a list of grade thresholds based on the provided highest grade.
std::array<int, 4> letter_grades(int highest_score) {
    int step = int(floor((highest_score - 40) / 4));
    std::array<int, 4> result {41, 41+step, 41+2*step, 41+3*step};
    return result;
}

// Organize the student's rank, name, and grade information in ascending order.
std::vector<std::string> student_ranking(std::vector<int> student_scores, std::vector<std::string> student_names) {
    std::vector<std::string> result;
    for(unsigned int i = 0; i < student_scores.size(); i++) {
        result.push_back(std::to_string(i+1) + ". " + student_names[i] + ": " + std::to_string(student_scores[i]));
    }
    return result;
}

// Create a string that contains the name of the first student to make a perfect score on the exam.
std::string perfect_score(std::vector<int> student_scores, std::vector<std::string> student_names) {
    for(unsigned int i = 0; i < student_scores.size(); i++) {
        if(student_scores[i] == 100) {
            return student_names[i];
        }
    }
    return "";
}