#define _POSIX_C_SOURCE 200809L
#include "kindergarten_garden.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define NUM_STUDENTS 12

static int student_to_index(const char *student)
{
    static const char *students[NUM_STUDENTS] = {
        "Alice",  "Bob",    "Charlie", "David",
        "Eve",    "Fred",   "Ginny",   "Harriet",
        "Ileana", "Joseph", "Kincaid", "Larry"
    };
    
    for (size_t i = 0; i < NUM_STUDENTS; ++i) {
        if (strcmp(students[i], student) == 0) {
            return i;
        }
    }
    fprintf(stderr, "error: student %s not found\n", student);
    exit(EXIT_FAILURE);
}

static plant_t letter_to_plant(char letter)
{
    switch (letter) {
        case 'C': return CLOVER;
        case 'G': return GRASS;
        case 'R': return RADISHES;
        case 'V': return VIOLETS;
        default:
            fprintf(stderr, "error: letter %c does not match any plant\n", letter);
            exit(EXIT_FAILURE);
    }
}

plants_t plants(const char *diagram, const char *student)
{
    char *rows[2];
    char *diagram_copy = strdup(diagram); // Make a copy to tokenize
    if (diagram_copy == NULL) {
        fprintf(stderr, "error: memory allocation failed\n");
        exit(EXIT_FAILURE);
    }

    rows[0] = strtok(diagram_copy, "\n");
    rows[1] = strtok(NULL, "\n");

    if (rows[0] == NULL || rows[1] == NULL) {
        fprintf(stderr, "error: invalid diagram format\n");
        free(diagram_copy);
        exit(EXIT_FAILURE);
    }

    const size_t student_col = student_to_index(student) * 2;
    plants_t res;
    
    for (size_t i = 0; i < 4; ++i) {
        const size_t row_index = i / 2;
        const size_t col_index = student_col + i % 2;
        res.plants[i] = letter_to_plant(rows[row_index][col_index]);
    }

    free(diagram_copy);
    return res;
}
