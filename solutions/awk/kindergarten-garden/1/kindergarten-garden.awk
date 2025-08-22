# This script maps kindergarten students to their garden plots

BEGIN {
    # Initialize student names and plant codes
    split("Alice Bob Charlie David Eve Fred Ginny Harriet Ileana Joseph Kincaid Larry", names)
    plants["G"] = "grass"
    plants["C"] = "clover"
    plants["V"] = "violets"
    plants["R"] = "radishes"

    # Set field pattern to match two characters
    FPAT = ".."
}

{
    # Map each student's plants
    for (i = 1; i <= NF; i++)
        plots[names[i]] = plots[names[i]] $i
}

END {
    # Ensure the specified student's name exists
    if (!(name in plots)) {
        print "Error: Student name not found" > "/dev/stderr"
        exit 1
    }

    # Split the student's plant codes and print their corresponding plant names
    n = split(plots[name], p, "")
    printf "%s", plants[p[1]]
    for (i = 2; i <= n; i++)
        printf " %s", plants[p[i]]
    printf "\n"
}
