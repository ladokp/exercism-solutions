using System;

public static class CollatzConjecture
{
    public static int Steps(int number)
    {
        if (number < 1)
            throw new ArgumentOutOfRangeException("Number has to be positive!");
        if (number == 1)
            return 0;
        if (number % 2 == 0)
            return 1 + CollatzConjecture.Steps(number / 2);
        return 1 + CollatzConjecture.Steps(number * 3 + 1);
    }
}