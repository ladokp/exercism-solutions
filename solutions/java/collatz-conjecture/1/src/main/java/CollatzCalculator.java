import java.lang.IllegalArgumentException;

class CollatzCalculator {

    int computeStepCount(int number) throws java.lang.IllegalArgumentException  {
        if (number <= 0)
            throw new IllegalArgumentException("Only positive integers are allowed");
        if (number == 1)
            return 0;
        if (number % 2 == 0)
            return 1 + this.computeStepCount(number / 2);
        return 1 + this.computeStepCount(3 * number + 1);
    }

}
