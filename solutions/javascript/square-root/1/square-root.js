/**
 * Calculates the square root of a non-negative number using the Newton-Raphson method.
 * Truncates the result to a given number of decimal digits (no rounding).
 *
 * @param {number} x - The number to compute the square root of.
 * @param {number} [tolerance=1e-10] - Precision tolerance for convergence.
 * @param {number} [maxIterations=1000] - Maximum number of iterations to avoid infinite loops.
 * @returns {number} The truncated square root of x.
 * @throws {Error} If x is negative.
 */
export function squareRoot(x, tolerance = 1e-10, maxIterations = 1000) {
    if (x < 0) {
        throw new Error("Negative input is not supported.");
    }

    if (x < 2) {
        return x;
    }

    let guess = x / 2;
    let iterations = 0;

    while (Math.abs(guess * guess - x) > tolerance && iterations < maxIterations) {
        guess = (guess + x / guess) / 2;
        iterations++;
    }

    return guess | 0;
};
