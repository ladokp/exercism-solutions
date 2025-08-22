/**
 * Calculates the score of a dart throw based on its coordinates.
 *
 * Scoring rules:
 * - 10 points for a throw within or on the circle with radius 1.
 * - 5 points for a throw within or on the circle with radius 5.
 * - 1 point for a throw within or on the circle with radius 10.
 * - 0 points for anything outside of the 10-radius circle.
 *
 * @param {number} x - The x-coordinate of the dart throw.
 * @param {number} y - The y-coordinate of the dart throw.
 * @returns {number} The score for the dart throw.
 */
export function score(x, y) {
  const distance_from_center = Math.sqrt(x * x + y * y);
  if (distance_from_center <= 1) return 10;
  if (distance_from_center <= 5) return 5;
  if (distance_from_center <= 10) return 1;
  return 0;
}
