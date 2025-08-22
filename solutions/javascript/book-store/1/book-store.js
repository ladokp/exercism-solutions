const BOOK_PRICE = 800;
const DISCOUNTS = [1, 0.95, 0.9, 0.8, 0.75];

/**
 * Calculate the cost for a group of unique books.
 * @param {number} size - The number of unique books.
 * @returns {number} - The total cost for the group with discount applied.
 */
const groupCost = (size) => size * BOOK_PRICE * DISCOUNTS[size - 1];

/**
 * Count the occurrences of each book.
 * @param {number[]} books - The array of book IDs.
 * @returns {Map} - A Map with book counts.
 */
const groupBooks = (books) => {
  const counter = new Map();
  books.forEach((book) => {
    counter.set(book, (counter.get(book) || 0) + 1);
  });
  return counter;
};

/**
 * Create a new book count by removing `size` most frequent books.
 * @param {Map} bookCounts - The current book counts.
 * @param {number} size - The number of books to remove.
 * @returns {Map} - A new Map with the updated counts.
 */
const removeMostFrequent = (bookCounts, size) => {
  const sortedBooks = [...bookCounts.entries()].sort((a, b) => b[1] - a[1]);
  const updatedCounts = new Map(bookCounts);

  sortedBooks.slice(0, size).forEach(([book, count]) => {
    if (count === 1) {
      updatedCounts.delete(book);
    } else {
      updatedCounts.set(book, count - 1);
    }
  });

  return updatedCounts;
};

/**
 * Convert a Map of book counts back to a sorted array of books.
 * @param {Map} bookCounts - The Map of book counts.
 * @returns {number[]} - The array of books.
 */
const booksFromCounts = (bookCounts) => {
  return [].concat(
    ...[...bookCounts.entries()].map(([book, count]) =>
      Array(count).fill(book)
    )
  );
};

/**
 * Recursively calculate the minimum cost of buying the books.
 * @param {number[]} books - The sorted array of books.
 * @returns {number} - The minimum total cost.
 */
const calculate = (books) => {
  if (books.length === 0) return 0;

  const bookCounts = groupBooks(books);
  let minPrice = books.length * BOOK_PRICE;

  for (let size = bookCounts.size; size > 1; size--) {
    const reducedCounts = removeMostFrequent(bookCounts, size);
    const remainingBooks = booksFromCounts(reducedCounts);
    minPrice = Math.min(minPrice, groupCost(size) + calculate(remainingBooks));
  }

  return minPrice;
};

/**
 * Calculate the total cost for purchasing the books with the best discount strategy.
 * @param {number[]} books - The array of book IDs.
 * @returns {number} - The total cost.
 */
export const cost = (books) => {
  if (books.length === 0) return 0;
  return calculate(books.sort((a, b) => a - b));
};
