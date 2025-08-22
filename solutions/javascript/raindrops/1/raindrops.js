export const convert = (number) => {
  let result = "";
  for (const [key, value] 
       of Object.entries({3: "Pling", 5: "Plang", 7: "Plong"})
      ) {
    if (!(number % key)) result += value;
  }
  return !result ? number.toString() : result;
};
