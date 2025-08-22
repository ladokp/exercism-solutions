/**
 * Default list of students.
 * @type {string[]}
 */
const DEFAULT_STUDENTS = [
  'Alice',
  'Bob',
  'Charlie',
  'David',
  'Eve',
  'Fred',
  'Ginny',
  'Harriet',
  'Ileana',
  'Joseph',
  'Kincaid',
  'Larry',
];

/**
 * Mapping of plant codes to plant names.
 * @type {Object.<string, string>}
 */
const PLANT_CODES = {
  G: 'grass',
  V: 'violets',
  R: 'radishes',
  C: 'clover',
};

/**
 * Gets the plants for a specific student based on the diagram.
 * @param {string[]} pots - The rows of the diagram.
 * @param {number} index - The index of the student.
 * @returns {string[]} The list of plants for the student.
 */
function getPlants(pots, index) {
  const plants = [];
  const position = 2 * index;
  plants.push(pots[0][position]);
  plants.push(pots[0][position + 1]);
  plants.push(pots[1][position]);
  plants.push(pots[1][position + 1]);
  return plants;
}

/**
 * Parses the diagram into a 2D array of plant codes.
 * @param {string} diagram - The diagram to parse.
 * @returns {string[][]} The parsed diagram.
 */
function parse(diagram) {
  return diagram
    .split('\n')
    .map((row) => [...row].map((sign) => PLANT_CODES[sign]));
}

/**
 * Represents a garden.
 */
export class Garden {
  /**
   * Creates a garden instance.
   * @param {string} diagram - The diagram of the garden.
   * @param {string[]} [students=DEFAULT_STUDENTS] - The list of students.
   */
  constructor(diagram, students = DEFAULT_STUDENTS) {
    this.students = students.slice().sort();

    this.plots = {};

    this.students.forEach((student, index) => {
      this.plots[student] = getPlants(parse(diagram), index);
    });
  }

  /**
   * Gets the plants for a specific student.
   * @param {string} student - The name of the student.
   * @returns {string[]} The list of plants for the student.
   */
  plants(student) {
    return this.plots[student];
  }
}