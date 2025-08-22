#!/usr/bin/env node
const fs = require('fs');
const path = require('path');

const VALID_OPTIONS = ['n', 'l', 'i', 'v', 'x'];

const ARGS = process.argv;

/**
 * Checks if a line matches the given pattern.
 * @param {string} line - The line to check.
 * @param {string} pattern - The pattern to match.
 * @returns {boolean} - True if the line matches the pattern, false otherwise.
 */
function checkLineMatchesPattern(line, pattern) {
  let left = line;
  let right = pattern;

  if (isOptionSet('i')) {
    left = line.toLowerCase();
    right = pattern.toLowerCase();
  }

  if (isOptionSet('x')) {
    return left === right;
  }

  return left.match(right) !== null;
}

/**
 * Reads the given file and returns its lines.
 * @param {string} file - Path to the file.
 * @returns {string[]} - The lines of the file.
 */
function readLines(file) {
  const data = fs.readFileSync(path.resolve(file), { encoding: 'utf-8' });
  return data.split(/\r?\n/);
}

/**
 * Parses the command-line arguments and returns the configuration.
 * @returns {object} - The configuration object.
 */
function getConfigFromArgs() {
  const config = {
    pattern: '',
    options: [],
    files: [],
  };

  let patternFound = false;

  ARGS.slice(2).forEach((val) => {
    if (patternFound) {
      config.files.push(val);
      return;
    }

    if (val.startsWith('-')) {
      const option = val.replace('-', '');

      if (!VALID_OPTIONS.includes(option)) {
        throw new Error(`Unknown option ${option}`);
      }

      config.options.push(option);
      return;
    }

    patternFound = true;
    config.pattern = val;
  });

  return config;
}

/**
 * Checks if an option is set.
 * @param {string} option - The option to check.
 * @returns {boolean} - True if the option is set, false otherwise.
 */
const isOptionSet = (option) => options.includes(option);

const { options, pattern, files } = getConfigFromArgs();

files.forEach((file) => {
  const lines = readLines(file);

  if (isOptionSet('l')) {
    const foundMatch = lines.find((line) => {
      const lineMatchesPattern = checkLineMatchesPattern(line, pattern);
      return isOptionSet('v') ? !lineMatchesPattern : lineMatchesPattern;
    });

    if (foundMatch) {
      console.log(file);
    }

    return;
  }

  lines.forEach((line, index) => {
    let result = '';
    let shouldOutputLine = checkLineMatchesPattern(line, pattern);

    if (isOptionSet('v')) {
      shouldOutputLine = !shouldOutputLine;
    }

    if (shouldOutputLine) {
      if (files.length > 1) {
        result += `${file}:`;
      }

      if (isOptionSet('n')) {
        result += `${index + 1}:`;
      }

      result += line;
      console.log(result);
    }
  });
});