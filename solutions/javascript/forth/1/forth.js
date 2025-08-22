/**
 * Class representing a Forth interpreter.
 */
export class Forth {
  /**
   * Create a Forth interpreter.
   * @param {Array} stack - The initial stack.
   * @param {Object} commands - The initial commands.
   */
  constructor(stack = [], commands = Forth.basicCommands()) {
    this.stack = stack;
    this.commands = commands;
  }

  /**
   * Evaluate a Forth program.
   * @param {string} program - The Forth program to evaluate.
   * @throws Will throw an error if an unknown command or an unterminated definition is encountered.
   */
  evaluate(program) {
    const words = program.toLowerCase().split(' ');

    for (let t = 0; t < words.length; t++) {
      const word = words[t];

      // numbers
      if (/^-?\d+$/.test(word)) {
        this.stack.push(Number(word));

      // word definition
      } else if (word === ':') {
        const semicolon = words.indexOf(';', t);

        if (semicolon === -1) {
          throw new Error('Unterminated definition');
        }

        this.defineCommand(
          words[t + 1],
          words.slice(t + 2, semicolon).join(' ')
        );

        t = semicolon;

      // commands
      } else {
        const command = this.commands[word];

        if (!command) {
          throw new Error('Unknown command');
        }

        this.performCommand(command);
      }
    }
  }

  /**
   * Define a new command in the Forth interpreter.
   * @param {string} word - The word representing the command.
   * @param {string} subprogram - The subprogram to run for the command.
   * @throws Will throw an error if the word is a keyword or if there is an invalid definition.
   */
  defineCommand(word, subprogram) {
    if (Forth.isKeyword(word)) {
      throw new Error('Invalid definition');
    }

    let execute;

    // Evaluate subprogram immediately if possible, otherwise evaluate later
    try {
      const stackSize = this.stack.length;
      this.evaluate(subprogram);
      const result = this.stack.splice(stackSize);
      execute = () => result;
    } catch {
      execute = this.evaluate.bind(this, subprogram);
    }

    this.commands[word] = {
      arity: 0, // handled inside the call
      execute,
    };
  }

  /**
   * Perform a command in the Forth interpreter.
   * @param {Object} command - The command to perform.
   * @throws Will throw an error if the stack is empty or if there are not enough arguments.
   */
  performCommand(command) {
    if (command.arity > this.stack.length) {
      throw new Error('Stack empty');
    }

    const args = this.stack.splice(this.stack.length - command.arity);
    const vals = command.execute.apply(this, args);
    this.stack.push.apply(this.stack, vals);
  }

  /**
   * Check if a word is a Forth keyword.
   * @param {string} word - The word to check.
   * @returns {boolean} True if the word is a keyword, otherwise false.
   */
  static isKeyword(word) {
    return word === ':' || word === ';' || /^-?\d+$/.test(word);
  }

  /**
   * Get the basic commands of the Forth interpreter.
   * @returns {Object} The basic commands.
   */
  static basicCommands() {
    return {
      '+': { arity: 2, execute: function(a, b) { return [a + b]; } },
      '-': { arity: 2, execute: function(a, b) { return [a - b]; } },
      '*': { arity: 2, execute: function(a, b) { return [a * b]; } },
      '/': {
        arity: 2,
        execute: function(a, b) {
          if (b === 0) {
            throw new Error('Division by zero');
          }
          return [Math.floor(a / b)];
        },
      },
      dup: { arity: 1, execute: function(a) { return [a, a]; } },
      drop: { arity: 1, execute: function() { return []; } },
      swap: { arity: 2, execute: function(a, b) { return [b, a]; } },
      over: { arity: 2, execute: function(a, b) { return [a, b, a]; } },
    };
  }
}
