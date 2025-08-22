/**
 * Represents a character with various abilities.
 */
export class Character {
  /**
   * Creates an instance of Character.
   */
  constructor() {
    /**
     * @type {number} The strength ability score.
     */
    this.strength = Character.rollAbility();
    
    /**
     * @type {number} The dexterity ability score.
     */
    this.dexterity = Character.rollAbility();
    
    /**
     * @type {number} The constitution ability score.
     */
    this.constitution = Character.rollAbility();
    
    /**
     * @type {number} The intelligence ability score.
     */
    this.intelligence = Character.rollAbility();
    
    /**
     * @type {number} The wisdom ability score.
     */
    this.wisdom = Character.rollAbility();
    
    /**
     * @type {number} The charisma ability score.
     */
    this.charisma = Character.rollAbility();
    
    /**
     * @type {number} The hit points of the character.
     */
    this.hitpoints = 10 + abilityModifier(this.constitution);
  }

  /**
   * Rolls the dice to determine an ability score.
   * @returns {number} The calculated ability score.
   */
  static rollAbility() {
    let diceRolls = [];

    for (let i = 0; i < 4; i++) {
      diceRolls.push(Math.floor(Math.random() * 5) + 1);
    }

    return diceRolls
      .sort()
      .slice(1)
      .reduce((partialSum, num) => partialSum + num, 0);
  }
}

/**
 * Calculates the modifier for a given ability score.
 * @param {number} abilityScore - The ability score to calculate the modifier for.
 * @returns {number} The calculated ability modifier.
 * @throws Will throw an error if the ability score is less than 3 or greater than 18.
 */
export const abilityModifier = (abilityScore) => {
  if (abilityScore < 3) {
    throw new Error('Ability scores must be at least 3');
  }

  if (abilityScore > 18) {
    throw new Error('Ability scores can be at most 18');
  }

  return Math.floor((abilityScore - 10) / 2);
};
