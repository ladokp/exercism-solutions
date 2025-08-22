class RomanNumerals {
    private int number;
    
    RomanNumerals(int number) {
        this.number = number;
    }

    String getRomanNumeral() {
        StringBuilder resultBuilder = new StringBuilder();
        while (this.number > 0) {
            if (this.number >= 1000) {
                resultBuilder.append("M"); 
                this.number -= 1000;                  
            }
            else if (this.number >= 900) {
                resultBuilder.append("CM"); 
                this.number -= 900;                  
            }
            else if (this.number >= 500) {
                resultBuilder.append("D"); 
                this.number -= 500;                  
            }
            else if (this.number >= 400) {
                resultBuilder.append("CD"); 
                this.number -= 400;                  
            }
            else if (this.number >= 100) {
                resultBuilder.append("C"); 
                this.number -= 100;                  
            }
            else if (this.number >= 90) {
                resultBuilder.append("XC"); 
                this.number -= 90;                  
            }
            else if (this.number >= 50) {
                resultBuilder.append("L"); 
                this.number -= 50;                  
            }
            else if (this.number >= 40) {
                resultBuilder.append("XL"); 
                this.number -= 40;                  
            }
            else if (this.number >= 10) {
                resultBuilder.append("X"); 
                this.number -= 10;                  
            }
            else if (this.number >= 9) {
                resultBuilder.append("IX"); 
                this.number -= 9;                  
            }
            else if (this.number >= 5) {
                resultBuilder.append("V"); 
                this.number -= 5;                  
            }
            else if (this.number >= 4) {
                resultBuilder.append("IV"); 
                this.number -= 4;                  
            }
            else if (this.number >= 1) {
                resultBuilder.append("I"); 
                this.number -= 1;                  
            }
        }
        return resultBuilder.toString();
    }
}
