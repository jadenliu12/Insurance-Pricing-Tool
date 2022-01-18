# Insurance-Pricing-Tool
  
## Basic Informations  
Description: An insurance pricing tool that will evaluate the evolution of the reserve  
Language: R using shiny  
Features:  
- 2 types of insurance product (Assurance/Annuity)
- 2 types of life product (Single/Joint)
- Evolution of reserve graph with detailed points
- Premium calculation down to 3 decimal points
- Various inputs (Age, Benefit, Benefit Payment, Premium Payment, etc)
- Interactive design with animations and loader
- Overview table of the inputs
   
## Input Explaination
The first 2 main inputs are Insurance Product and Life Product. After deciding those 2 inputs, user can choose between a range of insurance benefit bellow: 
  
| Insurance/Life Product | `Single` | `Joint` |
| :---: | :---: | :---: |
| `Assurance` | Whole Assurance, Term Assurance, Endowment Assurance, Pure Endowment | Whole Assurance, Term Assurance |
| `Annuity` | Whole Annuity, Term Annuity | Whole Annuity, Term Annuity |
  
After selection of the insurance benefit type, there are other parameters that users can modify namely, age group (if single life), insurance benefit payment and premium payment (only applicable to assurance), assured sum, expenses, bonuses, inflation, age, interest rate, and benefit term.  
  
Extra Notes:
1. Expenses Rate
   * Claim Expense: To all product
   * Initial Expense: To all product
   * Premium Expense: Applicable to assurance product with level premium
   * Annuity Expense: Applicable to annuity insurance product
2. Bonuses Rate - is only relevant to whole assurance, term assurance, endowment assurance
3. Inflation Rate - is only relevant to whole annuity and term annuity
4. Benefit Term - is hidden for whole assurance and whole annuity product. The range of benefit term depends on the selected age. Namely, as the max age is 100, if selected age is 80, then the maximum term is 20
5. All rates including interest are ranging from 0%-50%


