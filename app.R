library("shiny")
library("shinyjs")
library("shinyWidgets")
library("shinycssloaders")
library("rsconnect")
library("dplyr")
library("ggplot2")

Lxs = c(10000.0000,9994.0000,9988.0636,9982.2006,9976.3909,9970.6346,9964.9313,9959.2613,9953.6144,9947.9807,9942.3402,9936.6730,9930.9694,9925.2094,9919.3535,9913.3821,9907.2655,9900.9645,9894.4299,9887.6126,9880.4540,9872.8954,9864.8688,9856.2863,9847.0510,9837.0661,9826.2060,9814.3359,9801.3123,9786.9534,
        9771.0789,9753.4714,9733.8865,9712.0728,9687.7149,9660.5021,9630.0522,9595.9715,9557.8179,9515.1040,9467.2906,9413.8004,9354.0040,9287.2164,9212.7143,9129.7170,9037.3973,8934.8771,8821.2612,8695.6199,8557.0118,8404.4916,8237.1329,8054.0544,7854.4508,7637.6208,7403.0084,7150.2401,6879.1673,6589.9258,
        6282.9803,5959.1680,5619.7577,5266.4604,4901.4789,4527.4960,4147.6708,3765.5998,3385.2479,3010.8395,2646.7416,2297.2976,1966.6499,1658.5545,1376.1906,1121.9889,897.5025,703.3242,539.0643,403.4023,294.2061,208.7060,143.7120,95.8476,61.7733,38.3796,22.9284,13.1359,7.1968,3.7596,1.8669,0.8784,0.3903,0.1632,0.0640,0.0234,
        0.0080,0.0025,0.0007,0.0002,0.0000,0.0000,0.0000,0.0000)

# Constant states ==================

states <- c(
  c("single", "pure_endowment", "end_of_year_of_death"), #1
  c("single", "term_insurance", "end_of_year_of_death"), #2
  c("single", "term_insurance", "immediately_on_death"), #3
  c("single", "endowment", "end_of_year_of_death"),      #4
  c("single", "endowment", "immediately_on_death"),      #5
  c("single", "whole_assurance", "end_of_year_of_death"),#6
  c("single", "whole_assurance", "immediately_on_death"),#7
  c("single", "whole_annuity", "end_of_year_of_death"),  #8
  c("single", "term_annuity", "end_of_year_of_death"),   #9
  c("level", "pure_endowment", "end_of_year_of_death"), #10
  c("level", "term_insurance", "end_of_year_of_death"), #11  
  c("level", "term_insurance", "immediately_on_death"), #12
  c("level", "endowment", "end_of_year_of_death"),      #13
  c("level", "endowment", "immediately_on_death"),      #14
  c("level", "whole_assurance", "end_of_year_of_death"),#15
  c("level", "whole_assurance", "immediately_on_death") #16
)

# ==================================

# BASIC FUNCTIONS =================

# WHOLE ANNUITY
wholeannuity <- 
  function(
    age,
    interest,
    age_group
  ){
    interestPercentage = interest / 100
    discountrate = 1 / (1 + interestPercentage)
    
    x = age - 20 + 4
    if(!age_group)
      x = x - 3
    
    boundary = 0
    if(!age_group)
      boundary = 3  
    
    
    lx = Lxs[-(1:(x-1))] 
    if (age == 20 && age_group) 
      lx = Lxs[-(1:3)]
    else if (age == 20 && !age_group)
      lx = Lxs[(1:101)]
    
    Output = sum(discountrate^(0:(120-age+boundary))*lx)/Lxs[x]
    
    return(Output)
  }


# TERM ANNUITY DUE
term_annuity_due <- 
  function(
    age,
    benefit_term,
    interest,
    age_group
  ){
    interestPercentage = interest / 100
    discountrate = 1 / (1 + interestPercentage)    
    
    x = age - 20 + 4
    if(!age_group)
      x = x - 3    
    
    probability = Lxs[x + benefit_term] / Lxs[x]
    
    res = probability * (discountrate ^ benefit_term)
    
    Output = wholeannuity(age, interest, age_group) - (res * wholeannuity(age + benefit_term, interest, age_group))
    
    return(Output)
  }

# TERM ANNUITY
term_annuity <- 
  function(
    age,
    benefit_term,
    interest,
    age_group
  ){
    interestPercentage = interest / 100
    discountrate = 1 / (1 + interestPercentage) 
    
    x = age - 20 + 4
    if(!age_group)
      x = x - 3    
    
    probability = Lxs[x + benefit_term] / Lxs[x]
    res = probability * (discountrate ^ benefit_term)
    
    Output = term_annuity_due(age,benefit_term, interest, age_group) - 1 + res
    
    return(Output)
  }

# WHOLE ASSURANCE CONT
wholeassurance_cont <- 
  function(
    age,
    interest,
    age_group
  ){
    interestPercentage = interest / 100
    int = 1 + interestPercentage
    discount = log(int)
    
    expected = 1 - (discount * (wholeannuity(age,interest,age_group)-0.5))
    return(expected)
  }

# WHOLE ASSURANCE
wholeassurance <- 
  function(
    age,
    interest,
    age_group
  ){
    interestPercentage = interest / 100
    
    discountfactor = (interestPercentage) / (1 + interestPercentage)
    expected = 1 - discountfactor * wholeannuity(age, interest, age_group)
    
    return(expected)
  }

# TERM ASSURANCE (ARREARS)
term_assurance <- 
  function(
    age,
    benefit_term,
    interest,
    age_group
  ){
    x = age - 20 + 4
    if(!age_group)
      x = x - 3       
    
    interestPercentage = interest / 100
    
    discountrate =(Lxs[x + benefit_term] / Lxs[x]) * ((1 / (1 + interestPercentage)) ^ benefit_term)
    Output = wholeassurance(age, interest, age_group) - (discountrate * wholeassurance(age + benefit_term, interest, age_group))
    
    return(Output)
  }

# ENDOWMENT ASSURANCE
endowment_assurance <- 
  function(
    age,
    benefit_term,
    interest,
    age_group
  ){
    x = age - 20 + 4
    if(!age_group)
      x = x - 3       
    
    interestPercentage = interest / 100
    discountfactor = 1 / (1 + interestPercentage)
    pure = (discountfactor^benefit_term)*Lxs[x+benefit_term]/Lxs[x]
    
    Output = term_assurance(age, benefit_term, interest, age_group) + pure
    return(Output)
  }

# ENDOWMENT ASSURANCE DUE
endowment_assurance_due <- 
  function(
    age,
    benefit_term,
    interest,
    age_group
  ){
    x = age - 20 + 4
    if(!age_group)
      x = x - 3       
    
    interestPercentage = interest / 100
    discountfactor = 1 / (1 + interestPercentage)
    pure = (discountfactor^benefit_term)*Lxs[x+benefit_term]/Lxs[x]
    
    Output = ((1+ interestPercentage)^0.5)* term_assurance(age, benefit_term, interest, age_group) + pure
    return(Output)
  }

# INCREASING WHOLE ASSURANCE (ARREARS)
increase_wholeass <- 
  function(
    age,
    interest,
    age_group
  ){
    interestPercentage = interest / 100
    discountrate = 1 / (1 + interestPercentage)
    
    x = age - 20 + 4
    if(!age_group)
      x = x - 3
    
    # boundary = 0
    # if(!age_group)
    #   boundary = 3  
    # 
    # 
    # lx = Lxs[-(1:(x-1))] 
    # if (age == 20 && age_group) 
    #   lx = Lxs[-(1:3)]
    # else if (age == 20 && !age_group)
    #   lx = Lxs[(1:101)]
    
    total = 0
    
    for (term in 0 : (100-age)){
      prob = Lxs[x + term] / Lxs[x]
      death = 1 - (Lxs[x+term+1]/Lxs[x+term])
      total = total + ((term+1)*discountrate^(term+1) * prob * death)
    }
    
    
    return(total)
  }

# INCREASING TERM ASSURANCE 
increase_termass <- 
  function(
    age,
    benefit_term,
    interest,
    age_group
  ){
    interestPercentage = interest / 100
    discountrate = 1 / (1 + interestPercentage)
    x = age - 20 + 4
    if(!age_group)
      x = x - 3
    
    disc = (discountrate^benefit_term) * (Lxs[x+benefit_term]/Lxs[x])
    assurance = benefit_term * wholeassurance(age+benefit_term,interest,age_group)
    output = increase_wholeass(age,interest,age_group) - disc * (increase_wholeass(age+benefit_term,interest,age_group) + assurance)
    
    return(output)
  }

# INCREASING ENDOWMENT  ASSURANCE
increase_endowment <-
  function(
    age,
    benefit_term,
    interest,
    age_group
  ){
    interestPercentage = interest / 100
    discountrate = 1 / (1 + interestPercentage)
    
    x = age - 20 + 4
    if(!age_group)
      x = x - 3
    
    disc = (discountrate^benefit_term) * (Lxs[x+benefit_term]/Lxs[x])
    output = increase_termass(age,benefit_term, interest, age_group) + benefit_term * disc
    
    return(output)
  }

# INCREASING ENDOWMENT ASSURANCE ADVANCE
increase_endowment_due <-
  function(
    age,
    benefit_term,
    interest,
    age_group
  ){
    interestPercentage = interest / 100
    discountrate = 1 / (1 + interestPercentage)
    
    x = age - 20 + 4
    if(!age_group)
      x = x - 3
    
    disc = (discountrate^benefit_term) * (Lxs[x+benefit_term]/Lxs[x])
    output = (1+interest)^0.5 * increase_termass(age,benefit_term, interest, age_group) + benefit_term * disc
    
    return(output)
  }

# ==================================

# JOINT BASIC FUNCTIONS ===========

# JOINT WHOLE ANNUITY (DUE)
joint_wholeannuity <- 
  function(
    agex,
    agey,
    interest
  ){
    interestPercentage = interest / 100
    discountrate = 1 / (1 + interestPercentage)
    x = agex - 20 + 4
    y = agey - 20 + 1
    bound = max(agex,agey)
    output = 0
    
    for (i in 0:(100-bound)) {
      probx = Lxs[x+i] / Lxs[x]
      proby = Lxs[y+i] / Lxs[y]
      
      
      output = output + (discountrate ^ (i) * probx * proby)
    }
    
    return(output)
    
  }

# JOINT TERM ANNUITY (DUE)
joint_termannuity <- 
  function(
    agex,
    agey,
    benefit_term,
    interest
  ){
    interestPercentage = interest / 100
    discount = 1 / (1 + interestPercentage)
    discountrate = discount ^ benefit_term
    x = agex - 20 + 4
    y = agey - 20 + 1
    probx = Lxs[x+benefit_term] / Lxs[x]
    proby = Lxs[y+benefit_term] / Lxs[y]
    
    output = joint_wholeannuity(agex,agey,interest) - (discountrate * probx * proby * joint_wholeannuity(agex + benefit_term, agey+benefit_term, interest))
    return(output)
  }


# JOINT WHOLE ASSURANCE 
joint_wholeassur <- 
  function(
    agex,
    agey,
    interest
  ){
    interestPercentage = interest / 100
    discountrate = interestPercentage / (1+interestPercentage)
    
    output = 1 - discountrate*(joint_wholeannuity(agex,agey,interest))
    return(output)
  }


# JOINT WHOLE ASSURANCE (DUE)
joint_wholeassur_due <- 
  function(
    agex,
    agey,
    interest
  ){
    interestPercentage = interest / 100
    output = ((1+interestPercentage) ^ 0.5) * joint_wholeassur(agex,agey, interest)
    return(output)
  }


# JOINT TERM ASSURANCE
joint_termassur <- 
  function(
    agex,
    agey,
    benefit_term,
    interest
  ){
    interestPercentage = interest / 100
    discount = 1 / (1+interestPercentage)
    discountrate = discount ^ benefit_term
    
    x = agex - 20 + 4
    y = agey - 20 + 1
    probx = Lxs[x+benefit_term] / Lxs[x]
    proby = Lxs[y+benefit_term] / Lxs[y]
    
    joint1 = joint_wholeassur(agex, agey, interest)
    joint2 = joint_wholeassur(agex+benefit_term, agey+benefit_term, interest)
    
    output = joint1 - (discountrate * probx * proby) * joint2
    return(output)
  }

# JOINT TERM ASSURANCE (DUE)
joint_termassur_due <- 
  function(
    agex,
    agey,
    benefit_term,
    interest
  ){
    interestPercentage = interest / 100
    output = ((1+interestPercentage) ^ 0.5) * joint_termassur(agex,agey, benefit_term,interest)
    return(output)
  }


# INCREASING JOINT WHOLE ASSURANCE
joint_increase_wholeassur <- 
  function(
    agex,
    agey,
    interest
  ){
    interestPercentage = interest / 100
    discountrate = 1/ (1 + interestPercentage)
    x = agex - 20 + 4
    y = agey - 20 + 1
    bound = max(agex,agey)
    
    output = 0
    
    for(i in 0 : (100 - bound)){
      probx = Lxs[x+i] / Lxs[x]
      proby = Lxs [y+i] / Lxs[y]
      diex = Lxs [x+i+1] / Lxs[x+i]
      diey = Lxs [y+i+1] / Lxs[y+i]
      joint = diex*diey
      
      output = output + ((i+1)*discountrate^(i+1)*probx*proby*(1-joint))
    }
    
    return(output)
  }

# INCREASING JOINT TERM ASSURANCE
joint_increase_termassur <- 
  function(
    agex,
    agey,
    benefit_term,
    interest
  ){
    interestPercentage = interest / 100
    discount = 1/ (1 + interestPercentage)
    discountrate = discount ^ benefit_term
    x = agex - 20 + 4
    y = agey - 20 + 1
    probx = Lxs[x+benefit_term] / Lxs[x]
    proby = Lxs[y+benefit_term] / Lxs[y]
    joint1 = joint_increase_wholeassur(agex,agey, interest)
    joint2 = joint_increase_wholeassur(agex + benefit_term, agey + benefit_term , interest)
    
    output = joint1 - discountrate*probx*proby*(joint2 + benefit_term * joint_wholeassur(agex + benefit_term, agey + benefit_term, interest))
    return(output)
  }

# ==================================

# STATE 1 SINGLE-PE-EOY ============

## Premium
single_pure_eoy_premium <-
  function(
    params
  ){
    # Deconstruct parameter
    age = params[1]
    benefit_term = params[2]
    assured_sum = params[3]
    interest = params[4]
    claim_expense = params[5]
    initial_expense = params[6]
    premium_expense = params[7]
    age_group = params[8]
    bonus = params[9]
    inflation = params[10]
    
    interestPercentage = interest / 100
    discountrate = 1 / (1 + interestPercentage)
    
    x = age - 20 + 4
    if(!age_group)
      x = x - 3
    
    probability = Lxs[x + benefit_term] / Lxs[x]
    initial = initial_expense/100
    claim = claim_expense /100
    prem = premium_expense / 100
    
    premium = assured_sum * (discountrate ^ benefit_term) * probability * (1 + claim) / (1-initial)
    
    return(premium)
  }

## Reserve
single_pure_eoy_reserve <-
  function(
    params
  ){
    # Deconstruct parameter
    age = params[1]
    benefit_term = params[2]
    assured_sum = params[3]
    interest = params[4]
    premium = params[5]
    claim_expense = params[6]
    initial_expense = params[7]
    premium_expense = params[8]
    age_group = params[9]
    bonus = params[10]
    inflation = params[11]
    
    interestPercentage = interest / 100
    discountrate = 1 / (1 + interestPercentage)
    
    x = age - 20 + 4
    if(!age_group)
      x = x - 3
    
    reserve = c()
    initial = initial_expense/100
    claim = claim_expense /100
    prem = premium_expense / 100
    
    for(term in 0:benefit_term) {
      power = benefit_term - term
      
      probability = Lxs[x + power]/Lxs[x]
      
      res = assured_sum * (discountrate ^ power) * probability * (1 + claim)
      
      if(power == benefit_term)
        res = res + ((initial-1)*premium)  
      
      
      reserve[term + 1] = res
    }
    
    return(reserve)
  }

#===================================

# STATE 2 SINGLE-TI-EOY ============

## Premium
single_term_eoy_premium <- 
  function(
    params
  ){
    # Deconstruct parameter
    age = params[1]
    benefit_term = params[2]
    assured_sum = params[3]
    interest = params[4]
    claim_expense = params[5]
    initial_expense = params[6]
    premium_expense = params[7]
    age_group = params[8]
    bonus = params[9]
    inflation = params[10]
    
    initial = initial_expense/100
    claim = claim_expense /100
    prem = premium_expense / 100
    bon = bonus / 100 * assured_sum
    
    premium1 = assured_sum * term_assurance(age, benefit_term, interest, age_group) + bon * increase_termass(age,benefit_term,interest,age_group)
    premium = premium1 * (1+claim) / ( 1 - initial)
    
    return(premium)
  }

## Joint Premium
joint_single_term_eoy_premium <- function(
    params
  ){
    # Deconstruct parameter
    age_x = params[1]
    age_y = params[2]
    benefit_term = params[3]
    assured_sum = params[4]
    interest = params[5]
    claim_expense = params[6]
    initial_expense = params[7]
    premium_expense = params[8]
    bonus = params[9]
    inflation = params[10]  
  
    initial = initial_expense/100
    claim = claim_expense /100
    bon = bonus / 100 * assured_sum
    joint_term = joint_termassur(age_x,age_y,benefit_term,interest)
    increase = joint_increase_termassur(age_x,age_y,benefit_term,interest)
    
    premium = (assured_sum * joint_term + bon * increase) * (1 + claim) / (1 - initial)
  }

## Reserve
single_term_eoy_reserve <- 
  function(
    params
  ){
    # Deconstruct parameter
    age = params[1]
    benefit_term = params[2]
    assured_sum = params[3]
    interest = params[4]
    premium = params[5]
    claim_expense = params[6]
    initial_expense = params[7]
    premium_expense = params[8]
    age_group = params[9]
    bonus = params[10]
    inflation = params[11]
    
    reserve = c()
    initial = initial_expense/100
    claim = claim_expense /100
    prem = premium_expense / 100
    bon = bonus / 100 * assured_sum
    
    for (term in 0:benefit_term) {
      TI = term_assurance(age + term, benefit_term - term, interest, age_group)
      increase = bon * increase_termass(age + term ,benefit_term - term,interest,age_group)
      if(term == 0) 
        reserve[1] = (assured_sum* TI + increase) * (1+claim) + (initial -  1) *premium  
      else
        reserve[term + 1] = (assured_sum * TI + increase) * (1 + claim)
    }
    
    return(reserve)
  }

## Joint Reserve
joint_single_term_eoy_reserve <- 
  function(
    params
  ){
    # Deconstruct parameter
    age_x = params[1]
    age_y = params[2]
    benefit_term = params[3]
    assured_sum = params[4]
    interest = params[5]
    premium = params[6]
    claim_expense = params[7]
    initial_expense = params[8]
    premium_expense = params[9]
    bonus = params[10]
    inflation = params[11]
    
    reserve = c()
    initial = initial_expense/100
    claim = claim_expense /100
    prem = premium_expense / 100
    bon = bonus / 100 * assured_sum
    
    for (term in 0:benefit_term) {
      joint_term = joint_termassur(age_x + term, age_y + term, benefit_term - term, interest)
      increase = joint_increase_termassur(age_x + term, age_y + term, benefit_term - term, interest)
      if (term == 0)
        reserve[1] = (assured_sum * joint_term + bon * increase)*(1+claim) + initial * premium - premium
      else
        reserve[term + 1] = (assured_sum * joint_term + bon * increase)*(1+claim)
    }
    
    return(reserve)
  }

#===================================

# STATE 3 SINGLE-TI-IOD ============

## Premium
single_term_iod_premium <- 
  function(
    params
  ){
    # Deconstruct parameter
    age = params[1]
    benefit_term = params[2]
    assured_sum = params[3]
    interest = params[4]
    claim_expense = params[5]
    initial_expense = params[6]
    premium_expense = params[7]
    age_group = params[8]
    bonus = params[9]
    inflation = params[10]
    
    interestPercentage = interest / 100
    premium = (1+interestPercentage) ^ 0.5 * single_term_eoy_premium(params)
    return(premium)
  }

## Joint Premium
joint_single_term_iod_premium <- function(
    params
  ){
    # Deconstruct parameter
    age_x = params[1]
    age_y = params[2]
    benefit_term = params[3]
    assured_sum = params[4]
    interest = params[5]
    claim_expense = params[6]
    initial_expense = params[7]
    premium_expense = params[8]
    bonus = params[9]
    inflation = params[10]
    
    claim = claim_expense / 100
    initial = initial_expense / 100
    bon = bonus / 100 * assured_sum
    int = interest / 100
    
    joint_term = joint_termassur_due(age_x,age_y,benefit_term,interest)
    increase = joint_increase_termassur(age_x,age_y,benefit_term,interest) *((1 + int) ^0.5)
    premium = (assured_sum * joint_term + bon * increase)*(1+claim) / (1 - initial)
    
    return(premium)
  }

## Reserve
single_term_iod_reserve <- 
  function(
    params
  ){
    # Deconstruct parameter
    age = params[1]
    benefit_term = params[2]
    assured_sum = params[3]
    interest = params[4]
    premium = params[5]
    claim_expense = params[6]
    initial_expense = params[7]
    premium_expense = params[8]
    age_group = params[9]
    bonus = params[10]
    inflation = params[11]
    
    interestPercentage = interest / 100
    reserve = c()
    
    eoy_premium = single_term_eoy_premium(c(age,benefit_term,assured_sum,interest,claim_expense,initial_expense,premium_expense,age_group,bonus))
    eoy_reserve = single_term_eoy_reserve(c(age,benefit_term,assured_sum,interest,eoy_premium,claim_expense,initial_expense,premium_expense,age_group,bonus))
    
    for(term in 0:benefit_term) {
      reserve [term + 1]= (1+interestPercentage)^0.5 * eoy_reserve[term + 1]
    }
    
    return(reserve)
  }

## Joint Reserve
joint_single_term_iod_reserve <- 
  function (
    params
  ){
    # Deconstruct parameter
    age_x = params[1]
    age_y = params[2]
    benefit_term = params[3]
    assured_sum = params[4]
    interest = params[5]
    premium = params[6]
    claim_expense = params[7]
    initial_expense = params[8]
    premium_expense = params[9]
    bonus = params[10]
    inflation = params[11]
    
    interestPercentage = interest / 100
    claim = claim_expense / 100
    initial = initial_expense / 100
    bon = bonus / 100 * assured_sum
    int = interest / 100
    reserve = c()
    
    for (term in 0:benefit_term){
      joint_term = joint_termassur_due(age_x+term, age_y + term, benefit_term - term, interest)
      joint_ann = joint_termannuity(age_x+term, age_y + term, benefit_term - term, interest)
      increase = ((1+int) ^ 0.5 )*joint_increase_termassur(age_x+term, age_y + term, benefit_term - term, interest)
      
      if (term == 0)
        reserve[1] = (assured_sum * joint_term + bon * increase)*(1+claim) + initial * premium - premium
      else
        reserve[term + 1] = (assured_sum * joint_term + bon * increase)*(1+claim) 
    }
    
    return(reserve)
  }

#===================================

# STATE 4 SINGLE-EA-EOY ============

## Premium
single_ea_eoy_premium <- 
  function(
    params
  ){
    # Deconstruct parameter
    age = params[1]
    benefit_term = params[2]
    assured_sum = params[3]
    interest = params[4]
    claim_expense = params[5]
    initial_expense = params[6]
    premium_expense = params[7]
    age_group = params[8]
    bonus = params[9]
    inflation = params[10]
    
    claim = claim_expense / 100
    initial = initial_expense / 100
    bon = bonus/100 * assured_sum
    
    if (bonus == 0)
      premium = single_pure_eoy_premium(params) + single_term_eoy_premium(params)
    else
      premium = (assured_sum*endowment_assurance(age, benefit_term, interest, age_group) + bon * increase_endowment(age,benefit_term,interest,age_group))* (1+ claim) / (1 - initial)
      
    return(premium)
    }


## Reserve
single_ea_eoy_reserve <- 
  function(
    params
  ){
    # Deconstruct parameter
    age = params[1]
    benefit_term = params[2]
    assured_sum = params[3]
    interest = params[4]
    premium = params[5]
    claim_expense = params[6]
    initial_expense = params[7]
    premium_expense = params[8]
    age_group = params[9]
    bonus = params[10]
    inflation = params[11]
    
    # if (bonus == 0) {
    #   term_premium = single_term_eoy_premium(c(age,benefit_term,assured_sum,interest,claim_expense,initial_expense,premium_expense,age_group,bonus))
    #   term_reserve = single_term_eoy_reserve(c(age,benefit_term,assured_sum,interest,term_premium,claim_expense,initial_expense,premium_expense,age_group,bonus))
    #   pure_premium = single_pure_eoy_premium(c(age,benefit_term,assured_sum,interest,claim_expense,initial_expense,premium_expense,age_group,bonus))
    #   pure_reserve = single_pure_eoy_reserve(c(age,benefit_term,assured_sum,interest,pure_premium,claim_expense,initial_expense,premium_expense,age_group,bonus))
    #   reserve = c()
    #   
    #   for (term in 0 : benefit_term) {
    #     reserve[term+1] = term_reserve[term+1] + pure_reserve[term+1]
    #   }
    # }
    
    initial = initial_expense / 100
    claim = claim_expense / 100
    bon = bonus / 100 * assured_sum
    
    reserve = c()
    
    for (term in 0:benefit_term){
      ass = endowment_assurance(age + term, benefit_term - term, interest, age_group)
      increase = increase_endowment(age + term, benefit_term - term, interest, age_group)
      
      if (term == 0)
        reserve[1] = (assured_sum*ass + bon*increase) * (1 + claim)  + premium*initial  - premium
      else
        reserve[term+1] = (assured_sum*ass + bon*increase) * (1 + claim)
    }
    return(reserve)
  }

#===================================

# STATE 5 SINGLE-EA-IOD ============

## Premium
single_ea_iod_premium <- 
  function(
    params  
  ){
    # Deconstruct parameter
    age = params[1]
    benefit_term = params[2]
    assured_sum = params[3]
    interest = params[4]
    claim_expense = params[5]
    initial_expense = params[6]
    premium_expense = params[7]
    age_group = params[8]
    bonus = params[9]
    inflation = params[10]
    
    # x = age - 20 + 4
    # interestPercentage = interest / 100
    # discount = 1/(1 + interestPercentage)
    # endowment_iod = 1 - log(1+interestPercentage)*(term_annuity_due(age,benefit_term,interest)-0.5+0.5*(discount^benefit_term)*Lxs[x+benefit_term]/Lxs[benefit_term])
    # premium = assured_sum * endowment_iod/term_annuity_due(age,benefit_term,interest )
    
    initial = initial_expense/100
    claim  =claim_expense/100
    bon = bonus/100 * assured_sum
    
    
    if (bonus == 0)
      premium = single_pure_eoy_premium(params) + single_term_iod_premium(params)
    else
      premium = (assured_sum * endowment_assurance_due(age, benefit_term, interest, age_group) + bon * increase_endowment_due(age, benefit_term, interest, age_group) ) * (1 + claim) / (1 - initial)
    return(premium)
  }

## Reserve
single_ea_iod_reserve <- 
  function(
    params  
  ){
    # Deconstruct parameter
    age = params[1]
    benefit_term = params[2]
    assured_sum = params[3]
    interest = params[4]
    premium = params[5]
    claim_expense = params[6]
    initial_expense = params[7]
    premium_expense = params[8]
    age_group = params[9]
    bonus = params[10]
    inflation = params[11]
    
    # term_premium = single_term_iod_premium(c(age,benefit_term,assured_sum,interest,claim_expense,initial_expense,premium_expense,age_group))
    # term_reserve = single_term_iod_reserve(c(age,benefit_term,assured_sum,interest, term_premium,claim_expense,initial_expense,premium_expense,age_group))
    # pure_premium = single_pure_iod_premium(c(age,benefit_term,assured_sum,interest, claim_expense,initial_expense,premium_expense,age_group))
    # pure_reserve = single_pure_iod_reserve (c(age,benefit_term,assured_sum,interest, pure_premium, claim_expense,initial_expense,premium_expense,age_group))
    # reserve = c()
    # 
    # for (term in 0: benefit_term ){
    #   reserve[term+1] = term_reserve[term+1] + pure_reserve[term+1]
    # }
    initial = initial_expense / 100
    claim = claim_expense / 100
    bon = bonus / 100 * assured_sum
    
    reserve = c()
    
    for (term in 0:benefit_term){
      ass = endowment_assurance_due(age + term, benefit_term - term, interest, age_group)
      increase = increase_endowment_due(age + term, benefit_term - term, interest, age_group)
      
      if (term == 0)
        reserve[1] = (assured_sum*ass + bon*increase) * (1 + claim)  + premium*initial  - premium
      else
        reserve[term+1] = (assured_sum*ass + bon*increase) * (1 + claim)
    }
    
    return(reserve)
  }

#===================================

# STATE 6 SINGLE-WHOLEASSUR-EOY ============

## Premium
single_wholeassur_eoyd_premium <- 
  function(
    params
  ){
    # Deconstruct parameter
    age = params[1]
    benefit_term = params[2]
    assured_sum = params[3]
    interest = params[4]
    claim_expense = params[5]
    initial_expense = params[6]
    premium_expense = params[7]
    age_group = params[8]
    bonus = params[9]
    inflation = params[10]
    
    initial = initial_expense / 100
    claim = claim_expense / 100
    bon = bonus / 100 * assured_sum
    
    premium = (assured_sum * wholeassurance(age, interest, age_group) + bon * increase_wholeass(age, interest, age_group))* ( 1 + claim) / (1 - initial) 
    return(premium)
  }

## Joint premium
joint_single_wholeassure_eoyd_premium <- 
  function(
    params
  ){
    # Deconstruct parameter
    age_x = params[1]
    age_y = params[2]
    benefit_term = params[3]
    assured_sum = params[4]
    interest = params[5]
    claim_expense = params[6]
    initial_expense = params[7]
    premium_expense = params[8]
    bonus = params[9]
    inflation = params[10]
    
    initial = initial_expense / 100
    claim = claim_expense / 100
    bon = bonus / 100 * assured_sum
    joint = joint_wholeassur(age_x,age_y,interest)
    increase = joint_increase_wholeassur(age_x,age_y,interest)
    
    premium = (assured_sum * joint + bon * increase) * (1+claim) / (1-initial)
    
    return(premium)
  }

## Reserve
single_wholeassur_eoyd_reserve <- 
  function(
    params
  ){
    # Deconstruct parameter
    age = params[1]
    benefit_term = params[2]
    assured_sum = params[3]
    interest = params[4]
    premium = params[5]
    claim_expense = params[6]
    initial_expense = params[7]
    premium_expense = params[8]
    age_group = params[9]
    bonus = params[10]
    inflation = params[11]
    
    initial = initial_expense / 100
    claim = claim_expense / 100
    bon = bonus/100 * assured_sum
    reserve = c()
    
    whole_prem = single_wholeassur_eoyd_premium(c(age,benefit_term,assured_sum,interest,claim_expense,initial_expense,premium_expense,age_group,bonus))
    for(term in 0 : (100-age)){
      whole_assurance = wholeassurance(age+term,interest,age_group)
      increase = increase_wholeass(age + term, interest, age_group)
      if (term==0)
        reserve[1] = (assured_sum* whole_assurance + bon * increase)*(1+claim) + initial * whole_prem - whole_prem
      else
        reserve[term + 1] = (assured_sum* whole_assurance + bon * increase)*(1+claim)
    }
    
    return(reserve)
  }

## Joint reserve
joint_single_wholeassur_eoyd_reserve <- 
  function(
    params
  ){
    # Deconstruct parameter
    age_x = params[1]
    age_y = params[2]
    benefit_term = params[3]
    assured_sum = params[4]
    interest = params[5]
    premium = params[6]
    claim_expense = params[7]
    initial_expense = params[8]
    premium_expense = params[9]
    bonus = params[10]
    inflation = params[11]
    
    initial = initial_expense / 100
    claim = claim_expense / 100
    bon = bonus/100 * assured_sum
    reserve = c()
    bound = max(age_x,age_y)
    
    for(term in 0:(100 - bound)){
      joint_whole = joint_wholeassur(age_x+term, age_y+term, interest )
      increase = joint_increase_wholeassur(age_x + term, age_y + term, interest)
      
      if (term == 0)
        reserve[1] = (assured_sum * joint_whole + bon * increase)*(1+claim) + initial * premium - premium
      else
        reserve[term+1] = (assured_sum * joint_whole + bon * increase)*(1+claim)
    }
    return(reserve)
  }

#===================================

# STATE 7 SINGLE-WHOLEASSUR-IOD ====

## Premium
single_wholeassur_iod_premium <- 
  function(
    params
  ){
    # Deconstruct parameter
    age = params[1]
    benefit_term = params[2]
    assured_sum = params[3]
    interest = params[4]
    claim_expense = params[5]
    initial_expense = params[6]
    premium_expense = params[7]
    age_group = params[8]
    bonus = params[9]
    inflation = params[10]
    
    claim = claim_expense / 100
    initial = initial_expense / 100
    bon = bonus / 100 * assured_sum
    interestPercentage = interest / 100
    
    premium = (assured_sum * wholeassurance_cont(age,interest,age_group) + bon * ((1+interestPercentage)^0.5)* increase_wholeass(age,interest, age_group)) *(1+claim)/(1-initial)
    return(premium)
  }

## Joint premium
joint_single_wholeassur_iod_premium <-
  function(
    params
  ){
    # Deconstruct parameter
    age_x = params[1]
    age_y = params[2]
    benefit_term = params[3]
    assured_sum = params[4]
    interest = params[5]
    claim_expense = params[6]
    initial_expense = params[7]
    premium_expense = params[8]
    bonus = params[9]
    inflation = params[10]
    
    claim = claim_expense / 100
    initial = initial_expense / 100
    bon = bonus / 100 * assured_sum
    int = interest / 100
    joint = joint_wholeassur_due(age_x,age_y,interest)
    increase = joint_increase_wholeassur(age_x,age_y,interest)*((1+int)^0.5)
    
    premium = (assured_sum * joint + bon * increase) * (1 + claim) / (1 - initial)
    
    return(premium)
  }

## Reserve
single_wholeassur_iod_reserve <- 
  function(
    params
  ){
    # Deconstruct parameter
    age = params[1]
    benefit_term = params[2]
    assured_sum = params[3]
    interest = params[4]
    premium = params[5]
    claim_expense = params[6]
    initial_expense = params[7]
    premium_expense = params[8]
    age_group = params[9]
    bonus = params[10]
    inflation = params[11]
    
    claim = claim_expense / 100
    initial = initial_expense / 100
    bon = bonus / 100 * assured_sum
    interestPercentage = interest / 100
    reserve = c()
    
    whole_prem = single_wholeassur_iod_premium(c(age,benefit_term,assured_sum,interest,claim_expense,initial_expense,premium_expense,age_group,bonus))
    for(term in 0 : (100-age)){
      whole_assurance = wholeassurance_cont(age+term,interest,age_group)
      increase = ((1 + interestPercentage)^0.5) * increase_wholeass(age + term, interest, age_group)
      if (term==0)
        reserve[1] = (assured_sum*whole_assurance + bon * increase)*(1+claim) + initial * whole_prem - whole_prem
      else
        reserve[term + 1] = (assured_sum*whole_assurance + bon * increase)*(1+claim)
    }

    
    return(reserve)
  }

## Joint reserve
joint_single_wholeassur_iod_reserve <- 
  function(
    params
  ){
    # Deconstruct parameter
    age_x = params[1]
    age_y = params[2]
    benefit_term = params[3]
    assured_sum = params[4]
    interest = params[5]
    premium = params[6]
    claim_expense = params[7]
    initial_expense = params[8]
    premium_expense = params[9]
    bonus = params[10]
    inflation = params[11]  
    
    prem = premium_expense/100
    claim = claim_expense / 100
    initial = initial_expense / 100
    bon = bonus / 100 * assured_sum
    int = interest / 100
    reserve = c()
    bound = max(age_x,age_y)
    
    for(term in 0:(100 - bound)){
      joint_whole = joint_wholeassur_due(age_x+term, age_y+term, interest)
      increase = joint_increase_wholeassur(age_x + term, age_y + term, interest) * ((1+int)^0.5)
      benefit_term = 100 - bound
      joint_ann = joint_termannuity(age_x+term, age_y+term, benefit_term-term, interest)
      
      if (term == 0)
        reserve[1] = (assured_sum*joint_whole + bon * increase) * (1+claim) + premium * initial - premium
      else
        reserve[term+1] = (assured_sum*joint_whole + bon * increase) * (1+claim) 
    }
    return(reserve)
  }

#===================================

# STATE 8 SINGLE-WAN-EOY ============

## Premium
single_wan_eoyd_premium <- 
  function(
    params
  ){
    # Deconstruct parameter
    age = params[1]
    benefit_term = params[2]
    assured_sum = params[3]
    interest = params[4]
    claim_expense = params[5]
    initial_expense = params[6]
    premium_expense = params[7]
    age_group = params[8]
    bonus = params[9]
    inflation = params[10]
    
    claim = claim_expense / 100
    initial = initial_expense / 100
    prem = premium_expense / 100
    inf = inflation / 100
    int = interest / 100
    new = (((1+int)/(1+inf)) - 1) * 100
    
    premium = assured_sum * (1/(1+inf))*(wholeannuity(age,new,age_group)-1) * (1+claim) / ( 1 - initial)
    return(premium)
  }

## Joint premium
joint_wan_eoyd_premium <- 
  function(
    params
  ){
    # Deconstruct parameter
    age_x = params[1]
    age_y = params[2]
    benefit_term = params[3]
    assured_sum = params[4]
    interest = params[5]
    claim_expense = params[6]
    initial_expense = params[7]
    premium_expense = params[8]
    bonus = params[9]
    inflation = params[10]
    
    int = interest / 100
    ann = claim_expense/100
    initial = initial_expense / 100
    inf = inflation / 100
    new = ((1+int)/(1+inf) -1 )* 100
    
    premium = assured_sum * (1/(1+inf)) * (joint_wholeannuity(age_x,age_y,new)-1)*(1+ann)/(1-initial)
    return(premium)
  }

## Reserve
single_wan_eoyd_reserve <- 
  function(
    params
  ){
    # Deconstruct parameter
    age = params[1]
    benefit_term = params[2]
    assured_sum = params[3]
    interest = params[4]
    premium = params[5]
    claim_expense = params[6]
    initial_expense = params[7]
    premium_expense = params[8]
    age_group = params[9]
    bonus = params[10]
    inflation = params[11]
    
    claim = claim_expense / 100
    initial = initial_expense / 100
    reserve = c()
    inf = inflation / 100
    int = interest / 100
    new = (((1+int)/(1+inf)) - 1) * 100
    discountrate = 1 / (1 + int)
    
    #whole_premium = single_wan_eoyd_premium(c(age,assured_sum,interest, claim_expense,initial_expense,premium_expense,age_group,inflation))
    
    
    for (term in 0 : (100 - age)){
      annuity = (1/(1+inf)) * (wholeannuity(age+term,new,age_group)-1)
      if (term == 0)
        reserve[1] = assured_sum*annuity*(1+claim) + premium*initial - premium
      else
        reserve[term + 1] = assured_sum* annuity *(1+claim) 
    }
    return(reserve)
  }

## Joint Reserve
joint_wan_eoyd_reserve <- 
  function(
    params
  ){
    # Deconstruct parameter
    age_x = params[1]
    age_y = params[2]
    benefit_term = params[3]
    assured_sum = params[4]
    interest = params[5]
    premium = params[6]
    claim_expense = params[7]
    initial_expense = params[8]
    premium_expense = params[9]
    bonus = params[10]
    inflation = params[11]  
    
    int = interest / 100
    ann = claim_expense/100
    initial = initial_expense / 100
    inf = inflation / 100
    new = ((1+int)/(1+inf) -1 )* 100
    reserve = c()
    bound = max(age_x,age_y)
    
    for (i in 0:(100 - bound)){
      joint_whole = (joint_wholeannuity(age_x + i ,age_y + i ,new)-1)
      
      if (i == 0)
        reserve[1] = assured_sum*joint_whole*(1/(1+inf))*(1+ann) + premium * initial - premium
      else
        reserve[i+1] = assured_sum*joint_whole*(1/(1+inf))*(1+ann)
    }
    return(reserve)
    
  }

#===================================

# STATE 9 SINGLE-TAN-EOY ============

## Premium
single_tan_eoyd_premium <- 
  function(
    params
  ){
    age = params[1]
    benefit_term = params[2]
    assured_sum = params[3]
    interest = params[4]
    claim_expense = params[5]
    initial_expense = params[6]
    premium_expense = params[7]
    age_group = params[8]
    bonus = params[9]
    inflation = params[10]
    
    x = age - 20 + 4
    if(!age_group)
      x = x - 3
    
    claim = claim_expense / 100
    initial = initial_expense / 100
    inf = inflation / 100
    int = interest / 100
    
    new = (((1+int)/(1+inf)) - 1) * 100
    
    prob = Lxs[x+1]/Lxs[x]
    
    disc = 1 / (1 + interest/100)
    
    premium = (assured_sum* (1/(1+inf)) * (prob * disc * term_annuity_due(age+1,benefit_term, new, age_group))*(1+claim)) / ( 1 - initial)
    return(premium)
  }

joint_tan_eoyd_premium <- 
  function(
    params
  ){
    # Deconstruct parameter
    age_x = params[1]
    age_y = params[2]
    benefit_term = params[3]
    assured_sum = params[4]
    interest = params[5]
    claim_expense = params[6]
    initial_expense = params[7]
    premium_expense = params[8]
    bonus = params[9]
    inflation = params[10]
    
    claim = claim_expense / 100
    initial = initial_expense / 100
    prem = premium_expense / 100
    inf = inflation / 100
    int = interest / 100
    new = (((1+int)/(1+inf)) - 1) * 100
    disc = 1 / (1+int)
    
    x = age_x - 20 + 4
    y = age_y - 20 + 1
    probx = Lxs[x+1]/ Lxs[x]
    proby = Lxs[y+1]/ Lxs[y]
    joint = probx * proby
    
    premium = assured_sum * (1/(1+inf)) * disc  * probx * proby * joint_termannuity(age_x+1,age_y+1,benefit_term, new) * (1+claim) / (1-initial)
    return(premium)
  }

## Reserve
single_tan_eoyd_reserve <- 
  function(
    params
  ){
    age = params[1]
    benefit_term = params[2]
    assured_sum = params[3]
    interest = params[4]
    premium = params[5]
    claim_expense = params[6]
    initial_expense = params[7]
    premium_expense = params[8]
    age_group = params[9]
    bonus = params[10]
    inflation = params[11]
    
    x = age - 20 + 4
    if(!age_group)
      x = x - 3
    
    claim = claim_expense / 100
    initial = initial_expense / 100
    inf = inflation / 100
    int = interest / 100
    
    new = (((1+int)/(1+inf)) - 1) * 100
    
    disc = 1 / (1 + interest/100)
    
    reserve = c()
    
    for (term in 0 : benefit_term){
      prob = Lxs[x+term+1] / Lxs[x+term]
      annuity = 1/(1+inf) * disc * prob * term_annuity_due(age+term+1, benefit_term - term, new, age_group)
      
      if ( term == 0)
        reserve[1] = assured_sum*annuity  * (1+claim) + initial * premium - premium
      else
        reserve[term + 1] = assured_sum* annuity * (1+claim) 
    }
    return(reserve)  
  }

## Joint Reserve
joint_tan_eoyd_reserve <- 
  function(
    params
  ){
    # Deconstruct parameter
    age_x = params[1]
    age_y = params[2]
    benefit_term = params[3]
    assured_sum = params[4]
    interest = params[5]
    premium = params[6]
    claim_expense = params[7]
    initial_expense = params[8]
    premium_expense = params[9]
    bonus = params[10]
    inflation = params[11]   
    
    claim = claim_expense / 100
    initial = initial_expense / 100
    inf = inflation / 100
    int = interest / 100
    disc = (1/(1+int))
    reserve = c()
    new = (((1+int)/(1+inf)) - 1) * 100
    
    x = age_x - 20 + 4
    y = age_y - 20 + 1
    
    for (term in 0:benefit_term){
      probx = Lxs[x+term+1] / Lxs[x+term]
      proby = Lxs[y+term+1] / Lxs[y+term]
      
      joint_term = disc * probx * proby * joint_termannuity(age_x+term+1,age_y+term+1,benefit_term-term,new)
      if (term == 0)
        reserve[1] = assured_sum * joint_term * (1/(1+inf)) * (1+claim) + premium * initial - premium
      else
        reserve[term + 1] = assured_sum * joint_term * (1/(1+inf)) * (1+claim)
    
     }
    
    return(reserve)
  }

#===================================

# STATE 10 LEVEL-PE-EOY ============

## Premium
level_pure_eoy_premium <- 
  function(
    params
  ){
    # Deconstruct parameter
    age = params[1]
    benefit_term = params[2]
    assured_sum = params[3]
    interest = params[4]   
    claim_expense = params[5]
    initial_expense = params[6]
    premium_expense = params[7]   
    age_group = params[8]
    bonus = params[9]
    inflation = params[10]
    
    interestRate = interest / 100
    discountrate = 1 / (1 + interestRate)
    
    x = age - 20 + 4
    if(!age_group)
      x = x - 3

    initial = initial_expense/ 100
    claim = claim_expense /100
    prem = premium_expense / 100
    
    ## --- MATURITY ---
    mat_probability= Lxs[x + benefit_term] / Lxs[x]
    maturity= assured_sum * (discountrate ^ benefit_term) * mat_probability * (1 + claim)
    
    ## ------
    premium = maturity  / (term_annuity_due(age,benefit_term, interest, age_group) - initial - prem*(term_annuity_due(age,benefit_term, interest, age_group) -1 ))
    
    return(premium)
  }


## Reserve
level_pure_eoy_reserve <- 
  function(
    params
  ){
    # Deconstruct parameter
    age = params[1]
    benefit_term = params[2]
    assured_sum = params[3]
    interest = params[4]
    premium = params[5]
    claim_expense = params[6]
    initial_expense = params[7]
    premium_expense = params[8]
    age_group = params[9]
    bonus = params[10]
    inflation = params[11]
    
    interestRate = interest / 100
    discountrate = 1 / (1 + interestRate)
    
    x = age - 20 + 4
    if(!age_group)
      x = x - 3
    
    reserve = c()
    initial = initial_expense/100
    claim = claim_expense /100
    prem = premium_expense / 100
    
    
    premium = level_pure_eoy_premium(c(age,benefit_term,assured_sum,interest,claim_expense,initial_expense,premium_expense,age_group))
    for(term in 0:benefit_term){
      prob = (discountrate ^ (benefit_term - term)) * (Lxs[x + benefit_term] / Lxs[x + term])
      
      annuity = term_annuity_due(age + term, benefit_term - term, interest, age_group)
      
      if (term==0)
        reserve[1] = assured_sum * prob * (1 + claim) + initial * premium + prem * premium * (annuity-1) - premium * annuity
      else
        reserve[term + 1] = assured_sum * prob * (1 + claim)  + prem * premium * (annuity) - premium * annuity
    }
    
    return(reserve)
  }

#===================================

# STATE 11 LEVEL-TI-EOY ============

## Premium
level_term_eoy_premium <-  
  function(
    params
  ){
    # Deconstruct parameter
    age = params[1]
    benefit_term = params[2]
    assured_sum = params[3]
    interest = params[4]
    claim_expense = params[5]
    initial_expense = params[6]
    premium_expense = params[7]
    age_group = params[8]
    bonus = params[9]
    inflation = params[10]
    
    initial = initial_expense/100
    claim = claim_expense /100
    prem = premium_expense / 100
    bon = bonus / 100 * assured_sum
    
    premium = (assured_sum * term_assurance(age, benefit_term, interest, age_group) + bon * increase_termass(age, benefit_term, interest, age_group)) * (1+claim) / (term_annuity_due(age,benefit_term, interest, age_group) - initial - prem* (term_annuity_due(age, benefit_term, interest, age_group)-1))
    
    return(premium)
  }

## Joint premium
joint_level_term_eoy_premium <- 
  function(
    params
  ){
    # Deconstruct parameter
    age_x = params[1]
    age_y = params[2]
    benefit_term = params[3]
    assured_sum = params[4]
    interest = params[5]
    claim_expense = params[6]
    initial_expense = params[7]
    premium_expense = params[8]
    bonus = params[9]
    inflation = params[10]
      
    initial = initial_expense/100
    claim = claim_expense /100
    prem = premium_expense / 100
    bon = bonus / 100 * assured_sum
    
    joint_term = joint_termassur(age_x,age_y,benefit_term,interest)
    increase = joint_increase_termassur(age_x,age_y,benefit_term,interest)
    joint_termann = joint_termannuity(age_x,age_y,benefit_term,interest)
    
    premium = (assured_sum * joint_term + bon * increase)*(1+claim) / (joint_termann - initial - prem*(joint_termann-1))
    return(premium)
  
  }

## Reserve
level_term_eoy_reserve <- 
  function(
    params
  ){
    # Deconstruct parameter
    age = params[1]
    benefit_term = params[2]
    assured_sum = params[3]
    interest = params[4]
    premium = params[5]
    claim_expense = params[6]
    initial_expense = params[7]
    premium_expense = params[8]
    age_group = params[9]
    bonus = params[10]
    inflation = params[11]
    
    reserve = c()
    initial = initial_expense/100
    claim = claim_expense /100
    prem = premium_expense / 100
    bon = bonus / 100 * assured_sum
    
    premium = level_term_eoy_premium(c(age,benefit_term,assured_sum,interest,claim_expense,initial_expense, premium_expense,age_group,bonus))
    for(term in 0:benefit_term) {
      termass = term_assurance(age+term, benefit_term - term, interest, age_group)
      annuity = term_annuity_due(age+term,benefit_term-term, interest, age_group) 
      increase = increase_termass(age + term, benefit_term - term, interest, age_group)
      if (term == 0)
        reserve[1] = (assured_sum * termass + bon * increase) * (1 + claim) + initial * premium + prem * (annuity -1) * premium - annuity * premium
      else
        reserve [term + 1]= (assured_sum * termass + bon * increase) * (1 + claim) + prem * annuity * premium - annuity* premium
    }
    
    return(reserve)
  }

## Joint Reserve
joint_level_term_eoy_reserve <- 
  function (
    params
  ){
    # Deconstruct parameter
    age_x = params[1]
    age_y = params[2]
    benefit_term = params[3]
    assured_sum = params[4]
    interest = params[5]
    premium = params[6]
    claim_expense = params[7]
    initial_expense = params[8]
    premium_expense = params[9]
    bonus = params[10]
    inflation = params[11]
    
    reserve = c()
    initial = initial_expense/100
    claim = claim_expense /100
    prem = premium_expense / 100
    bon = bonus / 100 * assured_sum
    
    for (term in 0:benefit_term){
      joint_term = joint_termassur(age_x + term, age_y + term, benefit_term - term, interest)
      joint_ann = joint_termannuity(age_x + term, age_y + term, benefit_term - term, interest)
      increase = joint_increase_termassur(age_x + term, age_y + term, benefit_term - term, interest)
      
      if (term == 0)
        reserve[1] = (assured_sum * joint_term + bon * increase)*(1+claim) + premium * initial  + premium * prem * (joint_ann - 1) - premium * joint_ann
      else
        reserve[term + 1] = (assured_sum * joint_term + bon * increase)*(1+claim) + premium * prem * (joint_ann) - premium * joint_ann
      
    }
    return(reserve)
  }

#===================================

# STATE 12 LEVEL-TI-IOD ============

## Premium
level_term_iod_premium <- 
  function(
    params
  ){
    # Deconstruct parameter
    age = params[1]
    benefit_term = params[2]
    assured_sum = params[3]
    interest = params[4]
    claim_expense = params[5]
    initial_expense = params[6]
    premium_expense = params[7]
    age_group = params[8]
    bonus = params[9]
    inflation = params[10]
    
    interestPercentage = interest / 100
    claim = claim_expense / 100
    initial = initial_expense / 100
    prem = premium_expense / 100
    bon = bonus / 100 * assured_sum
    term = (1 + interestPercentage)^0.5 * term_assurance(age, benefit_term, interest, age_group)
    increase = (1 + interestPercentage)^0.5 *increase_termass(age, benefit_term, interest, age_group)
    
    if (bonus == 0)
      premium = (1+interestPercentage)^0.5 * level_term_eoy_premium(params)
    else
      premium = (assured_sum * term + bon * increase) * (1+claim) / (term_annuity_due(age,benefit_term, interest, age_group) - initial - prem* (term_annuity_due(age, benefit_term, interest, age_group)-1))
    
    return(premium)
  }

## Joint premium
joint_level_term_iod_premium <- 
  function(
    params
  ){
    # Deconstruct parameter
    age_x = params[1]
    age_y = params[2]
    benefit_term = params[3]
    assured_sum = params[4]
    interest = params[5]
    claim_expense = params[6]
    initial_expense = params[7]
    premium_expense = params[8]
    bonus = params[9]
    inflation = params[10]
    
    initial = initial_expense/100
    claim = claim_expense /100
    prem = premium_expense / 100
    bon = bonus / 100 * assured_sum
    int = interest/100
    
    joint_term = joint_termassur_due(age_x,age_y,benefit_term,interest)
    increase = ((1+int)^0.5)*joint_increase_termassur(age_x,age_y,benefit_term,interest)
    joint_termann = joint_termannuity(age_x,age_y,benefit_term,interest)
    
    premium = (assured_sum*joint_term + bon * increase) * (1 + claim) / (joint_termann - initial - prem*(joint_termann-1))
    return(premium)
  }

## Reserve
level_term_iod_reserve <- 
  function(
    params
  ){
    # Deconstruct parameter
    age = params[1]
    benefit_term = params[2]
    assured_sum = params[3]
    interest = params[4]
    premium = params[5]
    claim_expense = params[6]
    initial_expense = params[7]
    premium_expense = params[8]
    age_group = params[9]
    bonus = params[10]
    inflation = params[11]
    
    # eoy_premium = level_term_eoy_premium(c(age,benefit_term,assured_sum,interest,claim_expense,initial_expense,premium_expense,age_group))
    # eoy_reserve = level_term_eoy_reserve(c(age,benefit_term,assured_sum,interest,eoy_premium,claim_expense,initial_expense,premium_expense,age_group))
    #  
    # for (term in 0 : benefit_term) {
    #   reserve[term +1] = (1+interestPercentage)^0.5 * eoy_reserve[term+1]
    # }
    
    reserve = c()
    interestPercentage = interest / 100
    initial = initial_expense/100
    claim = claim_expense /100
    prem = premium_expense / 100
    bon = bonus / 100 * assured_sum
    
    premium = level_term_iod_premium(c(age,benefit_term,assured_sum,interest,claim_expense,initial_expense, premium_expense,age_group,bonus))
    for(term in 0:benefit_term) {
      new_term = ((1 + interestPercentage) ^ 0.5) * term_assurance(age+term, benefit_term - term, interest, age_group)
      annuity = term_annuity_due(age+term,benefit_term-term, interest, age_group) 
      increase = ((1 + interestPercentage) ^ 0.5) * increase_termass(age + term, benefit_term - term, interest, age_group)
     
       if (term == 0)
        reserve[1] = (assured_sum * new_term + bon * increase) * (1 + claim) + initial * premium + prem * (annuity -1) * premium - annuity * premium
      else
        reserve [term + 1]= (assured_sum * new_term + bon * increase) * (1 + claim) + prem * annuity * premium - annuity* premium
    }
    return(reserve)
  }

## Joint Reserve
joint_level_term_iod_reserve <- 
  function (
    params
  ){
    # Deconstruct parameter
    age_x = params[1]
    age_y = params[2]
    benefit_term = params[3]
    assured_sum = params[4]
    interest = params[5]
    premium = params[6]
    claim_expense = params[7]
    initial_expense = params[8]
    premium_expense = params[9]
    bonus = params[10]
    inflation = params[11]
    
    reserve = c()
    int = interest / 100
    initial = initial_expense/100
    claim = claim_expense /100
    prem = premium_expense / 100
    bon = bonus / 100 * assured_sum
    
    for (term in 0:benefit_term){
      joint_term = joint_termassur_due(age_x + term, age_y + term, benefit_term -term, interest)
      joint_ann = joint_termannuity(age_x + term, age_y + term, benefit_term -term, interest)
      increase = ((1+int)^0.5) * joint_increase_termassur(age_x + term, age_y + term, benefit_term -term, interest)
      
      if (term == 0)
        reserve[1] = (assured_sum * joint_term + bon * increase) * (1 + claim) + initial * premium + premium * prem * (joint_ann -1 ) - premium * joint_ann
      else
        reserve[term + 1] = (assured_sum * joint_term + bon * increase) * (1 + claim) + premium * prem * joint_ann - premium * joint_ann
    }
    
    return(reserve)
  }

#===================================

# STATE 13 LEVEL-EA-EOY ============

## Premium
level_ea_eoy_premium <- 
  function(
    params  
  ){
    # Deconstruct parameter
    age = params[1]
    benefit_term = params[2]
    assured_sum = params[3]
    interest = params[4]
    claim_expense = params[5]
    initial_expense = params[6]
    premium_expense = params[7]
    age_group = params[8]
    bonus = params[9]
    inflation = params[10]
    
    initial = initial_expense / 100
    claim = claim_expense / 100
    prem = premium_expense / 100
    bon = bonus / 100 * assured_sum
    
    
    if (bonus == 0)
      premium = level_pure_eoy_premium(params) + level_term_eoy_premium(params)
    else
      premium = (assured_sum * endowment_assurance(age, benefit_term, interest, age_group) + bon * increase_endowment(age, benefit_term, interest, age_group) )* (1 + claim)/ (term_annuity_due(age, benefit_term, interest, age_group) - initial - prem*(term_annuity_due(age, benefit_term, interest, age_group)-1))
    return(premium)
  }


## Reserve
level_ea_eoy_reserve <- 
  function(
    params  
  ){
    # Deconstruct parameter
    age = params[1]
    benefit_term = params[2]
    assured_sum = params[3]
    interest = params[4]
    premium = params[5]
    claim_expense = params[6]
    initial_expense = params[7]
    premium_expense = params[8]
    age_group = params[9]
    bonus = params[10]
    inflation = params[11]
    
    # term_premium = level_term_eoy_premium(c(age,benefit_term,assured_sum,interest,claim_expense,initial_expense,premium_expense,age_group))
    # term_reserve = level_term_eoy_reserve(c(age,benefit_term,assured_sum,interest,term_premium,claim_expense,initial_expense,premium_expense,age_group))
    # pure_premium = level_pure_eoy_premium(c(age,benefit_term,assured_sum,interest,claim_expense,initial_expense,premium_expense,age_group))
    # pure_reserve = level_pure_eoy_reserve(c(age,benefit_term,assured_sum,interest,pure_premium,claim_expense,initial_expense,premium_expense,age_group))
    # reserve = c()
    # 
    # for (term in 0 : benefit_term) {
    #   reserve[term+1] = term_reserve[term+1] + pure_reserve[term+1]
    # }
    
    initial= initial_expense/100
    claim =claim_expense/100
    prem = premium_expense/100
    bon = bonus/100 * assured_sum
    
    reserve = c()
    
    for (term in 0:benefit_term){
      endowment = endowment_assurance(age + term, benefit_term - term, interest, age_group)
      annuity = term_annuity_due(age + term, benefit_term - term, interest, age_group)
      increase = increase_endowment(age + term, benefit_term - term, interest, age_group)
      
      if (term == 0) 
        reserve[1] = (assured_sum * endowment + bon * increase) * (1 + claim) + premium* initial + premium * prem*(annuity-1) - premium*annuity
      else 
        reserve[term+1] = (assured_sum * endowment + bon * increase) * (1 + claim) + premium * prem * annuity - premium*annuity
      
    }
    
    return(reserve)
  }


#===================================

# STATE 14 LEVEL-EA-IOD ============

## Premium
level_ea_iod_premium <- 
  function(
    params  
  ){
    # Deconstruct parameter
    age = params[1]
    benefit_term = params[2]
    assured_sum = params[3]
    interest = params[4]
    claim_expense = params[5]
    initial_expense = params[6]
    premium_expense = params[7]
    age_group = params[8]
    bonus = params[9]
    inflation = params[10]
    
    initial = initial_expense/100
    claim  =claim_expense/100
    bon = bonus/100 * assured_sum
    prem = premium_expense / 100
    
    if (bonus == 0)
      premium = level_pure_eoy_premium(params) + level_term_iod_premium(params)
    else
      premium = (assured_sum * endowment_assurance_due(age, benefit_term, interest, age_group) + bon * increase_endowment_due(age, benefit_term, interest, age_group) ) * (1 + claim) / (term_annuity_due(age, benefit_term, interest, age_group) - initial - prem*(term_annuity_due(age, benefit_term, interest, age_group)-1))
    return(premium)
  }


## Reserve
level_ea_iod_reserve <- 
  function(
    params
  ){
    # Deconstruct parameter
    age = params[1]
    benefit_term = params[2]
    assured_sum = params[3]
    interest = params[4]
    premium = params[5]
    claim_expense = params[6]
    initial_expense = params[7]
    premium_expense = params[8]
    age_group = params[9]
    bonus = params[10]
    inflation = params[11]
    
    # term_premium = level_term_iod_premium(c(age,benefit_term,assured_sum,interest, claim_expense,initial_expense,premium_expense,age_group))
    # term_reserve = level_term_iod_reserve(c(age,benefit_term,assured_sum,interest,term_premium, claim_expense,initial_expense,premium_expense,age_group))
    # pure_premium = level_pure_iod_premium(c(age,benefit_term,assured_sum,interest, claim_expense,initial_expense,premium_expense,age_group))
    # pure_reserve = level_pure_iod_reserve(c(age,benefit_term,assured_sum,interest,pure_premium, claim_expense,initial_expense,premium_expense,age_group))
    # reserve = c()
    # 
    # for(term in 0: benefit_term){
    #   reserve[term + 1] = term_reserve[term+1] + pure_reserve[term+1]
    # }
    
    initial= initial_expense/100
    claim =claim_expense/100
    prem = premium_expense/100
    bon = bonus/100 * assured_sum
    
    reserve = c()
    
    for (term in 0:benefit_term){
      endowment = endowment_assurance_due(age + term, benefit_term - term, interest, age_group)
      annuity = term_annuity_due(age + term, benefit_term - term, interest, age_group)
      increase = increase_endowment_due(age + term, benefit_term - term, interest, age_group)
      
      if (term == 0) 
        reserve[1] = (assured_sum * endowment + bon * increase) * (1 + claim) + premium* initial + premium * prem*(annuity-1) - premium*annuity
      else 
        reserve[term+1] = (assured_sum * endowment + bon * increase) * (1 + claim) + premium * prem * annuity - premium*annuity
      
    }
    
    return(reserve)
  }

#===================================

# STATE 15 LEVEL-WHOLEASSURE-EOY ============

## Premium
level_wholeassur_eoyd_premium <- 
  function(
    params
  ){
    # Deconstruct parameter
    age = params[1]
    benefit_term = params[2]
    assured_sum = params[3]
    interest = params[4]
    claim_expense = params[5]
    initial_expense = params[6]
    premium_expense = params[7]
    age_group = params[8]
    bonus = params[9]
    inflation = params[10]
    
    claim = claim_expense / 100
    initial = initial_expense / 100
    prem = premium_expense / 100
    benefit_term = 100 - age
    term = term_annuity_due(age,benefit_term,interest,age_group)
    bon = bonus / 100 * assured_sum
    
    premium = (assured_sum * wholeassurance(age,interest,age_group) + bon * increase_wholeass(age, interest, age_group))* (1+claim) / (term - initial - (prem * (term-1))) 
    
    return(premium)
  }

## Joint premium
joint_level_wholeassur_eoyd_premium <- 
  function(
    params
  ){
    # Deconstruct parameter
    age_x = params[1]
    age_y = params[2]
    benefit_term = params[3]
    assured_sum = params[4]
    interest = params[5]
    claim_expense = params[6]
    initial_expense = params[7]
    premium_expense = params[8]
    bonus = params[9]
    inflation = params[10]
      
    initial = initial_expense / 100
    claim = claim_expense / 100
    prem = premium_expense / 100
    bon = bonus / 100 * assured_sum
    bound = max(age_x,age_y)
    
    term = 100 - bound
    increase = joint_increase_wholeassur(age_x,age_y,interest)
    joint_term = joint_termannuity(age_x,age_y,term,interest)
    joint_whole = joint_wholeassur(age_x,age_y, interest)
    
    
    premium = (assured_sum * joint_whole + bon * increase) * (1+ claim) / (joint_term - initial - prem*(joint_term - 1))
    return(premium)
    
  }

## Reserve
level_wholeassur_eoyd_reserve <- 
  function(
    params
  ){
    # Deconstruct parameter
    age = params[1]
    benefit_term = params[2]
    assured_sum = params[3]
    interest = params[4]
    premium = params[5]
    claim_expense = params[6]
    initial_expense = params[7]
    premium_expense = params[8]
    age_group = params[9]
    bonus = params[10]
    inflation = params[11]
    
    initial = initial_expense / 100
    claim = claim_expense / 100
    prem = premium_expense / 100
    reserve = c()
    bon = bonus / 100 * assured_sum
    
    #whole_prem = level_wholeassur_eoyd_premium(c(age,benefit_term,assured_sum,interest,claim_expense,initial_expense,premium_expense,age_group))
    
    for(term in 0 : (100-age)){
      benefit_term = 100 - age
      whole_assurance = wholeassurance(age+term,interest,age_group)
      annuity = term_annuity_due(age+term, benefit_term-term, interest, age_group)
      increase = increase_wholeass(age + term, interest, age_group)
      if (term==0)
        reserve[1] = (assured_sum*whole_assurance + bon*increase) *(1+claim) + initial * premium + prem* (annuity - 1) * premium - premium * annuity
      else
        reserve[term + 1] =(assured_sum*whole_assurance + bon*increase) *(1+claim) +  prem*  annuity  * premium - premium * annuity
    }
    
    return(reserve)
  }

## Joint reserve
joint_level_wholeassur_eoyd_reserve <- 
  function(
    params
  ){
    # Deconstruct parameter
    age_x = params[1]
    age_y = params[2]
    benefit_term = params[3]
    assured_sum = params[4]
    interest = params[5]
    premium = params[6]
    claim_expense = params[7]
    initial_expense = params[8]
    premium_expense = params[9]
    bonus = params[10]
    inflation = params[11]       
      
    initial = initial_expense / 100
    claim = claim_expense / 100
    prem = premium_expense / 100
    reserve = c()
    bon = bonus / 100 * assured_sum
    bound = max(age_x,age_y)
    
    for (term in 0:(100 - bound)){
      joint_whole = joint_wholeassur_due(age_x + term ,age_y + term,interest)
      increase = joint_increase_wholeassur(age_x + term, age_y + term, interest)
      benefit_term = 100 - bound
      joint_ann = joint_termannuity(age_x + term , age_y + term, benefit_term - term, interest)
      
      if (term == 0)
        reserve[1] = (assured_sum*joint_whole + bon * increase)*(1+claim) + premium * initial + premium * prem * (joint_ann -1) - premium * joint_ann
      else
        reserve[term+1] = (assured_sum*joint_whole + bon * increase)*(1+claim) + premium * prem * joint_ann - premium * joint_ann
    }
    return(reserve)
  }
  
#===================================

# STATE 16 LEVEL-WHOLEASSURE-IOD ====

## Premium
level_wholeassur_iod_premium <- 
  function(
    params
  ){
    # Deconstruct parameter
    age = params[1]
    benefit_term = params[2]
    assured_sum = params[3]
    interest = params[4]
    claim_expense = params[5]
    initial_expense = params[6]
    premium_expense = params[7]
    age_group = params[8]
    bonus = params[9]
    inflation = params[10]
    
    claim = claim_expense / 100
    initial = initial_expense / 100
    prem = premium_expense / 100
    bon = bonus / 100 * assured_sum
    
    benefit_term = 100 - age
    term = term_annuity_due(age, benefit_term, interest, age_group)
    increase = ((1 + interest/100)^0.5)*increase_wholeass(age,interest, age_group)
    premium = (assured_sum * wholeassurance_cont(age,interest,age_group) + bon * increase) * (1+claim) / (term - initial - (prem * (term-1))) 
    
    return(premium)
  }

joint_level_wholeassur_iod_premium <- 
  function(
    params
  ){
    # Deconstruct parameter
    age_x = params[1]
    age_y = params[2]
    benefit_term = params[3]
    assured_sum = params[4]
    interest = params[5]
    claim_expense = params[6]
    initial_expense = params[7]
    premium_expense = params[8]
    bonus = params[9]
    inflation = params[10]
    
    claim = claim_expense / 100
    initial = initial_expense / 100
    prem = premium_expense / 100
    bon = bonus / 100 * assured_sum
    bound = max(age_x,age_y)
    int = interest/ 100
    
    term = 100 - bound
    increase = joint_increase_wholeassur(age_x,age_y,interest) * ((1+int)^0.5)
    joint_whole = joint_wholeassur_due(age_x,age_y,interest)
    joint_term = joint_termannuity(age_x,age_y,term,interest)
    
    premium = (assured_sum * joint_whole + bon * increase)*(1+claim) / (joint_term - initial - prem*(joint_term-1))
    return(premium)
  }

## Reserve
level_wholeassur_iod_reserve <- 
  function(
    params
  ){
    # Deconstruct parameter
    age = params[1]
    benefit_term = params[2]
    assured_sum = params[3]
    interest = params[4]
    premium = params[5]
    claim_expense = params[6]
    initial_expense = params[7]
    premium_expense = params[8]
    age_group = params[9]
    bonus = params[10]
    inflation = params[11]
    
    initial = initial_expense / 100
    claim = claim_expense / 100
    prem = premium_expense / 100
    reserve = c()
    bon = bonus / 100 * assured_sum
    
    #whole_prem = level_wholeassur_iod_premium(c(age,benefit_term,assured_sum,interest,claim_expense,initial_expense,premium_expense,age_group))
    for(term in 0 : (100-age)){
      benefit_term = 100 - age
      whole_assurance = wholeassurance_cont(age+term,interest,age_group)
      annuity = term_annuity_due(age+term, benefit_term-term, interest, age_group)
      increase = ((1 + interest/100)^0.5)*increase_wholeass(age + term,interest, age_group)
      if (term==0)
        reserve[1] = (assured_sum*whole_assurance + bon * increase) *(1+claim) + initial * premium + prem* (annuity - 1) * premium- premium * annuity
      else
        reserve[term + 1] = (assured_sum*whole_assurance + bon * increase) *(1+claim) +  prem* annuity * premium - premium * annuity
    }
    
    return(reserve)
  }

## Joint Reserve
joint_level_wholeassur_iod_reserve <- 
  function(
    params
  ){
    # Deconstruct parameter
    age_x = params[1]
    age_y = params[2]
    benefit_term = params[3]
    assured_sum = params[4]
    interest = params[5]
    premium = params[6]
    claim_expense = params[7]
    initial_expense = params[8]
    premium_expense = params[9]
    bonus = params[10]
    inflation = params[11] 
    
    reserve = c()
    claim = claim_expense / 100
    initial = initial_expense / 100
    prem = premium_expense / 100
    bon = bonus / 100 * assured_sum
    bound = max(age_x,age_y)
    int = interest/ 100
    
    
    for(term in 0:(100 - bound)){
      benefit_term = 100 - bound
      joint_whole = joint_wholeassur_due(age_x + term, age_y + term, interest)
      increase = ((1+int)^0.5) *joint_increase_wholeassur(age_x + term, age_y + term, interest)
      joint_ann = joint_termannuity(age_x + term, age_y + term, benefit_term - term, interest)
      
      if (term == 0)
        reserve[1] = (assured_sum * joint_whole + bon * increase) * (1 + claim) + premium * initial + prem * premium * (joint_ann -1) - premium * joint_ann
      else
        reserve[term + 1] = (assured_sum * joint_whole + bon * increase) * (1 + claim) + prem * premium * joint_ann  - premium * joint_ann
    }
    return(reserve)
  }

#===================================

# UI Code ==========================

ui <- fluidPage(
  # CSS Code
  tags$head(
    tags$style(
      HTML(
        "
        @import url('https://fonts.googleapis.com/css2?family=Lobster&display=swap');
        
        body {
          background: rgb(238,174,202);
          background: radial-gradient(circle, rgba(238,174,202,1) 0%, rgba(148,187,233,1) 100%);
          max-width: 1000px;
          margin: auto;
        }
        
        #title {
          text-align: center;
          font-family: 'Lobster', cursive;
          color: white;  
          text-shadow: rgb(132, 24, 217) 3px 0px 0px, rgb(132, 24, 217) 2.83487px 0.981584px 0px, rgb(132, 24, 217) 2.35766px 1.85511px 0px, rgb(132, 24, 217) 1.62091px 2.52441px 0px, rgb(132, 24, 217) 0.705713px 2.91581px 0px, rgb(132, 24, 217) -0.287171px 2.98622px 0px, rgb(132, 24, 217) -1.24844px 2.72789px 0px, rgb(132, 24, 217) -2.07227px 2.16926px 0px, rgb(132, 24, 217) -2.66798px 1.37182px 0px, rgb(132, 24, 217) -2.96998px 0.42336px 0px, rgb(132, 24, 217) -2.94502px -0.571704px 0px, rgb(132, 24, 217) -2.59586px -1.50383px 0px, rgb(132, 24, 217) -1.96093px -2.27041px 0px, rgb(132, 24, 217) -1.11013px -2.78704px 0px, rgb(132, 24, 217) -0.137119px -2.99686px 0px, rgb(132, 24, 217) 0.850987px -2.87677px 0px, rgb(132, 24, 217) 1.74541px -2.43999px 0px, rgb(132, 24, 217) 2.44769px -1.73459px 0px, rgb(132, 24, 217) 2.88051px -0.838247px 0px;
        }
        
        #textHead {
          text-align: center;
          font-family: 'Lobster', cursive;
          color: white;
          text-shadow: rgb(132, 24, 217) 3px 0px 0px, rgb(132, 24, 217) 2.83487px 0.981584px 0px, rgb(132, 24, 217) 2.35766px 1.85511px 0px, rgb(132, 24, 217) 1.62091px 2.52441px 0px, rgb(132, 24, 217) 0.705713px 2.91581px 0px, rgb(132, 24, 217) -0.287171px 2.98622px 0px, rgb(132, 24, 217) -1.24844px 2.72789px 0px, rgb(132, 24, 217) -2.07227px 2.16926px 0px, rgb(132, 24, 217) -2.66798px 1.37182px 0px, rgb(132, 24, 217) -2.96998px 0.42336px 0px, rgb(132, 24, 217) -2.94502px -0.571704px 0px, rgb(132, 24, 217) -2.59586px -1.50383px 0px, rgb(132, 24, 217) -1.96093px -2.27041px 0px, rgb(132, 24, 217) -1.11013px -2.78704px 0px, rgb(132, 24, 217) -0.137119px -2.99686px 0px, rgb(132, 24, 217) 0.850987px -2.87677px 0px, rgb(132, 24, 217) 1.74541px -2.43999px 0px, rgb(132, 24, 217) 2.44769px -1.73459px 0px, rgb(132, 24, 217) 2.88051px -0.838247px 0px;
        }
    
        #text {
          text-align: center;
          font-size: 16px;
          font-weight: bold;
          box-shadow: rgba(0, 0, 0, 0.25) 0px 54px 55px, rgba(0, 0, 0, 0.12) 0px -12px 30px, rgba(0, 0, 0, 0.12) 0px 4px 6px, rgba(0, 0, 0, 0.17) 0px 12px 13px, rgba(0, 0, 0, 0.09) 0px -3px 5px;          
          color: black;
          height: 35px;
          width: 125px;
          line-height: 35px;
          margin: 0 auto;
        }
        
        .row {
          color: white;
        }
        
        .col-sm-2 .form-group.shiny-input-container {
          display: flex;
          flex-direction: column;
          justify-content: center;
          align-items: center;
          margin: 0 auto;
        }
        
        .col-sm-2 .form-group.shiny-input-container > div {
          margin: 0 !important;
          padding: 0 !important;          
        }
        
        .noUi-target {
          background: #facfff;
          margin: 15px auto 20px !important;
        }
        
        .noUi-connect {
          background: #b176bb;
        }
        
        .noUi-handle {
          background: #f995e8;
        }
        
        pre.shiny-text-output {
          word-wrap: normal;
          text-align: center;
          border: none;
          width: 50px;
          margin: 0 auto;
        }
        
        .shiny-split-layout:nth-child(2) {
          height: 109px
        }
        
        #type {
          width: 100px;
        }
        
        #method {
          margin-left: 40px;
          width: 100px;
        }
        
        .form-group.shiny-input-container.shiny-input-container-inline {
          margin-bottom: 6px;
        }
        
        table {
          margin: 20px auto;
        }
        "
      )
    )
  ),
  
  # Enable animations and hide/show elements
  useShinyjs(),
  
  # Pure HTML for title
  HTML("<h1 id='title'>Insurance Benefit Valuation Tool</h2>"),
  
  # Plot
  div(
    style = "position: relative",
    withSpinner(
      plotOutput(outputId = "line", hover = hoverOpts("plot_hover", delay = 100, delayType = "debounce")),
      color = "#CC99FF",
      size = 2
    ),
    uiOutput("hover_info")
  ),
  
  # Empty space
  hr(),  
  
  splitLayout(
    style = "height: 100%",
    cellArgs = list(style = "height: 109px"),
    column(
      3,
      tags$b(
        "Insurance Product:"
      ),
      actionButton(
        inputId = "type",
        label = "Assurance",
        icon = icon("file-contract")
      ),
      hr(
        style = "width: 225px"
      ),
      tags$b(
        "Life Product:"
      ),        
      actionButton(
        inputId = "method",
        label = "Single",
        icon = icon("user")
      )
    ),
    column(
      4,
      # Premium (Text) 
      tags$div(
        class = "preium",
        HTML("<h3 id='textHead'>Premium:</h2>"),
        textOutput(outputId = "text")
      )      
    )
  ),
  
  # Empty space
  hr(),
  
  tabsetPanel(
    tabPanel(
      "Inputs",
      icon = icon("keyboard"),
      column(
        3,
        selectInput(
          inputId = "age_group",
          label = "Age Group",
          choices = c(
            "X" = 1,
            "Y" = 0
          ),
          selected = 1
        ),
        selectInput(
          inputId = "insurance_benefit",
          label = "Insurance Benefit",
          choices = c(
            "Pure endowment" = "pure_endowment",
            "Term insurance" = "term_insurance",
            "Whole assurance" = "whole_assurance",
            "Endowment assurance" = "endowment"
          ),
          selected = "pure_endowment"
        ),      
        selectInput(
          inputId = "insurance_benefit_payment",
          label = "Insurance Benefit Payment",
          choices = c(
            "End of year of death" = "end_of_year_of_death",
            "Immediately on death" = "immediately_on_death"
          ),
          selected = "end_of_year_of_death"
        ),        
        radioButtons(
          inputId = "payment",
          label = "Premium Payment",
          choices = c(
            "Single" = "single",
            "Level" = "level"
          ),
          selected = "level",
          inline = TRUE
        ),
        hidden(
          noUiSliderInput(
            inputId = "age_x", label = "Age (X)",
            min = 20, max = 100, step = 1,
            value = 30, margin = 100,
            orientation = "horizontal",
            width = "200px", height = "20px",
            inline = TRUE
          )
        ),
        hidden(
          noUiSliderInput(
            inputId = "age_y", label = "Age (Y)",
            min = 20, max = 100, step = 1,
            value = 30, margin = 100,
            orientation = "horizontal",
            width = "200px", height = "20px",
            inline = TRUE
          )
        )
      ),
      column(
        3,
        numericInput(
          inputId = "assured_sum",
          label = "Assured Sum",
          value = 1000
        ),
        numericInput(
          inputId = "bonus",
          label = "Bonuses Rate (%)",
          value = 0,
          min = 0,
          max = 50
        ), 
        numericInput(
          inputId = "inflation",
          label = "Inflation Rate (%)",
          value = 0,
          min = 0,
          max = 50
        ),         
        numericInput(
          inputId = "claim_expense",
          label = "Claim Expenses (% of Benefit Amount)",
          value = 0,
          min = 0,
          max = 50
        ),
        numericInput(
          inputId = "initial_expense",
          label = "Initial Expense (% of Gross Premium)",
          value = 0,
          min = 0,
          max = 50
        ),
        numericInput(
          inputId = "premium_expense",
          label = "Premium Expense (% of Gross Premium)",
          value = 0,
          min = 0,
          max = 50
        )
      ),
      column(
        2,
        noUiSliderInput(
          inputId = "age", label = "Age",
          min = 20, max = 100, step = 1,
          value = 30, margin = 100,
          orientation = "vertical",
          width = "300px", height = "200px"
        ),
        verbatimTextOutput(outputId = "max_age")
      ),
      column(
        2, 
        noUiSliderInput(
          inputId = "interest", label = "Interest Rate",
          min = 0, max = 50, step = 1,
          value = 4, margin = 100,
          orientation = "vertical",
          width = "300px", height = "200px"
        ),
        verbatimTextOutput(outputId = "max_interest")
      ),
      column(
        2, 
        noUiSliderInput(
          inputId = "benefit_term", label = "Benefit Term",
          min = 1, max = 50, step = 1,
          value = 10, margin = 100,
          orientation = "vertical",
          width = "300px", height = "200px"
        ),
        verbatimTextOutput(outputId = "max_term")
      )
    ),
    tabPanel(
      "Overview",
      icon = icon("eye"),  
      withSpinner(
        tableOutput('overview_type'),
        color = "#CC99FF",
        size = 2
      ),
      withSpinner(
        tableOutput('overview_num'),
        color = "#CC99FF",
        size = 2
      )
    )
  )
)

#===================================

# Server Code ======================

server <- function(input, output, session) {
  # Reactive values
  rv <- reactiveValues(
    type = "Assurance",
    method = "Single",
    age_group = 1,
    age = 30,
    age_x = 30,
    age_y = 30,
    interest = 4,
    payment = "level",
    insurance_benefit = "pure_endowment",
    insurance_benefit_payment = "end_of_year_of_death",
    assured_sum = 1000,
    benefit_term = 10,
    premium = 0,
    claim_expense = 0,
    initial_expense = 0,
    premium_expense = 0,
    bonus = 0,
    inflation = 0,
    df = data.frame(c()),
    curState = 10
  )
  
  # Event observer
  observeEvent(
    input$type, 
    {
      if(rv$type == "Assurance") {
        updateActionButton(
          session,
          "type",
          label = "Annuity"
        )
        
        updateSelectInput(
          session,
          "insurance_benefit",
          choices = c(
            "Term annuity" = "term_annuity",
            "Whole annuity" = "whole_annuity"
          ),
          selected = "term_annuity"
        )
        
        updateNumericInput(
          session,
          "claim_expense",
          label = "Annuity Expense"
        )
        
        hide(id = "payment", anim = TRUE)
        hide(id = "premium_expense", anim = TRUE)
        show(id = "inflation", anim = TRUE)
        hide(id = "bonus", anim = TRUE)

        updateSelectInput(
          session,
          "insurance_benefit_payment",
          choices = c(
            "End of year of death" = "end_of_year_of_death"
          ),
          selected = "end_of_year_of_death"
        )
        
        updateRadioButtons(
          session, 
          "payment",
          selected = "single"
        )
        rv$payment = "single"
        
        rv$type = "Annuity"
      }
      else {
        updateActionButton(
          session,
          "type",
          label = "Assurance"
        )
        
        if(rv$method == "Single") {
          updateSelectInput(
            session,
            "insurance_benefit",
            choices = c(
              "Pure endowment" = "pure_endowment",
              "Term insurance" = "term_insurance",
              "Whole assurance" = "whole_assurance",
              "Endowment" = "endowment"
            ),
            selected = "pure_endowment"
          )             
        }
        else {
          updateSelectInput(
            session,
            "insurance_benefit",
            choices = c(
              "Term insurance" = "term_insurance",
              "Whole assurance" = "whole_assurance"
            ),
            selected = "term_insurance"
          )   
        }
        
        updateNumericInput(
          session,
          "claim_expense",
          label = "Claim Expenses (% of Benefit Amount)"
        )
        
        show(id = "payment", anim = TRUE)
        show(id = "premium_expense", anim = TRUE)
        hide(id = "inflation", anim = TRUE)
        show(id = "bonus", anim = TRUE)
        
        if(rv$insurance_benefit == "pure_endowment") {
          updateSelectInput(
            session,
            "insurance_benefit_payment",
            choices = c(
              "End of year of death" = "end_of_year_of_death",
              "Immediately on death" = "immediately_on_death"
            ),
            selected = "end_of_year_of_death"
          ) 
        }
        
        updateRadioButtons(
          session, 
          "payment",
          selected = "level"
        )
        rv$payment = "level"
        
        rv$type = "Assurance"        
      }
    }
  )
  
  observeEvent(
    input$method,
    {
      if(rv$method == "Single") {
        updateActionButton(
          session,
          "method",
          label = "Joint",
          icon = icon("users")
        )
        
        if(rv$type == "Assurance") {
          updateSelectInput(
            session,
            "insurance_benefit",
            choices = c(
              "Term insurance" = "term_insurance",
              "Whole assurance" = "whole_assurance"
            ),
            selected = "term_insurance"
          )
        }
        
        hide(id = "age_group", anim = TRUE)
        hide(id = "age", anim = TRUE)
        hide(id = "max_age", anim = TRUE)
        show(id = "age_x", anim = TRUE)
        show(id = "age_y", anim = TRUE)
        
        rv$method = "Joint"
      }
      else {
        updateActionButton(
          session,
          "method",
          label = "Single",
          icon = icon("user")
        )
        
        if(rv$type == "Assurance") {
          updateSelectInput(
            session,
            "insurance_benefit",
            choices = c(
              "Pure endowment" = "pure_endowment",
              "Term insurance" = "term_insurance",
              "Whole assurance" = "whole_assurance",
              "Endowment" = "endowment"
            ),
            selected = "pure_endowment"
          )
        }        
        
        show(id = "age_group", anim = TRUE)
        show(id = "age", anim = TRUE)
        show(id = "max_age", anim = TRUE)
        hide(id = "age_x", anim = TRUE)
        hide(id = "age_y", anim = TRUE)

        rv$method = "Single"        
      }
    }
  )
  
  observeEvent(input$age_group, {rv$age_group <- input$age_group})
  
  observeEvent(input$age, {rv$age <- input$age})
  
  observeEvent(input$age_x, {rv$age_x <- input$age_x})
  
  observeEvent(input$age_y, {rv$age_y <- input$age_y})
  
  observeEvent(input$interest, {rv$interest <- input$interest})
  
  observeEvent(
    input$payment, 
    {
      rv$payment <- input$payment
      
      if(input$payment == "single")
        hide(id = "premium_expense", anim = TRUE)
      else
        show(id = "premium_expense", anim = TRUE)
    }
  )
  
  observeEvent(
    input$insurance_benefit, 
    {
      rv$insurance_benefit <- input$insurance_benefit
      
      if(input$insurance_benefit == "pure_endowment"){
        updateSelectInput(
          session,
          "insurance_benefit_payment",
          choices = c(
            "End of year of death" = "end_of_year_of_death"
          ),
          selected = "end_of_year_of_death"
        )
        
        hide(id = "bonus", anim = TRUE)
        hide(id = "inflation", anim = TRUE)
      }
      else{
        if(rv$type == "Assurance") {
          updateSelectInput(
            session,
            "insurance_benefit_payment",
            choices = c(
              "End of year of death" = "end_of_year_of_death",
              "Immediately on death" = "immediately_on_death"
            ),
            selected = "end_of_year_of_death"
          )
        }
        
        if(rv$type == "Assurance")
          show(id = "bonus", anim = TRUE)
        else
          hide(id = "bonus", anim = TRUE)
      }
      
      if(input$insurance_benefit == "whole_assurance" || input$insurance_benefit == "whole_annuity"){
        hide(id = "benefit_term", anim = TRUE)
        hide(id = "max_term", anim = TRUE)
      }
      else{
        show(id = "benefit_term", anim = TRUE)
        show(id = "max_term", anim = TRUE)
      }
      
      updateSelectInput(
        session,
        "insurance_benefit_payment",
        selected = "end_of_year_of_death"
      )
      rv$insurance_benefit_payment = "end_of_year_of_death"
    }
  )
  
  observeEvent(input$insurance_benefit_payment, {rv$insurance_benefit_payment <- input$insurance_benefit_payment})
  
  observeEvent(input$assured_sum, {rv$assured_sum <- input$assured_sum})
  
  observeEvent(input$benefit_term, {rv$benefit_term <- input$benefit_term})
  
  observeEvent(input$claim_expense, {rv$claim_expense <- input$claim_expense})
  
  observeEvent(input$initial_expense, {rv$initial_expense <- input$initial_expense})
  
  observeEvent(input$premium_expense, {rv$premium_expense <- input$premium_expense})
  
  observeEvent(input$bonus, {rv$bonus <- input$bonus})
  
  observeEvent(input$inflation, {rv$inflation <- input$inflation})
  
  # Get current state
  observeEvent(
    {
      input$payment
      input$insurance_benefit
      input$insurance_benefit_payment
    },
    {
      for(idx in 1:length(states)/3){
        if(
          states[(3 * (idx - 1)) + 1] == rv$payment &&
          states[(3 * (idx - 1)) + 2] == rv$insurance_benefit &&
          states[(3 * (idx - 1)) + 3] == rv$insurance_benefit_payment
        ){
          rv$curState <- idx
          break
        }
      }
      
      # Debugging purposes
      print(paste0("Current state: ", rv$curState))
    }
  )
  
  # Maximum labels for inputs
  output$max_age <- renderText(100)
  output$max_interest <- renderText(paste0(50, "%"))
  output$max_term <- renderText({
    if(rv$method == "Single") {
      max = 100 - rv$age
      
      updateNoUiSliderInput(
        session,
        "benefit_term",
        range = c(1, max)
      )
      
      return(max)
    }
    else {
      if(rv$age_x > rv$age_y)
        max = 100 - rv$age_x
      else
        max = 100 - rv$age_y
      
      updateNoUiSliderInput(
        session,
        "benefit_term",
        range = c(1, max)
      )
      
      return(max)
    }
  })
  
  output$text <- renderText({
    params_single = c(
      rv$age,
      rv$benefit_term,
      rv$assured_sum,
      rv$interest,
      rv$claim_expense,
      rv$initial_expense,
      rv$premium_expense,
      rv$age_group,
      rv$bonus,
      rv$inflation
    )
    
    params_joint = c(
      rv$age_x,
      rv$age_y,
      rv$benefit_term,
      rv$assured_sum,
      rv$interest,
      rv$claim_expense,
      rv$initial_expense,
      rv$premium_expense,
      rv$bonus,
      rv$inflation
    )    

    if(rv$method == "Single") {
      # Single conditions
      # Make sure vector is integer
      options(digits = 6)
      params = as.numeric(params_single)
      
      rv$premium = switch(
        rv$curState,
        single_pure_eoy_premium(params),
        single_term_eoy_premium(params),
        single_term_iod_premium(params),
        single_ea_eoy_premium(params),
        single_ea_iod_premium(params),
        single_wholeassur_eoyd_premium(params),
        single_wholeassur_iod_premium(params),
        single_wan_eoyd_premium(params),
        single_tan_eoyd_premium(params),      
        level_pure_eoy_premium(params),
        level_term_eoy_premium(params),
        level_term_iod_premium(params),
        level_ea_eoy_premium(params),
        level_ea_iod_premium(params),
        level_wholeassur_eoyd_premium(params),
        level_wholeassur_iod_premium(params)      
      )
    } 
    else {
      # Joint conditions
      # Make sure vector is integer
      options(digits = 6)
      params = as.numeric(params_joint)
      
      rv$premium = switch(
        rv$curState,
        single_pure_eoy_premium(params),
        joint_single_term_eoy_premium(params),
        joint_single_term_iod_premium(params),
        single_ea_eoy_premium(params),
        single_ea_iod_premium(params),
        joint_single_wholeassure_eoyd_premium(params),
        joint_single_wholeassur_iod_premium(params),
        joint_wan_eoyd_premium(params),
        joint_tan_eoyd_premium(params),      
        level_pure_eoy_premium(params),
        joint_level_term_eoy_premium(params),
        joint_level_term_iod_premium(params),
        level_ea_eoy_premium(params),
        level_ea_iod_premium(params),
        joint_level_wholeassur_eoyd_premium(params),
        joint_level_wholeassur_iod_premium(params)      
      )      
    }
  
    p = rv$premium
    
    round(p, 3)
    
    # Debugging purposes
    # print(paste0("Premium: ", rv$premium))
  })
  
  output$overview_type <- renderTable(
    {
      if(rv$age_group == 1)
        a = "X"
      else
        a = "Y"
      
      ip <- c("Insurance Product", rv$type)
      lp <- c("Life Product", rv$method)
      ag <- c("Age Group", a)
      b <- c("Benefit", rv$insurance_benefit)
      p <- c("Payment", rv$insurance_benefit_payment)
      pp <- c("Premium Payment", rv$payment)

      if(rv$method == "Single") {
        if(rv$type == "Assurance") {
          data.frame(ip, lp, ag, b, p, pp)
        }
        else {
          data.frame(ip, lp, ag, b, p)
        }
      }
      else {
        if(rv$type == "Assurance") {
          data.frame(ip, lp, b, p, pp)
        }
        else {
          data.frame(ip, lp, b, p)
        }        
      }
    },
    bordered = TRUE,
    spacing = "m",
    align = "c",
    colnames = FALSE
  )
  
  output$overview_num <- renderTable(
    {
      a <- c("Age", rv$age)
      ax <- c("Age X", rv$age_x)
      ay <- c("Age Y", rv$age_y)
      i <- c("Interest", paste0(rv$interest, "%"))
      t <- c("Term", rv$benefit_term)
      s <- c("Sum", paste0("$", rv$assured_sum))
      ce <- c("Claim Expense", paste0(rv$claim_expense, "%"))
      ann <- c("Annuity Expense", paste0(rv$claim_expense))
      ie <- c("Initial Expense", paste0(rv$initial_expense, "%"))
      pe <- c("Premium Expense", paste0(rv$premium_expense, "%"))
      bon <- c("Bonus", paste0(rv$bonus, "%"))
      inf <- c("Inflation", paste0(rv$inflation, "%"))
      
      if(rv$method == "Single") {
        if(rv$type == "Assurance") {
          if(rv$payment == "single") {
            if(rv$insurance_benefit == "pure_endowment")
              data.frame(a, i, t, s, ce, ie)
            else if(rv$insurance_benefit == "whole_assurance")
              data.frame(a, i, s, ce, ie, bon)
            else
              data.frame(a, i, t, s, ce, ie, bon)
          }
          else {
            if(rv$insurance_benefit == "pure_endowment")
              data.frame(a, i, t, s, ce, ie, pe)
            else if(rv$insurance_benefit == "whole_assurance")
              data.frame(a, i, s, ce, ie, pe, bon)            
            else
              data.frame(a, i, t, s, ce, ie, pe, bon)
          }
        }
        else {
          data.frame(a, i, t, s, ann, ie, pe, inf)
        }        
      }
      else {
        if(rv$type == "Assurance") {
          if(rv$payment == "single"){
            if(rv$insurance_benefit == "whole_assurance")
              data.frame(ax, ay, i, s, ce, ie, bon)
            else
              data.frame(ax, ay, i, t, s, ce, ie, bon)
          }
          else {
            if(rv$insurance_benefit == "whole_assurance")
              data.frame(ax, ay, i, s, ce, ie, pe, bon)
            else            
              data.frame(ax, ay, i, t, s, ce, ie, pe, bon)
          }
        }
        else {
          data.frame(ax, ay, i, t, s, ann, ie, pe, inf)
        }        
      }
    },
    bordered = TRUE,
    spacing = "m",
    align = "c",
    colnames = FALSE
  )
  
  output$line <- renderPlot({
    reserve = c()

    params_single = c(
      rv$age, 
      rv$benefit_term, 
      rv$assured_sum, 
      rv$interest, 
      rv$premium,
      rv$claim_expense,
      rv$initial_expense,
      rv$premium_expense,
      rv$age_group,
      rv$bonus,
      rv$inflation
    )
    
    params_joint = c(
      rv$age_x, 
      rv$age_y, 
      rv$benefit_term, 
      rv$assured_sum, 
      rv$interest, 
      rv$premium,
      rv$claim_expense,
      rv$initial_expense,
      rv$premium_expense,
      rv$bonus,
      rv$inflation
    )

    # Initialize reserve
    reserve = c()
    if(rv$method == "Single") {
      options(digits = 6)
      params = as.numeric(params_single)
      
      reserve = switch(
        rv$curState,
        single_pure_eoy_reserve(params),
        single_term_eoy_reserve(params),
        single_term_iod_reserve(params),
        single_ea_eoy_reserve(params),
        single_ea_iod_reserve(params),
        single_wholeassur_eoyd_reserve(params),
        single_wholeassur_iod_reserve(params),
        single_wan_eoyd_reserve(params),
        single_tan_eoyd_reserve(params),      
        level_pure_eoy_reserve(params),
        level_term_eoy_reserve(params),
        level_term_iod_reserve(params),
        level_ea_eoy_reserve(params),
        level_ea_iod_reserve(params),
        level_wholeassur_eoyd_reserve(params),
        level_wholeassur_iod_reserve(params)
      )
    }
    else {
      options(digits = 6)
      params = as.numeric(params_joint)

      reserve = switch(
        rv$curState,
        single_pure_eoy_reserve(params),
        joint_single_term_eoy_reserve(params),
        joint_single_term_iod_reserve(params),
        single_ea_eoy_reserve(params),
        single_ea_iod_reserve(params),
        joint_single_wholeassur_eoyd_reserve(params),
        joint_single_wholeassur_iod_reserve(params),
        joint_wan_eoyd_reserve(params),
        joint_tan_eoyd_reserve(params),      
        level_pure_eoy_reserve(params),
        joint_level_term_eoy_reserve(params),
        joint_level_term_iod_reserve(params),
        level_ea_eoy_reserve(params),
        level_ea_iod_reserve(params),
        joint_level_wholeassur_eoyd_reserve(params),
        joint_level_wholeassur_iod_reserve(params)
      )   
    }
    
    # Debugging purposes
    print(reserve)
    
    df <- data.frame(reserve)
    df$id <- 0:(nrow(df)-1)  
    
    rv$df = df
    
    ggplot(data = df, aes(x=id, y=reserve)) + 
      geom_line(color = "#FF89FF", size = 2) +
      geom_point(color = "#FF89FF", size = 5) +    
      scale_x_continuous(n.breaks = 6) +
      xlab("Policy seniority") + ylab("") + 
      labs(
        title = "Evolution of the reserve",
      ) +
      theme(
        plot.title = element_text(color = "#FF89FF", size = 20, face = "bold", hjust = 0.5),
        plot.caption = element_text(face = "italic", hjust = 1),
        axis.title.x = element_text(color = "#FF89FF", size = 16, face = "bold")
      )
  })
  
  # Hover information (src: https://gitlab.com/-/snippets/16220)
  output$hover_info <- renderUI({
    hover <- input$plot_hover
    point <- nearPoints(rv$df, hover, threshold = 5, maxpoints = 1, addDist = TRUE)
    if (nrow(point) == 0) return(NULL)

    # calculate point position INSIDE the image as percent of total dimensions
    # from left (horizontal) and from top (vertical)
    left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
    top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
    
    # calculate distance from left and bottom side of the picture in pixels
    left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
    top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
    
    # create style property for tooltip
    # background color is set so tooltip is a bit transparent
    # z-index is set so we are sure are tooltip will be on top
    style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                    "left:", left_px + 2, "px; top:", top_px + 2, "px;")
    
    # actual tooltip created as wellPanel
    wellPanel(
      style = style,
      p(
        HTML(
          paste0(
            "===================", "<br/>",
            "<b> Year: </b>", point$id , "<br/>",
            "<b> Reserve: </b>", round(point$reserve, 3), "<br/>",
            "==================="
          )
        )
      )
    )
  })  
}

#===================================

shinyApp(ui, server)