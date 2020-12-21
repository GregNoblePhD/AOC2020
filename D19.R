#options
options(scipen = 1000) #needed this to remove scientific notation

#libraries
library(data.table)
library(dplyr)
library(stringr)

#Part 1
#Read in the data
Input = readLines('C:/Users/noble/Desktop/AOC 2020/D19P1 Data.txt')
Input = data.table(Input)
setnames(Input, c('Value'))

#separate out the messages to validate
Messages = Input[!grepl(':', Input$Value, fixed = FALSE)]
Messages = Messages[Value != '',]

#Format the rules
Rules = Input[grepl(':', Input$Value, fixed = FALSE)]
Rules$Number = sapply(strsplit(Rules$Value, ':'), '[', 1)
Rules$Rule = paste('(', trimws(sapply(strsplit(Rules$Value, ':'), '[', 2), which = c("left")), ')', sep = '')
Rules$Number = as.numeric(Rules$Number)
Rules = Rules[order(Number),]

#Format the 'a' and 'b' values
Rules$Rule[Rules$Rule == '("a")'] = '(a)'
Rules$Rule[Rules$Rule == '("b")'] = '(b)'  

#vector of rules that have already been fully subsetted into the other rules
Completed = NULL

#While there are rules that still have numbers in them...
while(grepl("[0-9]", Rules$Rule[Rules$Number == 0])){
  
  #identify the indecies of the rules that have no numbers and are not already subsetted into other rules
  temp_rules = grep("^[^0-9]+$",Rules$Rule)
  temp_rules = temp_rules[!(temp_rules %in% Completed)]
  
  #loop through each targeted all letters rule to subset into other rules.
  for(nRule in temp_rules){
    
    #if the temp_sub_rule does not have a "or" statement in it then remove the parentheses because it will be direct replacement
    #need to keep parentheses when substituing in a rule with an 'or' statement
    temp_sub_rule = Rules$Rule[nRule]
    if(!grepl("\\|", temp_sub_rule)){
      temp_sub_rule = gsub('\\(', '', temp_sub_rule)
      temp_sub_rule = gsub('\\)', '', temp_sub_rule)
    }
    
    #use regex to bound the number to avoid situations like flagging '11' when searching for '1'
    temp_rule_num = paste0('\\b', Rules$Number[nRule], '\\b')
    
    #for each instance of the targeted rule number in the rules, replace with substituted rule
    for(nSub in grep(temp_rule_num, Rules$Rule)){
      Rules$Rule[nSub] = gsub(temp_rule_num, temp_sub_rule, Rules$Rule[nSub])
    }
  }
  
  #remove the fully substituted rules.
  Completed = c(Completed, temp_rules) 
}

#Part 1 answer
Regex_Result = paste0('^', Rules$Rule[1], '$')
Regex_Result = gsub(' ', '', Regex_Result)
length(Messages$Value[grepl(Regex_Result, Messages$Value)])




#Part 2
#Read in the data
Input = readLines('C:/Users/noble/Desktop/AOC 2020/D19P1 Data.txt')
Input = data.table(Input)
setnames(Input, c('Value'))

#separate out the messages to validate
Messages = Input[!grepl(':', Input$Value, fixed = FALSE)]
Messages = Messages[Value != '',]

#Format the rules
Rules = Input[grepl(':', Input$Value, fixed = FALSE)]
Rules$Number = sapply(strsplit(Rules$Value, ':'), '[', 1)
Rules$Rule = paste('(', trimws(sapply(strsplit(Rules$Value, ':'), '[', 2), which = c("left")), ')', sep = '')
Rules$Number = as.numeric(Rules$Number)
Rules = Rules[order(Number),]

#Format the 'a' and 'b' values
Rules$Rule[Rules$Rule == '("a")'] = '(a)'
Rules$Rule[Rules$Rule == '("b")'] = '(b)'  


#make a really large replacement value for rules 8 and 11
New_Rule8 = '(42 | 42 8)'
New_Rule11 = '(42 31 | 42 11 31)'

temp_Rule8 = '(42 | 42 8)'
temp_Rule11 = '(42 31 | 42 11 31)'

for(nloop in 1:20){
  New_Rule8 = sub(pattern = '8', replacement = temp_Rule8, New_Rule8)
  New_Rule11 = sub(pattern = '11', replacement = temp_Rule11, New_Rule11)
}
Rules$Rule[Rules$Number == 8] = gsub(pattern = '8', '42', New_Rule8)
Rules$Rule[Rules$Number == 11] = gsub(pattern = '11', '42', New_Rule11)


#vector of rules that have already been fully subsetted into the other rules
Completed = NULL

#While there are rules that still have numbers in them...
while(grepl("[0-9]", Rules$Rule[Rules$Number == 0])){
  
  #identify the indecies of the rules that have no numbers and are not already subsetted into other rules
  temp_rules = grep("^[^0-9]+$",Rules$Rule)
  temp_rules = temp_rules[!(temp_rules %in% Completed)]
  
  #loop through each targeted all letters rule to subset into other rules.
  for(nRule in temp_rules){
    
    #if the temp_sub_rule does not have a "or" statement in it then remove the parentheses because it will be direct replacement
    #need to keep parentheses when substituing in a rule with an 'or' statement
    temp_sub_rule = Rules$Rule[nRule]
    if(!grepl("\\|", temp_sub_rule)){
      temp_sub_rule = gsub('\\(', '', temp_sub_rule)
      temp_sub_rule = gsub('\\)', '', temp_sub_rule)
    }
    
    #use regex to bound the number to avoid situations like flagging '11' when searching for '1'
    temp_rule_num = paste0('\\b', Rules$Number[nRule], '\\b')
    
    #for each instance of the targeted rule number in the rules, replace with substituted rule
    for(nSub in grep(temp_rule_num, Rules$Rule)){
      Rules$Rule[nSub] = gsub(temp_rule_num, temp_sub_rule, Rules$Rule[nSub])
    }
  }
  
  #remove the fully substituted rules.
  Completed = c(Completed, temp_rules) 
}

#Part 2 answer
Regex_Result = paste0('^', Rules$Rule[1], '$')
Regex_Result = gsub(' ', '', Regex_Result)
length(Messages$Value[grepl(Regex_Result, Messages$Value)])


