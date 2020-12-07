
library(data.table)
library(dplyr)
library(stringr)
library(tidyr)

Input = readLines('C:/Users/noble/Desktop/AOC 2020/D4P1 Data.txt')
Input = data.table(Input)
setnames(Input, c('Value'))

#Part1
Codes = c('byr:', 'iyr:', 'eyr:', 'hgt:', 'hcl:', 'ecl:', 'pid:', 'cid:')
Req_Codes = c('byr:', 'iyr:', 'eyr:', 'hgt:', 'hcl:', 'ecl:', 'pid:')

Check_Input = data.table(
                Input %>%
                mutate(Break_FL = ifelse(Input$Value == '', 1, 0),
                       Break_Group = cumsum(Break_FL))
              )

Check_Input = data.table(
                Check_Input %>%
                group_by(Break_Group) %>%
                summarize(Full_Group = paste(Value, collapse = " "),
                          Req_Check = ifelse(all(str_detect(Full_Group, Req_Codes)), 1, 0)
                )
              )

sum(Check_Input$Req_Check)



#Part 2
Input_Present = Check_Input[Req_Check == 1,]

hcl_val_Char = c('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f')
hcl_special_Char = substr(Input_Present$Full_Group[2], 5, 5)
ecl_req = c('amb', 'blu', 'brn', 'gry', 'grn', 'hzl', 'oth')
pid_req = c('0', '1', '2', '3', '4', '5', '6', '7', '8', '9')

r = Input_Present %>% separate(Full_Group, c('one', 'two', 'three', 'four', 'five', 'six', 'seven', 'eight', 'nine', 'ten'), sep = ' ', remove = F)
str_detect(r$Full_Group, 'eyr:')

test = Input_Present %>%
       group_by(Break_Group) %>%
       mutate(x = strsplit(scan(text = Full_Group, what = '', sep = ' '), ':'))

x = strsplit(scan(text = Input_Present$Full_Group, what = '', sep = ' '), ':')
which(length(x) == 0)
lapply(x, length)
y = do.call(rbind,x)
strsplit(scan(text=passports[i],what="",sep=" "),":")

Valid_Input = data.table(
                Input_Present %>%
                mutate(byr_Value = as.numeric(substr(sapply(strsplit(Input_Present$Full_Group, 'byr:'), "[[", 2), 1, 4)),
                       byr_Check = ifelse(byr_Value >= 1920 & byr_Value <= 2002, 1, 0),
                       iyr_Value = as.numeric(substr(sapply(strsplit(Input_Present$Full_Group, 'iyr:'), "[[", 2), 1, 4)),
                       iyr_Check = ifelse(iyr_Value >= 2010 & iyr_Value <= 2020, 1, 0),
                       eyr_Value = as.numeric(substr(sapply(strsplit(Input_Present$Full_Group, 'eyr:'), "[[", 2), 1, 4)),
                       eyr_Check = ifelse(eyr_Value >= 2020 & eyr_Value <= 2030, 1, 0),
                       hgt_val = substr(sapply(strsplit(Input_Present$Full_Group, 'hgt:'), "[[", 2), 1, 6),
                       hgt_cm_Val = sapply(strsplit(hgt_val, 'cm'), "[[", 1),
                       hgt_cm_Val = as.numeric(ifelse(nchar(hgt_cm_Val) == 3, hgt_cm_Val, '0')),
                       hgt_cm_Check = ifelse(hgt_cm_Val >= 150 & hgt_cm_Val <= 193, 1, 0),
                       hgt_in_Val = sapply(strsplit(hgt_val, 'in'), "[[", 1),
                       hgt_in_Val = as.numeric(ifelse(nchar(hgt_in_Val) == 2, hgt_in_Val, '0')),
                       hgt_in_Check = ifelse(hgt_in_Val >= 59 & hgt_in_Val <= 76, 1, 0),
                       hgt_Check = ifelse(hgt_in_Check == 1 | hgt_cm_Check == 1, 1, 0),
                       ecl_Val = substr(sapply(strsplit(Input_Present$Full_Group, 'ecl:'), "[[", 2), 1, 3),
                       ecl_Check = ifelse(ecl_Val %in% ecl_req, 1, 0),
                       pid_Val = substr(sapply(strsplit(Input_Present$Full_Group, 'pid:'), "[[", 2), 1, 9),
                       pid_Check = ifelse(!is.na(as.numeric(pid_Val)), 1, 0),
                       hcl_Val = substr(sapply(strsplit(Input_Present$Full_Group, 'hcl:'), "[[", 2), 1, 7),
                       hcl_Val = ifelse(substr(hcl_Val, 1, 1) == hcl_special_Char & nchar(hcl_Val) == 7, hcl_Val , '       '),
                       hcl_Check = ifelse(sapply(strsplit(hcl_Val, ""), "[[", 2) %in% hcl_val_Char & 
                                            sapply(strsplit(hcl_Val, ""), "[[", 3) %in% hcl_val_Char & 
                                            sapply(strsplit(hcl_Val, ""), "[[", 4) %in% hcl_val_Char & 
                                            sapply(strsplit(hcl_Val, ""), "[[", 5) %in% hcl_val_Char & 
                                            sapply(strsplit(hcl_Val, ""), "[[", 6) %in% hcl_val_Char & 
                                            sapply(strsplit(hcl_Val, ""), "[[", 7) %in% hcl_val_Char, 1, 0),
                       Final_Check = ifelse(byr_Check == 1 & iyr_Check == 1 & eyr_Check == 1 & hgt_Check == 1 & ecl_Check == 1 & pid_Check == 1 & hcl_Check == 1, 1, 0)
                       
                       )
              )

sum(Valid_Input$Final_Check)

#167 is answer ???

