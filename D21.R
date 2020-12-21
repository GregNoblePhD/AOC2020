#options
options(scipen = 1000) #needed this to remove scientific notation

#libraries
library(data.table)
library(dplyr)
library(stringr)

#Part 1
#Read in the data
Input = readLines('C:/Users/noble/Desktop/AOC 2020/D21P1 Data.txt')
Input = data.table(Input)
setnames(Input, c('Value'))

Input$Ing = sapply(strsplit(Input$Value, '\\('), '[', 1)
Input$Alergen = sapply(strsplit(Input$Value, '\\(contains'), '[', 2)
Input$Alergen = sub(')', '', Input$Alergen)
Input$Alergen = sub(',', '', Input$Alergen)

Ingredients = unique(unlist(strsplit(Input$Ing, ' ')))

Alergens = unique(unlist(strsplit(Input$Alergen, ' ')))
Alergens = Alergens[Alergens != '']
Alergens = sub(',', '', Alergens)
Alergens = unique(Alergens)

Exclude_List = NULL
Ing_Alg = NULL
for(nAlg in Alergens){
  temp_Input = Input[str_detect(Input$Alergen, pattern = paste0(' ', nAlg)),]
  Food_List = data.table('Ing' = unlist(strsplit(temp_Input$Ing, split = ' ')))
  Food_List = data.table(Food_List %>%
                         group_by(Ing) %>%
                         summarize(group_n = n())
  )
  Exclude_List  = c(Exclude_List, Food_List$Ing[Food_List$group_n == nrow(temp_Input)])
  Ing_Alg = rbindlist(list(Ing_Alg, data.table('Ing' = Food_List$Ing[Food_List$group_n == nrow(temp_Input)], 
                                               'Alg' = nAlg
                                               )
                           )
                      )
}

#Result Part 1
Safe_Ing = Ingredients[which(!(Ingredients %in% unique(Exclude_List)))]
All_Ing_Instance = unlist(strsplit(Input$Ing, ' '))
length(which(All_Ing_Instance %in% Safe_Ing))


#Part2
Result = NULL
while (nrow(Ing_Alg) > 0){
  temp = data.table(Ing_Alg %>%
                    group_by(Alg) %>%
                    summarize(Group_Count = n())
                    )
  temp_Alg = temp$Alg[temp$Group_Count == 1][1]
  temp_Ing = Ing_Alg$Ing[Ing_Alg$Alg == temp_Alg][1]
  Result = rbindlist(list(Result, data.table('Alg' = temp_Alg, 'Ing' = temp_Ing)))
  Ing_Alg = Ing_Alg[Ing_Alg$Ing != temp_Ing]
}
Result = Result[order(Result$Alg),]

#part 2 answer
paste(Result$Ing, collapse = ',')





