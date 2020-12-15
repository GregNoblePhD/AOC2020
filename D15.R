#options
options(scipen = 1000) #needed this to remove scientific notation

#Time tracking
t1 = proc.time()

#Part 1 & 2
#Read in the data
Input = c(2,1,10,11,0,6)

#Set up Output structure
Tot_Iter = 30000000  #Max possible value given the problem
Output = rep(NA, Tot_Iter)  #use NA for indicator of the value not been previously used
Output[1 + Input] = 1:length(Input) #first value is for '0' then the rest of the Inputs assigned the seq value

#Set loop variables
Iter = length(Input) + 1 #tracks the upcomming iteration number (we start at 7)
Value = 0 #tracks the last value (first one is initialized to 0 given our input)

#Loop through until last iteration reached
while(Iter <= Tot_Iter){
  Last_Value = Output[1+Value]  #pull the value for the last iteration
  Output[1+Value] = Iter   #update the Output
  Value = ifelse(is.na(Last_Value), 0, Iter - Last_Value) #determine the next Value 
  Iter = Iter + 1 #increment the iteration tracker
}

#Result
which(Output == Tot_Iter)-1

#Total Time
proc.time() - t1
