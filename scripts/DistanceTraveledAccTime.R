# Pw = Proportion of the people who travel less than 1km at night
# Pwd = Proportion of the people who travel less than 1km at morning

#Hypothesis
#H0 => Pw = Pwd
#Ha => Pw < Pwd

# Read CSV files which only contatins the bought quantity 1 <- less than 10, 0<- 10 or more than 10

CSV_Night = read.csv("../responses/DistanceTravelledLesstOneEvening.csv", header = TRUE,sep = ',')
attributes(CSV_Night)
#get Weekends data from csv file
night_data = c()

for(i in 1:64){
  night_data <- CSV_Night$Travelling.Distance
}

CSV_Morning = read.csv("../responses/DistanceTravelledLesstOneMorning.csv")
attributes(CSV_Morning)
#get Week days data from csv file
morning_data = c()
for(i in 1:90){
  morning_data <- CSV_Morning$Travelling.Distance
}

# Get number of 1 occurrences in the data
x1_numberOf1 = sum(night_data == 1)
x1_Prop = x1_numberOf1/64
x2_numberOf1 = sum(morning_data == 1)
x2_Prop = x2_numberOf1/90

# create a list adding results from above occurences and 0 occurences
listweekend = c()
for (i in 1: x1_numberOf1) {
  listweekend[i] <- 1
}
for (i in 30 : 64) {
  listweekend[i] <- 0
}

listweekdays = c()
for (i in 1: x2_numberOf1) {
  listweekdays[i] <- 1
}
for (i in 42 : 90) {
  listweekdays[i] <- 0
}

# Get the proportion difference
p_diff = x1_Prop -x2_Prop

# Create a new list by appending total values of the 2 lists
Total = c(listweekend, listweekdays)

#boostraps samples
arr = c()

for (i in 1:1000) {
  smp = sample(Total)
  x1_smp = smp[1:64]
  x2_smp = smp[65:154]
  Prop_x1_Smp = sum(x1_smp)/64
  Prop_x2_Smp = sum(x2_smp)/90
  p_diff2 = Prop_x1_Smp - Prop_x2_Smp
  arr = c(arr, p_diff2)
}

count = 0 
for (x in 1:length(arr)) {
  if(arr[x]>p_diff){
    count=count+1;
  }
}
# calculate P-Value
p_value = (count/1000)

# If p_value < 0.05 Reject null hypothesis
# p_value = 0.446
# Here, p_value > 0.05, So we cannot reject the null hypothesis.