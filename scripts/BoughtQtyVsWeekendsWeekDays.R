# Pw = Proportion of the bought quantity < 10 in weekends
# Pwd = Proportion of the bought quantity < 10 in week days

#Hypothesis
#H0 => Pw = Pwd
#Ha => Pw > Pwd

# Read CSV files which only contatins the bought quantity 1 <- less than 10, 0<- 10 or more than 10

CSV_Weekends = read.csv("../responses/Bought_Quantity_Weekends.csv", header = TRUE,sep = ',')
attributes(CSV_Weekends)
#get Weekends data from csv file
Weekends_data = c()
for(i in 1:160){
  Weekends_data <- CSV_Weekends$Item.Quantity
}

CSV_Weekdays = read.csv("../responses/Bought_Quantity_Weekdays.csv")
attributes(CSV_Weekdays)
#get Week days data from csv file
Weekdays_data = c()
for(i in 1:87){
  Weekdays_data <- CSV_Weekdays$Item.Quantity
}

# Get number of 1 occurrences in the data
x1_numberOf1 = sum(Weekends_data == 1)
x1_Prop = x1_numberOf1/160
x2_numberOf1 = sum(Weekdays_data == 1)
x2_Prop = x2_numberOf1/87

# create a list adding results from above occurences and 0 occurences
listweekend = c()
for (i in 1: x1_numberOf1) {
  listweekend[i] <- 1
}
for (i in 109 : 160) {
  listweekend[i] <- 0
}

listweekdays = c()
for (i in 1: x2_numberOf1) {
  listweekdays[i] <- 1
}
for (i in 69 : 87) {
  listweekdays[i] <- 0
}

# Get the proportion difference
p_diff = x1_Prop -x2_Prop

# Create a new list by appending total values of the 2 lists
Total = c(listweekend, listweekdays)

#boostraps samples
arr = c()

for (i in 1:10000) {
  smp = sample(Total)
  x1_smp = smp[1:160]
  x2_smp = smp[161:247]
  Prop_x1_Smp = sum(x1_smp)/160
  Prop_x2_Smp = sum(x2_smp)/87
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
p_value = (count/10000)

# If p_value < 0.05 Reject null hypothesis
# p_value = 0.9999
# Here, p_value > 0.05, So we cannot reject the null hypothesis.
