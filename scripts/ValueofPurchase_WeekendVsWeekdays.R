# data for week days processed by using the initial response csv file
weekdays_data=read.csv("../responses/weekdays_purchases.csv",header = TRUE,sep = ',')
print(weekdays_data)
attributes(weekdays_data)
dim(weekdays_data)

# data for weekend(saturday & sunday) processed by using the initial response csv file
weekend_data = read.csv("../responses/weekend_purchase.csv", header = TRUE,sep = ',')
print(weekend_data)
attributes(weekend_data)
dim(weekend_data)

# created vector for weight and added weight of purchase - value & weight is as follows
# Less than 500 rupees - 1
# 500 - 1000 rupees - 2
# 1000 - 2000 rupees - 3
# More than 2000 rupees - 4

# calculate weighted spent value for weekdays data
weekdays_weight = c()
for (i in 1:87) {
  if (weekdays_data[i,7]== "Less than 500 rupees") {
    weekdays_weight = c(weekdays_weight,1)
  } 
  else if (weekdays_data[i,7]== "500 - 1000 rupees"){
    weekdays_weight = c(weekdays_weight,2)
  }
  else if (weekdays_data[i,7]== "1000 - 2000 rupees") {
    weekdays_weight = c(weekdays_weight,3)
  }
  else {
    weekdays_weight = c(weekdays_weight,4)
  }
}
print(weekdays_weight) # inserted these value set to weight coloumn of weekdays_purchases.csv file 
weekdays_weighted_spent = c(weekdays_data$Weighted.Spent) # weighted spent for all the records of the data set 
print(weekdays_weighted_spent)

# calculate the weighted spent for weekend data 
weekend_weight = c()
for (k in 1:160) {
  if (weekend_data[k,4] == "Less than 500 rupees") {
    weekend_weight = c(weekend_weight,1)
  } 
  else if (weekend_data[k,4] == "500 - 1000 rupees"){
    weekend_weight = c(weekend_weight,2)
  }
  else if (weekend_data[k,4] == "1000 - 2000 rupees") {
    weekend_weight = c(weekend_weight,3)
  }
  else {
    weekend_weight = c(weekend_weight,4)
  }
}
print(weekend_weight) # inserted these value set to weight coloumn of weekend_purchases.csv file 
weekend_weighted_spent = c(weekend_data$Weighted.Spent) # weighted spent for all the records of the data set 
print(weekend_weighted_spent)

# Hypothesis Testing 
# w1 - average weighted spent for weekdays  
# w2 - average weighted spent for weekends 

# Hypothesis
# Null Hypothesis - Ho: w1=w2 
# Alternative Hypothesis - Ha: w1>w2
mean_diff = mean(weekdays_weighted_spent) - mean(weekend_weighted_spent) 
print(mean_diff)
# weekdays weigted spent > weekend weighted spent should adjust 
# adjusted values in Adjusted Weighted Spent coloumn in weekend_purchases.csv file
adjusted_weekend_spent = c(weekend_data$Adjusted.Weighted.Spent)
print(adjusted_weekend_spent)

# generate boostrap samples for weekdays & weekend data
# boostraping for weekdays 
boostrap_weekdays_sample = c()
boostrap_weekdays_mean=c()
for (x in 1:10000) {
  random_sample1 = sample(weekdays_weighted_spent,87,replace = TRUE)
  boostrap_weekdays_sample = c(boostrap_weekdays_sample,random_sample1)
  random_mean1 = mean(random_sample1);
  boostrap_weekdays_mean = c(boostrap_weekdays_mean,random_mean1)
}
print(boostrap_weekdays_sample)
print(boostrap_weekdays_mean)

# boostraping for weekend 
boostrap_weekend_sample = c()
boostrap_weekend_mean=c()
for (x in 1:10000) {
  random_sample2 = sample(adjusted_weekend_spent,160,replace = TRUE)
  boostrap_weekend_sample = c(boostrap_weekend_sample,random_sample2)
  random_mean2 = mean(random_sample2);
  boostrap_weekend_mean = c(boostrap_weekend_mean,random_mean2)
}
print(boostrap_weekend_sample)
print(boostrap_weekend_mean)

# calculate p values for boostrap sample
sample_mean_diff = (boostrap_weekdays_mean - boostrap_weekend_mean)
print(sample_mean_diff)
count = 0 
for (x in 1:length(sample_mean_diff)) {
  if(mean_diff[c(x)]>mean_diff){
    count=count+1;
  }
}
print(count)
p_value = (count/10000)
print(p_value)
# since we got 0 for p_value we have to reject the Null Hypothesis - H0: w1=w2

 