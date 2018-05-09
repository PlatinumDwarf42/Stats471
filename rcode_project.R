#importing Data and setting up
all_coin_data = read.csv("all_coin_data.csv")

TOP_COINS = c(c("eth", "ethereum"), c("xrp","ripple"),c("ltc", "litecoin"),c("xmr", "monero"),c("etc", "ethereum classic"),c("dash", "dash"),c("maid", "maidsafecoin"), c("rep", "augur"), c("steem", "steem"), c("xem", "nem"))

#Playing With Data

###All Levels:
coins = levels(all_coin_data$Coin)

###Closing worth each day for a year.

par(mfrow=c(2,5))
for (i in coins) {
  plot(y= all_coin_data$Close[which(as.character(all_coin_data$Coin) == i)], x = all_coin_data$Day.1[which(as.character(all_coin_data$Coin) == i)], main = i, type = 'b', pch = 'o', xlab="Day", ylab="closing ")
  
}

##Group 1: Steem, augur, ethereum, ethereum classic: have similar shapes. They have this hump between 100 and 250

##Group 2: Dash, litecoin, monero, maidsafecoin: have a lot more up and down

##Group 3: nem and ripple: also have super similar shapes, having a peak between 100 and 200

##Function for graphing groups
Setting_Window<- function(x){
  if( x == 1){
    par(mfrow=c(1,1))
  } else if(x == 2){
    par(mfrow=c(1,2))
  } else if( x == 3){
    par(mfrow=c(1,3))
  }else{
    par(mfrow=c(ceiling(x/2),ceiling(x/2)))
  }
}

plot_dot_closing <- function(groups, range_values){
  Setting_Window(length(groups))
  
  for (i in groups) {
    plot(y = all_coin_data$Close[which(all_coin_data$Coin == i & all_coin_data$Day.1 >= range_values[1] & all_coin_data$Day.1 < range_values[2])], 
         x = all_coin_data$Day.1[which(all_coin_data$Coin == i & all_coin_data$Day.1 >= range_values[1] & all_coin_data$Day.1 < range_values[2])], main = paste("Closing:", i), type = 'b', pch = 'o', xlab= "Day", ylab = "Price")
  }
}

plot_dot_Total_Coin_Number_Comments <- function(groups, range_values){
  
  Setting_Window(length(groups))
  
  for (i in groups) {
    plot(y= all_coin_data$Total_Number_Coin_Comments[which(all_coin_data$Coin == i & all_coin_data$Day.1 >= range_values[1] & all_coin_data$Day.1 < range_values[2])], 
         x = all_coin_data$Day.1[which(all_coin_data$Coin == i & all_coin_data$Day.1 >= range_values[1] & all_coin_data$Day.1 < range_values[2])], main = paste("Total Number Coin Comments", i), type = 'b', pch = 'o', xlab= "day", ylab = "Quantity")
  }
  
  
}

plot_dot_Total_Coin_Number_Submissions_FOMO <- function(groups, range_values){
  Setting_Window(length(groups))
  for (i in groups) {
    plot(y= all_coin_data$Total_Coin_Submissions_Num_Fomo[which(all_coin_data$Coin == i & all_coin_data$Day.1 >= range_values[1] & all_coin_data$Day.1 < range_values[2])], 
         x = all_coin_data$Day.1[which(all_coin_data$Coin == i & all_coin_data$Day.1 >= range_values[1] & all_coin_data$Day.1 < range_values[2])], main = paste("Total Number Coin Submission FOMO", i), type = 'b', pch = 'o', xlab= "day", ylab = "Quantity")
  }
  
}

plot_dot_Total_Coin_Number_Submissions_FUD <- function(groups, range_values){
  Setting_Window(length(groups))
  
  for (i in groups) {
    plot(y= all_coin_data$Total_Coin_Submissions_Num_Fud[which(all_coin_data$Coin == i & all_coin_data$Day.1 >= range_values[1] & all_coin_data$Day.1 < range_values[2])], 
         x = all_coin_data$Day.1[which(all_coin_data$Coin == i & all_coin_data$Day.1 >= range_values[1] & all_coin_data$Day.1 < range_values[2])], main = paste("Total Number Coin Submissions FUD", i), type = 'b', pch = 'o', xlab= "day", ylab = "Quantity")
  }
}

plot_dot_Total_Coin_Number_Comments_FOMO <- function(groups, range_values){
  Setting_Window(length(groups))
  for(i in groups){
    plot( y = all_coin_data$Total_Coin_Comment_Num_Fomo[which(all_coin_data$Coin == i & all_coin_data$Day.1 >= range_values[1] & all_coin_data$Day.1 < range_values[2])],
          x =  all_coin_data$Day.1[which(all_coin_data$Coin == i & all_coin_data$Day.1 >= range_values[1] & all_coin_data$Day.1 < range_values[2])], main = paste("Quanity Comments FOMO", i), type = 'b', pch = 'o', xlab= "day", ylab = "Quantity")
  }
}

plot_dot_Total_Coin_Number_Comments_FUD <- function(groups, range_values){
  Setting_Window(length(groups))
  
  for(i in groups){
    plot( y = all_coin_data$Total_Coin_Comment_Num_Fomo[which(all_coin_data$Coin == i & all_coin_data$Day.1 >= range_values[1] & all_coin_data$Day.1 < range_values[2])],
          x  = all_coin_data$Day.1[which(all_coin_data$Coin == i & all_coin_data$Day.1 >= range_values[1] & all_coin_data$Day.1 < range_values[2])], main = paste("Quanity Comments FUD", i), type = 'b', pch = 'o', xlab= "day", ylab = "Quantity")
  }
}

###Inspecting humps in first group
Group_1 <- c(coins[1], coins[10], coins[3], coins[4])
Group_1_orginal <- Group_1
plot_dot_closing(Group_1, c(100, 250))

#Seems that there is peaks in between 150 days and 200

plot_dot_closing(Group_1, c(150, 200))

#Looking at these graphs something is definitely going on between days 160 and 180

plot_dot_closing(Group_1, c(160, 175))

###Inspceting humps in the second group

Group_2 <- c(coins[2], coins[5], coins[7], coins[6])
Group_2_orginal = Group_2
plot_dot_closing(Group_2, c(200, 300))

#Definitely something between 220 and 280 for these coins. interesting that this is at completely different time then group 1.
plot_dot_closing(Group_2, c(220,280))

#okay looking at these closer it seems like we should inspect between 230 and 250
plot_dot_closing(Group_2, c(230, 250))
#From here it seems that there is something going on with day 240ish, there seems to be a 
#change or something happening in particular

###Looking through group 3
Group_3 = c()
for( i in coins){
  if(!(i %in% Group_1) & !(i %in% Group_2)){
    Group_3 = c(Group_3, i)
  }
}
Group_3_Orginal <- Group_3
plot_dot_closing(Group_3, c(1, 365))
par(mfrow=c(2,1))
plot_dot_closing(Group_3, c(100, 200))
plot_dot_closing(Group_3, c(120, 180))
plot_dot_closing(Group_3, c(120, 150))

#So once again we have found a special hump. 


##SUMMARY AFTER LOOKING AT 3 GROUPS

#They each have their own particular days where closing coin value spikes up. None of them over lap or by very little if at all. Things to look into from here. Is there any expertise as to why these coins are grouped like this (meanig why do they have similar distributions) Also did anything else peak during that time? Anything else change out of the ordinary?
  
###Looking At Comments in General.

mean(all_coin_data$Total_Number_Comments)
median(all_coin_data$Total_Number_Comments)
hist(all_coin_data$Total_Number_Comments )

par(mfrow=c(1,2))
plot(x = all_coin_data$Day.1, y = all_coin_data$Total_Number_Comments, xlab =  "Day", ylab = "Number of Comments")
plot(x = all_coin_data$Day.1, y = all_coin_data$Total_Number_Submissions, xlab =  "Day", ylab = "Number of Posts")

#We see that in general that the comments have an upward trend in general. 
#However we are able to seee two bumps, one between 100-200 and the other is between 200 -300. 
#These could possibly line up with the bumps we saw in the above 3 groups. We will explore each of these groups individually.

#100-200 Hump
par(mfrow=c(2,2))
plot(x = all_coin_data$Day.1[which(all_coin_data$Day.1 >= 100 & all_coin_data$Day.1 < 200)], y = all_coin_data$Total_Number_Comments[which(all_coin_data$Day.1 >= 100 & all_coin_data$Day.1 < 200)], ylab= "comments", xlab = "day")

plot(x = all_coin_data$Day.1[which(all_coin_data$Day.1 >= 140 & all_coin_data$Day.1 < 200)], y = all_coin_data$Total_Number_Comments[which(all_coin_data$Day.1 >= 140 & all_coin_data$Day.1 < 200)], ylab= "comments", xlab = "day")

plot(x = all_coin_data$Day.1[which(all_coin_data$Day.1 >= 160 & all_coin_data$Day.1 < 200)], y = all_coin_data$Total_Number_Comments[which(all_coin_data$Day.1 >= 160 & all_coin_data$Day.1 < 200)], ylab= "comments", xlab = "day")

plot(x = all_coin_data$Day.1[which(all_coin_data$Day.1 >= 160 & all_coin_data$Day.1 < 180)], y = all_coin_data$Total_Number_Comments[which(all_coin_data$Day.1 >= 160 & all_coin_data$Day.1 < 180)], ylab= "comments", xlab = "day")
#Once we get to the 160-180 it is harder to see that hump, but before that you can 
#definitely see the hump. Makes me believe something happened in those days. So we will inspect 160-180. 
#This is lines up with Group_1. So let's look as to what is going on.

#What type of comments
par(mfrow=c(2,1))
plot(all_coin_data$Day.1, all_coin_data$Total_Number_Comments, ylab= "comments", xlab = "day", main =  "Total Number Comments")
plot(all_coin_data$Day.1[which(all_coin_data$Day.1 >= 160 & all_coin_data$Day.1 < 180)], all_coin_data$Total_Number_Comments[which(all_coin_data$Day.1 >= 160 & all_coin_data$Day.1 < 180)], ylab= "comments", xlab = "day", main =  "Total Number Comments 160-180")
plot_dot_Total_Coin_Number_Comments(Group_1, c(100,200))

#Oh you can totoally see that the comments for that time in all those coins. 
#So maybe # comments means something
#Let's start looking at comments more specifically.
plot(x = all_coin_data$Day.1, y=all_coin_data$Total_Coin_Comment_Num_Fomo, ylab= "Day", xlab="Quantity", main= "FOMO comments all coins")
cat("So Fomo looks like is increasing exponetially. ")

plot_dot_Total_Coin_Number_Comments(Group_1, c(1,365))
plot_dot_Total_Coin_Number_Comments_FOMO(Group_1, c(1,365))
plot_dot_Total_Coin_Number_Comments_FUD(Group_1, c(1,365))

plot_dot_Total_Coin_Number_Comments_FOMO(Group_1, c(160,180))
plot_dot_Total_Coin_Number_Comments_FUD(Group_1, c(160, 180))
cat("now let's expan the range a bit to see if anything suspicious happen days before after")

plot_dot_Total_Coin_Number_Comments_FOMO(Group_1, c(140, 200))
plot_dot_Total_Coin_Number_Comments_FUD(Group_1, c(140, 200))

all_coin_data[which(all_coin_data$Day.1 < 180 & all_coin_data$Day.1 > 150 & all_coin_data$Coin == "steem" & all_coin_data$Total_Coin_Comment_Num_Fomo == 1),"Day.1"]

all_coin_data[which(all_coin_data$Day.1 < 180 & all_coin_data$Day.1 > 150 & all_coin_data$Coin == "steem" & all_coin_data$Total_Coin_Comment_Num_Fud == 1),"Day.1"]

#We see the humps of # comments also peaks at the same time their price peaks. 
#Possible peaks at the 160-180 mark

#Looking at Submissions in general and specifics

plot_dot_Total_Coin_Number_Submissions_FOMO(Group_1, c(1,365))
plot_dot_Total_Coin_Number_Submissions_FUD(Group_1, c(1, 365))

plot_dot_Total_Coin_Number_Submissions_FOMO(Group_1, c(140, 200))
plot_dot_Total_Coin_Number_Submissions_FUD(Group_1, c(140, 200))

plot_dot_Total_Coin_Number_Submissions_FOMO(Group_1, c(160, 180))
plot_dot_Total_Coin_Number_Submissions_FUD(Group_1, c(160, 180))

mean(all_coin_data$Total_Coin_Number_Submissions[which(all_coin_data$Day.1 <= 200 & all_coin_data$Day.1 >= 180 & all_coin_data$Coin == "steem" )])

#Definitely looking like those humps mean something. Fud Submissions seem to be 
#about the coin seem to be as it starts to fall.


#200-300 Hump
par(mfrow=c(2,2))
plot(x = all_coin_data$Day.1[which(all_coin_data$Day.1 >= 200 & all_coin_data$Day.1 < 300)], y = all_coin_data$Total_Number_Comments[which(all_coin_data$Day.1 >= 200 & all_coin_data$Day.1 < 300)], ylab= "comments", xlab = "day")

plot(x = all_coin_data$Day.1[which(all_coin_data$Day.1 >= 220 & all_coin_data$Day.1 < 280)], y = all_coin_data$Total_Number_Comments[which(all_coin_data$Day.1 >= 220 & all_coin_data$Day.1 < 280)], ylab= "comments", xlab = "day")

plot(x = all_coin_data$Day.1[which(all_coin_data$Day.1 >= 230 & all_coin_data$Day.1 < 270)], y = all_coin_data$Total_Number_Comments[which(all_coin_data$Day.1 >= 230 & all_coin_data$Day.1 < 270)], ylab= "comments", xlab = "day")

plot(x = all_coin_data$Day.1[which(all_coin_data$Day.1 >= 240 & all_coin_data$Day.1 < 260)], y = all_coin_data$Total_Number_Comments[which(all_coin_data$Day.1 >= 240 & all_coin_data$Day.1 < 260)], ylab= "comments", xlab = "day")
#Once again, you see that the 20 day one you don't see that hump, but as soon as you widen the
#scope you can see that this is a hump.

#After looking

##Helps to build the binary responses
build_response = function(all_coin_data, coins){
row_data_response = c()
for(i in 3:365){
for(j in seq(2, length(coins), 2)){
day_curr = which(all_coin_data$Day.1 == i & all_coin_data$Coin == coins[j])
day_before = which(all_coin_data$Day.1 == (i-1) & all_coin_data$Coin == TOP_COINS[j])
if((all_coin_data$Close[day_curr]-all_coin_data$Close[day_before]) > 0){
#print("Went up")
row_data_response = c(row_data_response, 1)
}else{
#print("Went down")
row_data_response = c(row_data_response, 0)
}
}
}
row_data_response = c(rep(0, length(coins)/2),row_data_response)
return(row_data_response)
}

all_coin_data = cbind(build_response(all_coin_data, TOP_COINS), all_coin_data)
colnames(all_coin_data)[1] = "Response"


compute_errors = function(truth, guess){
output = c()
for(i in 1:length(guess)){
if(guess[i] == 1 && truth[i] == 1){
output = c(output, 1)
}

if(truth[i] == 1 && guess[i] == 0){
output = c(output, 2)
}

if(truth[i] == 0  && guess[i] == 1){
output = c(output, 3)
}

if(truth[i] == 0  && guess[i] == 0){
output = c(output, 4)
}
}
return(output)

}


##Fitting A Model


build_all_model = function(all_coin_data, verbose=FALSE){
##Fill NA with zero
all_coin_data[is.na(all_coin_data)] <- 0

##Exclude the price data
data_refined = all_coin_data[,-c(1,2,c(5:8))]
response = all_coin_data[,1]

##Exclude the coin factor
Sigma = cor(data_refined[,-1])

evec = eigen(Sigma)$vectors 
eval = eigen(Sigma)$values

##Exclude the coin factor
pr.out = prcomp(data_refined[,-1],  scale=TRUE)
pve = eval/sum(eval)
yvec = cumsum(pve)

if(verbose){
##Find 80% cut off
plot(yvec, type='b', xlab = "Principal Components",ylab = "Cumulative variance", main = "",ylim=c(0,1))
}

##Lets take first 5 about 80%

##Compute the data value for principle component #1 at each data point, exclude coin
pca_var = data.matrix(data_refined[,-1])%*%matrix(pr.out$rotation[,1:5], nrow=27, ncol=5)

model = glm(response~pca_var)

if(verbose){
summary(model)
}

##
outputs = model$coefficients[1] + model$coefficients[2]*pca_var[,1] + model$coefficients[3]*pca_var[,2] + model$coefficients[4]*pca_var[,3] + model$coefficients[5]*pca_var[,4] +model$coefficients[6]*pca_var[,5]

as_binary = as.integer(outputs > .5)

if(verbose){
cat("Accuracy:", mean(as_binary == response))
plot(as_binary)
}
return(c(list(response), list(as_binary), list(model)))
}

full_model =  build_all_model(all_coin_data)

plot(all_coin_data$Total_Number_Comments[1:364], main="Total Number Comments", xlab="Day", ylab="Total Comments", type='l')


##Fitting A Model - Groups

colnames(all_coin_data)

build_group_model = function(Group, all_coin_data, verbose=FALSE){
##Fill NA with zero
all_coin_data[is.na(all_coin_data)] = 0

##Exclude the price data
data_refined = all_coin_data[,-c(1,2,c(5:8))]

response = all_coin_data[which(data_refined$Coin%in%Group),1]

##Find group that is all coins. This is a bit sloppy
if(length(Group) < 9){
data_refined = data_refined[which(data_refined$Coin%in%Group),]
}


##Exclude the coin factor
Sigma = cor(data_refined[,-1])

evec = eigen(Sigma)$vectors 
eval = eigen(Sigma)$values

##Exclude the coin factor
pr.out = prcomp(data_refined[,-1],  scale=TRUE)
pve = eval/sum(eval)
yvec = cumsum(pve)

pr.out
##Find 80% cut off
if(verbose){
plot(yvec, type='b', xlab = "Principal Components",ylab = "Cumulative variance", main = "",ylim=c(0,1))
}

##Lets take first 5 about 80%

##Compute the data value for principle component #1 at each data point, exclude coin
pca_var = data.matrix(data_refined[,-1])%*%matrix(pr.out$rotation[,1:5], nrow=27, ncol=5)

model = glm(response~pca_var)

if(verbose){
summary(model)
}

##
outputs = model$coefficients[1] + model$coefficients[2]*pca_var[,1] + model$coefficients[3]*pca_var[,2] + model$coefficients[4]*pca_var[,3] + model$coefficients[5]*pca_var[,4] +model$coefficients[6]*pca_var[,5]

as_binary = as.integer(outputs > .5)

if(verbose){ 
cat("Accuracy:", mean(as_binary == response))
plot(as_binary)
}

## real data, model's guess, model
return(c(list(response), list(as_binary), list(model)))
}

Group_1 = c(c("eth", "ethereum"),c("etc", "ethereum classic"), c("rep", "augur"), c("steem", "steem"))
model_1 = build_group_model(Group_1, all_coin_data)

Group_2 = c(c("dash", "dash"),c("ltc", "litecoin"), c("xmr", "monero"), c("maid", "maidsafecoin"))
model_2 = build_group_model(Group_2, all_coin_data)

Group_3 =  c(c("xem", "nem"),c("xrp", "ripple"))
model_3 = build_group_model(Group_3, all_coin_data, verbose=TRUE)

##Bootstrap to try to see the standard error of our estimator
## It looks great to see what we are ~53% correct
## But looking at # of times it says up it is very few.
## Looking at percent of times it says yes and it is actually yes is more showing
## This is near 50% or random

error_full_1 = c()
error_full_2 = c()
error_full_3 = c()
error_full_4 = c()


error_1_1 = c()
error_1_2 = c()
error_1_3 = c()
error_1_4 = c()


error_2_1 = c()
error_2_2 = c()
error_2_3 = c()
error_2_4 = c()


error_3_1 = c()
error_3_2 = c()
error_3_3 = c()
error_3_4 = c()

for(i in 1:50){
  ##Fill NA with zero
  data = all_coin_data[sample(nrow(all_coin_data), replace = TRUE, nrow(all_coin_data)),]
  
  full_model =  build_all_model(data)
  error_full = compute_errors(full_model[[1]], full_model[[2]])
  error_full_1  = c(error_full_1, mean(error_full == 1))
  error_full_2  = c(error_full_2, mean(error_full == 2))
  error_full_3  = c(error_full_3, mean(error_full == 3))
  error_full_4  = c(error_full_4, mean(error_full == 4))
  
  model_1 = build_group_model(Group_1, data)
  error_1 = compute_errors(model_1[[1]], model_1[[2]])
  error_1_1  = c(error_1_1, mean(error_1 == 1))
  error_1_2  = c(error_1_2, mean(error_1 == 2))
  error_1_3  = c(error_1_3, mean(error_1 == 3))
  error_1_4  = c(error_1_4, mean(error_1 == 4))
  
  model_2 = build_group_model(Group_2, data)
  error_2 = compute_errors(model_2[[1]], model_2[[2]])
  error_2_1  = c(error_2_1, mean(error_2 == 1))
  error_2_2  = c(error_2_2, mean(error_2 == 2))
  error_2_3  = c(error_2_3, mean(error_2 == 3))
  error_2_4  = c(error_2_4, mean(error_2 == 4))
  
  
  model_3 = build_group_model(Group_3, data)
  error_3 = compute_errors(model_3[[1]], model_3[[2]])
  error_3_1  = c(error_3_1, mean(error_3 == 1))
  error_3_2  = c(error_3_2, mean(error_3 == 2))
  error_3_3  = c(error_3_3, mean(error_3 == 3))
  error_3_4  = c(error_3_4, mean(error_3 == 4))
}

cat("\n Error Grid: Full Model")
e_f = c(mean(error_full_1), mean(error_full_2), mean(error_full_3), mean(error_full_4))
e_f_sd = 2*c(sd(error_full_1), sd(error_full_2), sd(error_full_3), sd(error_full_4))
matrix(e_f, ncol=2, nrow=2, byrow = TRUE)
matrix(e_f_sd, ncol=2, nrow=2, byrow = TRUE)

cat("\n Error Grid: Model 1")
e_1 = c(mean(error_1_1), mean(error_1_2), mean(error_1_3), mean(error_1_4))
e_1_sd = 2*c(sd(error_1_1), sd(error_1_2), sd(error_1_3), sd(error_1_4))
matrix(e_1, ncol=2, nrow=2, byrow = TRUE)
matrix(e_1_sd, ncol=2, nrow=2, byrow = TRUE)


cat("\n Error Grid: Model 2")
e_2 = c(mean(error_2_1), mean(error_2_2), mean(error_2_3), mean(error_2_4))
e_2_sd = 2*c(sd(error_2_1), sd(error_2_2), sd(error_2_3), sd(error_2_4))
matrix(e_2, ncol=2, nrow=2, byrow = TRUE)
matrix(e_2_sd, ncol=2, nrow=2, byrow = TRUE)


cat("\n Error Grid: Model 3")
e_3 = c(mean(error_3_1), mean(error_3_2), mean(error_3_3), mean(error_3_4))
e_3_sd = 2*c(sd(error_3_1), sd(error_3_2), sd(error_3_3), sd(error_3_4))
matrix(e_3, ncol=2, nrow=2, byrow = TRUE)
matrix(e_3_sd, ncol=2, nrow=2, byrow = TRUE)

#Figures:
####TotalCommentsPosts
par(mfrow=c(1,2))
plot(main="Comments", x = all_coin_data$Day.1, y = all_coin_data$Total_Number_Comments, xlab =  "Day", ylab = "Number of Comments")
plot(main="Posts",x = all_coin_data$Day.1, y = all_coin_data$Total_Number_Submissions, xlab =  "Day", ylab = "Number of Posts")
####AllCoinTrends
par(mfrow=c(2,5))
for (i in coins) {
  plot(y= all_coin_data$Close[which(as.character(all_coin_data$Coin) == i)], x = all_coin_data$Day.1[which(as.character(all_coin_data$Coin) == i)], main = i, type = 'b', pch = 'o', xlab="Day", ylab="closing ")
  
}

Year <- c(1, 365)
####Group1TrendsTotal
plot_dot_closing(Group_1_orginal, Year)
####Group2TrendsTotal
plot_dot_closing(Group_2_orginal, Year)
####Group3TrendsTotal
plot_dot_closing(Group_3_Orginal, Year)
