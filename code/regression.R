#read the data and view the structure
library(xlsx)
kl_df <- read.xlsx("./data.xlsx", sheetName = "Sheet3", header = TRUE)
str(kl_df)
View(kl_df)

#making thew rows
row.names(kl_df) <- kl_df[, 1]
kl_df <- kl_df[-8, -1]
View(kl_df)

#scaling the data
kl_norm <- data.frame(lapply(kl_df, scale))

# considering the month of June
# first lm for # of civilians killed
if(!require("corrplot")) {
  install.packages("corrplot")
  library(corrplot)
}
# getting the correlation matrix
correlation_df<-cor(kl_norm,kl_norm , method="spearman", use="pairwise.complete.obs")

# graph correlation specific columns
corrplot(correlation_df,
         method="color", addCoef.col = "black")

#boxplot of the distribution of the variables
boxplot(kl_df)
boxplot(kl_norm)

#function for developinmg model 1
lm_killed <- function(df) {
  
  lm_k <- lm(Killed~Killed_count, data = df)
  print(summary(lm_k))
  plot(df$Killed~df$Killed_count)
  abline(lm_k,col=2,lwd=3)
  
  predicted <- predict(lm_k, newdata = data.frame(Killed_count = df$Killed_count))
  MAE <- sum(abs(df$Killed - predicted)) / nrow(df)
  print(sprintf("MAE %.4f", MAE))
  MSE <- mean((df$Killed - predicted)^2)
  print(sprintf("MSE %.4f", MSE))
  return (lm_k)
}

#model 1
m1 <- lm_killed(kl_norm)


#removing the month of June
new_df <- kl_df[-3,]
View(new_df)
kl_new_norm <- data.frame(lapply(new_df, scale))

correlation_df<-cor(new_df,new_df , method="spearman", use="pairwise.complete.obs")

# graph correlation specific columns
corrplot(correlation_df,
         method="color", addCoef.col = "black")

#model 1 without the month of June
m2 <- lm_killed(kl_new_norm)

#prediction
avg_ktweets <- mean(kl_df$Killed_count)
predict_future <- predict(m1, data.frame(Killed_count = avg_ktweets))
print(sprintf("With the average number of tweets: %d the predicted number of death in Ukraine is %d per month", round(avg_ktweets), round(predict_future)))

#intercept and coefficient
intercept <- summary(m1)$coefficients[1, 1]
coeff <- summary(m1)$coefficients[2, 1]

#model 2
lm_injured <- function(df) {
  lm_k <- lm(Injured~Injured_count, data = df)
  print(summary(lm_k))
  plot(df$Injured~df$Injured_count)
  abline(lm_k,col=2,lwd=3)
  
  predicted <- predict(lm_k, newdata = data.frame(Injured_count = df$Injured_count))
  MAE <- sum(abs(df$Injured - predicted)) / nrow(df)
  print(sprintf("MAE %.4f", MAE))
  MSE <- mean((df$Injured - predicted)^2)
  print(sprintf("MSE %.4f", MSE))
  return (lm_k)
}
  
#getting the model
m1 <- lm_injured(kl_norm)

# predictions
avg_intweets <- mean(kl_df$Injured_count)
predict_future <- predict(m1, data.frame(Injured_count = avg_intweets))
print(sprintf("With the average number of tweets: %d the predicted number of injured in Ukraine is %d per month", round(avg_intweets), round(predict_future)))

#intercept and the coefficent for the model
intercept <- summary(m1)$coefficients[1, 1]
coeff <- summary(m1)$coefficients[2, 1]
intercept
coeff
