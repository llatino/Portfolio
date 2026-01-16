library(tidyverse)
library(caret)
library(mlbench)
library(MLmetrics)
library(forcats)

df_houseprice <- read_csv("C:/Users/asus/Documents/Kaggle project/House price/houseprice_train.csv")
df_test <- read_csv("C:/Users/asus/Documents/Kaggle project/House price/test.csv")

df_clean_hp <- df_houseprice %>%
  select(Id,OverallQual, GrLivArea, GarageCars,TotalBsmtSF, FullBath, YearBuilt, SalePrice) %>%
  replace(is.na(.), 0)

df_clean_test <- df_test %>%
  select(Id,OverallQual, GrLivArea, GarageCars,TotalBsmtSF, FullBath, YearBuilt)%>%
  replace(is.na(.), 0)

set.seed(42)
 
n <- nrow(df_clean_hp)
id <- sample(1:n,  size = 0.8*n)

train_df <- df_clean_hp[id, ]
test_df <- df_clean_hp[-id, ]

set.seed(42)
ctrl <- trainControl(method = "cv",
                     number = 5)

model <- train(SalePrice ~ OverallQual + GrLivArea + GarageCars + TotalBsmtSF + FullBath + YearBuilt,
               data = train_df,
               method = "rf",
               trControl = ctrl)

test_price <- predict(model, newdata = df_clean_test)

test_price

submission <- df_clean_test %>%
  select(Id) %>%
  mutate(SalePrice = test_price)

write.csv(submission, 
          "C:\\Users\\asus\\Documents\\Kaggle project\\House price\\submission.csv", 
          row.names = FALSE)


## Summary 
# Rsquare > 0.8
# RMSE > MAE: Model cannot predict "high price" or out-liner


#eva_price <- test_df %>%
  #mutate(pre_price = p_price) %>%
  #select(SalePrice,pre_price)


#sum_abs_error = 0

#for (i in 1:nrow(eva_price)){
  #error = abs(eva_price$SalePrice[i] - eva_price$pre_price)
  #sum_abs_error = sum_abs_error + error
#}

#mae_result = sum_abs_error / nrow(eva_price)
#print(paste("Average Error (MAE):", mae_result))
