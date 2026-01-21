library(tidyverse)
library(caret)
library(mlbench)
library(janitor)


df_loan <- read_csv("C:/Users/asus/Documents/Kaggle project/Predict loan payback/train_loan.csv")
df_loan_test <- read_csv("C:/Users/asus/Documents/Kaggle project/Predict loan payback/test_loan.csv")
df_clean_loan <- df_loan %>%
  remove_empty("cols") %>%
  drop_na()
df_clean <- df_loan_test %>%
  remove_empty("cols")

set.seed(42)
n <- nrow(df_clean_loan)
id <- sample(1:n,  size = 0.8*n)

train_df <- df_clean_loan[id, ]
test_df <- df_clean_loan[-id, ]

set.seed(42)
ctrl <- trainControl(method = "boot",
                     number = 5,
                     verboseIter = TRUE)

#debt_to_income_ratio + credit_score + dummy_em + dummy_ed + interest_rate
(model <- train(loan_paid_back ~ debt_to_income_ratio + credit_score + dummy_em + dummy_ed + interest_rate,
                data = train_df,
                method = "knn",
                trControl = ctrl))

p_loan <- predict(model, newdata = test_df)

compare <- test_df %>%
  mutate(predicted_score = p_loan) %>%
  mutate(predicted_score = case_when(
    p_loan >= 0.5 ~ 1,
    TRUE ~ 0
  ))

score <- 0
for (i in 1:nrow(compare)){
  if(compare$loan_paid_back[i] == compare$predicted_score[i]){
    score <- score + 1
  }
}

cat("Accuracy:", (score / nrow(compare)) * 100, "%\n")

Pre_loan <- predict(model, newdata = df_clean)


submission <- df_loan_test %>%
  mutate(loan_paid_back = Pre_loan) %>%
  mutate(loan_paid_back = case_when(
    loan_paid_back >= 0.5 ~ 1,
    TRUE ~ 0
  )) %>%
  select(id,loan_paid_back )

write.csv(submission, 
          "C:/Users/asus/Documents/Kaggle project/Predict loan payback/submission.csv", 
          row.names = FALSE)
