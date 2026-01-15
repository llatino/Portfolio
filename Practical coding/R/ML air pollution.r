library(tidyverse)
library(caret)
library(mlbench)
library(MLmetrics)
library(forcats)

df_pollution <- read_csv("C:/Users/asus/Documents/Kaggle project/Air Pollution/updated_pollution_dataset.csv")
df_clean_pullution <- df_pollution %>%
  drop_na() %>%
  data.frame()

set.seed(42)

n <- nrow(df_clean_pullution)
id <- sample(1:n,  size = 0.8*n)

train_df <- df_clean_pullution[id, ]
test_df <- df_clean_pullution[-id, ]

ctrl <- trainControl(method = "cv",
                     number = 5
                     )

k_grid <- data.frame(k = c(3,5,7,9))

(model <- train(Air.Quality ~.,
               data = train_df,
               method = "knn",
               tuneGrid = k_grid,
               trControl = ctrl,
               preProcess = c("center", "scale")))

Air <- predict(model, newdata = test_df)

compare_air <- test_df
compare_air$pre_Air.Quality <- Air


score <- 0

for (i in 1:1000){
  if(compare_air$Air.Quality[i] == compare_air$pre_Air.Quality[i]){
    score <- score + 1
  }
}

#Correct pecentage

(percent <- (score/1000)*100)
