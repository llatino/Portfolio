menus <- c(haiwaian = 20, cheese = 25, coke = 5)
order_pizza <- function(){
  ## bot: greeting
  print("Hi!")
  u_name <- readline("What's your name? ")
  print(paste0("Welcome to our online restaurant, ", u_name))
  counter <- TRUE
  sum_order_df <- data.frame()
  while (counter) {
    order <- readline("What would you like to order? ")
    if (order == "exit") {
        print("Thank you very much!")
        counter <- FALSE
        print(sum_order_df)
        print(paste0("Total ", (sum(sum_order_df$Price)), " Bath"))
    } else {
    amount <- as.numeric(readline("How many would you like? "))
    price <- menus[order]*amount
    order_df <- data.frame(
      Amount = amount,
      Price = price)
    sum_order_df <- rbind(sum_order_df,order_df)
    }
  }
}
