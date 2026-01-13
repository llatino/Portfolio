rsps <- c("rock", "scissor", "paper")
play_game <- function(){
round <- 0
my_score <- 0
bot_score <- 0
  while (round < 5) {
    my <- readline("I choose:  ")
    bot <- sample(rsps,1)
    for (rsp in rsps){
      if (my == rsp)
        if( my == "rock" & bot == "scissor"){
          my_score = my_score + 1
          round = round  + 1
        }
        else if( my == "scissor" & bot == "paper"){
          my_score = my_score + 1
          round = round  + 1
        }
        else if( my == "paper" & bot == "rock"){
          my_score = my_score + 1
          round = round  + 1
        }
        else if (my == bot){
          round = round
        }
        else{
          bot_score = bot_score + 1
          round = round  + 1
        }
      }
    print(paste0("I choose ", my , " and Bot choose ", bot))
    print(paste0("My score is ", my_score , " Bot score is ", bot_score))
  }
  if(my_score > bot_score){
    print("YOU WIN!!")
  }
  else{
    print("BOT WIN!!")
  }
}



