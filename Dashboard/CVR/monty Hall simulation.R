library(magrittr) # for %>% operator


# strategy argument must be one of "always_switch" or "never_switch"
play_monty <- function(ndoors = 3, strategy = "always_switch") {
  if(ndoors < 3) stop("number of doors must be at least 3")
  doors <- paste0("door_", 1:ndoors)
  prize_door <- sample(doors, 1)
  player_first_choice <- sample(doors, 1)
  host_opens_doors <- setdiff(doors, c(prize_door, player_first_choice)) %>% 
    sample(ndoors - 2)
  player_final_choice <- switch(strategy,
                                "always_switch" = setdiff(doors, c(player_first_choice, host_opens_doors)),
                                "never_switch" = player_first_choice
  )
  ifelse(player_final_choice == prize_door, "Won", "Lost")
}


set.seed(123) # for reproducible results


# play 1000 times with 3 doors and "always_switch" strategy
replicate(10000, play_monty(ndoors = 3, "always_switch")) %>% 
  table() %>% 
  prop.table()


# result
# Lost   Won 
# 0.346 0.654 


# play 1000 times with 3 doors and "never_switch" strategy
replicate(1000, play_monty(ndoors = 3, "never_switch")) %>% 
  table() %>% 
  prop.table()


# result
# Lost   Won 
# 0.676 0.324 


# play 1000 times with 100 doors and "always_switch" strategy which gives 99% expected wins
replicate(1000, play_monty(ndoors = 100, "always_switch")) %>% 
  table() %>% 
  prop.table()


# result
# Lost   Won 
# 0.011 0.989 