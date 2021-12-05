
# Case where you always choose 1
N <- 1e6
Car_door <- sample(1:3, size = N, replace = TRUE)

# Say we always choose door 1
shown_door <- sapply(Car_door, FUN = function(x){ifelse(x == 1, sample(2:3,1), ifelse(x == 2, 3, 2))})

alternative_choice <- ifelse(shown_door == 2, 3, 2)

mean(Car_door == alternative_choice)
mean(Car_door == 1)



# Case where you choose a random door:
N <- 1e5
Random_choice <- sample(1:3, size = N, replace = TRUE)
Car_door <- sample(1:3, size = N, replace = TRUE)

shown_door <- sapply(1:N, FUN = function(x){
  doors_not_chosen <- (1:3)[(1:3)!=Random_choice[x]]
  ifelse(Car_door[x] == Random_choice[x], 
         sample(doors_not_chosen,1), 
         ifelse(Car_door[x] == doors_not_chosen[1], doors_not_chosen[2], doors_not_chosen[1]))})

alternative_choice <- sapply(1:N, FUN = function(x){
  ifelse(Random_choice[x] == 1,
         (2:3)[2:3 != shown_door[x]], 
         ifelse(Random_choice[x]==2,
                c(1,3)[c(1,3)!=shown_door[x]],
                (1:2)[(1:2)!=shown_door[x]]))})



mean(Car_door == alternative_choice)
mean(Car_door == 1)
