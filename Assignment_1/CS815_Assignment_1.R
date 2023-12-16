library(genalg)
library(ggplot2)

#Assignment 1a: Implement a GA solution to the OneMAx problem.

chromos1<- sample(rep(0:1,each=25)) #The problem could be represented by a random binary vector.
chromos1
max_score <-25 #We set the maximum score to 25 whihc would be the maximum if all 25 positions were "1".

#The fitness function is represented by the sum of all entries. The higher the sum the better the fitness
#If the sum exceeds 25 the penalty factor -1 has been assigned.This should not happen however because the chromosome size has been restricted to 25.
#Because we have used the ga algorithm the fitness function aims to maximise. This is why we have taken the sum.
f <- function(chromos1){
  if (sum(chromos1) > 25) {
    -1}
  else {val = sum(chromos1)}
  
}

GAmodel <- ga("binary", fitness = f, nBits = 25, 
          popSize = 100, maxiter = 100, seed = 1, monitor = TRUE)


plot(GAmodel)
#From the plot we see that the model converges within 40 generations to the best fitness value and while the mean of the population increases in a slower pace
# it is just for a matter of time to evolve towards the best value too.
summary(GAmodel)


#####Assignment 1b.


dataset <- data.frame(item = c("pocketknife", "beans", "potatoes", "onions", "sleeping bag", "rope", "compass"), 
survivalpoints = c(10, 20, 15, 2, 30, 10, 30), 
weight = c(1, 5, 10, 1, 7, 5, 1))
weightlimit <- 20
#The problem can be represenated in a binary form where 1 indicates the item has been chosen and 0 it has not.
#A representative example would be:
chromosome = c(1, 1, 0, 1, 0, 0, 0) #where only a pocketnife, beans and onions has been chosen. However, this may not be the optimal solution.
#To adress that, the fitness function aims to maximize the survival points and penalize any solution that exceeds the weight.
#When using the GA libary we aim to maximise the fitness, therefor we assign a positive value for fitness score  "current_solution_survivalpoints". 
#As per comparison In genalg algorithm we aim to minimise the fitness function and this is why we assigned the negative value of "current_solution_survivalpoints"

evalFunc <- function(x){
  current_solution_survivalpoints <- x %*% dataset$survivalpoints
  current_solution_weight <- x %*% dataset$weight
  
  if (current_solution_weight > weightlimit) 
    return(0) else return(current_solution_survivalpoints)
}

GAmodel1 <- ga( nBits = 7, type = "binary", fitness = evalFunc, upper = weightlimit, monitor = TRUE, seed =1)

#From the summary we see that the best solution is the one where we take all items with us apart from "potatoes".
summary(GAmodel1)

plot(GAmodel1)
#the model very quickly finds the best fitness value and mean of the population  reaches its maximum progressively, within 70 generations. 
#There is a level of oscillation , but the population mean stabilises around the maximum value.

solution = c(1, 1, 0, 1, 1, 1, 1)
dataset[solution == 1, ] #When we assign the vector "solution" with the optimal combination, we can then retrieve the weight and the total survival points of that combination

cat(paste(solution %*% dataset$survivalpoints, "/", sum(dataset$survivalpoints), "points with a weight of ", solution %*% dataset$weight))
#We see that the optimal solution accumulates 102points out of a potential maximum of 117 points, for a maximum weight of 20kgs.




