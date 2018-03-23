#File path
setwd("~/GitHub/markov-processes")

source('transition_matrix.R')
source('value_iteration.R')
source('simulation_game.R')


#---------Création des stratégies et matrice de résultats-------------------

opt_policy_rule1 <- v_iter(p_normal1,p_secure1,p_risk1,C)[[1]]
opt_policy_rule2 <- v_iter(p_normal2,p_secure2,p_risk2,C)[[1]]
always_dice1 <- c(rep(1,15))
always_dice2 <- c(rep(2,15))
always_dice3 <- c(rep(3,15))
set.seed(1234)  # pour avoir toujours la même stratégie random
random_dice <- sample(c(1,2,3),15,replace=TRUE)
policy_list <- list(opt_policy_rule1,opt_policy_rule2,always_dice1,always_dice2,always_dice3,random_dice)

result_matrix_rule1 <- matrix(ncol=5,nrow=15)
result_matrix_rule2 <- matrix(ncol=5,nrow=15)
colnames(result_matrix_rule1) <- c("Optimal policy","Dice 1","Dice 2", "Dice 3", "Random dice")
colnames(result_matrix_rule2) <- c("Optimal policy","Dice 1","Dice 2", "Dice 3", "Random dice")

#---------Simulation des jeux-------------------
# simulation des jeux pour la règle 1
for(i in 1 : 5){
  for ( j in 1 : 15){
    result_matrix_rule1[j,i] <- mean(simulation_game(1,policy_list[[i]],j))
  }
}
write.table(result_matrix_rule1,"result_matrix_rule1.txt")

# simulation des jeux pour la règle 2
for(i in 1 : 5){
  for ( j in 1 : 15){
    result_matrix_rule1[j,i] <- mean(simulation_game(1,policy_list[[i]],j))
  }
}
write.table(result_matrix_rule2,"result_matrix_rule2.txt")

