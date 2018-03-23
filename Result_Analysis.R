#File path
setwd("~/GitHub/markov-processes")
#setwd("C:/Users/p/Documents/GitHub/markov-processes")

source('transition_matrix.R')
source('value_iteration.R')
source('simulation_game.R')

#---------Création des stratégies et matrice de résultats-------------------

opt_policy_rule1 <- v_iter(p_secure1,p_normal1,p_risk1,Cost)[[1]]
opt_policy_rule2 <- v_iter(p_secure2,p_normal2,p_risk2,Cost)[[1]]
always_dice1 <- c(rep(1,15))
always_dice2 <- c(rep(2,15))
always_dice3 <- c(rep(3,15))
set.seed(1234)  # pour avoir toujours la même stratégie random
random_dice <- sample(c(1,2,3),15,replace=TRUE)
policy_list_rule1 <- list(opt_policy_rule1,always_dice1,always_dice2,always_dice3,random_dice)
policy_list_rule2 <- list(opt_policy_rule2,always_dice1,always_dice2,always_dice3,random_dice)

result_matrix_rule1 <- matrix(ncol=5,nrow=14)
result_matrix_rule2 <- matrix(ncol=5,nrow=14)
colnames(result_matrix_rule1) <- c("Optimal policy","Dice 1","Dice 2", "Dice 3", "Random dice")
colnames(result_matrix_rule2) <- c("Optimal policy","Dice 1","Dice 2", "Dice 3", "Random dice")

#---------Simulation des jeux à partir de toutes les cases-------------------

# simulation des jeux pour la règle 1
for(i in 1 : 5){
  for ( j in 1 : 14){
    result_matrix_rule1[j,i] <- mean(simulation_game(1,policy_list_rule1[[i]],j))
  }
}
write.table(result_matrix_rule1,"result_matrix_rule1.txt")

# simulation des jeux pour la règle 2
for(i in 1 : 5){
  for ( j in 1 : 14){
    result_matrix_rule2[j,i] <- mean(simulation_game(2,policy_list_rule2[[i]],j))
  }
}
write.table(result_matrix_rule2,"result_matrix_rule2.txt")

#---------Relire les tables de résultats--------------------------------------

result_matrix_rule1 <- read.table("result_matrix_rule1.txt")
result_matrix_rule2 <- read.table("result_matrix_rule2.txt")

#---------Resultats à partir de la case 1 : Boxplots-Règle 1------------------------

windows()
par(mfrow=c(1,5))
for( i in 1 : 5){
  boxplot(simulation_game(1,policy_list_rule1[[1]],1),main=colnames(result_matrix_rule1)[i],ylab=c(0,250))
}

#---------Resultats à partir de la case 1 : Boxplots-Règle 2------------------------

windows()
par(mfrow=c(1,5))
for( i in 1 : 5){
  boxplot(simulation_game(2,policy_list_rule2[[1]],1),main=colnames(result_matrix_rule2)[i],ylab=c(0,250))
  
}

