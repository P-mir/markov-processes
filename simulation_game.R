#-----FONCTIONS NECESSAIRES----------------------------

#matrices de transition d'ordre t
matpowert<-function(a,t){ 
  p<-a
  for (i in 1 : t){
    p<-p%*%a
    
  }
  return(p)
}

# fonction pour jeter le dé
roll_dice <- function(typeOfDice){
  dice <- NULL
  if (typeOfDice == 1){
    
    dice <- sample(c(0,1),1)
  }
  else if (typeOfDice==2){
    
    dice <- sample(c(0,1,2),1)
  }
  else {
    dice <- sample(c(0,1,2,3),1)
  }
  return(dice)
}

#fonction pour gérer la case 3
handle_three <- function(dice){
  
  newposition <- NULL
  random <- sample(c(1,2),1)
  
  if (random==1 && dice > 0 ){
    newposition <- 3 + dice
  }
  else if (random == 2 && dice > 0){
    newposition <- 12 + (dice-1)
  }
  else{ 
    newposition = 3
  }
  return(newposition)
}

#fonction pour gérer l'activation des pièges
activate_trap <- function (position) {
  
  random <- sample(c(1,2),1)
  
  if (random == 1){
    trap <-TRUE
  }
  
  if (random == 2){
    trap <- FALSE
  }
  return(trap)
}


#-----SIMULATION DU JEU AVEC LA PRISON-------------------------
simulation_game_prison<-function(R,S,P) {
  
  rule<-R
  policy<-S
  start <- P
  vector_step=c()
  
  for (i in 1 : 10000){
    step = 0
    position = start
    end=FALSE
    
    while (end == FALSE) {
      if (policy[position] == 1) {
        dice<- roll_dice(1)
        if (position  == 3){
          position <- handle_three(dice)
        }
        else if (position == 11 && dice == 1){
          position <- 16
        }
        else {
          position <- position + dice
        }
        step = step +1
      }
      else if (policy[position] == 2){
        dice <- roll_dice(2)
        if(position == 3){
          position <- handle_three(dice)
        }
        else if (position == 9 && dice == 2 && activate_trap()==FALSE ){
          position <- 16
        }
        else if (position == 9 && dice == 1 && activate_trap()==FALSE ){
          position <- 11
        }
        else if (position == 10 && dice == 2){
          position <- 16
        }
        else if (position == 11 && dice >0){
          position <-16 +(dice-1)
        }
        else {
          position <- position + dice
          if( position == 7 && activate_trap()){
            
            position = 4
          }
          else if (position == 13 && activate_trap()){
            position = 1
          }
        }
        step = step +1
      }
      else {
        dice <- roll_dice(3)
        
        if (position == 3){
          position <- handle_three(dice)
        }
        else if (position == 10 && dice >= 2){
          position = 16 + dice -2
        }
        else if (position == 11 && dice >= 1){
          position = 16 + dice -1
        }
        else {
          
          position <- position + dice
          
          if (position == 7){
            position = 4
          }
          else if(position ==13){
            position =1
          }
        }
        step = step +1
      }
      if (position == 16 ) {
        end = TRUE
        vector_step[i] = step
      }
      if(position > 16){
        if (rule==2){
          position = position -16
        }
        else if (rule==1){
          end = TRUE
          vector_step[i] = step
        }
      }
    }
  }
  
  return(vector_step)
}

#-----SIMULATION DU JEU SANS LA PRISON-------------------------
simulation_game<-function(R,S,P) {
  
  rule<-R
  policy<-S
  start <- P
  vector_step=c()
  
  for (i in 1 : 10000){
    step = 0
    position = start
    end=FALSE
    
    while (end == FALSE) {
      if (policy[position] == 1) {
        dice<- roll_dice(1)
        if (position  == 3){
          position <- handle_three(dice)
        }
        else if (position == 10 && dice == 1){
          position <- 15
        }
        else {
          position <- position + dice
        }
        step = step +1
      }
      else if (policy[position] == 2){
        dice <- roll_dice(2)
        if(position == 3){
          position <- handle_three(dice)
        }
        else if (position == 9 && dice == 2 ){
          position <- 15
        }
        else if (position == 10 && dice >0){
          position <-15 +(dice-1)
        }
        else {
          position <- position + dice
          if( position == 7 && activate_trap()){
            
            position = 4
          }
          else if (position == 13 && activate_trap()){
            position = 1
          }
        }
        step = step +1
      }
      else {
        dice <- roll_dice(3)
        
        if (position == 3){
          position <- handle_three(dice)
        }
        
        else if (position == 8 && dice ==3){
          position == 15
        }
        else if (position == 9 && dice>=2){
          position = 15 + dice -2
        }
        else if (position == 10 && dice >= 1){
          position = 15 + dice -1
        }
        else {
          
          position <- position + dice
          
          if (position == 7){
            position = 4
            
          }
          if(position ==13){
            position =1
          }
        }
        step = step +1
      }
      if (position == 15 ) {
        end = TRUE
        vector_step[i] = step
      }
      if(position > 15){
        if (Rule==2){
          position = position -15 
        }
        else if (Rule==1){
          end = TRUE
          vector_step[i] = step
        }
      }
    }
    
  }
  
  return(vector_step)
}
