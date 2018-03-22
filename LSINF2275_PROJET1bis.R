#---------------CREATION DES FONCTIONS-------------------------------
#test
matpowert<-function(a,t){
  p<-a
  for (i in 1 : t){
    p<-p%*%a
    
  }
  return(p)
}


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

handle_three <- function(dice){
  
  newposition <- NULL
  
  random <- sample(c(1,2),1)
  
  if (random==1 && dice > 0 ){
    
    newposition <- 3 + dice
  }
  
  else if (random == 2 && dice > 0){
    
    newposition <- 11 + (dice-1)
  }
  
  else{ 
    
    newposition = 3
  }
  
  return(newposition)
  
}

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

#---------------MATRIce DE TRANSITION , rule 1 (win as soon as you have passed the arrival square)------------------------------ 

# piege sur le sur le 7(je recule de 3) et piege sur le 13 je reviens au départ
p_secure1<-matrix(ncol=15,nrow=15)
p_secure1 <-rbind(
  c(0.5,0.5,0,0,0,0,0,0,0,0,0,0,0,0,0),
  c(0,0.5,0.5,0,0,0,0,0,0,0,0,0,0,0,0),
  c(0,0,0.5,0.25,0,0,0,0,0,0,0.25,0,0,0,0),
  c(0,0,0,0.5,0.5,0,0,0,0,0,0,0,0,0,0),
  c(0,0,0,0,0.5,0.5,0,0,0,0,0,0,0,0,0),
  c(0,0,0,0,0,0.5,0.5,0,0,0,0,0,0,0,0),
  c(0,0,0,0,0,0,0.5,0.5,0,0,0,0,0,0,0),
  c(0,0,0,0,0,0,0,0.5,0.5,0,0,0,0,0,0),
  c(0,0,0,0,0,0,0,0,0.5,0.5,0,0,0,0,0),
  c(0,0,0,0,0,0,0,0,0,0.5,0,0,0,0,0.5),
  c(0,0,0,0,0,0,0,0,0,0,0.5,0.5,0,0,0),
  c(0,0,0,0,0,0,0,0,0,0,0,0.5,0.5,0,0),
  c(0,0,0,0,0,0,0,0,0,0,0,0,0.5,0.5,0),
  c(0,0,0,0,0,0,0,0,0,0,0,0,0,0.5,0.5),
  c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,1))

p_normal1<-matrix(nrow=15,ncol=15)
p_normal1 <-rbind(
  c(1/3,1/3,1/3,0,0,0,0,0,0,0,0,0,0,0,0),
  c(0,1/3,1/3,1/3,0,0,0,0,0,0,0,0,0,0,0),
  c(0,0,1/3,1/6,1/6,0,0,0,0,0,1/6,1/6,0,0,0),
  c(0,0,0,1/3,1/3,1/3,0,0,0,0,0,0,0,0,0),
  c(0,0,0,1/6,1/3,1/3,1/6,0,0,0,0,0,0,0,0),
  c(0,0,0,1/6,0,1/3,1/6,1/3,0,0,0,0,0,0,0),
  c(0,0,0,1/6,0,0,1/6,1/3,1/3,0,0,0,0,0,0),
  c(0,0,0,0,0,0,0,1/3,1/3,1/3,0,0,0,0,0),
  c(0,0,0,0,0,0,0,0,1/3,1/3,0,0,0,0,1/3),
  c(0,0,0,0,0,0,0,0,0,1/3,0,0,0,0,2/3),
  c(1/6,0,0,0,0,0,0,0,0,0,1/3,1/3,1/6,0,0),
  c(1/6,0,0,0,0,0,0,0,0,0,0,1/3,1/6,1/3,0),
  c(1/6,0,0,0,0,0,0,0,0,0,0,0,1/6,1/3,1/3),
  c(0,0,0,0,0,0,0,0,0,0,0,0,0,1/3,2/3),    
  c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,1))


p_risk1<-matrix(nrow=15,ncol=15)
p_risk1 <-rbind(
  c(1/4,1/4,1/4,1/4,0,0,0,0,0,0,0,0,0,0,0),
  c(0,1/4,1/4,1/4,1/4,0,0,0,0,0,0,0,0,0,0),
  c(1/8,0,1/4,1/8,1/8,1/8,0,0,0,0,1/8,1/8,0,0,0),
  c(0,0,0,2/4,1/4,1/4,0,0,0,0,0,0,0,0,0),
  c(0,0,0,1/4,1/4,1/4,0,1/4,0,0,0,0,0,0,0),
  c(0,0,0,1/4,0,1/4,0,1/4,1/4,0,0,0,0,0,0),
  c(0,0,0,1/4,0,0,0,1/4,1/4,1/4,0,0,0,0,0),
  c(0,0,0,0,0,0,0,1/4,1/4,1/4,0,0,0,0,1/4),
  c(1/4,0,0,0,0,0,0,0,1/4,1/4,0,0,0,0,1/4),
  c(0,0,0,0,0,0,0,0,0,1/4,0,0,0,0,3/4),
  c(1/4,0,0,0,0,0,0,0,0,0,1/4,1/4,0,1/4,0),
  c(1/4,0,0,0,0,0,0,0,0,0,0,1/4,0,1/4,1/4),
  c(1/4,0,0,0,0,0,0,0,0,0,0,0,0,1/4,2/4),
  c(0,0,0,0,0,0,0,0,0,0,0,0,0,1/4,3/4),
  c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,1))




#---------------CREATION MATRIX DE TRANSITION, rule 2 (circle game)------------------------------ 

# piege sur le sur le 7(je recule de 3) et piege sur le 13 je reviens au départ
p_secure2<-matrix(ncol=15,nrow=15)
p_secure2 <-rbind(
  c(0.5,0.5,0,0,0,0,0,0,0,0,0,0,0,0,0),
  c(0,0.5,0.5,0,0,0,0,0,0,0,0,0,0,0,0),
  c(0,0,0.5,0.25,0,0,0,0,0,0,0.25,0,0,0,0),
  c(0,0,0,0.5,0.5,0,0,0,0,0,0,0,0,0,0),
  c(0,0,0,0,0.5,0.5,0,0,0,0,0,0,0,0,0),
  c(0,0,0,0,0,0.5,0.5,0,0,0,0,0,0,0,0),
  c(0,0,0,0,0,0,0.5,0.5,0,0,0,0,0,0,0),
  c(0,0,0,0,0,0,0,0.5,0.5,0,0,0,0,0,0),
  c(0,0,0,0,0,0,0,0,0.5,0.5,0,0,0,0,0),
  c(0,0,0,0,0,0,0,0,0,0.5,0,0,0,0,0.5),
  c(0,0,0,0,0,0,0,0,0,0,0.5,0.5,0,0,0),
  c(0,0,0,0,0,0,0,0,0,0,0,0.5,0.5,0,0),
  c(0,0,0,0,0,0,0,0,0,0,0,0,0.5,0.5,0),
  c(0,0,0,0,0,0,0,0,0,0,0,0,0,0.5,0.5),
  c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,1))

p_normal2<-matrix(nrow=15,ncol=15)
p_normal2 <-rbind(
  c(1/3,1/3,1/3,0,0,0,0,0,0,0,0,0,0,0,0),
  c(0,1/3,1/3,1/3,0,0,0,0,0,0,0,0,0,0,0),
  c(0,0,1/3,1/6,1/6,0,0,0,0,0,1/6,1/6,0,0,0),
  c(0,0,0,1/3,1/3,1/3,0,0,0,0,0,0,0,0,0),
  c(0,0,0,1/6,1/3,1/3,1/6,0,0,0,0,0,0,0,0),
  c(0,0,0,1/6,0,1/3,1/6,1/3,0,0,0,0,0,0,0),
  c(0,0,0,1/6,0,0,1/6,1/3,1/3,0,0,0,0,0,0),
  c(0,0,0,0,0,0,0,1/3,1/3,1/3,0,0,0,0,0),
  c(0,0,0,0,0,0,0,0,1/3,1/3,0,0,0,0,1/3),
  c(1/3,0,0,0,0,0,0,0,0,1/3,0,0,0,0,1/3),
  c(1/6,0,0,0,0,0,0,0,0,0,1/3,1/3,1/6,0,0),
  c(1/6,0,0,0,0,0,0,0,0,0,0,1/3,1/6,1/3,0),
  c(1/6,0,0,0,0,0,0,0,0,0,0,0,1/6,1/3,1/3),
  c(1/3,0,0,0,0,0,0,0,0,0,0,0,0,1/3,1/3),
  c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,1))


p_risk2<-matrix(nrow=15,ncol=15)
p_risk2 <-rbind(
  c(1/4,1/4,1/4,1/4,0,0,0,0,0,0,0,0,0,0,0),
  c(0,1/4,1/4,1/4,1/4,0,0,0,0,0,0,0,0,0,0),
  c(1/8,0,1/4,1/8,1/8,1/8,0,0,0,0,1/8,1/8,0,0,0),
  c(0,0,0,2/4,1/4,1/4,0,0,0,0,0,0,0,0,0),
  c(0,0,0,1/4,1/4,1/4,0,1/4,0,0,0,0,0,0,0),
  c(0,0,0,1/4,0,1/4,0,1/4,1/4,0,0,0,0,0,0),
  c(0,0,0,1/4,0,0,0,1/4,1/4,1/4,0,0,0,0,0),
  c(0,0,0,0,0,0,0,1/4,1/4,1/4,0,0,0,0,1/4),
  c(1/4,0,0,0,0,0,0,0,1/4,1/4,0,0,0,0,1/4),
  c(1/4,1/4,0,0,0,0,0,0,0,1/4,0,0,0,0,1/4),
  c(1/4,0,0,0,0,0,0,0,0,0,1/4,1/4,0,1/4,0),
  c(1/4,0,0,0,0,0,0,0,0,0,0,1/4,0,1/4,1/4),
  c(2/4,0,0,0,0,0,0,0,0,0,0,0,0,1/4,1/4),
  c(1/4,1/4,0,0,0,0,0,0,0,0,0,0,0,1/4,1/4),
  c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,1))




#---------------ALGORITHME D'ITERATION DE LA VALEUR------------------------
# Vecteur coût de depart
C <-c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,0)

# c(seq(14,0,-1))
# c(10,9,8,7,6,5,4,3,2,1,4,3,2,1,0)
# c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,0)
# c(20,18,16,14,12,10,8,6,4,1,8,6,4,1,0)


V<- C
a<-c(rep(0,15))

for( t  in 1 : 1000){
  for (i in 1 : 15 ){
    secure <- C[i]+p_secure1[i,]%*%V
    normal <- C[i]+p_normal1[i,]%*%V
    risk   <- C[i]+p_risk1[i,]%*%V
    
    V[i] <- max(secure,normal,risk)
    
    if(secure < normal & secure < risk){
      a[i]=1
      
    }
    else if (normal < secure & normal < risk) {
      a[i]=2
      
      
    }
    else {
      a[i]=3
      
    }
  }
}
#----- Algorithme pour la règle 2 ("circle game") -------------

V<- C
b<-c(rep(0,15))

for ( t in 1 : 100){
  
  for (i in c(seq(15,1,-1))){
    secure <- C[i]+p_secure2[i,]%*%V
    normal <- C[i]+p_normal2[i,]%*%V
    risk   <- C[i]+p_risk2[i,]%*%V
    
    V[i] <- max(secure,normal,risk)
    
    if(secure < normal & secure < risk){
      b[i]=1
      
    }
    else if (normal < secure & normal < risk) {
      b[i]=2
      
    }
    else {
      b[i]=3
      
    }
  }
}

## ------------ Use of dice 1,2,3 only -----------------

c=rep(1,15)
d=rep(2,15)
e=rep(3,15)


rule1 <- list()
rule1[[1]]=a
rule1[[2]]=c
rule1[[3]]=d
rule1[[4]]=e

rule2 <- list()
rule2[[1]]=b
rule2[[2]]=c
rule2[[3]]=d
rule2[[4]]=e


#---------------SIMULATION DU JEU-------------------------------------------------------------

for (k in c(1,2)){#circle game ou non 
  Rule = k
  
  for (j in 1:4){
    if (Rule ==1){
      policy = rule1[[j]]}
    
    if (Rule == 2){
      policy = rule2[[j]]}
    
    vector_step=c()
    
    for (i in 1 : 10000){
      
      step = 1
      position = 1
      end=FALSE
      
      while (end == FALSE) {
        
        if (policy[position] == 1) {
          dice <- roll_dice(1)
          
          if (position  == 3){
            position <- handle_three(dice)
            
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
            
          } else if(position == 7 && activate_trap()){
            position = 4
            
          } else if(position == 13 && activate_trap()){
            position = 1
            
          } else {
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
          
          else if(position == 7){
            
            position = 4
          }
          
          else if(position == 13){
            
            position =1
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
    
    if (Rule ==1){
      
      if (j == 1) {
        cat("Value Iteration: nombre de coups moyen avec la règle 1: ",mean(vector_step),"\n\n")
      }
      
      else if (j == 2) {
        cat('Nombre de coups moyen en utilisant seulement le dé "secure", avec la règle 1: ',mean(vector_step),"\n")
      }
      else if (j == 3) {
        cat('Nombre de coups moyen en utilisant seulement le dé "normal", avec la règle 1: ',mean(vector_step),"\n")
      }    
      else if (j == 4) {
        cat('Nombre de coups moyen en utilisant seulement le dé "risky", avec la règle 1: ',mean(vector_step),"\n\n")  }
    }
    
    
    if (Rule ==2){
      
      if (j == 1) {
        cat("Value Iteration: nombre de coups moyen avec la règle 2: ",mean(vector_step),"\n\n")
      }
      
      else if (j == 2) {
        cat('Nombre de coups moyen en utilisant seulement le dé "secure", avec la règle 2: ',mean(vector_step),"\n")
      }
      else if (j == 3) {
        cat('Nombre de coups moyen en utilisant seulement le dé "normal", avec la règle 2: ',mean(vector_step),"\n")
      }    
      else if (j == 4) {
        cat('Nombre de coups moyen en utilisant seulement le dé "risky", avec la règle 2: ',mean(vector_step),"\n")
      }
    }
  }
}


# ------THEORETICAL EXPECTED COST WITH VALUE ITERATION-----------






