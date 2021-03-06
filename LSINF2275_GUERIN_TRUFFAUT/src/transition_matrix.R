#---------------MATRICE DE TRANSITION-REGLE 1-PRISON------------------------------ 

# piege sur le sur le 7(je recule de 3) et piege sur le 13 je reviens au d�part
#prison
p_secure1_prison<-matrix(ncol=16,nrow=16)
p_secure1_prison <-rbind(
  c(0.5,0.5,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
  c(0,0.5,0.5,0,0,0,0,0,0,0,0,0,0,0,0,0),
  c(0,0,0.5,0.25,0,0,0,0,0,0,0,0.25,0,0,0,0),
  c(0,0,0,0.5,0.5,0,0,0,0,0,0,0,0,0,0,0),
  c(0,0,0,0,0.5,0.5,0,0,0,0,0,0,0,0,0,0),
  c(0,0,0,0,0,0.5,0.5,0,0,0,0,0,0,0,0,0),
  c(0,0,0,0,0,0,0.5,0.5,0,0,0,0,0,0,0,0),
  c(0,0,0,0,0,0,0,0.5,0.5,0,0,0,0,0,0,0),  
  c(0,0,0,0,0,0,0,0,0.5,0,0.5,0,0,0,0,0),
  c(0,0,0,0,0,0,0,0,0,0.5,0.5,0,0,0,0,0),
  c(0,0,0,0,0,0,0,0,0,0,0.5,0,0,0,0,0.5),
  c(0,0,0,0,0,0,0,0,0,0,0,0.5,0.5,0,0,0),
  c(0,0,0,0,0,0,0,0,0,0,0,0,0.5,0.5,0,0),
  c(0,0,0,0,0,0,0,0,0,0,0,0,0,0.5,0.5,0),
  c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0.5,0.5),
  c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1))

p_normal1_prison<-matrix(nrow=16,ncol=16)
p_normal1_prison <-rbind(
  c(1/3,1/3,1/3,0,0,0,0,0,0,0,0,0,0,0,0,0),
  c(0,1/3,1/3,1/3,0,0,0,0,0,0,0,0,0,0,0,0),
  c(0,0,1/3,1/6,1/6,0,0,0,0,0,0,1/6,1/6,0,0,0),
  c(0,0,0,1/3,1/3,1/3,0,0,0,0,0,0,0,0,0,0),
  c(0,0,0,1/6,1/3,1/3,1/6,0,0,0,0,0,0,0,0,0),
  c(0,0,0,1/6,0,1/3,1/6,1/3,0,0,0,0,0,0,0,0),
  c(0,0,0,1/6,0,0,1/6,1/3,1/3,0,0,0,0,0,0,0), 
  c(0,0,0,0,0,0,0,1/3,1/3,0,1/3,0,0,0,0,0),   # si la case prison ne s'active pas le joueur passe de 8 � 10
  c(0,0,0,0,0,0,0,0,1/3,1/6,1/3,0,0,0,0,1/6), 
  c(0,0,0,0,0,0,0,0,0,1/3,1/3,0,0,0,0,1/3), #case supl�mentaire 
  c(0,0,0,0,0,0,0,0,0,0,1/3,0,0,0,0,2/3),
  c(1/6,0,0,0,0,0,0,0,0,0,0,1/3,1/3,1/6,0,0),
  c(1/6,0,0,0,0,0,0,0,0,0,0,0,1/3,1/6,1/3,0),
  c(1/6,0,0,0,0,0,0,0,0,0,0,0,0,1/6,1/3,1/3),
  c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,1/3,2/3),    
  c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1))


p_risk1_prison<-matrix(nrow=16,ncol=16)
p_risk1_prison <-rbind(
  c(1/4,1/4,1/4,1/4,0,0,0,0,0,0,0,0,0,0,0,0),
  c(0,1/4,1/4,1/4,1/4,0,0,0,0,0,0,0,0,0,0,0),
  c(1/8,0,1/4,1/8,1/8,1/8,0,0,0,0,0,1/8,1/8,0,0,0),
  c(0,0,0,2/4,1/4,1/4,0,0,0,0,0,0,0,0,0,0),
  c(0,0,0,1/4,1/4,1/4,0,1/4,0,0,0,0,0,0,0,0),
  c(0,0,0,1/4,0,1/4,0,1/4,1/4,0,0,0,0,0,0,0),
  c(0,0,0,1/4,0,0,0,1/4,1/4,0,1/4,0,0,0,0,0),
  c(0,0,0,0,0,0,0,1/4,1/4,0,1/4,0,0,0,0,1/4),
  c(0,0,0,0,0,0,0,0,1/4,1/4,1/4,0,0,0,0,1/4),
  c(0,0,0,0,0,0,0,0,0,1/4,1/4,0,0,0,0,2/4),
  c(0,0,0,0,0,0,0,0,0,0,1/4,0,0,0,0,3/4),
  c(1/4,0,0,0,0,0,0,0,0,0,0,1/4,1/4,0,1/4,0),
  c(1/4,0,0,0,0,0,0,0,0,0,0,0,1/4,0,1/4,1/4),
  c(1/4,0,0,0,0,0,0,0,0,0,0,0,0,0,1/4,2/4),
  c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,1/4,3/4),
  c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1))

#---------------MATRICE DE TRANSITION-REGLE 2(circle game)-PRISON------------------------------ 

# piege sur le sur le 7(je recule de 3) et piege sur le 13 je reviens au d�part
p_secure2_prison<-matrix(ncol=16,nrow=16)
p_secure2_prison <-rbind(
  c(0.5,0.5,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
  c(0,0.5,0.5,0,0,0,0,0,0,0,0,0,0,0,0,0),
  c(0,0,0.5,0.25,0,0,0,0,0,0,0,0.25,0,0,0,0),
  c(0,0,0,0.5,0.5,0,0,0,0,0,0,0,0,0,0,0),
  c(0,0,0,0,0.5,0.5,0,0,0,0,0,0,0,0,0,0),
  c(0,0,0,0,0,0.5,0.5,0,0,0,0,0,0,0,0,0),
  c(0,0,0,0,0,0,0.5,0.5,0,0,0,0,0,0,0,0),
  c(0,0,0,0,0,0,0,0.5,0.5,0,0,0,0,0,0,0),   
  c(0,0,0,0,0,0,0,0,0.5,0,0.5,0,0,0,0,0),
  c(0,0,0,0,0,0,0,0,0,0.5,0.5,0,0,0,0,0),
  c(0,0,0,0,0,0,0,0,0,0,0.5,0,0,0,0,0.5),
  c(0,0,0,0,0,0,0,0,0,0,0,0.5,0.5,0,0,0),
  c(0,0,0,0,0,0,0,0,0,0,0,0,0.5,0.5,0,0),
  c(0,0,0,0,0,0,0,0,0,0,0,0,0,0.5,0.5,0),
  c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0.5,0.5),
  c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1))

p_normal2_prison<-matrix(nrow=16,ncol=16)
p_normal2_prison <-rbind(
  c(1/3,1/3,1/3,0,0,0,0,0,0,0,0,0,0,0,0,0),
  c(0,1/3,1/3,1/3,0,0,0,0,0,0,0,0,0,0,0,0),
  c(0,0,1/3,1/6,1/6,0,0,0,0,0,0,1/6,1/6,0,0,0),
  c(0,0,0,1/3,1/3,1/3,0,0,0,0,0,0,0,0,0,0),
  c(0,0,0,1/6,1/3,1/3,1/6,0,0,0,0,0,0,0,0,0),
  c(0,0,0,1/6,0,1/3,1/6,1/3,0,0,0,0,0,0,0,0),
  c(0,0,0,1/6,0,0,1/6,1/3,0,1/3,0,0,0,0,0,0), 
  c(0,0,0,0,0,0,0,1/3,1/3,0,1/3,0,0,0,0,0),   
  c(0,0,0,0,0,0,0,0,1/3,1/6,1/3,0,0,0,0,1/6), 
  c(0,0,0,0,0,0,0,0,0,1/3,1/3,0,0,0,0,1/3), #case supl�mentaire 
  c(1/3,0,0,0,0,0,0,0,0,0,1/3,0,0,0,0,1/3),
  c(1/6,0,0,0,0,0,0,0,0,0,0,1/3,1/3,1/6,0,0),
  c(1/6,0,0,0,0,0,0,0,0,0,0,0,1/3,1/6,1/3,0),
  c(1/6,0,0,0,0,0,0,0,0,0,0,0,0,1/6,1/3,1/3),
  c(1/3,0,0,0,0,0,0,0,0,0,0,0,0,0,1/3,1/3),    
  c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1))


p_risk2_prison<-matrix(nrow=16,ncol=16)
p_risk2_prison <-rbind(
  c(1/4,1/4,1/4,1/4,0,0,0,0,0,0,0,0,0,0,0,0),
  c(0,1/4,1/4,1/4,1/4,0,0,0,0,0,0,0,0,0,0,0),
  c(1/8,0,1/4,1/8,1/8,1/8,0,0,0,0,0,1/8,1/8,0,0,0),
  c(0,0,0,2/4,1/4,1/4,0,0,0,0,0,0,0,0,0,0),
  c(0,0,0,1/4,1/4,1/4,0,1/4,0,0,0,0,0,0,0,0),
  c(0,0,0,1/4,0,1/4,0,1/4,1/4,0,0,0,0,0,0,0),
  c(0,0,0,1/4,0,0,0,1/4,1/4,0,1/4,0,0,0,0,0),
  c(0,0,0,0,0,0,0,1/4,1/4,0,1/4,0,0,0,0,1/4),
  c(0,0,0,0,0,0,0,0,1/4,1/4,1/4,0,0,0,0,1/4),
  c(1/4,0,0,0,0,0,0,0,0,1/4,1/4,0,0,0,0,1/4),
  c(1/4,1/4,0,0,0,0,0,0,0,0,1/4,0,0,0,0,1/4),
  c(1/4,0,0,0,0,0,0,0,0,0,0,1/4,1/4,0,1/4,0),
  c(1/4,0,0,0,0,0,0,0,0,0,0,0,1/4,0,1/4,1/4),
  c(2/4,0,0,0,0,0,0,0,0,0,0,0,0,0,1/4,1/4),
  c(1/4,1/4,0,0,0,0,0,0,0,0,0,0,0,0,1/4,1/4),
  c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1))

#---------------MATRICE DE TRANSITION-REGLE 1- SANS PRISON------------------------------ 

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
  c(0,0,0,0,0,0,0,0,1/4,1/4,0,0,0,0,2/4),
  c(0,0,0,0,0,0,0,0,0,1/4,0,0,0,0,3/4),
  c(1/4,0,0,0,0,0,0,0,0,0,1/4,1/4,0,1/4,0),
  c(1/4,0,0,0,0,0,0,0,0,0,0,1/4,0,1/4,1/4),
  c(1/4,0,0,0,0,0,0,0,0,0,0,0,0,1/4,2/4),
  c(0,0,0,0,0,0,0,0,0,0,0,0,0,1/4,3/4),
  c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,1))


#---------------MATRICE DE TRANSITION-REGLE 2- SANS PRISON------------------------------ 

# piege sur le sur le 7(je recule de 3) et piege sur le 13 je reviens au d�part
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