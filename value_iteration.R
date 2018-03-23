#---------------ALGORITHME D'ITERATION DE LA VALEUR------------------------
# Vecteur coût de depart
C <-c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,0)

v_iter=function(S,N,R,C) {
  V<- C
  a<-c(rep(0,15))
  for( t  in 1 : 1000){
    for (i in 1 : 15 ){
      secure <- C[i]+S[i,]%*%V
      normal <- C[i]+N[i,]%*%V
      risk   <- C[i]+R[i,]%*%V
      
      V[i] <- min(secure,normal,risk)
      
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
  return(list(a,V))
}

a <- v_iter(p_secure1,p_normal1,p_risk1,C)[[1]]
V <- v_iter(p_secure1,p_normal1,p_risk1,C)[[2]]
b <- v_iter(p_secure2,p_normal2,p_risk2,C)[[1]]
Vbis <- v_iter(p_secure2,p_normal2,p_risk2,C)[[2]]
