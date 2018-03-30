#---------------ALGORITHME D'ITERATION DE LA VALEUR------------------------
# Vecteur coût de depart
Cost <-c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,0)
Cost_prison <-c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0)

v_iter=function(S,N,R,cost) {
  C<- cost
  V<- cost
  a<-c(rep(0,length(cost)))

  for( t  in 1 : 1000){
    for (i in 1 : length(cost) ){
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


