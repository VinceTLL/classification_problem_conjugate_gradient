
# Classification problem, predicting the type of transmission (1-manual,0-automatic) based on the weight and horse power of the car

# Logistic  function

sigmoidF<-function(z){
  
  g<- 1/(1+exp(-z))
  g
}

# logistc regression cost function 

y<- matrix(mtcars$am)
X<-cbind(1,mtcars$wt,mtcars$hp)

LCostFunction<-function(theta){
  
  
  m<- length(y)
  h<- sigmoidF(X%*%theta)
  J<- (1/m)*(-t(y) %*% log(h) - t((1-y))%*%log(1-h))
  return(J)
  
}

derivate<-function (theta){
  
  grad = (1/m) * (t(X)%*%(h-y))
  grad
}
initial_theta <- rep(0,ncol(X))

#Conjugate gradient optimization

cg <- optim(par=initial_theta,fn=LCostFunction, method  = 'CG', control=c(maxit=500,reltol=1e-8,type=2))

#Gradient descent optimization

gd <- optim(par=initial_theta,fn=LCostFunction)

#prediction 


prediction<- function(z){
  
  g<- sigmoidF(z)
  p<- NULL
  
  for ( i in 1:length(g)){
    if (g[i] >= 0.5 ){
      
      p =c(p, 1)
    }
    else{
      p  =c(p, 0)
    }
    
    
  }
  
  return(p)
}
# Returning coefficient estimates, using conjugate gradient

theta_2<-as.matrix(cg$par)

#Returining coefficient estimates, using gradient descent

theta_3<-as.matrix(gd$par)

#Accuracy using conjugate descent coefficient estimates
res<- cbind(known_results =y,algo_prediction = prediction(X%*% theta_2))
mean(res[,1] == res[,2])

#Accuracy using gradient descent coefficient estimates
res<- cbind(known_results =y,algo_prediction = prediction(X%*% theta_3))
mean(res[,1] == res[,2])
