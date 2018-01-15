kappa=0.8
theta=0.25
r=0.01
rho<-0.5
sigma=0.10
s0=100
v0<-0.25
delta<-1/252
time<-seq(0,1,by=delta)

parameters<-list()
parameters[["s0"]]<-s0
parameters[["kappa"]]<-kappa
parameters[["r"]]<-r
parameters[["sigma"]]<-sigma
parameters[["theta"]]<-theta
parameters[["delta"]]<-delta
parameters[["rho"]]<-rho
parameters[["time"]]<-time
parameters[["v0"]]<-v0


varianceMu<-function(kappa,theta,x){
  return(-1*kappa*(theta-max(x,0)))
}

varianceSigma<-function(sigma,x){
  return(sigma*sqrt(max(x,0)))
}

logStockMu<-function(r,x){
  return(r-0.5*x)
}

logStockSigma<-function(x){
  return(sqrt(max(x,0)))
}

eulerHeston<-function(parameters){
  
  s0<-parameters[["s0"]]
  kappa<-parameters[["kappa"]]
  r<-parameters[["r"]]
  sigma<-parameters[["sigma"]]
  theta<-parameters[["theta"]]
  delta<-parameters[["delta"]]
  rho<-parameters[["rho"]]
  time<-parameters[["time"]]
  v0<-parameters[["v0"]]
  
  v<-c()
  s<-c()
  
  v[1]<-v0
  s[1]<-log(s0)
  
  
  for(i in 2:length(time)){
    zv<-rnorm(1)
    z<-rnorm(1)
    zs<-rho*zv+sqrt(1-rho^2)*z
    
    v[i]<-v[i-1]+varianceMu(kappa,theta,v[i-1])*delta+varianceSigma(sigma,v[i-1])*sqrt(delta)*zv
    s[i]<-s[i-1]+logStockMu(r,v[i-1])*delta+logStockSigma(v[i-1])*sqrt(delta)*zs
  }
  
  return(data.frame(stock.price=exp(s),variance=v))
  
}