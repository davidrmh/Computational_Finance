r=0.01
delta=1/252
time=seq(0,5,by=delta)
s0=100
sig=0.25
kappa=0.8
theta=90

parameters<-list()
parameters[["s0"]]<-s0
parameters[["delta"]]<-delta
parameters[["r"]]<-r
parameters[["sigma"]]<-sig
parameters[["kappa"]]<-kappa
parameters[["theta"]]<-theta
parameters[["time"]]<-time

sigmaCIR<-function(x,sig){
  return(sig*sqrt(x))
}

muCIR<-function(kappa,theta,x){
  return(kappa*(theta-x))
}

rungeKuttaCIR<-function(parameters){
  s0<-parameters[["s0"]]
  delta<-parameters[["delta"]]
  r<-parameters[["r"]]
  sig<-parameters[["sigma"]]
  kappa<-parameters[["kappa"]]
  theta<-parameters[["theta"]]
  
  s<-c()
  s[1]=s0
  sHat<-s[1] + muCIR(kappa,theta,s[1])*delta + sigmaCIR(s[1],sig)
  
  for(i in 2:length(parameters[["time"]])){
    
    s[i]=s[i-1] + muCIR(kappa,theta,s[i-1])*delta + sigmaCIR(s[1],sig)*sqrt(delta)*rnorm(1)
    + 1/(2*sqrt(delta))*(sigmaCIR(sHat,sig)-sigmaCIR(s[i-1],sig))*((sqrt(delta)*rnorm(1))^2-delta)
    
    sHat<-s[i] + muCIR(kappa,theta,s[i])*delta + sigmaCIR(s[i],sig)
    
  }
  
  return(s)
  
}

