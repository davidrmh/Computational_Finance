##=================Portfolio optimization using CVaR as risk measure================
## Author: David Montalván
##==================================================================================

##==================================================================================
## Function for reading the csv data
## The csv has the following structure:
## DATE column (dates in yyyy-mm-dd format)
## For each stock series a column with adjusted close prices
##
## NOTES:
## Columns' name must be included and the first column must be DATE column
## NA data should be removed
## Stock prices should be numeric type
## Dates should be in increasing order
##
## INPUT:
## filename: String with the path for the csv file containing the data
##
## OUTPUT:
## data: data.frame with the data
##==================================================================================
readData<-function(filename="cvar-data.csv"){
  data=read.csv(filename)
  #Converts dates in character
  data$DATE=as.character(data$DATE)
  return (data)
}

##==================================================================================
## Function for calculating logarithmic returns using the adjusted prices
##
## INPUT
## data: A data.frame created with readData function
##
## OUTPUT:
## returns: data.frame with logarithmic returns for each stock series
##==================================================================================
calculateReturns<-function(data){
  
  #Number of rows and columns
  nCol=dim(data)[2]
  nRow=dim(data)[1]
  
  #Initializes returns data.frame
  returns=data[2:nRow,]
  
  #Calculates log-returns for each stock series
  for(i in 2:nCol){
    returns[,i]=log(data[2:nRow,i]/data[1:(nRow-1),i])
  }
  
  return (returns)
  
}

##==================================================================================
## Function for calculating portfolio's returns according to a set of weigths
##
## INPUT:
## w: vector containing the weights
## returns: Data.frame with log-returns (created with calculateReturns function)
##
## OUTPUT:
## portRet: vector containing portfolio's returns for each period
##==================================================================================
portfolioReturns<-function(w,returns){
  
  #number of columns
  nCol=ncol(returns)
  
  #Creates a matrix with logarithmic returns
  matRet=as.matrix(returns[,2:nCol])
  
  #converts the weigths into a column vector
  w=as.matrix(w)
  
  #calculates portfolio return
  portRet=matRet%*%w
  
  #Converts into vector
  portRet=as.vector(portRet)
  
  return (portRet)
}

##==================================================================================
## Function for calculating Value-at-Risk at a certain confidence level
##
## INPUT:
## portRet: vector containing the portfolio's returns
## alpha: number in (0,1) representing confidence level
##
## OUTPUT:
## VAR: Value-at-Risk
##==================================================================================
calculateVAR<-function(portRet,alpha=0.95){
  
  VAR=quantile(portRet,probs = 1-alpha)
  
  return(VAR)
  
}

##==================================================================================
## Function for calculating Conditional-Value-at-Ristk (CVAR)
##
## INPUT:
## portRet: vector containing the portfolio's returns
## alpha: number in (0,1) representing confidence level
##
## OUTPUT:
## CVAR: Conditional-Value-at-Ristk (CVAR)
##==================================================================================
calculateCVAR<-function(portRet,alpha=0.95){
  #Calculates Value-at-risk
  VAR=calculateVAR(portRet,alpha)
  
  #Obtains the observations which are below VAR
  indexBelow=which(portRet<=VAR)
  
  #Calculates CVAR
  CVAR=mean(portRet[indexBelow])
  
  return(CVAR)
}

##==================================================================================
## Function for creating a particle
## A particle is a set of weights (one for each stock series)
## such that:
## --Each weight is positive (no short-sales)
## --Each weight has a maximum value less than 1 (no leverage)
## --The sum of weights is equal to one
##
## INPUT:
## numW: Integer representing the number of weights in the particle
## maxW: Maximum weight (number between 0 and 1)
##
## OUTPUT:
## particle:Vector representing a particle
##==================================================================================
createParticle<-function(numW,maxW=""){
  
  #If maxW not provided, then maxW=1/numW
  if(maxW==""){maxW=1/numW}
  
  #Flag for checking that the particle is in the search space
  flag=FALSE
  
  while(!flag){
    
    #Initializes particle with zeros
    particle=rep(0,numW)
    
    #Sets the first numW-1 weights
    particle[1:(numW-1)]=runif(numW-1,max=maxW)
    
    #The last weight ensures that they sum to 1
    #For sake of simplicity and in order to avoid looping infinitely
    #this is the only weight that might violate maxW constraint
    particle[numW]=1-sum(particle[1:(numW-1)])
    
    #Checks if the particle is in the search space
    if(length(which(particle<0))==0){flag=TRUE}
    
  }
  
  return(particle)
}

##==================================================================================
## Function for creating the initial population of particles
##
## INPUT:
## numPart: Number of particles in the population
## numW: Number of weights in each particle
## maxW: Maximum weight
##
## OUTPUT:
## population: A list of vectors, each one representing a particle
##==================================================================================
createPopulation<-function(numPart,numW,maxW=""){
  population=list()
  
  for(i in 1:numPart){
    population[[i]]=createParticle(numW,maxW)
  }
  
  return(population)
}

##==================================================================================
## Function for creating a velocity vector
## INPUT:
## numW: Integer representing the number of weights
##
## OUTPUT:
## velocity: Vector representing a particle's velocity
##==================================================================================
createVelocity<-function(numW){
  
  #uniform (-1,1) distribution
  velocity=runif(numW,min=-1,max=1)
  
  return(velocity)
}

##==================================================================================
## Function for creating the initial velocities
##
## INPUT
## numVel: Number of velocities
## numW: Number of components for each velocity
##
## OUTPUT
## velocities: A list of vectors representing a particle's velocity
##==================================================================================
createVelocities<-function(numVel,numW){
  velocities=list()
  
  for(i in 1:numVel){
    velocities[[i]]=createVelocity(numW)
  }
  
  return(velocities)
}

##==================================================================================
## Fitness function (average portfolio return + CVAR + CVARLimit)
##
## INPUT
## particle: A particle with the weigths (see createParticle function)
## returns: A data.frame with the log-returns for each stock series (see calculateReturns function)
## alpha: CVAR confidence level
## limit: CVAR limit (we are seeking a CVAR >= limit)
##
## OUTPUT
## fitness: A number representing particle's fitness
##==================================================================================
calculateFitness<-function(particle,returns,alpha=0.95,limit=-0.01){
  
  #Calculates average portfolio return
  portRet=portfolioReturns(particle,returns)
  average=mean(portRet)
  
  #Calculates CVAR
  cvar=calculateCVAR(portRet,alpha)
  
  #Calculates fitness
  fitness=average + cvar + limit
  
  return (fitness)
  
}

##==================================================================================
## Function for finding the k most close neighbors of a given particle
## The metric used the is euclidean distance
##
## INPUT:
## particle: Particle of interest
## population: population of particles
## k: Number of neighbors
##
## OUTPUT:
## neighbors: A list with the k most close neighbors of particle
##==================================================================================
findNeighbors<-function(particle,population,k=4){
  
  #Finds the distances
  distances=c()
  numParticles=length(population)
  for(i in 1:numParticles){
    neighbor=population[[i]]
    distances=c(distances,sqrt(sum((particle-neighbor)^2)))
  }
  
  #Finds the index or the sorted distances
  indexOrder=order(distances,decreasing=FALSE)
  #The first entry in indexOrder is equal to zero
  #because is the distance of the particle with itself
  #that is why is removed
  indexOrder=indexOrder[-1]
  
  #Stores the k neighbors
  neighbors=list()
  aux=1 #for counting the number of elements added to neighbors
  for(i in indexOrder){
    
    if(aux<=k){
      neighbors[[aux]]=population[[i]]
      aux=aux+1
    }
  }
  
  return (neighbors)
  
}

##==================================================================================
## Particle Swarm Optimization
##
## INPUT
## filename: String with the path for the csv file containing the data
## popSize: population size
## numGen: number of generations
## vMax: max value for velocity
## k: nearest neighbors
## fi1max: max value for cognition learning rate
## fi2max: max value for social learning rate
## wMax: maximum weight for each stock
## wMin: minimum weight for each stock
## alpha: CVaR confidence level
## limit: CVAR limit
##==================================================================================
simplePSO<-function(filename,popSize=60,numGen=30,vmax=0.5,k=4,fi1max=2.05,fi2max=2.05,wMax=0.25,wMin=0.01,alpha=0.95,limit=-0.01){
  
  #Read the data
  data=readData(filename)
  
  #Calculates returns for each stock series
  returns=calculateReturns(data)
  
  #Gets the number of stocks
  numStocks=ncol(returns)-1
  
  #Initial population and velocities
  population=createPopulation(popSize,numStocks,wMax)
  velocities=createVelocities(popSize,numStocks)
  
  #Best position of each particle
  bestPositionPop=population
  
  #best particle and best fitness
  bestParticle=c()
  bestFitness=-100
  
  for(n in 1:numGen){
    
    for(i in 1:popSize){
      
      #gets a particle from the population
      particle=population[[i]]
      bestPositionParticle=bestPositionPop[[i]]
      bestPositionFitness=calculateFitness(bestPositionParticle,returns,alpha,limit)
      particleFitness=calculateFitness(particle,returns,alpha,limit)
      
      #finds particle's neighbors
      neighbors=findNeighbors(particle,population,k)
      
      neighborsFitness=c()
      #finds neighbors' fitness
      for(j in 1:k){
        neighbor=neighbors[[j]]
        neighborsFitness=c(neighborsFitness,calculateFitness(neighbor,returns,alpha,limit))
      }
      
      #Finds best neighbor
      bestNeighbor=neighbors[[which.max(neighborsFitness)[1] ]]
      
      #Generates the new velocity
      currentVelocity=velocities[[i]]
      fi1=runif(numStocks,max=fi1max)
      fi2=runif(numStocks,max=fi2max)
      newVelocity = currentVelocity + fi1*(bestPositionParticle - particle) + fi2*(bestNeighbor - particle)
      
      #Adjusts to the max velocity
      for(j in 1:length(newVelocity)){
        if(abs(newVelocity[j])>vmax){
          
          newVelocity[j]=newVelocity[j]*vmax/abs(newVelocity[j])
          
        }
      }
      
      #Creates the new particle
      newParticle=particle + newVelocity
      
      #Ensures that the new particle stays in the search space
      for(j in 1:length(newParticle)){
        if(newParticle[j]>wMax){newParticle[j]=wMax}
        if(newParticle[j]<wMin){newParticle[j]=wMin}
      }
      
      #Calculates the new fitness
      newFitness=calculateFitness(newParticle,returns,alpha,limit)
      
      #Registers the new best position
      if(newFitness>bestPositionFitness){
        bestPositionPop[[i]]=newParticle
        #population[[i]]=newParticle
        } 
      
    } #Next particle
    
    #Finds the best particle so far
    popFitness=c()
    for(j in 1:popSize){
      part=bestPositionPop[[j]]
      popFitness=c(popFitness,calculateFitness(part,returns,alpha,limit))  
    }
    
    indexMax=which.max(popFitness)
    currBest=popFitness[indexMax]
    
    if(currBest>bestFitness){
      bestFitness=currBest
      bestParticle=bestPositionPop[[indexMax]]
    }
    
    print(paste("Generation ",n,sep=""),quote = FALSE)
    print(paste("Best fitness so far = ",round(bestFitness,6),sep=""),quote = FALSE)
    print(paste("With average return = ",round(mean(portfolioReturns(bestParticle,returns)),6),sep="" ),quote = FALSE)
    
  }#Next generation
  
  return(bestParticle)
  
  
}