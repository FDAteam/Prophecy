library(shiny)
library(quantmod)
library(shinydashboard)
library(plotly)
library(DT)
library(timeSeries)
library(bcp)
library(FRAPO)
library(quadprog)
library(shiny)
library(quantmod)
library(shinydashboard)
library(plotly)
library(DT)



#DataSet <-read.delim('C:\\Users\\ibrahim\\Documents\\Ibrahim\\Mes docs\\Bayesian\\ShinyDashboard18 02 17 - Copie\\ShinyDashboard\\CAC10Y.txt', header=TRUE) 



#symbols<- data.frame(Symbol = colnames(DataSet[,2:ncol(DataSet)]))

bcp.investor <- function(DataSet,assetsName, lag, sensibility){
  
  #asset<-getSymbols(assetsName,auto.assign = FALSE)
  #wkreturn<- weeklyReturn(asset,auto.assign = FALSE)-0.004
  
  
  #asset<-getSymbols(assetsName,auto.assign = FALSE)
  
  #wkreturn<-read.delim('C:\\Users\\ibrahim\\Documents\\Ibrahim\\Mes docs\\Bayesian\\ShinyDashboard18 02 17 - Copie\\ShinyDashboard\\STXX50+SBF120DataReuters10Y 31 01 2017_ Clean.txt', header=TRUE) 
  
  #wkreturn<- weeklyReturn(asset,auto.assign = FALSE)-0.004
  
  #wkreturn <- DataSet$assetsName
  #assetsName<-'ACCOR'
  wkreturn <- DataSet[,assetsName] * 0.01
  wkreturn <- wkreturn[,assetsName]
 # wkreturn <-data.frame(return = wkreturn)
#  row.names(wkreturn) <- DataSet$Date
  
  ts.asset <-timeSeries(wkreturn)
  
  # Noter les dates
  wkdate <- DataSet[,'Date']
  wkdate <- wkdate$Date
  # wkreturn <-data.frame(return = wkreturn)
  #  row.names(wkreturn) <- DataSet$Date
  
  #date.asset <-timeSeries(wkdate)
  
  ###################
  bcp.asset <-bcp(ts.asset, burnin = 5000, mcmc = 5000) #Ajout MC number , mcmc= 5000
  
  #Proba decale de 1 (forecast)
  #prob = c(0, bcp.asset$posterior.prob[1:length(bcp.asset$posterior.prob)-1])
  prob = c(NA,NA, bcp.asset$posterior.prob[1:length(bcp.asset$posterior.prob)-1])
  
  df <- data.frame( 
                    "posterior.mean.o" = c(bcp.asset$posterior.mean,NA),
                    "posterior.var.o"  = c(bcp.asset$posterior.var,NA),
                    "posterior.prob.O" = c(bcp.asset$posterior.prob,NA),
                    "posterior.mean" = c(NA,bcp.asset$posterior.mean),
                    "posterior.var"  = c(NA,bcp.asset$posterior.var),
                    #"prob" = c(NA,bcp.asset$posterior.prob),
                    #price = c(bcp.asset$data,0),
                    "prob" = prob
  )

  rownames(df)<-c(index(wkreturn),"01")
  df$return <- c(wkreturn,NA)
  
  n <- length(df$prob)
  
  #Sharp decale de 1 (forecast)
  sharp.temp <- ((df$posterior.mean)/(sqrt(df$posterior.var)))*(1/(df$prob+0.001))
  df$sharp <- sharp.temp
  thresld <- rep(99999,n)
  
  
  
  for(i in 1:n){
    if(i<20){
      thresld[i] <- quantile( df$sharp[1:i], c(.1, .2, .3, .4, .5, .6, .7, .8, .9), na.rm = TRUE)[3]
    }
    else{
      thresld[i] <- quantile( df$sharp[(i-(20-1)):i], c(.1, .2, .3, .4, .5, .6, .7, .8, .9), na.rm = TRUE)[3]
    }
    
  }
  
  
  df$thresld <- thresld
  #df$strat <- ifelse(df$sharp - df$thresld>=sensibility, 1,0)
  df$strat <- ifelse(df$sharp - df$thresld >= 0.05*df$thresld & df$sharp>0, 1,0)
  
  wealth <- rep(100,n)
  wealth2 <- rep(100,n)  
  
  for(i in 4:n){
    
    wealth[i] <- (1+df$return[i])*wealth[i-1]
    #print(wealth2[i-1] + (df$return[i] -  df$return[i]*(1 - df$strat[i])  )*wealth2[i-1])
    
    wealth2[i] <- wealth2[i-1] + (df$return[i] -  df$return[i]*(1-df$strat[i])  )*wealth2[i-1]
    
  }
  
  df$wealth <- wealth
  df$wealth.strat <- wealth2
 
  df$date <- c(as.vector(wkdate),NA)
  
  #View(df)
  
  return(df)
  
  
}




error.investment <- function(df.bcp){
  n<-length(df.bcp$strat)
  erreur.grave<-ifelse( df.bcp$strat[4:(n-1)] - ifelse(df.bcp$return[4:(n-1)]>=0, 1,0)>0, 1,0 )
  return(1-(sum(erreur.grave)/length(erreur.grave)))
  
}



agressivity.investmen <- function(df.bcp){
  n<-length(df.bcp$strat)
  indicator <- TRUE
  i <- 0
  index <- 9999999
  period.type <- df.bcp$strat[n]
  
  #recherche de la sequence
  while(indicator==TRUE){
    if(df.bcp$strat[n-i]!=period.type){
      indicator <- FALSE
      index <- n-i
    }else{
      i <- i+1
    }
  }
  
  #print(df.bcp$sharp[n])
  #print(n)
  #print(df.bcp$sharp[index])
  #print(index)
  if(n - index ==0){
    return(list(0,n - index,0))
  }
  else{
    aggressivity.coef <- (df.bcp$sharp[n] - df.bcp$sharp[index]) / (n - index)
    perf <- (df.bcp$wealth[n-1] - df.bcp$wealth[index+1])/df.bcp$wealth[index+1]
    #print(df.bcp$wealth[n])
    #print(n)
    print(df.bcp$wealth[(index+1):n])
    print(df.bcp$return[(index+1):n])
    print('')
    
    #Relever la date de changement de position
    
    aggressivity.dur <- df.bcp$date[index]
    perf <- (df.bcp$wealth[n-1] - df.bcp$wealth[index+1])/df.bcp$wealth[index+1]
    #print(df.bcp$wealth[n])
    #print(n)
    print(df.bcp$wealth[(index+1):n])
    print(df.bcp$return[(index+1):n])
    print('')
    
    return(list(aggressivity.coef, n - index,round(perf,4)*100,aggressivity.dur))
  }
  
  
  
  
}



#df<-bcp.investor('ELE',20,0.1)
#error.investment(bcp.investor('ELE',20,0.1))

investment.approver <- function(df.bcp){
  if(tail(df.bcp$strat, n=1)>0){
    return("Buy")
  }else{
    return("Sell")
  }
}




invest.color<-function(action.inv){
  if(action.inv == "Buy")
  {
    return("green")
  }
  else{
    return("red")
  }
}

invest.thumbs<-function(action.inv){
  if(action.inv == "Buy")
  {
    return(icon("thumbs-up", lib = "glyphicon"))
  }
  else{
    return(icon("thumbs-down", lib = "glyphicon"))
  }
}





getData<-function(vect.symbol, window){
  #vect.symbol<-c('BIC','ADIDAS','EDF')
  #window <-20
  n <- length(vect.symbol)
  
  for (i in 1:n){
    #asset <- getSymbols(vect.symbol[i],auto.assign = FALSE)
    #asset <- dailyReturn(asset,auto.assign = FALSE)-0.004
    
    asset.name <<- vect.symbol[i]
    print(asset.name)
    asset <- as.vector(DataSet[(nrow(DataSet)-window):(nrow(DataSet)),asset.name])
    #df.portf <- data.frame(ini = rep(0,window+1))
    #df.portf <- data.frame()
    if(i==1){
      print(asset.name)
      df.portf <-asset *0.01
      print('OK')
      #print(df.portf)
    }
    else{
      print(asset.name)
      #df.portf[,asset.name] <- asset * 0.01
      df.portf<- cbind(df.portf,asset * 0.01)
      print(' ')
      #print(df.portf)
    }
    
  }
  
  print('OK4')
  df.portf <- data.frame(df.portf)
  colnames(df.portf)<-vect.symbol
  #df.portf<-tail(df.portf,window)

  df.portf<-as.matrix(df.portf)
  
  return(df.portf)
}





eff.frontier <- function (returns, short="no", max.allocation=NULL, risk.premium.up=.5, risk.increment=.005){
  # return argument should be a m x n matrix with one column per security
  # short argument is whether short-selling is allowed; default is no (short selling prohibited)
  # max.allocation is the maximum % allowed for any one security (reduces concentration)
  # risk.premium.up is the upper limit of the risk premium modeled (see for loop below)
  # risk.increment is the increment (by) value used in the for loop
  
  covariance <- cov(returns, use = 'na.or.complete')
  print(covariance)
  n <- ncol(covariance)
  
  # Create initial Amat and bvec assuming only equality constraint (short-selling is allowed, no allocation constraints)
  Amat <- matrix (1, nrow=n)
  bvec <- 1
  meq <- 1
  
  # Then modify the Amat and bvec if short-selling is prohibited
  if(short=="no"){
    Amat <- cbind(1, diag(n))
    bvec <- c(bvec, rep(0, n))
  }
  
  # And modify Amat and bvec if a max allocation (concentration) is specified
  if(!is.null(max.allocation)){
    if(max.allocation > 1 | max.allocation <0){
      stop("max.allocation must be greater than 0 and less than 1")
    }
    if(max.allocation * n < 1){
      stop("Need to set max.allocation higher; not enough assets to add to 1")
    }
    Amat <- cbind(Amat, -diag(n))
    bvec <- c(bvec, rep(-max.allocation, n))
  }
  
  # Calculate the number of loops based on how high to vary the risk premium and by what increment
  loops <- risk.premium.up / risk.increment + 1
  loop <- 1
  
  # Initialize a matrix to contain allocation and statistics
  # This is not necessary, but speeds up processing and uses less memory
  eff <- matrix(nrow=loops, ncol=n+3)
  # Now I need to give the matrix column names
  colnames(eff) <- c(colnames(returns), "Std.Dev", "Exp.Return", "sharpe")
  
  # Loop through the quadratic program solver
  for (i in seq(from=0, to=risk.premium.up, by=risk.increment)){
    dvec <- colMeans(returns, na.rm = TRUE) * i # This moves the solution up along the efficient frontier
    sol <- solve.QP(covariance, dvec=dvec, Amat=Amat, bvec=bvec, meq=meq)
    eff[loop,"Std.Dev"] <- sqrt(sum(sol$solution *colSums((covariance * sol$solution))))
    eff[loop,"Exp.Return"] <- as.numeric(sol$solution %*% colMeans(returns,na.rm = TRUE))
    eff[loop,"sharpe"] <- eff[loop,"Exp.Return"] / eff[loop,"Std.Dev"]
    eff[loop,1:n] <- sol$solution
    loop <- loop+1
  }
  
  return(as.data.frame(eff))
  
  
}




MostDiversifited <- function(returns){
  r<-PMD(returns)
  return(t(FRAPO::Weights(r)))
}

