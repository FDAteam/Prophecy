
library(quantmod)
library(plotly)
library(DT)
library(timeSeries)
library(bcp)
library(FRAPO)
library(quadprog)
library(quantmod)
library(DT)
library(readxl)
library(GGally)
library(ggplot2)




#Fonction qui va tester la robustesse du modele
#On verifie si le backtesting est affecté par les données futures
#On compare 5 backtesting, avec une quantité de donnée differentes
#On se base sur des backtests deja stabilisés
robust.check <- function(datas,assetsName, lag, sensibility,loop){
  #message(nrow(datas))
  dd1<-datas[1:(nrow(datas)-40),]
  #message('OK')
  dd2<-datas[1:(nrow(datas)-30),]
  dd3<-datas[1:(nrow(datas)-20),]
  dd4<-datas[1:(nrow(datas)-10),]
  dd5<-datas
  
  df1 <- strategy.stabilization(dd1,assetsName, lag, sensibility, loop)
  df2 <- strategy.stabilization(dd2,assetsName, lag, sensibility, loop)
  df3 <- strategy.stabilization(dd3,assetsName, lag, sensibility, loop)
  df4 <- strategy.stabilization(dd4,assetsName, lag, sensibility, loop)
  df5 <- strategy.stabilization(dd5,assetsName, lag, sensibility, loop)
  
  s1 <- as.vector(df1$StableStrat)
  s2 <- as.vector(df2$StableStrat)
  s3 <- as.vector(df3$StableStrat)
  s4 <- as.vector(df4$StableStrat)
  s5 <- as.vector(df5$StableStrat)
  
  maxlength <- length(s5)

  s1 <- c(s1, rep(NA,maxlength - length(s1)))
  s2 <- c(s2, rep(NA,maxlength - length(s2)))
  s3 <- c(s3, rep(NA,maxlength - length(s3)))
  s4 <- c(s4, rep(NA,maxlength - length(s4)))
  s5 <- c(s5, rep(NA,maxlength - length(s5)))
  
  return(data.frame('a1' = s1, 'a2' = s2, 'a3' = s3, 'a4' = s4, 'a5' = s5))
  
  
  
}

#Fonction de base, genere le backtest et la strategie d'investisement a t+1
bcp.investor <- function(DataSet,assetsName, lag, sensibility){
  
  wkreturn <- DataSet[,assetsName] * 0.01
  wkreturn <- wkreturn[,assetsName]

  ts.asset <- timeSeries(wkreturn)
  
  # Noter les dates
  wkdate <- DataSet[,'Date']
  wkdate <- as.character(wkdate$Date)

  

  bcp.asset <-bcp(ts.asset, burnin = 10000, mcmc = 10000) #Ajout MC number , mcmc= 5000

  
  #Proba decale de 1 (forecast)
  #prob = c(0, bcp.asset$posterior.prob[1:length(bcp.asset$posterior.prob)-1])
  prob = c(NA,NA, bcp.asset$posterior.prob[1:length(bcp.asset$posterior.prob) - 1])
  
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

  rownames(df) <- c(index(wkreturn),"01")
  df$return <- c(wkreturn,NA)
  
  n <- length(df$prob)
  
  #Sharp decale de 1 (forecast)
  sharp.temp <- ((df$posterior.mean)/(sqrt(df$posterior.var)))*(1/(df$prob + 0.001))
  df$sharp <- sharp.temp
  thresld <- rep(99999,n)
  
  
  
  for (i in 1:n){
    if (i < 20){
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

  View(df)
  # write.xlsx2(df, 'data\\Mad1.xlsx', sheetName="Feuil1",col.names=TRUE, row.names=TRUE, append=FALSE)
  return(df)
  
  
}


#Fonction qui lance plusieurs fois bcp.investor
#et fais la moyenne des decisions d'investissement
#pour une bonne stabilisation pensée à mettre loop > 100
strategy.stabilization <- function(DataSet,assetsName, lag, sensibility, loop){
  
  strat <- rep(0,nrow(DataSet))
  for(i in seq(1,loop)){
    message(i)
    df_temp = bcp.investor(DataSet,assetsName, lag, sensibility)
    strat <- strat + as.vector(df_temp$strat)
  }
  
  strat <- strat/loop
  
  df_temp$StableStrat <- strat
  message('End')
  return(df_temp)
}





df.format <- function(DataSet, lag){
  
  #lag 1
  lag1 <- c(NA,NA,NA, DataSet[4:(nrow(DataSet))-1,'sharp'])
  DataSet$lag1 <- lag1
  
  lag2 <- c(NA,NA,NA,NA, DataSet[5:(nrow(DataSet))-2,'sharp'])
  DataSet$lag2 <- lag2
  
  
  lag3 <- c(NA,NA,NA,NA,NA, DataSet[6:(nrow(DataSet))-3,'sharp'])
  DataSet$lag3 <- lag3
  
  DataSet$agressivity1 <- (DataSet$sharp - DataSet$lag1)/DataSet$lag1
  DataSet$agressivity2 <- (DataSet$sharp - DataSet$lag2)/DataSet$lag2
  DataSet$agressivity3 <- (DataSet$sharp - DataSet$lag3)/DataSet$lag3
  
  return(DataSet)
}



error.investment <- function(df.bcp){
  n<-length(df.bcp$strat)
  erreur.grave<-ifelse( df.bcp$strat[4:(n-1)] - ifelse(df.bcp$return[4:(n-1)]>=0, 1,0)>0, 1,0 )
  return(1-(sum(erreur.grave)/length(erreur.grave)))
  
}



agressivity.investment <- function(df.bcp){
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
    #print(df.bcp$wealth[(index+1):n])
    #print(df.bcp$return[(index+1):n])
    #print('')
    
    #Relever la date de changement de position
    
    aggressivity.dur <- df.bcp$date[index]
    perf <- (df.bcp$wealth[n-1] - df.bcp$wealth[index+1])/df.bcp$wealth[index+1]

    
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

