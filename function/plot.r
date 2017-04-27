




backtest.plot<-function(df.bcp){
  data = data.frame(date=index(df.bcp), val=df.bcp[,11,drop=T])
  p<-plot_ly(
    
    data = data,
    x = ~date,
    y = ~val,
    name = 'Market',
    type = 'scatter',
    mode = 'lines',
    colors = "Dark2",
    connectgaps = TRUE
  )%>%
    add_trace(y = df.bcp[,12,drop=T], name = 'Bayesian Investor', mode = 'lines')
  
  data = data.frame(date=index(df.bcp), val=df.bcp[,8,drop=T])
  q<-plot_ly(
    
    data = data,
    x = ~date,
    y = ~val,
    name = 'Bayesian Sharpe',
    type = 'scatter',
    mode = 'lines',
    colors = "Dark2",
    connectgaps = TRUE
  )%>%
    add_trace(y = df.bcp[,9,drop=T], name = 'Threshold', mode = 'lines')
  
  subplot(p,q, nrows = 2, shareX = TRUE) %>% layout(legend = list(orientation = 'h'))
}


ratio.plot<-function(df.bcp){  
  
  data = data.frame(date=index(df.bcp), val=df.bcp[,8,drop=T])
  plot_ly(
    
    data = data,
    x = ~date,
    y = ~val,
    name = 'Bayesian Sharpe',
    type = 'scatter',
    mode = 'lines',
    connectgaps = TRUE
  )%>%
    add_trace(y = df.bcp[,9,drop=T], name = 'Threshold', mode = 'lines')
}