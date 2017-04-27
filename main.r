source("function/global.r")

#tout les plots ici
backtest.plot<-function(df.bcp){

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
    
    data = data.frame(date=index(x), val=df.bcp[,8,drop=T])
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
  })


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















    # Same as above, but with fill=TRUE





    output$aggressivityBox <- renderValueBox({
      x<-dataInput()
      li <- agressivity.investmen(x)
      coef <- li[[1]]
      aggressivity.coef <- round(coef,2)
      valueBox(
        aggressivity.coef, "Aggressivity", icon = icon("signal", lib = "glyphicon"),
        color = "orange" #, fill = TRUE
      )
    })
    output$periodDurationBox <- renderValueBox({
      x<-dataInput()
      li <- agressivity.investmen(x)
      duration <- li[[2]]
      valueBox(
        paste0(duration,' week(s)'), "Selling/Buying perdiod", icon = icon("time", lib = "glyphicon"),
        color = "yellow" #, fill = TRUE
      )
    })
    output$perfermanceBox <- renderValueBox({
      x<-dataInput()
      li <- agressivity.investmen(x)
      duration <- li[[3]]
      valueBox(
        paste0(duration,'%'), "Performance period", icon = icon("time", lib = "glyphicon"),
        color = "yellow" #, fill = TRUE
      )
    })
    
    output$DateBox <- renderValueBox({
      x<-dataInput()
      li <- agressivity.investmen(x)
      duration <- li[[4]]
      valueBox(
        paste0('Since ',duration), paste0(" Selling/Buying perdiod"), icon = icon("time", lib = "glyphicon"),
        color = "yellow" #, fill = TRUE
      )
    })
    





