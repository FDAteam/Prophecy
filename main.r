source("function/global.r")
source("function/global.r")
#tout les plots ici

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
    





