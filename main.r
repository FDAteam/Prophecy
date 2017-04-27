source("function/global.r")
source("function/plot.r")
#tout les plots ici

    # Same as above, but with fill=TRUE



#Loading data
DataSet <- read_excel("data/data.xlsx",
col_types = c("date", "numeric", "numeric",
              "numeric", "numeric", "numeric",
              "numeric", "numeric", "numeric",
              "numeric", "numeric", "numeric",
              "numeric", "numeric", "numeric",
              "numeric", "numeric", "numeric",
              "numeric", "numeric", "numeric",
              "numeric", "numeric", "numeric",
              "numeric", "numeric", "numeric",
              "numeric", "numeric", "numeric",
              "numeric", "numeric", "numeric",
              "numeric", "numeric", "numeric",
              "numeric", "numeric", "numeric",
              "numeric"))


#Get assets names
symbols<- data.frame(Symbol = colnames(DataSet[,2:ncol(DataSet)]))


df <- bcp.investor(DataSet, 'ACCOR',20,0.01)

backtest.plot(df)
ratio.plot(df)

li <- agressivity.investmen(df)
coef <- li[[1]]
duration <- li[[3]]
date <- li[[4]]

aggressivity.coef <- round(coef,2)
print(aggressivity.coef)
print(duration)
print(date)






