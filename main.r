library(readxl)
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
message("Commit branch")
message("Je suis sur la branche Niak12")

df <- bcp.investor(DataSet, 'AXA',20,0.01)

backtest.plot(df)
ratio.plot(df)


bs<- investment.approver(df)
li <- agressivity.investmen(df)
coef <- li[[1]]
duration <- li[[3]]
date <- li[[4]]

aggressivity.coef <- round(coef,2)

print(bs)
print(aggressivity.coef)
print(duration)
print(date)






