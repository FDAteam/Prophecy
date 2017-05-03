source("function/global.r")
source("function/plot.r")
#tout les plots ici

    # Same as above, but with fill=TRUE



#Loading data
DataSet <- read_excel("data/data.xlsx")


#Get assets names
symbols<- data.frame(Symbol = colnames(DataSet[,2:ncol(DataSet)]))

#Asset dataframe
df <- bcp.investor(DataSet, 'ACCOR',20,0.01)
df <-df.format(df,20)

View(df)
#Plot backtest
backtest.plot(df)
ratio.plot(df)


#KPI calculation
li <- agressivity.investment(df)
coef <- li[[1]]
duration <- li[[3]]
date <- li[[4]]

aggressivity.coef <- round(coef,2)
print(aggressivity.coef)
print(duration)
print(date)


df2 <- cbind(df, "ind" = ifelse(df$return>0, "Buy", "Sell"))



#Ploting
plot(df[,c(4,5,6,7,8,9,10,14,15,16,17,18,19)],
     col=ifelse(df$return>0, "red", "black"),
     pch = 19)



# Overlaid histograms
# Distribution of Buy Sell indicator over Bayesian Sharp ratio
ggplot(df2, aes(x=sharp, fill=ind)) +
  geom_histogram(binwidth=20, alpha=.4, position="identity")


# Distribution of Buy Sell indicator over Threshold
ggplot(df2, aes(x=thresld, fill=ind)) +
  geom_histogram(binwidth=20, alpha=.4, position="identity")


# Distribution of Buy Sell indicator over aggressivity ratio
ggplot(df2, aes(x=agressivity3, fill=ind)) +
  geom_histogram(binwidth=1, alpha=.4, position="identity")







