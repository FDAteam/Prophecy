
# Somme<-read_excel("data/demoLog.xlsx")
# 
# Somme <- read.table(file="C:/Users/ibrahim/Desktop/Quant Methods/Hub/Prophecy/data/demoLog2.txt",sep="\t",dec=".",header =T)
# Somme =  c("2,3","2,6","2,8")
# Somme = gsub(",",".",Somme)
# class(Somme)
# Somme[,1]
# 
# 
# remp<-function(x) {x = as.numeric(gsub(",",".",x)) } #cette fonction marche avec les data fram
# 
# Somme[,2] = remp(Somme[,2])
# 
# lapply(Somme[,1:4], FUN = function(x) as.numeric(gsub(",",".",x)))
# Somme[,2]
# 
# #Somme = substi(Somme) 
# #Somme = as.numeric(Somme);Somme
# Somme

###########################################################


data <- read.table(file="/data/Logit/dataLogit3.txt",sep="\t",dec=".",header =T) #cette parti marche mais pas
                                                                                                                              # mais pas en fichier excel
                                                                                                                      #car non numeric
#data <- read_excel("data/demoLog.xlsx")
#                    
#                     #col_types = c("numeric", "numeric", "numeric",
#                     #"numeric"))

#data <-read_excel("data/demoLog.xlsx",col_types = c("numeric", "numeric", "numeric", "factor")) #n'arrive pas exprimer la derniere formule


class(data)
summary(data)
data
modele <- glm(strat ~ mean+var+prob, data = data, family=binomial)
class(modele)
print(modele)
print(summary(modele))
