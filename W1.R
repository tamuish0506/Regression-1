install.packages("gmodels")
install.packages("Hmisc")
CrossTab <- read.csv("CrossTab_Example.csv", header=TRUE)
View(CrossTab)
AccountSize<-table(CrossTab[,2])
list(AccountSize)
AccountSize[AccountSize==1]="Small"
AccountSize[AccountSize==2]="Medium"
AccountSize[AccountSize==3]="Large"
dimnames(AccountSize)<-list(c("Small", "Medium", "Large"))
Recomm<-table(CrossTab[,3])
dim(CrossTab)
head(CrossTab)
tail(CrossTab)
summary(CrossTab)
n=dim(CrossTab)[1]
table(CrossTab)[2]/n
mytable=t(table(CrossTab[,2], CrossTab[,3]))
list(mytable)
chisq.test(mytable)
library(gmodels)
CrossTable(CrossTab[,3], CrossTab[,2], dnn=c("Recomm", "AcctSize"))
AccountSize=CrossTab[,2]
Recomm=CrossTab[,3]
Recomm[Recomm==0]="Not_Recomm"
CrossTable(Recomm,AccountSize, chisq=TRUE,expected=TRUE, dnn=c("Recomm", "AcctSize"))

cdc.df <- read.csv("http://goo.gl/5xQObB")
View(cdc.df)
CrossTable(cdc.df[,6], cdc.df[,8], dnn = c("cargo capacity", "Choice"))
table_1=t(table(cdc.df[,6], cdc.df[,8]))
chisq.test(table_1)
CrossTable(cdc.df[,5], cdc.df[,8], dnn = c("seat", "Choice"))
table_2=t(table(cdc.df[,5], cdc.df[,8]))
chisq.test(table_2)

Corr=read.csv("Correlation_Example.csv", header=TRUE)
price <- Corr[,2]
sales <- Corr[,3]
plot(price,sales)
cor(price, sales)
abline(lm(sales~price), col="red")
lines(lowess(price,sales), col="blue")
library(Hmisc)
rcorr(cbind(price, sales), type = "pearson")
