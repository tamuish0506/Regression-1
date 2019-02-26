LifeStyle <- read.csv(".../lifestyle.csv", header = TRUE)
dim(LifeStyle)
head(LifeStyle)
summary(LifeStyle)
#chi-square test
table_1=t(table(LifeStyle[,2],LifeStyle[,17]))
chisq.test(table_1)
#linear regression-1
ls_1 <- lm(netdome ~ emplmerg, data=LifeStyle)
summary(ls_1)
# Pearson Correlation
x_1 <- LifeStyle[,15]
y_1 <- LifeStyle[,6]
cor.test(x_1, y_1, method = "pearson", alternative = "greater")
#linear regression-2
ls_2 <- lm(netdome ~ age, data=LifeStyle)
summary(ls_2)
betas=ls_2$coefficients
x4=70
sum(betas[1]+betas[2]*x4)
#linear regression- gun ownners vs age
ls_3 <- lm(gun ~ age, data = LifeStyle)
summary(ls_3)
#linear regression- gun ownners vs gender
ls_4 <- lm(gun ~ gender, data = LifeStyle)
summary(ls_4)
#linear regression- liberal or not vs age, gun ownner and political tendency
ls_5 <- lm(liberal ~ age + gun + politics, data=LifeStyle)
summary(ls_5)
betas=ls_5$coefficients
x1=30
x2=1
x3=1
sum(betas[1]+betas[2]*x1+betas[3]*x2+betas[4]*x3)
# sales forecast
ExpPrice <- read.csv(".../experiment_price_ad.csv", header = TRUE)
head(ExpPrice)
dim(ExpPrice)
summary(ExpPrice)
Y=ExpPrice[,1]
View(ExpPrice)
p=ExpPrice[,2]
ad=ExpPrice[,3]
storesize=ExpPrice[,4]
e1 <- lm(Y ~ ad, data=ExpPrice)
summary(e1)
e2 <- lm(Y ~ p + ad, data=ExpPrice)
summary(e2)
e3 <- lm(Y ~ p + ad + storesize)
summary(e3)
betas=e3$coefficients
new_value_1=24
new_value_2=1
new_value_3=32
sum(betas[1]+betas[2]*new_value_1+betas[3]*new_value_2+betas[4]*new_value_3)
ExpPrice_1 <- read.csv(".../experiment_price_ad_1.csv", header = TRUE)
View(ExpPrice_1)
Y_1=ExpPrice_1[,1]
p_24=ExpPrice_1[,3]
p_29=ExpPrice_1[,4]
ad_1=ExpPrice_1[,6]
storesize_1=ExpPrice_1[,7]
e4 <- lm(Y_1 ~ p_24 + p_29 + ad_1 + storesize_1, data=ExpPrice_1)
summary(e4)
