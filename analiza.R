rm(list=ls())
df<-read.csv2("dane_oscar.csv")

# Transformacja zmiennych -------------------------------------------------

df$gross_ratio<-df$gross/df$budget
df$ow_ratio<-df$opening_weekend/df$budget
df$kom_zap_ratio<-df$komentarze/df$zewnetrzne_zapowiedzi
df$oscar_actor_binary<-ifelse(df$oscar_actor==0,0,1)

df$Oscar<-factor(df$Oscar)
df$oscar_actor<-factor(df$oscar_actor)
df$oscar_actor_binary<-factor(df$oscar_actor_binary)

df$log_gross<-log(df$gross)
df$log_budget<-log(df$budget)
df$log_ow<-log(df$opening_weekend)

df1<-df[complete.cases(df),]


#zbilansowanie zmiennej objaśnianej
table(df1$Oscar)
df1<-df1[-which(df1$Oscar==0 & df1$rok <2000),]

var1<-df1[,c(2,5:11,13,16)]# defaultowa partia zmiennych objasniajacych
var2<-df1[,c(2,8:11,13:17)] #dodane ratio
var3<-df1[,c(2,8:11,13,16,18:20)]#bez ratio ale z logarytmami



# Selekcja zmiennych ------------------------------------------------------

library(Boruta)
boruta1<-Boruta(Oscar~. -Film -Id -rok,df1)

plot(boruta1)

boruta_formula1<-getConfirmedFormula(boruta1)

test<-which(df1$rok==2016)

library(randomForest)
set.seed(123)
rf1<-randomForest(boruta_formula1,df1[-test,])

plot(rf1)

#534
predict(rf1,df1[test,],type = "prob")


lr1<-glm(Oscar~log_gross+log_budget+log_ow+oscar_actor_binary+gatunek+zewnetrzne_zapowiedzi+
           komentarze+rating+czas,df1[-test,],family = "binomial")
lr2<-step(lr1)

predict(lr1,df1[test,],type="response")
predict(lr2,df1[test,],type="response")

