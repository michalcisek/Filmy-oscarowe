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

#dyskretyzacja budget, opening_weekend, gross, czas, rating, komentarze, zewnetrzne_zapowiedzi

df1<-df[complete.cases(df),]


#zbilansowanie zmiennej objaśnianej
table(df1$Oscar)
df1<-df1[-which(df1$Oscar==0 & df1$rok <2000),]



# Selekcja zmiennych ------------------------------------------------------

library(Boruta)
boruta<-Boruta(Oscar~. -Film -rok -Id -budget -gross -opening_weekend,df1)

plot(boruta)
zmienne_boruta<-getSelectedAttributes(boruta)

test<-which(df1$rok==2016)
df_trening<-df1[-test]
df_test<-df1[test,]

library(randomForest)
rf1<-randomForest(x=df_trening[,zmienne_boruta],y=df_trening[,"Oscar"])
plot(rf1)
rf1<-randomForest(x=df_trening[,zmienne_boruta],y=df_trening[,"Oscar"],ntree=250)

#534
predict(rf1,df_test,type = "prob")


lr1<-glm(Oscar~. -Film -rok -Id -budget -gross -opening_weekend,df_trening,family = "binomial")
lr2<-glm(Oscar~ czas+rating+komentarze+zewnetrzne_zapowiedzi+gross_ratio,df_trening,family = "binomial")
predict(lr1,df_test,type="response")
predict(lr2,df_test,type="response")





