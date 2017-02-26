rm(list=ls())
library(datasets)
library(GGally)
library(ggplot2)

my_fn <- function(data, mapping, ...){
  p <- ggplot(data = data, mapping = mapping) + 
    geom_point() + 
    geom_smooth(method=loess, fill="red", color="red", ...) +
    geom_smooth(method=lm, fill="blue", color="blue", ...)
  p
}

# Zaladowanie danych ------------------------------------------------------
df<-read.csv2("dane_oscar.csv")

# Transformacja zmiennych -------------------------------------------------

df$gross<-df$gross/1e5
df$budget<-df$budget/1e5
df$opening_weekend<-df$opening_weekend/1e5

df$gross_ratio<-df$gross/df$budget
df$ow_ratio<-df$opening_weekend/df$budget
df$kom_zap_ratio<-df$komentarze/df$zewnetrzne_zapowiedzi
df$oscar_actor_binary<-ifelse(df$oscar_actor==0,0,1)

df$Oscar<-factor(df$Oscar)
df$oscar_actor<-factor(df$oscar_actor)
df$oscar_actor_binary<-factor(df$oscar_actor_binary)


df1<-df[complete.cases(df),]

test<-df1[which(df1$rok==2016),]
df1<-df1[-which(df1$rok==2016),]

#zbilansowanie zmiennej objaśnianej
table(df1$Oscar)

set.seed(56283)
wygrane_filmy<-which(df1$Oscar==1)
przegrane_filmy<-sample(which(df1$Oscar==0),50)

df1<-df1[c(przegrane_filmy,wygrane_filmy),]

#Rocky jest outlierem jeśli chodzi o wielkosc opening_weekend (wygral Oscara)
df1<-df1[-which(df1$ow_ratio==max(df1$ow_ratio)),]


# Wizualizacja zaleznosci miedzy zmiennymi --------------------------------

# g = ggpairs(df1,columns = c(2,7:12,14:17), lower = list(continuous = my_fn))
#gross, czas, rating, zewnetrzne_zapowiedzi i kom_zap_ratio wydaja sie najsensowniejszymi kandydatami na zmienne objasniajace
# g

# Modelowanie ------------------------------------------------------

lr1<-glm(Oscar~gross+czas+rating+zewnetrzne_zapowiedzi+kom_zap_ratio,df1,family = "binomial")
summary(lr1)

# La la land głównym kandydatem na zwycięzce...
predykcja_lr<-predict(lr1,test,type="response")

# ... co zgadza sie z kursami buckmacherskimi
final<-data.frame(film=test[,1],predykcja_lr,bukcmacher_kurs=c(100,100,100,100,21,1.14,50,30,5))
View(final[order(-final$predykcja_lr),])
