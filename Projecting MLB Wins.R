setwd("/Users/brentcortez/Desktop/MLBdata")
library("tidyverse")
library(plotly)
library(corrplot)

Team2017<-read_csv("MLB2017Team.csv") %>%
  mutate(Rdifferencepergame = RSpergame - RApergame)


Team2018<-read_csv("MLB2018Team.csv")%>%
  mutate(Rdifferencepergame = RSpergame - RApergame)


Team2019<-read_csv("MLB2019Team.csv") %>%
  mutate(Rdifferencepergame = RSpergame - RApergame)

Team1819<-rbind(Team2018,Team2019)
Team171819<-rbind(Team2017,Team1819)


Teamstats<-mutate(Team171819,RC = OBP * TB)

W<-ggplot(Teamstats,aes(x=Wins,y=Rdifferencepergame))+
  geom_point()+
  geom_smooth(method=lm,color="purple",fill="#69b3a2", se=TRUE)+
  ggtitle("Wins vs Run Differential")+
  xlab("Wins")+
  ylab("Run Differential Per Game")
Wins<-ggplotly(W)
#type Wins in console and hit enter and the graph should come up
summary(lm(formula=Wins ~ Rdifferencepergame, data=Teamstats))
#R^2 .8929

plot_ly(data=Teamstats, x = ~Wins, y = ~Rdifferencepergame, type = "scatter", mode = "markers")%>%
  layout(title= "How much does run difference afftect wins?", xaxis = list(title = " Run Difference Per Game"), yaxis = list(title = "Wins"))

Offstats<-select(Teamstats,RSpergame,BA,OBP,SLG,OPS,RC,HRb,SOb,SB,TB,LOBb)
data<-cor(Offstats)
corrplot(data,method="color",addCoef.col="yellow")

options(scipen = 1000)

summary(lm(formula=RSpergame ~ RC, data=Teamstats))
#RSpergame = .0792854 + .0060477(RC)  adjusted R^2 .9243
ggplot(Teamstats,aes(x=RSpergame,y=RC))+
  geom_point()+
  geom_smooth(method=lm,color="red",fill="#69b3a2", se=TRUE)+
  ggtitle("Runs Scored Per Game vs Runs Created")+
  xlab("Runs Scored Per Game")+
  ylab("Runs Created")



summary(lm(formula=RSpergame ~ OPS, data=Teamstats))
#RSpergame = -4.7414 + 12.5984(OPS) adjusted R^2 .9193
ggplot(Teamstats,aes(x=RSpergame,y=OPS))+
  geom_point()+
  geom_smooth(method=lm,color="blue", fill="#69b3a2",se=TRUE)+
  ggtitle("Runs Scored Per Game vs OPS")+
  xlab("Runs Scored Per Game")+
  ylab("OPS")


Defstats<-select(Teamstats,RApergame,ERA,FIP,Fld,DefEff,WHIP,H9,BB9,HR9,Sop)
data2<-cor(Defstats)
corrplot(data2, method="color",addCoef.col="yellow")

summary(lm(formula= RApergame ~ FIP, data=Teamstats))
#R^2 .8286
ggplot(Teamstats,aes(x=RApergame,y=FIP))+
  geom_point()+
  geom_smooth(method=lm,color="yellow",fill="#69b3a2", se=TRUE)+
  ggtitle("Runs Allowed Pergame vs FIP")+
  xlab("Runs Allowed Per Game")+
  ylab("FIP")


summary(lm(formula= RApergame ~ WHIP, data=Teamstats))
#R^2 .8501
ggplot(Teamstats,aes(x=RApergame, y=WHIP))+
  geom_point()+
  geom_smooth(method=lm,color="purple",fill="#69b3a2", se=TRUE)+
  ggtitle("Runs Allowed Per Game")+
  xlab("Runs Allowed Per Game")+
  ylab("WHIP")

summary(lm(formula=RApergame ~ WHIP + Fld , data=Teamstats))
#19.845 + 5.347(WHIP) - 22.661(Fld) R^2 .8491
summary(lm(formula= RApergame ~ FIP + DefEff, data=Teamstats))
#12.07422 + 1.01273(FIP) - -17.15935(DeffEff) R^2 .935



summary(lm(formula=Rdifferencepergame ~ RC + FIP + DefEff, Teamstats))
#Rdifferencepergame = -1.351e+1 + 6.245e-3(RC)  - 1.002(FIP) + 1.908e+1(DefEff) adjusted R^2 .9385
summary(lm(formula=Wins ~ RC + FIP + DefEff, Teamstats))
#Wins = -118.810513 + .095784(RC) - 15.571286(FIP) + 283.075885(DefEff) adjusted R^2 .847O

summary(lm(formula=Wins ~ RC + WHIP + DefEff,Teamstats))
ggplot(Teamstats,aes(x=Wins))+
  geom_histogram(binwidth=1,color="black", fill="orange")+
  ggtitle("Wins Distribution")+
  xlab("Wins")+
  ylab("Frequency")

mean(Teamstats$Wins)
#81
median(Teamstats$Wins)
#80
sd(Teamstats$Wins)
#14


sd(Teamstats$RC)
#78.83334
mean(Teamstats$RC)
#754.5504
median(Teamstats$RC)
#757.8875


sd(Teamstats$FIP)
#.4495361
mean(Teamstats$FIP)
#4.339333
median(Teamstats$FIP)
#4.295


sd(Teamstats$DefEff)
#.01122402
mean(Teamstats$DefEff)
#.6892333
median(Teamstats$DefEff)
#.688
