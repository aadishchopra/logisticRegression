library(readxl)
Mission<-read_excel('D:/UIC/Healthcare Information Management/Mission.xlsx')
str(Mission)
Miss<-as.data.frame(unclass(Mission))
str(Miss)

SLM=lm(Ln.Total.Cost.~BODY.WEIGHT,data=Miss)
summary(SLM)
TotaSLM=lm(Miss$Ln.Total.Cost.~.,data = Miss)
summary(TotaSLM)
CheckSLM=lm(Miss$Ln.Total.Cost.~AGE+MALE+UNMARRIED+RHD+BODY.WEIGHT+BP..HIGH+
BP.LOW+HR.PULSE+Diabetes2:UREA+CREATININE+LENGTH.OF.STAY...ICU+
+LENGTH.OF.STAY..WARD+Ln.Cost.of.Implant.+ELECTIVE+HB+RR+other..heart+other..respiratory+other.tertalogy,data=Miss)
summary(CheckSLM)

TotalSLM=lm(Miss$TOTAL.COST.TO.HOSPITAL.~.,data=Miss)
summary(TotalSLM)
TotaCoeff<-TotaSLM$coefficients
sort(TotaCoeff)
Miss$PAST.MEDICAL.HISTORY.CODE
plot(Miss$KEY.COMPLAINTS..CODE)
boxplot(Miss)
cor(Miss)
install.packages('Hmisc')
library(Hmisc)
AgeLn<-rcorr(as.matrix(Miss[,c(1,36)]),type='pearson')
AgeLn
Ageln1<-cor.test(Miss[,1],Miss[,36],type='pearson')
colnames(Miss)
Correlation <- read_excel("C:/Users/Aadish/Desktop/Correlation.xlsx")
View(Correlation)
Corrr<-rcorr(as.matrix(Correlation[,c(1:16)]),type='pearson')
Corrr1<-rcorr(as.matrix(Miss[,c(1:40)]),type='pearson')
Corrr
warnings()
Corrr$r
Corrr$p
warnings()
Corrr$r
try1<-as.data.frame(Corrr$r)
try2<-as.data.frame(Corrr1$r)
install.packages('xlsx')
library(rJava)
write.csv(try2, "c:/Users/Aadish/Desktop/try2.csv")

Missio<-read_excel('D:/UIC/Healthcare Information Management/Misadm.xlsx')
Missio<-as.data.frame(unclass(Missio))
admSLM=lm(Missio$Ln.Total.Cost.~AGE+MALE+UNMARRIED+RHD+BODY.WEIGHT+BP..HIGH+
            BP.LOW+HR.PULSE+Diabetes2:UREA+CREATININE+ELECTIVE+HB+RR+other..heart+other..respiratory+other.tertalogy,data = Missio)
summary(admSLM)
admSLM1=lm(Missio$Ln.Total.Cost.~.,data=Missio)
summary(admSLM1)









