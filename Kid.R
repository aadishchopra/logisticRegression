library(readxl)
Kid <- read_excel("D:/UIC/Healthcare Information Management/Kid.xls")
View(Kid)
#removing id
Kid<-Kid[,-1]
# removing misssing data on kid
Kid$Income=replace(Kid$Income,is.na(Kid$Income),0)
Kid$PoorVision=replace(Kid$PoorVision,is.na(Kid$PoorVision),0)

Kid.test<-Kid[6001:8820,]
Kid<-Kid[1:6000,]
Kid<-Kid[,-9]
ind=sample(2,nrow(Kid),replace=T,prob=c(0.70,0.30))
train=Kid[ind==1,]
test=Kid[ind==2,]

# logistic regression
kid.model=glm(CKD~.,data=train,family ='binomial')


#refactoring
kid.model$xlevels[["CareSource"]] <- union(kid.model$xlevels[["CareSource"]], levels(test$CareSource))
summary(kid.model)


co=coefficients(kid.model)
ex=exp(co)
sort(ex)


kid.model.pred=predict.glm(kid.model,newdata=test,type='response')
summary(kid.model.pred)
# what if were to omit NA
pred.model<-na.omit(kid.model.pred)

# to check what does xlevel mean

kid.model$xlevels

pred.model

kidney.pred=rep("no",1802)
kidney.pred[kid.model.pred >0.5]="yes"

table(kidney.pred,test$CKD)

sum(is.na(Kid))
for(i in 1:33){
print((paste(sum(is.na(Kid[,i])),colnames(Kid[i]))))
}


sort(colSums(is.na(Kid)))
Kids<-Kid
Kids[which(is.na(Kid)),]
View
dim(Kids)

Kids<-subset(na.omit(Kid))
View(Kids)


# analysing the missing data
library(dplyr)
Diff<-setdiff(Kid,Kids)
sort(colSums(is.na(Diff)))
Diff$Educ==1


# education level of 1
temp<-as.data.frame(temp)

#income level of 0
temp1<-as.data.frame(temp1)
tempdiff<-setdiff(temp1,temp)
temp<-sort(temp[,1],decreasing = F)
temp1<-sort(temp1[,1],decreasing = F)
temp1<-temp1[1:659,]
useit<-cbind(temp,temp1)


## why this piece of code isnt runnning 
for(i in 1:nrow(useit)){
  for(j in 1:nrow(useit)){
    ifelse(useit$temp[2]==useit$temp1[3],"Yes","No")
    flush.console()
  }
}

ifelse(useit$temp[2]==useit$temp1[3],"Yes","No")
temp==temp1

demo<-Kid[,1:6]
Ckd<-Kid[,33]
demo<-c(demo,Ckd)
demo<-as.data.frame(demo)

ind=sample(2,nrow(demo),replace=T,prob=c(0.70,0.30))
train=demo[ind==1,]
test=demo[ind==2,]

# logistic regression
kid.model=glm(CKD~.,data=train,family ='binomial')

#refactoring
kid.model$xlevels[["CareSource"]] <- union(kid.model$xlevels[["CareSource"]], levels(test$CareSource))
summary(kid.model)


co=coefficients(kid.model)
ex=exp(co)
sort(ex)


kid.model.pred=predict.glm(kid.model,newdata=test,type='response')
summary(kid.model.pred)

kidney.pred=rep("no",1797)
kidney.pred[kid.model.pred >0.5]="yes"

table(kidney.pred,test$CKD)



#logistic regression 

kid1.model=glm(CKD~Age+Racegrp+CHF+Stroke+HDL+PVD+Hypertension+Diabetes+Anemia,data=Kid,family='binomial')
summary(kid1.model)
kid1.model.pred=predict.glm(kid1.model,newdata=test,type='response')
summary(kid1.model.pred)
kidney.pred=rep("no",1891)
kidney.pred[kid1.model.pred >0.10]="yes"
kid1.model.pred
mean(Kid.test$Age[which(kid1.model.pred.new>0.50)])

conf=table(kidney.pred,test$CKD)
conf
summary(conf)
table(kidney.pred)
kid1.model.pred.new=predict.glm(kid1.model,newdata=Kid.test,type='response')




