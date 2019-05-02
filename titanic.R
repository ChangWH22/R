library(rio)
library(tidyverse)
test=import("test.csv")
train=import("train.csv")
full=bind_rows(train,test) ##bind_rows合併檔案Column數不同顯示NA
lt=dim(train)[1]
str(full)
#missing value
colSums(is.na(full))
colSums(full=="")
##change the empty of embark to "c"
full$Embarked[full$Embarked==""]="C"
table(full$Embarked)

##see how many features we can move to factor
apply(full,2,function(x) length(unique(x)))

# Let's move the features Survived, Pclass, Sex, Embarked to be factors
cols=c("Survived","Pclass","Sex","Embarked")
for (i in cols) {
full[,i]=as.factor(full[,i])  
}
str(full)

#analysis
ggplot(data=full[1:lt,],aes(x=Sex,fill=Survived))+
  geom_bar()

ggplot(data=full[1:lt,],aes(x=Embarked,fill=Survived))+
  geom_bar(position = "fill")+ylab("frequency")

t=table(full[1:lt,]$Embarked,full[1:lt,]$Survived)

for (i in 1:dim(t)[1]){
  t[i,]<-t[i,]/sum(t[i,])*100
}
t

ggplot(data=full[1:lt,],aes(x=Pclass,fill=Survived))+
  geom_bar(position = "fill")

ggplot(data=full[1:lt,],aes(x=Embarked,fill=Survived))+
  geom_bar(position = "fill")+facet_wrap(~Pclass)
full$Fare[is.na(full$Fare)] <- mean(full$Fare,na.rm=T)
#######
full$Age[is.na(full$Age)]=mean(full$Age,na.rm = T)
sum(is.na(full$Age))
full$Title <- gsub('(.*, )|(\\..*)', '', full$Name)
full$Title[full$Title == 'Mlle']<- 'Miss' 
full$Title[full$Title == 'Ms']<- 'Miss'
full$Title[full$Title == 'Mme']<- 'Mrs' 
full$Title[full$Title == 'Lady']<- 'Miss'
full$Title[full$Title == 'Dona']<- 'Miss'
officer<- c('Capt','Col','Don','Dr','Jonkheer','Major','Rev','Sir','the Countess')
full$Title[full$Title %in% officer]<-'Officer'
ggplot(data = full[1:lt,],aes(x=Title,fill=Survived))+geom_bar(position="fill")+ylab("Frequency")
full$Title<- as.factor(full$Title)

###
train_lm=full[1:lt,c("Survived","Pclass","Sex","Age","Fare","SibSp",
                     "Parch","Title")]
idx=sample(1:dim(train_lm)[1],500)
train1=train_lm[idx,]
train2=train_lm[-idx,]
model=glm(Survived~.,family = binomial(link = "logit"),data=train1)
summary(model)

pred.train=predict(model,train2)
pred.train=ifelse(pred.train>0.5,1,0)
mean(pred.train==train2$Survived)
t1=table(pred.train,train2$Survived)
t1
presicion=t1[1,1]/sum(t1[1,])
recall=t1[1,1]/sum(t1[,1])
F1<- 2*presicion*recall/(presicion+recall)
F1

test_lm=full[(lt+1):1309,c("Survived","Pclass","Sex","Age","Fare","SibSp",
                         "Parch","Title")]
pred.test=predict(model,test_lm)
pred.test=ifelse(pred.test>0.5,1,0)
res=data.frame(test$PassengerId,pred.test)
names(res)<-c("PassengerId","Survived")
write.csv(res,file="res.csv",row.names = F)
sum(is.na(res))
