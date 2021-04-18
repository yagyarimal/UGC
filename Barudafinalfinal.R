#Boruta Algorithum for feature selection of there were large variables and some times produces less accurate output due to  many reasons  if there were large number of features which deliminites time construansts to formed model
# there were some shadow attributes do not  required for the model  which shuffled all values then cleates model including both of them then importance will be analyzed for the reductuoin of model developments
library(Boruta)
library(mlbench)
library(caret)
library(randomForest)
library(dplyr)
library(tidyverse)
data=read.csv("D:/dataanalysisfinal/SOE/rcode/Dataanalysiscsv.csv")
#S <- read.csv("D:/dataanalysisfinal/SOE/rcode/BB.csv")
#data("Sonar")
S=as.data.frame(data)
S=na.omit(S)
str(S)
dim(S)
#subsetting data
ss=S %>% 
  select(GPA,SLC,School,Plus,Physics,Math,Chemistry,Bio,Parent1,Parent2,X1,X2,X3,X4,X5,X6,X111,X222,X333,X444,X555,X666) 
 #%>%  filter(GPA>0)
dim(ss)
Sr=as.data.frame(apply(ss[, 1:22], 2, function(x) (x - min(x))/(max(x)-min(x))))
str(Sr)
summary(Sr)
hist(Sr$Physics)
set.seed(111)
#feature selection
names(Sr)
write.csv(Sr,file ="D:/dataanalysisfinal/SOE/rcode/SSeeeee.csv")
#data("Sonar")
str(Sr)
Sr=as.data.frame(Sr)
Sr=na.omit(Sr)
boruta=Boruta(GPA~.,data=Sr,doTrace=2,maxRun=400)
print(boruta) #out of 21 predictor variables  17 conformed important, 2 unimportant and 1 tentative in 2.61 minitues runs on 400 iterations
# by default there were 100 iteration implies the output still lefft for the conformation more colser results it 400 times
# 17+2+1 attributes were important, 17 attribute were   important and some were cont conformed

plot(boruta,las=2,cex.axis=0.7)
# describes in box plot plotsgreen conformed box plot red not important yellow not tentative black shodow attributes
# 
plotImpHistory(boruta)
#for the purpose to  filter important and non important 33+8=41 attributes



bor=TentativeRoughFix(boruta)#
# for tentative roughfixed clearly expressed important or non important
print(bor)# conformed and rejected 17+1=18 and 2 unimportant Parent2 and X3 internal marks due to similarity found in other cells
attStats(boruta)
# mean  imp , mean max imp and decision describes conformed, tentative, and rejected with percent
#data partition for incresing accuracy or not
#data partiation

getNonRejectedFormula(boruta)#17+2=19
# for conformed formula
getConfirmedFormula(boruta)#19conformed

#Random Forest
# Random forest developed by aggregating trees. can be applicable in both classification as well as regression, which aboid oberfitting, can deal wwith large numbers of features, helps with feature selection based on importance userfriendly  because only tow parameters would be given tree 500 default and mtry variable randomly sampled as candidate at each split default sqrt(p) for classification and p/3 for regression
#where p is features draw ntree bootstrap sample. each bootstrap sample grow unprowned tree by choosing best split based on random sample of mtry predicator at each node and predict new data using mahority votes for classifcation and average for regression baased  on ntree 
data=read.csv("D:/dataanalysisfinal/SOE/rcode/Dataanalysiscsv.csv")
S <- read.csv("D:/dataanalysisfinal/SOE/rcode/SSeee.csv")
library(randomForest)
library(tidyverse)
S=data
set.seed(333)
str(S)
S=na.omit(S)
#only passed student
ss=S %>% 
  select(GPA,GPAA,GPAAA,SLC,School,Plus,Physics,Math,Chemistry,Bio,Parent1,Parent2,X1,X2,X3,X4,X5,X6,X111,X222,X333,X444,X555,X666) %>% 
filter(GPA>0)
S=as.data.frame(ss)
S$GPAA=as.factor(S$GPAA)# for regression 
S$GPAAA=as.factor(S$GPAAA)# for classification

str(S)
table(S$GPAA)# normal description
table(S$GPAAA)
str(S)
S
set.seed(123)
S=as.data.frame(S)
ind=sample(2,nrow(S),replace=T,prob=c(.7,.3))
train=S[ind==1,]#211 for pass only
test=S[ind==2,] # 79 out of 290 passed
str(S)
levels(S$GPAAA)
#full parameters in default mtry=4 and trees 500

rf23=randomForest(GPAAA~.,data=train)
#rf23=randomForest(GPAAA~SLC+School+Plus+Physics+Math+Chemistry+Bio+Parent1+Parent2+X1+X2+X3+X4+X5+X6+X111+X222+X333+X444+X555+X666,data=train,ntree=100, mtry=4, importance=TRUE, proximity=TRUE)


# here the GPAAA a categorical variable with numerical representation of student grades 1 for A,2for A-,3for B+,4 for B,5 for B-,6 for c+,7 for C- and 8 for Fail in GPAA and GPAA variables
#  as factor variables for all variables model prediction and  confusion matrix oob bag indicates error rate
print(rf23)
 #21 important+ 3 tentative attributes 19 unimportant
attributes(rf23)
names(S)

library(caret)
library(lattice)
library(purrr)
p=predict(rf23,train)
head(p) 
head(S$GPAAA)
confusionMatrix(p,train$GPAAA)
#overall 2 missclassification sensitivity  the class1 classification=30/0 c=0/1 and c+=3/1 
#as compared OOB errors for each bootstrap iteration and related tree prediction error using doata not in bootstrap sample is called OOB is estimated classification accuracy and regression RMSE
pp=predict(rf23,test)
head(pp) 
head(S$GPAA)

confusionMatrix(pp,test$GPAAA)
str(S)
plot(rf23)# Trees after 100 is ok
# is for Mtry determinism - indicates excluding with response GPAA
t=tuneRF(train[,-1,-2],train[,3],
       stepFactor = 0.5,# each iteration mtry will be inflicted _+
       plot=TRUE,ntreeTry = 500,
        trace=TRUE, 
        improve=0.05)# improve into outbag errors

#mtry after 16 is best
plot(rf23 )
#tree size in terms of number of nodes after 40 is correct
hist(treesize(rf23),
     main="Number of Nodes",
     col="green")
# there are 30 trees  15 trees

varImpPlot(rf23,sort=T,
           n.var=21,
           main="All Varaibles Ranking Plot")# this describes significant of variables  in model

importance(rf23)

varUsed(rf23)# first to end times occurred in the model
#Testing of different values
partialPlot(rf23,train,SLC,"C") #partial dependency plot describes marginal affect of probability
partialPlot(rf23,train,Plus,"A")
partialPlot(rf23,train,Math,"A")
partialPlot(rf23,train,X111,"A") #partial dependency plot describes marginal affect of probability
partialPlot(rf23,train,X111,"A-")
partialPlot(rf23,train,Parent1,"A")
partialPlot(rf23,train,X5,"A")
partialPlot(rf23,train,X5,"B+")

getTree(rf23,1,labelVar=TRUE)

#multidimensional Scaling plot of proximity matrix
MDSplot(rf23,train$GPAAA)





# if we considered before examination than final records

#Pure Internal factors the predictor is categorical A A-...
data=read.csv("D:/dataanalysisfinal/SOE/rcode/Dataanalysiscsv.csv")
library(randomForest)
library(tidyverse)
S=data
ss=S %>% 
  select(GPA,GPAAA,GPAA,SLC,School,Plus,Physics,Math,Chemistry,Bio,Parent1,Parent2,X1,X2,X3,X4,X5,X6) %>% 
  filter(GPA>0)
ss$GPAAA=as.factor(ss$GPAAA)
ss$GPAA=as.factor(ss$GPAA)
str(ss)
ind=sample(2,nrow(ss),replace=T,prob=c(.7,.3))
train=ss[ind==1,]#211 for pass only
test=ss[ind==2,] # 79 out of 290 passed
levels(ss$GPAAA)
#final=randomForest(GPAAA~.,data=train)# for accurate predicate
final=randomForest(GPAAA~SLC+School+Plus+Physics+Math+Chemistry+Bio+Parent1+Parent2+X1+X2+X3+X4+X5+X6,data=train,ntree=500, mtry=4,importance=TRUE,proximity=TRUE)
p=predict(final,train)
library(caret)
confusionMatrix(p,train$GPAAA)
#overall 2 miss classification sensitivity  the class1 classification=30/0 c=0/1 and c+=3/1 
#as compared OOB errors for each bootstrap iteration and related tree prediction error using doata not in bootstrap sample is called OOB is estimated classification accuracy and regression RMSE
pp=predict(final,test)
confusionMatrix(pp,test$GPAAA)
plot(final)
#mtey
t=tuneRF(train[,-1],train[,1],
         stepFactor = 0.5,# each iteration mtry will be inflicted _+
         plot=TRUE,ntreeTry = 400,
         trace=TRUE, 
         improve=0.05)# improve into outbag errors mtry=16
#tree size in terms of number of nodes after 40 is correct
hist(treesize(final),
     main="Number of Nodes",
     col="green")
# there are 30 trees  15 trees

varImpPlot(final,sort=T,
           n.var=15,
           main="All Varaibles Ranking Plot")# this describes significant of variables  in model

importance(final)

varUsed(final)# first to end times occurred in the model
#Testing of different values
par(mfrow=c(3,2))
partialPlot(final,train,SLC,"C") #partial dependency plot describes marginal affect of probability
partialPlot(final,train,Plus,"A")
partialPlot(final,train,Math,"A")
partialPlot(final,train,Physics,"A")
partialPlot(final,train,Bio,"A")
partialPlot(final,train,School,"A")
par(mfrow=c(3,2))
partialPlot(final,train,X1,"A") #partial dependency plot describes marginal affect of probability
partialPlot(final,train,X2,"A")
partialPlot(final,train,X3,"A")
partialPlot(final,train,X4,"A")
partialPlot(final,train,X5,"A")
partialPlot(final,train,X6,"A")
getTree(final,2,labelVar=TRUE)# first tree first-1 in status indicates terminal node
#multidimensional Scaling plot of proximity matrix
par(mfrow=c(1,1))
MDSplot(final,train$GPAAA)# mdsplot is is applicable with out prominent


#Pure Internal factors as >3.7 1
data=read.csv("D:/dataanalysisfinal/SOE/rcode/Dataanalysiscsv.csv")
library(randomForest)
library(tidyverse)
S=data
ss=S %>% 
  select(GPA,GPAAA,GPAA,SLC,School,Plus,Physics,Math,Chemistry,Bio,Parent1,Parent2,X1,X2,X3,X4,X5,X6) %>% 
  filter(GPA>0)
ss
ss$GPAA=as.factor(ss$GPAA)
ss$GPAAA=as.factor(ss$GPAAA)
ind=sample(2,nrow(ss),replace=T,prob=c(.7,.3))
train=ss[ind==1,]#211 for pass only
test=ss[ind==2,] # 79 out of 290 passed
levels(ss$GPAA) 
str(ss)
# for accuracy 
final=randomForest(GPAA~.,data=train)# for accurate predicate

final=randomForest(GPAA~SLC+School+Plus+Physics+Math+Chemistry+Bio+Parent1+Parent2+X1+X2+X3+X4+X5+X6, data=train,ntree=500, mtry=4, importance=TRUE, proximity=TRUE)
p=predict(final,train)
confusionMatrix(p,train$GPAA)
#overall 2 miss classification sensitivity  the class1 classification=30/0 c=0/1 and c+=3/1 
#as compared OOB errors for each bootstrap iteration and related tree prediction error using doata not in bootstrap sample is called OOB is estimated classification accuracy and regression RMSE
pp=predict(final,test)
confusionMatrix(pp,test$GPAA)
plot(final)
# For mtey
t=tuneRF(train[,-1],train[,1],
         stepFactor = 0.5,# each iteration mtry will be inflicted _+
         plot=TRUE,ntreeTry = 100,
         trace=TRUE, 
         improve=0.05)# improve into outbag errors mtry=16
#tree size in terms of number of nodes after 40 is correct
hist(treesize(final),
     main="Number of Nodes",
     col="green")
# there are 30 trees  15 trees

varImpPlot(final,sort=T,
           n.var=15,
           main="All Varaibles Ranking Plot")# this describes significant of variables  in model

importance(final)

varUsed(final)# first to end times occurred in the model
#Testing of different values
par(mfrow=c(3,2))
partialPlot(final,train,SLC,"2") #partial dependency plot describes marginal affect of probability
partialPlot(final,train,Plus,"2")
partialPlot(final,train,Math,"2")
partialPlot(final,train,Physics,"2")
partialPlot(final,train,Bio,"2")
partialPlot(final,train,School,"2")
par(mfrow=c(3,2))
partialPlot(final,train,X1,"2") #partial dependency plot describes marginal affect of probability
partialPlot(final,train,X2,"2")
partialPlot(final,train,X3,"2")
partialPlot(final,train,X4,"2")
partialPlot(final,train,X5,"2")
partialPlot(final,train,X6,"2")

getTree(final,1,labelVar=TRUE)# first tree first-1 in status indicates terminal node
#multidimensional Scaling plot of proximity matrix
MDSplot(final,train$GPAA) # only when Large model


#GPA Marks according to 10 Scale grade >90 A,>80,A->75 B+ "55" "60" "65" "70" "75" "80" "85" "90"

data=read.csv("D:/dataanalysisfinal/SOE/rcode/Dataanalysiscsv.csv")
library(randomForest)
library(tidyverse)
S=data
ss=S %>% 
  select(GPAM,OrdGPA,GPA,SLC,School,Plus,Physics,Math,Chemistry,Bio,Parent1,Parent2,X1,X2,X3,X4,X5,X6) %>% 
  filter(GPA>0)
ss
ss$GPAM=as.factor(ss$GPAM)
ss$OrdGPA=as.factor(ss$OrdGPA)
ind=sample(2,nrow(ss),replace=T,prob=c(.7,.3))
train=ss[ind==1,]#211 for pass only
test=ss[ind==2,] # 79 out of 290 passed
levels(ss$GPAM) 
str(ss)
# for accuracy 

#final=randomForest(GPAM~.,data=train)# for accurate predicate

final=randomForest(GPAM~SLC+School+Plus+Physics+Math+Chemistry+Bio+Parent1+Parent2+X1+X2+X3+X4+X5+X6, data=train,ntree=500, mtry=4, importance=TRUE, proximity=TRUE)
p=predict(final,train)
confusionMatrix(p,train$GPAM)
#overall 2 miss classification sensitivity  the class1 classification=30/0 c=0/1 and c+=3/1 
#as compared OOB errors for each bootstrap iteration and related tree prediction error using doata not in bootstrap sample is called OOB is estimated classification accuracy and regression RMSE
pp=predict(final,test)
confusionMatrix(pp,test$GPAM)
plot(final)
# For mtey
t=tuneRF(train[,-1],train[,1],
         stepFactor = 0.5,# each iteration mtry will be inflicted _+
         plot=TRUE,ntreeTry = 100,
         trace=TRUE, 
         improve=0.05)# improve into outbag errors mtry=16
#tree size in terms of number of nodes after 40 is correct
hist(treesize(final),
     main="Number of Nodes",
     col="green")
# there are 30 trees  15 trees

varImpPlot(final,sort=T,
           n.var=15,
           main="All Varaibles Ranking Plot")# this describes significant of variables  in model

importance(final)

varUsed(final)# first to end times occurred in the model
#Testing of different values
par(mfrow=c(3,2))
partialPlot(final,train,SLC,"60") #partial dependency plot describes marginal affect of probability
partialPlot(final,train,Plus,"70")
partialPlot(final,train,Math,"90")
partialPlot(final,train,Physics,"55")
partialPlot(final,train,Bio,"55")
partialPlot(final,train,X1,"90") #partial dependency plot describes marginal affect of probability
partialPlot(final,train,X2,"90")
partialPlot(final,train,X3,"80")
partialPlot(final,train,X4,"75")
partialPlot(final,train,X5,"85")
partialPlot(final,train,X6,"85")


getTree(final,1,labelVar=TRUE)# first tree first-1 in status indicates terminal node
#multidimensional Scaling plot of proximity matrix
MDSplot(final,train$GPAM) # only when Large model

#Plus to GPA Marks prediction grade >90 A,>80,A->70 B+> "60" B" >50 C+ >40 C>25 D
data=read.csv("D:/dataanalysisfinal/SOE/rcode/Dataanalysiscsv.csv")
library(randomForest)
library(tidyverse)
S=data
ss=S %>% 
  select(PlustoGrade,GPA,SLC,School,Physics,Math,Chemistry,Bio,Parent1,Parent2,X1,X2,X3,X4,X5,X6) %>% 
  filter(!GPA>0)
str(ss)
summary(ss)
ss=na.omit(ss)
ss$PlustoGrade=as.factor(ss$PlustoGrade)
ss$GPAAA=as.factor(ss$GPAAA)
table(ss$PlustoGrade,ss$GPAAA)
ind=sample(2,nrow(ss),replace=T,prob=c(.7,.3))
train=ss[ind==1,]#211 for pass only
test=ss[ind==2,] # 79 out of 290 passed
levels(ss$PlustoGrade) 
str(ss)
# for accuracy 

final=randomForest(PlustoGrade~.,data=train)# for accurate predicate

final=randomForest(PlustoGrade~SLC+School+Physics+Math+Chemistry+Bio+Parent1+Parent2+X1+X2+X3+X4+X5+X6, data=train,ntree=500, mtry=4, importance=TRUE, proximity=TRUE)
p=predict(final,train)
confusionMatrix(p,train$PlustoGrade)
#overall 2 miss classification sensitivity  the class1 classification=30/0 c=0/1 and c+=3/1 
#as compared OOB errors for each bootstrap iteration and related tree prediction error using doata not in bootstrap sample is called OOB is estimated classification accuracy and regression RMSE
pp=predict(final,test)
confusionMatrix(pp,test$PlustoGrade)
plot(final)
# For mtey
t=tuneRF(train[,-1,],train[,1],
         stepFactor = 0.5,# each iteration mtry will be inflicted _+
         plot=TRUE,ntreeTry = 100,
         trace=TRUE, 
         improve=0.05)# improve into outbag errors mtry=16
#tree size in terms of number of nodes after 40 is correct
hist(treesize(final),
     main="Number of Nodes",
     col="green")
# there are 30 trees  15 trees

varImpPlot(final,sort=T,
           n.var=14,
           main="All Varaibles Ranking Plot")# this describes significant of variables  in model

importance(final)

varUsed(final)# first to end times occurred in the model
#Testing of different values
parmfrow(c)
par(mfrow=c(3,4))
partialPlot(final,train,SLC,"A") #partial dependency plot describes marginal affect of probability
partialPlot(final,train,School,"A")
partialPlot(final,train,Math,"A")
partialPlot(final,train,Physics,"A")
partialPlot(final,train,Bio,"A")
partialPlot(final,train,Chemistry,"A") #partial dependency plot describes marginal affect of probability
partialPlot(final,train,X1,"A")
partialPlot(final,train,X3,"A")
partialPlot(final,train,X4,"A")
partialPlot(final,train,X5,"A")
partialPlot(final,train,X6,"A")


getTree(final,1,labelVar=TRUE)# first tree first-1 in status indicates terminal node
#multidimensional Scaling plot of proximity matrix
MDSplot(final,train$PlustoGrade) # only when Large model
#Bar plot
data=read.csv("D:/dataanalysisfinal/SOE/rcode/Dataanalysiscsv.csv")
library(randomForest)
library(tidyverse)
S=data
ss=S %>% 
  select(PlustoGrade,GPA,SLC,GPAA,GPAAA,Physics,Math,Chemistry,Bio,Parent1,Parent2,X1,X2,X3,X4,X5,X6) %>% 
  filter(GPA>0)
str(ss)
str(ss)
ss=na.omit(ss)
par(mfrow=c(1,2))
counts <- table(ss$PlustoGrade)
barplot(counts, main="Plus2 Grade Distribution",
        xlab="Number of Grades")
counts <- table(ss$GPAAA)
barplot(counts, main="Bachelors Degree Distribution",
        xlab="Number of Grades")
