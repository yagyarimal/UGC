library(keras)
library(mlbench)
library(dplyr)
library(MASS)
library(magrittr)
library(neuralnet)
data <- read.csv("D:/dataanalysisfinal/UGC/rcode/Dataanalysiscsv.csv", stringsAsFactors = FALSE)
# all variable must in numerical  representation the predicted variable in either0 or 1 too
str(data)
data=as.data.frame(data)
dim(data)

data$GPA=as.numeric(data$GPA)
data$GPAA=as.numeric(data$GPAA)
data$School=as.numeric(data$School)
data$Plus=as.numeric(data$Plus)
data$Physics=as.numeric(data$Physics)
data$Math=as.numeric(data$Math)
data$Chemistry=as.numeric(data$Chemistry)
data$Bio=as.numeric(data$Bio)
data$Parent1=as.numeric(data$Parent1)
data$X1=as.numeric(data$X1)
data$X2=as.numeric(data$X2)
data$X3=as.numeric(data$X3)
data$X3=as.numeric(data$X3)
data$X4=as.numeric(data$X4)
data$X5=as.numeric(data$X5)
data$X6=as.numeric(data$X6)
str(data)
install.packages("tidyverse")
library(magrittr)
library(tidyverse) 
ss=data %>% 
  select(GPAA,SLC,School,Plus,Physics,Math,Chemistry,Bio,Parent1,
         X1,X2,X3,X4,X5,X6) 
str(ss)
#write.csv(ss,file ="D:/dataanalysisfinal/UGC/rcode/S1.csv")
library(tidyverse)
sk=data %>% 
  select(GPAA,SLC,School,Plus,Physics,Math,Chemistry,Bio,Parent1,
         X1,X2,X3,X4,X5,X6) %>% filter(GPAA>0)
str(sk)
#write.csv(sk,file ="D:/dataanalysisfinal/UGC/rcode/Sk.csv")
str(ss)
ss=read.csv("D:/dataanalysisfinal/UGC/rcode/S1.csv")
ss=as.data.frame(ss)
sk=read.csv("D:/dataanalysisfinal/UGC/rcode/sk.csv")
sk=as.data.frame(sk)
sss=ss
skk=sk


#Deep learning  Full model
library(keras)
library(mlbench)
#library(dplyr)
library(MASS)
library(magrittr)
library(neuralnet)
library(tensorflow)

data <- read.csv("D:/dataanalysisfinal/UGC/rcode/s1.csv")
data%<>%mutate_if(is.factor,as.numeric)#converting factor to numeric
levels(data$GPAA)
n=neuralnet(GPAA~SLC+Plus+Physics+Math+Chemistry+Parent1+School+Bio+X1+X2+X3+X4+X5+X6,
            data=data,
            hidden=c(10,5),
            linear.output=F,
            lifesign='full',
            rep=1)
plot(n, col.hidden='darkgreen',
     col.hidden.synapse = 'darkgreen',
     show.weight=T,
     information=F,
     fill='lightblue')
#matrix
data=as.matrix(data)
dimnames(data)=NULL
#normalization
install.packages("dplyr")
install.packages("reticulate")
library(reticulate)
library(plyr)
library(caret)
library(plotly)
library(heatmaply)
data=ss#<- read.csv("D:/dataanalysisfinal/UGC/rcode/s1.csv")
normalize=function(x){
  return ((x-mean(x,na.rm=TRUE))/sd(x,na.rm=TRUE))
}
data=as.data.frame(apply(data[2:15],1,normalize)) #1 for columns 2 for row
summary(data)
data[,-5]=scale(data[,-5])# -1 to 1
summary(data) #keras

library(keras)
install_keras()
library(tensorflow)
install_tensorflow()
library(tensorflow)
library(reticulate)
data <- read.csv("D:/dataanalysisfinal/UGC/rcode/s1.csv")

data[,2:15]=normalize(data[,2:15])
summary(data)
#partition
set.seed(1234)
ind=sample(2,nrow(data),replace=T, prob=c(.7,.3))
training=data[ind==1,2:15]
test=data[ind==2,2:15]
trainingtarget=data[ind==1,1]#independent
testtarget=data[ind==2,1]#independent 
#hot coding
trainLevels=to_categorical(trainingtarget)
testLevels=to_categorical(testtarget)
print(trainLevels)
#model
model=keras_model_sequential()
model%>%
  layer_dense(units=10,activation = 'relu', #10 neuron in hidden  layer
              input_shape =c(14)) %>% 
  layer_dense(units=8)#default output activation Linear activation
summary(model)
#10*14+10
#8*10+8
#compile for learning process Y is numeric there is regression problem
model %>% compile(loss='mse',
                  optimizer='rmsprop', #default
                  metrics='mae')# mean absolute error
#model %>% compile(loss='categorical_crossentropy', optimizer='rmsprop',metrics='accuracy')# mean absolute error
#fit
histry=model %>% 
  fit(training,trainingtarget,
      epoch=100,
      batch_size=32,
      validation_split=0.2)#20% data validation loss for outof sample 
plot(histry)
#evaluation
model1 = model %>% 
evaluate(test,testtarget)
pred=model %>% predict_classes(test)
mean((testtarget-pred)^2)
plot(testtarget,pred)
prob=model %>% predict_proba(test)
table1=table(Predicted=pred,Actual=testtarget)
pred2=model %>% predict_classes(training)
table11=table(Predicted=pred2,Actual=trainingtarget)
cbind(prob,pred,testtarget)

#fine tune improvement(50,25,8)
model=keras_model_sequential()
model%>%
  layer_dense(units=50,activation = 'relu', 
              input_shape =c(14)) %>% 
  layer_dense(units=25,activation = 'relu') %>% 
  layer_dense(units=8)#default activation Linear activation
summary(model)
#compile Y is numeric there is regression problem
model %>% compile(loss='mse',
                  optimizer='rmsprop', #default
                  metrics='mae')# mean absolute error
#fit
histry=model %>% 
  fit(training,trainingtarget,
      epochs=100,
      batch_size=32,
      validation_split=0.2)
plot(histry)
#evaluation
model2 =model%>% 
  evaluate(test,testtarget)
pred=model %>% predict_classes(test)
mean((testtarget-pred)^2)
plot(testtarget,pred)
prob=model %>% predict_proba(test)
table2=table(Predicted=pred,Actual=testtarget)
pred2=model %>% predict_classes(training)
table22=table(Predicted=pred2,Actual=trainingtarget)
cbind(prob,pred,testtarget)

#fine tune improvement for improvement using (100,50,20, 8)
model=keras_model_sequential()
model%>%
  layer_dense(units=100,activation = 'relu',input_shape =c(14)) %>% 
  layer_dropout(rate=0.4) %>%      # 40 % neurons were being ignored while model
  layer_dense(units=50,activation = 'relu') %>% 
  layer_dropout(rate=0.3) %>% 
  layer_dense(units=20,activation = 'relu') %>% 
  layer_dropout(rate=0.2) %>% 
  layer_dense(units=8)#default activation Linear activation

summary(model)
#compile Y is numeric there is regression problem
model %>% compile(loss='mse',
                  optimizer=optimizer_rmsprop(lr=0.001), #rmsprop', #default
                  metrics='mae')# mean absolute error
#fit
history=model %>% 
  fit(training,trainingtarget,
      epochs=100,
      batch_size=32,
      validation_split=0.2)
plot(history)
#evaluation
model3 =model%>% 
  evaluate(test,testtarget)
pred=model %>% predict_classes(test)
mean((testtarget-pred)^2)
plot(testtarget,pred)
prob=model %>% predict_proba(test)
table3=table(Predicted=pred,Actual=testtarget)
pred2=model %>% predict_classes(training)
table33=table(Predicted=pred2,Actual=trainingtarget)
cbind(prob,pred,testtarget)
model1
#train
table1
#test
model2
table2
model3
table3
####################Deep Learning  Half Model
library(keras)
library(mlbench)
library(dplyr)
library(MASS)
library(magrittr)
library(neuralnet)
data <- read.csv("D:/dataanalysisfinal/UGC/rcode/s2.csv")
data%<>%mutate_if(is.factor,as.numeric)#converting factor to numeric
levels(data$GPAA)
n=neuralnet(GPAA~SLC+Plus+Physics+Math+Chemistry+Parent1+School+Bio+X1+X2+X3+X4+X5+X6,
            data=data,
            hidden=c(50,25,8),
            linear.output=F,
            lifesign='full',
            rep=1)
plot(n, col.hidden='darkgreen',
     col.hidden.synapse = 'darkgreen',
     show.weight=T,
     information=F,
     fill='lightblue')
#matrix
data=as.matrix(data)
dimnames(data)=NULL
#normalization
data[,2:15]=normalize(data[,2:15])
#partition
set.seed(1234)
ind=sample(2,nrow(data),replace=T, prob=c(.7,.3))
training=data[ind==1,2:15]
test=data[ind==2,2:15]
trainingtarget=data[ind==1,1]#independent
testtarget=data[ind==2,1]#independent 
#hot coding

trainLevels=to_categorical(trainingtarget)
testLevels=to_categorical(testtarget)
print(trainLevels)
#model MLP (10,7) Middle layer
model=keras_model_sequential()
model%>%
  layer_dense(units=10,activation = 'relu', #10 neuron in hidden  layer
              input_shape =c(14)) %>% 
  layer_dense(units=7)#default output activation Linear activation
summary(model)
#10*14+10
#7*10+7
#compile for learning process Y is numeric there is regression problem
model %>% compile(loss='mse',
                  optimizer='rmsprop', #default
                  metrics='mae')# mean absolute error
#model %>% compile(loss='categorical_crossentropy', optimizer='rmsprop',metrics='accuracy')# mean absolute error
#fit
histry=model %>% 
  fit(training,trainingtarget,
      epochs=100,
      batch_size=32,
      validation_split=0.2)#20% data validation loss for outof sample 
plot(histry)
#evaluation
model1 = model %>% 
  evaluate(test,testtarget)
pred=model %>% predict_classes(test)
mean((testtarget-pred)^2)
plot(testtarget,pred)
prob=model %>% predict_proba(test)
table1=table(Predicted=pred,Actual=testtarget)
pred2=model %>% predict_classes(training)
table11=table(Predicted=pred2,Actual=trainingtarget)
cbind(prob,pred,testtarget)





#fine tune improvement (50,25,7)
model=keras_model_sequential()
model%>%
  layer_dense(units=50,activation = 'relu', 
              input_shape =c(14)) %>% 
  layer_dense(units=25,activation = 'relu') %>% 
  layer_dense(units=7)#default activation Linear activation
summary(model)
#compile Y is numeric there is regression problem
model %>% compile(loss='mse',
                  optimizer='rmsprop', #default
                  metrics='mae')# mean absolute error
#fit
histry=model %>% 
  fit(training,trainingtarget,
      epochs=100,
      batch_size=32,
      validation_split=0.2)
plot(histry)
#evaluation
model2 =model%>% 
  evaluate(test,testtarget)
pred=model %>% predict_classes(test)
mean((testtarget-pred)^2)
plot(testtarget,pred)
prob=model %>% predict_proba(test)
pred2=model %>% predict_classes(training)
table22=table(Predicted=pred2,Actual=trainingtarget)
table2=table(Predicted=pred,Actual=testtarget)
cbind(prob,pred,testtarget)

#fine tune improvement (100,50,20,7)
model=keras_model_sequential()
model%>%
  layer_dense(units=100,activation = 'relu',input_shape =c(14)) %>% 
  layer_dropout(rate=0.4) %>%      # 40 % neurons were being ignored while model
  layer_dense(units=50,activation = 'relu') %>% 
  layer_dropout(rate=0.3) %>% 
  layer_dense(units=20,activation = 'relu') %>% 
  layer_dropout(rate=0.2) %>% 
  layer_dense(units=7)#default activation Linear activation
summary(model)
#compile Y is numeric there is regression problem
model %>% compile(loss='mse',
                  optimizer=optimizer_rmsprop(lr=0.001), #rmsprop', #default
                  metrics='mae')# mean absolute error
#fit
history=model %>% 
  fit(training,trainingtarget,
      epochs=100,
      batch_size=32,
      validation_split=0.2)
plot(history)
#evaluation
model3 =model%>% 
  evaluate(test,testtarget)
pred=model %>% predict_classes(test)

mean((testtarget-pred)^2)
plot(testtarget,pred)
prob=model %>% predict_proba(test)
table3=table(Predicted=pred,Actual=testtarget)
pred2=model %>% predict_classes(training)
table33=table(Predicted=pred2,Actual=trainingtarget)
cbind(prob,pred,testtarget)
model1
#train 209
table1
##test 81
table11
model2
#train 209
table2
##test 81
table22
model3
#train 209
table3
##test 81
table33
# hypermeter tuinig: according wikipedia in machine learning, hypermeter optimization is the p roblem of chosing a set of optmal hypermerers for learing alroritn which  control the lerning process with the valuue of other paramer learned.
#
getwd()
setwd("D:/dataanalysisfinal/UGC/rcode")
library(reticulate)
library(tensorflow)
devtools::install_github("r-lib/crayon",force = TRUE)
devtools::install_github("rstudio/keras",dependencies = TRUE,force = TRUE)
library(keras)
library(tfruns)

data <- read.csv("D:/dataanalysisfinal/UGC/rcode/sk.csv")
str(data)
data=na.omit(data)
is.na(data)
data=as.matrix(data)# convection matrix
dimnames(data)=NULL# making v1 v2
str(data)
#normalization
data[,2:15]=normalize(data[,2:15])
data[,1]=as.numeric(data[,1])-1
set.seed(1234)
ind=sample(2,nrow(data),replace=T, prob=c(.7,.3))
training=data[ind==1,2:15]
test=data[ind==2,2:15]
trainingtarget=data[ind==1,1]#independent
testtarget=data[ind==2,1]#independent 
#onehot encoding
trainlabels= to_categorical(trainingtarget)
testlabels= to_categorical(testtarget)
str(sk)
print(testlabels)
library(keras)
library(tensorflow)
library(tfruns)
str(skk)
levels(skk$GPAA)
runs=tuning_run("experiment1.R",
                flags=list(dense_units=c(32,64)))  #single hyper parameter

head(runs)
runs[,c(5,6)]

nruns=tuning_run("experiment2.R",flags=list(dense_units1=c(32,64),
                                            dense_units2=c(16,32),
                                            dropout1=c(0.1,0.2),
                                            dropout2=c(0.1,0.2),
                                            batch_size=c(32,64))) # 2^5=32 parameters  in five layers
#best hypermeter values
head(nruns)
nruns[,c(5,6)]
results=nruns[,c(3,5:10)] 
results
str(nruns)






data=cbind(66,66)
colnames(data)=c("Input","Output")
View(data)
library(neuralnet)
mod=neuralnet(formula=Output~Input,
              data=data,
              hidden=2,
              threshold=0.01,
              lifesign = "full",
              lifesign.step = 10)
plot(mod)

#ann
traininginput=as.data.frame(runif(50,min=0,max=225))
trainingoutput=sqrt(traininginput)
data=cbind(traininginput,trainingoutput)
colnames(data)=c("Input","Output")
View(data)
library(neuralnet)
mod=neuralnet(formula=Output~Input,
              data=data,
              hidden=10,
              threshold=0.01,
              lifesign = "full",
              lifesign.step = 10)
plot(mod)

