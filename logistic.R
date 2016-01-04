

rm(list = ls())
library(plyr)
MyData <- read.csv(file="training.csv", header=TRUE, sep=",")

#data checking
colnames(MyData)
dim(MyData)
### pdays = 999 and set it =0
### pmonths = 999 and set it =0
MyData[MyData$pdays==999,]$pdays =0
MyData[MyData$pmonths==999,]$pmonths =0
### From the Scatterplot we can see that pdays and pmonths are highly correlated
# pairs(~custAge+pdays+pmonths+campaign+previous+emp.var.rate+cons.price.idx+cons.conf.idx+euribor3m+nr.employed+profit,data=MyData, 
#       main="Simple Scatterplot Matrix")
## pdays and pmonths's correaltion coefficient is 1 so that they are linearly dependent
cor(MyData$pdays, MyData$pmonths)
## remove the column pdays in our dataset
#MyData = MyData[,-12]
MyData$pdays = NULL
# make 1 as yes and 0 as no for the responded variable
MyData$responded = ifelse (MyData$responded =="yes",1,0) 
table(MyData$responded)
## set up the missing variable of profit as -1000 and responded varibale as 0 when the profit <=30 
## since <=30 the insurance company do not earn money
MyData[is.na(MyData$profit),]$profit = -1000
MyData[MyData$profit<=30,]$responded = 0
#MyData = MyData[,-22]
MyData$profit = NULL
MyData$id = NULL
MyData1 = MyData[MyData$default!="yes",] # since there is only one yes response for Default and I remove this row

## splitting traing data into two parts: training and testing
set.seed(2)
train = sample(1:nrow(MyData1),floor(nrow(MyData1)*0.7))
all_rows = (1:nrow(MyData1))
test = all_rows[-train]
training_data_WNA = MyData1[train,]
training_data_WNA = rbind(MyData1[train,],MyData[MyData$default=="yes",]) # Make sure the one of default=yes is included in training set
testing_data = MyData1[-train,]
## remove any rows containg Missing Value
training_data = na.omit(training_data_WNA)

glm.fit=glm(responded~ . ,data=training_data,family=binomial)
glm.fit.sw = step(glm.fit)
summary(glm.fit.sw)

#save data to make univariate chart
write.csv(training_data, file = "training_data.csv")


#type I and type II
glm.probs =predict(glm.fit.sw,training_data,type ="response") # The type="response"
# option tells R to output probabilities of the form P(Y = 1|X), as opposed
# to other information such as the logit
glm.pred= ifelse (as.numeric(glm.probs) >= 0.5,1,0)
### Checking Miss calculation for the model build up using only variable without NA
observation = training_data$responded
mean(glm.pred != observation ) # miss classification error = 7.33%
table(glm.pred,observation )

#model 1 with missing values in predictors
glm.fit2=glm(responded ~ profession + default + contact + month + 
              campaign + poutcome + emp.var.rate + cons.price.idx + cons.conf.idx + 
              nr.employed,data=training_data,family=binomial)
glm.probs2 =predict(glm.fit2,MyData1,type ="response") # The type="response"

# option tells R to output probabilities of the form P(Y = 1|X), as opposed
# to other information such as the logit

glm.pred2= ifelse (as.numeric(glm.probs2) >= 0.5,1,0)

observation  = MyData1[train,]$responded
mean(glm.pred2[train] != observation ) # miss classification error = 8.002884%
table(glm.pred2[train],observation )

testing_high2 = MyData1[test,]$responded
mean(glm.pred2[test] != testing_high2) # miss classification error = 7.89916%
table(glm.pred2[test],testing_high2)

#create function to calculate Somers'D
SD = function(goodbad) {
  A = abs(0.5*sum((c(0,cumsum(goodbad$bad)[1:4])/sum(goodbad$bad) + (cumsum(goodbad$bad)/sum(goodbad$bad)))*
                    (cumsum(goodbad$good)/sum(goodbad$good) - c(0,cumsum(goodbad$good)[1:4])/sum(goodbad$good)))-0.5)/0.5
  return(A)
}

#create liftchart for training 
predict = glm.probs2[train]
train_results = as.data.frame(cbind(observation,predict))
colnames(train_results)= c("class","predict")

train_results$dec = as.numeric(with(train_results, cut(predict, breaks=quantile(train_results$predict,seq(0,1,0.2)),include.lowest=TRUE)))
lift = aggregate(train_results$class, by=list(decile=train_results$dec), FUN = "mean")
bad = aggregate(train_results$class, by=list(decile=train_results$dec), FUN = "sum")
good = c(table(train_results$dec)) - bad[,2]
goodbad = data.frame(decile=1:5, bad = bad[,2], good)

SD(goodbad)
plot(lift,type="b",pch=16,ylim=c(0,max(lift$x)+0.01),main=paste("Liftchart \nSomers'D = ",round(SD(goodbad),4)),xlab = "Qintiles",ylab = 'Malignant Rate')

#SD and liftchar on testing
predict = glm.probs2[test]
train_results = as.data.frame(cbind(testing_high2,predict))
colnames(train_results)= c("class","predict")

train_results$dec = as.numeric(with(train_results, cut(predict, breaks=quantile(train_results$predict,seq(0,1,0.2)),include.lowest=TRUE)))
lift = aggregate(train_results$class, by=list(decile=train_results$dec), FUN = "mean")
bad = aggregate(train_results$class, by=list(decile=train_results$dec), FUN = "sum")
good = c(table(train_results$dec)) - bad[,2]
goodbad = data.frame(decile=1:5, bad = bad[,2], good)

SD(goodbad)
plot(lift,type="b",pch=16,ylim=c(0,max(lift$x)+0.01),main=paste("Liftchart \nSomers'D = ",round(SD(goodbad),4)),xlab = "Qintiles",ylab = 'Malignant Rate')

