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
MyData$ID = NULL
MyData1 = MyData[MyData$default!="yes",] # since there is only one yes response for Default and I remove this row

## splitting traing data into two parts: training and testing
set.seed(2)
train = sample(1:nrow(MyData1),floor(nrow(MyData1)*0.7))
all_rows = (1:nrow(MyData1))
test = all_rows[-train]
training_data_WNA = rbind(MyData1[train,],MyData[MyData$default=="yes",])
testing_data = MyData1[-train,]
## remove any rows containg Missing Value
training_data = na.omit(training_data_WNA)

glm.fit=glm(responded~.-id,data=training_data,family=binomial)
glm.fit.sw = step(glm.fit)
summary(glm.fit.sw)


#model 1 with missing values in predictors
glm.fit2=glm(responded ~ profession + default + contact + month + 
               campaign + poutcome + emp.var.rate + cons.price.idx + cons.conf.idx + 
               nr.employed,data=training_data,family=binomial)
glm.probs2 =predict(glm.fit2,MyData1,type ="response") # The type="response"

# option tells R to output probabilities of the form P(Y = 1|X), as opposed
# to other information such as the logit

glm.pred2= ifelse (as.numeric(glm.probs2) >= 0.5,1,0)


# Apply logistic Model on testing Candidate Data
Testing_Candidate <- read.csv(file="/Users/xiuhongdu/Dropbox/Du Wang/uptake/testingCandidate.csv", header=TRUE, sep=",")
Testing_Candidate[Testing_Candidate$pdays==999,]$pdays =0
Testing_Candidate[Testing_Candidate$pmonths==999,]$pmonths =0
Testing_Candidate$pdays =NULL

glm.probs_testC =predict(glm.fit2,Testing_Candidate,type ="response") # The type="response"

# option tells R to output probabilities of the form P(Y = 1|X), as opposed
# to other information such as the logit

glm.pred_TestC= ifelse (as.numeric(glm.probs_testC) >= 0.5,1,0)
Testing_Candidate= cbind(Testing_Candidate,glm.pred_TestC)





