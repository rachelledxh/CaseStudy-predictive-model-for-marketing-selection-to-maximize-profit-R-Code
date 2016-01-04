##### Decision Tree
library (ISLR)
library (tree)

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


tree_model = tree(responded~.-id,training_data_WNA)
plot(tree_model)
text(tree_model,pretty=0)

observation_tree = training_data_WNA$responded
tree_pred = predict(tree_model,training_data_WNA)
tree_pred = ifelse (as.numeric(tree_pred) >= 0.5,1,0)
mean(tree_pred != observation_tree) # miss classification error = 9.035727%
table(tree_pred,observation_tree)

observation_tree_test = testing_data$responded
tree_pred_test = predict(tree_model,testing_data)
tree_pred_test = ifelse (as.numeric(tree_pred_test) >= 0.5,1,0)
mean(tree_pred_test!= observation_tree_test) # miss classification error = 9.466%
table(tree_pred_test,observation_tree_test)

tree.probs_testC =predict(tree_model,Testing_Candidate) # The type="response"

# option tells R to output probabilities of the form P(Y = 1|X), as opposed
# to other information such as the logit
tree.pred_TestC= ifelse (as.numeric(tree.probs_testC) >= 0.5,1,0)
Testing_Candidate= cbind(Testing_Candidate,tree.pred_TestC)
