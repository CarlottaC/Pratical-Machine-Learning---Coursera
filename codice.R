library(ggplot2)
library(lattice)
library(caret)
library(rpart)
library(randomForest)
library(gmodels)
library(kernlab)
set.seed(12345)

if(!file.exists("./data")){dir.create("./data")}
fileUrlTrain <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
download.file(fileUrlTrain,destfile="./data/TrainDataset.csv", method="curl")
fileUrlTest <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
download.file(fileUrlTest,destfile = "./data/TestDataset.csv", method="curl")

training <- read.csv('./data/TrainDataset.csv', na.strings=c("NA","#DIV/0!",""))
testing <- read.csv('./data/TestDataset.csv',na.strings=c("NA","#DIV/0!",""))
testing <- testing[, colSums(is.na(testing)) == 0]
training <- training[, colSums(is.na(training)) == 0]
training <-training[,-c(1:7)]
testing <-testing[,-c(1:7)]

inTrain <- createDataPartition(y=training$classe, p=0.7, list=FALSE)
bTraining <- training[inTrain, ]
bTesting <- training[-inTrain, ]
dim(bTraining)
dim(bTesting)

model <- randomForest(classe ~. , data=bTraining, method="class")
model2 <- ksvm(classe ~. ,data=bTraining, kernel="rbfdot")

prediction <- predict(model, bTesting, type = "class")
prediction2 <- predict(model2, bTesting)

confusionMatrix(prediction, bTesting$classe)
confusionMatrix(prediction2, bTesting$classe)

plot(model, log="y")
varImpPlot(model)

predictfinal <- predict(model, testing, type="class")
predictfinal

ct <- CrossTable(prediction, bTesting$classe,
                 prop.chisq = F,
                 prop.t = F,
                 prop.r = T,
                 prop.c = F,
                 dnn=c("predicted","actual"))

total.acc <- (ct$t[1,1] + ct$t[2,2] + ct$t[3,3] + ct$t[4,4] +ct$t[5,5]) / length(prediction)
total.acc

pml_write_files = function(x){
        n = length(x)
        for(i in 1:n){
                filename = paste0("problem_id_",i,".txt")
                write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
        }
}
pml_write_files(predictfinal)

