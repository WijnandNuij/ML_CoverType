# rm(list=ls(all=TRUE))

runTestSet <- function(targetLocation='/home/wijnand/R_workspace_covertype/resources/result.csv')
{
        trainingSet <- loadData()
        trainingSet$Cover_Type <- as.factor(trainingSet$Cover_Type)
        trainingSet$Id <- NULL
        
        trainedModel <- c50Tree(trainingSet)
        
        testSet <- loadData('/home/wijnand/R_workspace_covertype/resources/test.csv')
        
        resultSet <- predict(trainedModel, testSet)
        
        result <- NULL
        result$Id <- testSet$Id
        result$Cover_Type <- resultSet
        write.csv(result, targetLocation, quote=F, row.names=F)
}



runTraining <- function(percentageTrain=0.7)
{
        completeSet <- loadData()
        completeSet$Id <- NULL
        completeSet$Cover_Type <- as.factor(completeSet$Cover_Type)
        
        # order randomly
        set.seed(12345)
        completeSet <- completeSet[order(runif(nrow(completeSet)))]
        
        # row number of x% of the data for training set
        numberTrain <- round(nrow(completeSet) * percentageTrain, digits=0) - 1
        trainSet <- completeSet[1:numberTrain,]
        testSet <- completeSet[(numberTrain+1):nrow(completeSet),]
        
        modelC50 <- c50Tree(trainSet)
        resultC50 <- predict(object = modelC50, newdata = testSet)
        printResult(testSet$Cover_Type, resultC50)
        
        alternativeTree <- randomForestTree(trainSet)
        resultAlternativeTree <- predict(object = alternativeTree, newdata = testSet)
        printResult(testSet$Cover_Type, resultAlternativeTree)
}

printResult <- function(actual, prediction)
{
        require(caret)
        confMatrix <- confusionMatrix(prediction, reference = actual)
        print(confMatrix)
}

randomForestTree <- function(trainData)
{
        require(randomForest)
        trainedModel <- randomForest(Cover_Type ~ . , trainData, do.trace=T)
}


partyTree <- function(trainData)
{
        require(party)
        trainedModel <- ctree(Cover_Type ~ . , trainData)
}

c50Tree <- function(trainData)
{
        require(C50)
        model <- C5.0(trainData[,!"Cover_Type",with=FALSE], trainData$Cover_Type, trials=10)
}


loadData <- function(location='/home/wijnand/R_workspace_covertype/resources/train.csv')
{
        require(data.table)
        data <- fread(location)
}

