# rm(list=ls(all=TRUE))

runTestSet <- function(targetLocation='/home/wijnand/R_workspace_covertype/resources/result.csv')
{
        trainingSet <- loadData()
        trainingSet$Cover_Type <- as.factor(trainingSet$Cover_Type)
        trainingSet$Id <- NULL
        
        #trainedModel <- c50Tree(trainingSet)
        trainedModel <- extraTree(trainingSet)
        
        testSet <- loadData('/home/wijnand/R_workspace_covertype/resources/test.csv')
        
        resultSet <- predict(trainedModel, testSet[,!"Id",with=FALSE])
        
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
        
        alternativeTree <- extraTree(trainSet)
        #alternativeTree <- treeTree(trainSet)
        #alternativeTree <- randomForestTree(trainSet)
        
        resultAlternativeTree <- predict(object = alternativeTree, newdata = testSet[,!"Cover_Type",with=FALSE])

        printResult(testSet$Cover_Type, resultAlternativeTree)
 }

printResult <- function(actual, prediction)
{
        require(caret)
        confMatrix <- confusionMatrix(prediction, reference = actual)
        print(confMatrix)
}

extraTree <- function(trainData)
{
        # java.lang.OutOfMemoryError: Java heap space
        options( java.parameters = "-Xmx2g" )
        require(extraTrees)
        model <- extraTrees(trainData[,!"Cover_Type",with=FALSE], trainData$Cover_Type, ntree = 1500, 
                            numRandomCuts = 5, numThreads = 4)
}

randomForestTree <- function(trainData)
{
        require(randomForest)
        trainedModel <- randomForest(Cover_Type ~ . , trainData, do.trace=50, mtry=10, ntree = 1500, 
                                     nodesize=1, importance=T)
}

treeTree <- function(trainData)
{
  require(tree)
  trainedModel <- tree(Cover_Type ~ . , trainData, y=T)
}


partyTree <- function(trainData)
{
        require(party)
        trainedModel <- ctree(Cover_Type ~ . , trainData)
}

jRipTree <- function(trainData)
{
  require(RWeka)
  model <- JRip(Cover_Type ~ . , trainData)
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