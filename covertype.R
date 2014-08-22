# rm(list=ls(all=TRUE))

runTestSet <- function(targetLocation='/home/wijnand/R_workspace_covertype/resources/result.csv')
{
        trainingSet <- loadData()
        trainingSet$Cover_Type <- as.factor(trainingSet$Cover_Type)
        trainingSet$Id <- NULL
        
        #trainedModel <- c50Tree(trainingSet)
        #trainedModel <- extraTree(trainingSet)
        trainedModel <- extraTreeCaret(trainingSet)
        
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
        
        #model1 <- extraTree(trainSet)
        #model2 <- treeTree(trainSet)
        #model3 <- randomForestTree(trainSet)
        #model4 <- c50Tree(trainSet)
        #model5 <- extraTreeCaret(trainSet)
        model6 <- deepnet(trainSet)
        
        #result1 <- predict(object = model1, newdata = testSet[,!"Cover_Type",with=FALSE])
        #result2 <- predict(object = model2, newdata = testSet[,!"Cover_Type",with=FALSE])
        #result3 <- predict(object = model3, newdata = testSet[,!"Cover_Type",with=FALSE])
        #result4 <- predict(object = model4, newdata = testSet[,!"Cover_Type",with=FALSE])
        #result5 <- predict(object = model5, newdata = testSet[,!"Cover_Type",with=FALSE])
        result6 <- predict(object = model6, newdata = testSet[,!"Cover_Type",with=FALSE])
        
        #print(str(testSet$Cover_Type))
        #print(str(result2))
        
        #printResult(testSet$Cover_Type, result1, "extraTree ")
        #printResult(testSet$Cover_Type, result2, "treeTree ")
        #printResult(testSet$Cover_Type, result3, "rftree ")
        #printResult(testSet$Cover_Type, result4, "c50tree ")
        #printResult(testSet$Cover_Type, result5, "extraTreeCaret ")
        printResult(testSet$Cover_Type, result6, "deepnet ")
        
        #print(model1)
        #print(confusionMatrix(result1, reference = testSet$Cover_Type))
        #print("----------")
        
        print(model6)
        print(confusionMatrix(result6, reference = testSet$Cover_Type))
        print("----------")
        
        model5
}

printResult <- function(actual, prediction, name)
{
        require(caret)
        confMatrix <- confusionMatrix(prediction, reference = actual)
        print(paste0(name, confMatrix$overall[1]))
}

extraTree <- function(trainData)
{
        # java.lang.OutOfMemoryError: Java heap space
        options( java.parameters = "-Xmx2g" )
        require(extraTrees)
        model <- extraTrees(trainData[,!"Cover_Type",with=FALSE], trainData$Cover_Type, ntree = 1500, 
                            numRandomCuts = 5, numThreads = 4)
}

extraTreeCaret <- function(trainData)
{
        
        library(caret)
        library(doSNOW)
        registerDoSNOW(makeCluster(4, outfile=""))
        
        fitControl <- trainControl(method = "cv", number = 5, verboseIter = T, allowParallel = T)
        grid <- expand.grid(.mtry = 40,
                            .numRandomCuts = 3)
        options( java.parameters = "-Xmx4g" )
        
        trainedModel <- train(Cover_Type ~ . , trainData,
                              method="extraTrees",
                              trControl = fitControl,
                              numTreads = 4,
                              ntree=750,
                              nodesize=10,
                              tuneGrid=grid,
                              metric="Kappa")
}

deepnet <- function(trainData)
{
        
        library(caret)
        #library(doSNOW)
        #registerDoSNOW(makeCluster(4, outfile=""))
        
        #fitControl <- trainControl(method = "cv", number = 5, verboseIter = T, allowParallel = T)
        #grid <- expand.grid(.mtry = 40,.numRandomCuts = 3)
        #options( java.parameters = "-Xmx4g" )
        
        fitControl <- trainControl(verboseIter = T)
        output <- as.vector(factor(trainData$Cover_Type))        
        
        trainedModel <- train(x = trainData[,!"Cover_Type",with=FALSE], y = output,
                              method="dnn")
                              #trControl = fitControl,
                              #numTreads = 4,
                              #ntree=750,
                              #nodesize=10,
                              #tuneGrid=grid,
                              #metric="Kappa")
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
  trainedModel <- tree(Cover_Type ~ . , trainData, y=T, )
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