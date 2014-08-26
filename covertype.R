# rm(list=ls(all=TRUE))

runTestSet <- function(targetLocation='/home/wijnand/R_workspace_covertype/resources/result.csv')
{
        trainingSet <- loadData()
        trainingSet$Cover_Type <- as.factor(trainingSet$Cover_Type)
        trainingSet$Id <- NULL
        
        #trainedModel <- c50Tree(trainingSet)
        trainedModel <- extraTree(trainingSet)
        #trainedModel <- extraTreeCaret(trainingSet)
        
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
        
        model1 <- extraTree(trainSet)
        #model2 <- treeTree(trainSet)
        #model3 <- randomForestTree(trainSet)
        model5 <- extraTreeCaret(trainSet)
        #model6 <- c50Caret(trainSet)
        #model7 <- genericCaret(trainSet)
        
        result1 <- predict(object = model1, newdata = testSet[,!"Cover_Type",with=FALSE])
        #result2 <- predict(object = model2, newdata = testSet[,!"Cover_Type",with=FALSE])
        #result3 <- predict(object = model3, newdata = testSet[,!"Cover_Type",with=FALSE])
        result5 <- predict(object = model5, newdata = testSet[,!"Cover_Type",with=FALSE])
        #result6 <- predict(object = model6, newdata = testSet[,!"Cover_Type",with=FALSE])
        #result7 <- predict(object = model7, newdata = testSet[,!"Cover_Type",with=FALSE])
        
        #print(str(testSet$Cover_Type))
        #print(str(result2))
        
        printResult(testSet$Cover_Type, result1, "extraTree ")
        #printResult(testSet$Cover_Type, result2, "treeTree ")
        #printResult(testSet$Cover_Type, result3, "rftree ")
        printResult(testSet$Cover_Type, result5, "extraTreeCaret ")
        #printResult(testSet$Cover_Type, result6, "C50 ")
        #printResult(testSet$Cover_Type, result7, "elm ")
        
        #print(model6)
        #print(confusionMatrix(result6, reference = testSet$Cover_Type))
        #print("----------")
        
        model1
}

printResult <- function(actual, prediction, name)
{
        require(caret)
        confMatrix <- confusionMatrix(prediction, reference = actual)
        print(paste0(name, confMatrix$overall[1]))
}

genericCaret <- function(trainData)
{
        library(caret);library(doSNOW);registerDoSNOW(makeCluster(4, outfile=""))
        
        #grid = expand.grid(.n.trees = c(100),
        #                   .interaction.depth = c(5),
        #                   .shrinkage = c(0.1)) 
        
        fitControl <- trainControl(method = "cv", 
                                   number = 5, 
                                   verboseIter = T, 
                                   allowParallel = T,
                                   classProbs = T)
        
        trainedModel <- train(data.matrix(Cover_Type) ~ . , data = trainData,
                              method="bagEarth",
                              trControl = fitControl,
                              #tuneGrid = grid,
                              tuneLength = 4)
                              #metric="Accuracy")
        print(trainedModel)
        trainedModel
}

c50Caret <- function(trainData)
{
        library(caret);library(doSNOW);registerDoSNOW(makeCluster(4, outfile=""))
        
        grid = expand.grid(.trials = c(30), 
                           .model = c("tree", "rules"),
                           .winnow = c(T, F))
        
        fitControl <- trainControl(method = "cv", number = 5, verboseIter = T, allowParallel = T)
        
        trainedModel <- train(Cover_Type ~ . , data = trainData,
                              method="C5.0",
                              trControl = fitControl,
                              tuneGrid = grid,
                              metric="Accuracy")
        print(trainedModel)
        trainedModel
}

extraTree <- function(trainData)
{
        # java.lang.OutOfMemoryError: Java heap space
        options( java.parameters = "-Xmx2g" )
        require(extraTrees)
        model <- extraTrees(trainData[,!"Cover_Type",with=FALSE], 
                            trainData$Cover_Type, 
                            ntree = 750, 
                            numRandomCuts = 10, 
                            #nodesize = 2,
                            #mtry = 2,
                            #evenCuts = T,
                            numThreads = 4)
}

extraTreeCaret <- function(trainData)
{
        
        library(caret)
        library(doSNOW)
        registerDoSNOW(makeCluster(4, outfile=""))
        
        fitControl <- trainControl(method = "cv", number = 2, verboseIter = T, allowParallel = T)
        grid <- expand.grid(.mtry = 40,
                            .numRandomCuts = c(1,5,9,13))
        options( java.parameters = "-Xmx6g" )
        
        trainedModel <- train(Cover_Type ~ . , trainData,
                              method="extraTrees",
                              trControl = fitControl,
                              #numTreads = 4,
                              ntree=750,
                              #nodesize=10,
                              tuneGrid=grid,
                              metric="Accuracy")
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

loadData <- function(location='/home/wijnand/R_workspace_covertype/resources/train.csv')
{
        require(data.table)
        data <- fread(location)
}