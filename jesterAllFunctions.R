 jester all functions.

calculateCorrelations <- function(trainingWorld,enoughDecisions,o,p){
        pickTheAgent <- trainingWorld[enoughDecisions[o],]
        myTrainingWorld <- trainingWorld[-enoughDecisions[o],]
        choices <- which(pickTheAgent != 0)
        # randomize this part
        choices <- sample(choices,p)
        pickTheChoices <- pickTheAgent[choices]
        myTrainingWorld2 <- myTrainingWorld[,choices]
        myTrainingWorld2[myTrainingWorld2 == 0] <- NA
        similarity <- array(cor(pickTheChoices,t(myTrainingWorld2),use="pairwise.complete.obs")) # NA and
        similarity[is.na(similarity)] <- 0
        similarity2 <- order(similarity,decreasing = "TRUE")
        return(similarity2)
    }

calculateCorrelationsJester <- function(trainingWorld,p,o){
        pickTheAgent <- trainingWorld[o,]
        myTrainingWorld <- trainingWorld[-o,]  # randomize this?
        pickTheChoices <- pickTheAgent[1:p]
        myTrainingWorld2 <- myTrainingWorld[,1:p]
        similarity <- array(cor(pickTheChoices,t(myTrainingWorld2),use="pairwise.complete.obs")) # NA and
        similarity[is.na(similarity)] <- 0
        similarity2 <- order(similarity,decreasing = "TRUE")
        similarityAbsolute <- abs(similarity)
        similarityAbsoluteDecreasing <- order(similarityAbsolute,decreasing = "TRUE")
        similarityList <- list(similarity,similarity2,similarityAbsoluteDecreasing)
        return(similarityList)
    }

calculateCorrelationsJesterKendall <- function(trainingWorld,p,o){
        pickTheAgent <- trainingWorld[o,]
        myTrainingWorld <- trainingWorld[-o,]  # randomize this?
        pickTheChoices <- pickTheAgent[1:p]
        myTrainingWorld2 <- myTrainingWorld[,1:p]
        similarity <- array(cor(pickTheChoices,t(myTrainingWorld2),method = "kendall",use="pairwise.complete.obs")) # NA and
        similarity[is.na(similarity)] <- 0
        similarity2 <- order(similarity,decreasing = "TRUE")
        similarityAbsolute <- abs(similarity)
        similarityAbsoluteDecreasing <- order(similarityAbsolute,decreasing = "TRUE")
        similarityList <- list(similarity,similarity2,similarityAbsoluteDecreasing)
        return(similarityList)
    }


calculateCorrelationsJester2 <- function(trainingWorld,p,o){
        pickTheAgent <- trainingWorld[o,]
        myTrainingWorld <- trainingWorld[-o,]
        choices <-  sample(50,p) # randomize this?
        pickTheChoices <- pickTheAgent[choices]
        myTrainingWorld2 <- myTrainingWorld[,choices]
        similarity <- array(cor(pickTheChoices,t(myTrainingWorld2),use="pairwise.complete.obs")) # NA and
        return(similarity)
    }

calculateCorrelationsJesterMixed <- function(trainingWorld,p,o){
        pickTheAgent <- trainingWorld[o,]
        myTrainingWorld <- trainingWorld[-o,]
        choices <-  sample(50,p)  # randomize this?
        pickTheChoices <- pickTheAgent[choices]
        myTrainingWorld2 <- myTrainingWorld[,choices]
        similarity <- array(cor(pickTheChoices,t(myTrainingWorld2),use="pairwise.complete.obs")) # NA and
        similarityAbsolute <- abs(similarity)
        similarityAbsoluteDecreasing <- order(similarityAbsolute,decreasing = "TRUE")
        # similarityList <- list(similarity,similarityList)
        return(similarityAbsoluteDecreasing)
    }



myBestFriend <- function (myWorld,similarity2){
            bestFriend <- rep(0,100)
            for (t in 1:100){
                review <- 0
                k <- 1
                while (review == 0){              
                    review <- myWorld[similarity2[k],t]
                    k <- k + 1
                    bestFriend[t] <- review
                    
                }
            }
            return(bestFriend)
        }


myBestFriendJester <- function (myWorld,similarity2){
            bestFriend <- rep(0,100)
            review <- myWorld[similarity2[1],t]
            bestFriend[t] <- review
            return(bestFriend)
        }

# Maybe it is possible to further siplify this function. 
# The best friend strategy gives the same result with both functions. This is a little fishy.
# I could play around with a very small subset to get some intuitions.

judge <- function(theData,predictions){
    numbers <- seq(1,length(theData),1)
    allPairs <- combn(numbers,2)
    theJudgment1 <- theData[allPairs[1,]] - theData[allPairs[2,]]
    theJudgment1[theJudgment1 < 0] <- -1 
    theJudgment1[theJudgment1 >= 0] <- 1
    theJudgment2 <- as.vector(predictions[allPairs[1,]]) - as.vector(predictions[allPairs[2,]])
    theJudgment2[theJudgment2 < 0] <- -1 
    theJudgment2[theJudgment2 >= 0] <- 1
    theJudge <- theJudgment1 - theJudgment2
    scoreJudge <- sum(theJudge == 0)/length(theJudgment1)
    return(scoreJudge)
}

judgeRecommender <- function(theData,predictions){
    numbers <- seq(1,length(theData),1)
    allPairs <- combn(numbers,2)
    theJudgment1 <- theData[allPairs[1,]] - theData[allPairs[2,]]
    theJudgment1[theJudgment1 < 0] <- -1 
    theJudgment1[theJudgment1 > 0] <- 1
    index <- which(theJudgment1 == 0)
    theJudgment1[index] <- sample(c(-1,1),length(theJudgment1[index]),replace = TRUE)
    theJudgment2 <- as.vector(predictions[allPairs[1,]]) - as.vector(predictions[allPairs[2,]])
    theJudgment2[theJudgment2 < 0] <- -1 
    theJudgment2[theJudgment2 > 0] <- 1
    index2 <- which(theJudgment2 == 0)
    theJudgment2[index2] <- sample(c(-1,1),length(theJudgment2[index2]),replace = TRUE)
    theJudge <- theJudgment1 - theJudgment2
    scoreJudge <- sum(theJudge == 0)/length(theJudgment1)
    return(scoreJudge)
}

mySmallCrowd <- function(myWorld,similarity2,size){
    smallCrowd <- rep(0,100)
    for (t in 1:100){
                clique <- rep(0,size)
                k <- 1
                l <- 1
                while (length(which (clique == 0)) > 0){
                     review <- myWorld[similarity2[k],t]
                     if (review != 0){
                         clique[l] <- review
                         l <- l + 1}
                     k <- k + 1}
                smallCrowd[t] <- mean(clique)
            }
        return(smallCrowd)
       }
    

mySmallCrowdJester <- function(myWorld,similarity2,size){
    clique <- similarity2[1:size]
    review <- myWorld[clique,]
    smallCrowd <- colMeans(review)
    return(smallCrowd)
}

mySmallNegativeCrowdJester <- function(myWorld,similarity2,size){
    reversedSimilarity <- rev(similarity2)
    clique <- similarity2[1:size]
    sortedNegative <- sort(similarity3)
    negativeValues <-  which(sortedNegative < 0)
    if (length(negativeValues)< size){clique <- clique[1:length(negativeValues)]}
    review <- myWorld[clique,]
    smallCrowd <- colMeans(review)
    return(smallCrowd)
}

mySmallMixedCrowdJester <- function(myWorld,similarity3,similarity4,size){
    clique <- similarity4[1:size]
    similaritySigns <- similarity3[similarity4][1:size]
    similaritySigns[similaritySigns < 0] <- -1
    similaritySigns[similaritySigns >= 0] <- 1
    review <- myWorld[clique,]
    similaritySigns%*%review
    smallCrowd <- colMeans(review)
    return(smallCrowd)
}

mySmallMixedCrowdWeightedJester <- function(myWorld,similarity3,similarity4,size){
    clique <- similarity4[1:size]
    similaritySigns <- similarity3[similarity4]
    review <- myWorld[clique,]
    similaritySings%*%review
    smallCrowd <- colMeans(review)
    return(smallCrowd)
}








