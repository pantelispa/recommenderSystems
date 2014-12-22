# Recommender systems

# For today:
# Check how long it takes with mae and the pair comparisons. Not yet completed. 
# Add mae and check whether the results are similar.
# start to think about scaling up the simulation.
# Check the learning curve.


#rm(list = ls())
setwd("/Users/pantelispa/Desktop/Project-resources/Recommender-systems/Data")
load("jesterFull.Rdata")

# Normalization around the mean.I could also normalize differently.

#meanLaugh <- rowMeans(bigLaugh)
#check <- matrix(rep(meanLaugh,100),nrow = 14116)
#normalizedLaugh <- bigLaugh - meanLaugh

# Mae can be possibly used to reduce the simulation time. The pair-comparisons seen a bit better to use in general.

# graph with average correlations. A histogram would be interesting to 

#mae <- 1/50*sum(abs(judgeMe - weightedCrowd))
#system.time(mean(abs(judgeMe - randomAgent)))
#system.time(judgeRecommender(judgeMe,weightedCrowd))
#rmse <- sqrt((sum(judgeMe - randomAgent)^2)/50)
  
# bigLaugh <- bigLaugh + 10.
# repetitions <-  2

setwd("/Users/pantelispa/Dropbox/rx_Analytis_2014_Recommender_systems/")
length <- read.table("table", header = TRUE)
length[,1] <- NULL
length <-  length[,2]
#test$V1 <- as.numeric(as.character(test$V1))
#test$V2 <- as.numeric(as.character(test$V3))

repetitions <- 10
options(warn=1) 
# learningVector <- c(5,10,15,20,25,30,35,40,45,50)
learningVector2 <- c(5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90)
agents <- 2500
numberOfStrategies <- 14
numberOfGroups <- 10
agentsPerGroup <- agents/numberOfGroups
minusOne <- agentsPerGroup - 1
#conditions <- c(70,140,280,560,875,1750,3500,7000)

memoryStrategies <- array(dim = c(agents,18,numberOfStrategies))
memoryStrategies[is.na(memoryStrategies)] <- 0


shuffledIndex <- sample(agents,agents)
bigLaugh2 <- bigLaugh[shuffledIndex,]
#smallWorld <- bigLaugh[1 + (k-1)*conditions[k]:conditions]
#for (l in 1:(14000/conditions[k])){
#for (k in 1:200){

#trythis <- cor(test,t(bigLaugh2))
#trythis2 <- mean(abs(trythis))

  for (q in 1:repetitions){
     shuffledIndex <- sample(agents,agents)
     shuffledLaugh <- bigLaugh2[shuffledIndex,]

     
    performanceStrategies <- array(dim = c(agents,18,numberOfStrategies))
    performanceStrategies[is.na(performanceStrategies)] <- 0
      
     for (k in 1:numberOfGroups){
     # design the training world.
     smallWorld <- shuffledLaugh[(1 + (k-1)*agentsPerGroup):(k*agentsPerGroup),]
     allJokes <- seq(1,100)
     training <- sample(100,90)
     test <- setdiff(allJokes,training)
     trainingWorld <- smallWorld[,training]
     testWorld <- smallWorld[,test]
     lengthTraining <- length[training]

     # useful for item item.
     similarity5 <- cor(testWorld,trainingWorld)
     
    
    for (p in learningVector2){ # Later change that to a random assingment.
        # examine how many agents are eligible.
      
        # now loop in all the eligible agents. 
        for (o in 1:agentsPerGroup){
            # the correlations are in absolutes here. 
            similarity2 <- calculateCorrelationsJester(trainingWorld,p,o)[[2]]
            similarity3 <- calculateCorrelationsJester(trainingWorld,p,o)[[1]]
            similarity4 <- calculateCorrelationsJester(trainingWorld,p,o)[[3]]
            
            #similarity2 <- calculateCorrelationsJesterKendall(trainingWorld,p,o)[[2]]
            #similarity3 <- calculateCorrelationsJesterKendall(trainingWorld,p,o)[[1]]
            #similarity4 <- calculateCorrelationsJesterKendall(trainingWorld,p,o)[[3]]
        
            # define the three strategies. It seems that I have to define the strategies contextually for each of the pair-comparisons. Here I could also save the number of pair comparisons on which the result was based on.
            
            pickTheAgent <- trainingWorld[o,]
            pickTheAgent <- pickTheAgent[1:p]
            pickTheAgentTest <- testWorld[o,]
            myWorld <- testWorld[-o,]
            judgeMe <- pickTheAgentTest

            # random agent strategy. Done. 

            randomAgent <- myWorld[sample(minusOne,1),]
            scoreRandomAgent <- judgeRecommender(judgeMe,randomAgent)
            # maeRandomAgent <- mean(abs(judgeMe - randomAgent))

            # The weighted average strategy.Still needs a fix.

            weightedIndex <- similarity3
            #weightedIndex <- calculateCorrelationsJesterKendall(trainingWorld,p,o)[[1]]
            weightedCrowd <- (weightedIndex%*%myWorld)/as.numeric((weightedIndex)%*%rep(10,minusOne))
            weightedCrowd <- weightedCrowd*10
            scoreWeightedCrowd <- judgeRecommender(judgeMe,weightedCrowd)

            # The overweighted average strategy. Needs a fix.

            weightedIndex <- similarity3
            #weightedIndex <- calculateCorrelationsJesterKendall(trainingWorld,p,o)[[1]]
            weightedMultiplier <- weightedIndex
            weightedMultiplier[weightedMultiplier < 0] <- -1
            weightedMultiplier[weightedMultiplier >= 0] <- 1
            overWeightedIndex <- weightedIndex^2*weightedMultiplier
            overWeightedCrowd <-  overWeightedIndex%*%myWorld/as.numeric((overWeightedIndex%*%rep(10,minusOne)))
            overWeightedCrowd <- overWeightedCrowd*10
            scoreOverWeightedCrowd <- judgeRecommender(judgeMe,overWeightedCrowd)

            # The wisdom of the crowds binary. Needs a fix.  

            binaryIndex <- similarity3
            #binaryIndex <- calculateCorrelationsJesterKendall(trainingWorld,p,o)[[1]]
            binaryIndex[binaryIndex < 0] <- -1
            binaryIndex[binaryIndex >= 0] <- 1
            binaryCrowd <-  ((binaryIndex%*%myWorld)/minusOne)
            scoreBinaryCrowd <- judgeRecommender(judgeMe,binaryCrowd)

            # The wisdom of the crowds positive

            positiveIndex <- similarity3
            #positiveIndex <- calculateCorrelationsJesterKendall(trainingWorld,p,o)[[1]]
            positiveIndex[positiveIndex >= 0] <- 1
            positiveIndex[positiveIndex < 0] <- 0
            positiveCrowd <-  positiveIndex%*%myWorld/sum(positiveIndex)
            scorePositiveCrowd <- judgeRecommender(judgeMe,positiveCrowd)

            # The wisdom of the crowds negative 1.

            negativeIndex <- similarity3
            #negativeIndex <- calculateCorrelationsJesterKendall(trainingWorld,p,o)[[1]]
            negativeIndex[negativeIndex > 0] <- 0
            negativeIndex[negativeIndex < 0] <- 1
            negativeCrowd <-  negativeIndex%*%myWorld/abs(sum(negativeIndex))
            scoreNegativeCrowd <- judgeRecommender(judgeMe,negativeCrowd)

            # Positively weighted crowd.

            positivelyWeightedIndex <- similarity3
            #positiveIndex <- calculateCorrelationsJesterKendall(trainingWorld,p,o)[[1]]
            positivelyWeightedIndex[positivelyWeightedIndex >= 0] <- 1
            positivelyWeightedIndex[positivelyWeightedIndex < 0] <- 0
            positivelyWeightedCrowd <- (positivelyWeightedIndex%*%myWorld)/as.numeric((positivelyWeightedIndex)%*%rep(10,minusOne))
            positivelyWeightedCrowd <- positivelyWeightedCrowd*10
            scorePositivelyWeightedCrowd <- judgeRecommender(judgeMe,positivelyWeightedCrowd)

            # The wisdom of the crowds negative 2.

            negativeIndex2 <- similarity3
            #negativeIndex2 <- calculateCorrelationsJesterKendall(trainingWorld,p,o)[[1]]
            negativeIndex2[negativeIndex2 > 0] <- 0
            negativeIndex2[negativeIndex2 < 0] <- -1
            negativeCrowd2 <-  negativeIndex2%*%myWorld/abs(sum(negativeIndex2))
            scoreNegativeCrowd2 <- judgeRecommender(judgeMe,negativeCrowd2)
                              
            # The wisdom of the crowd strategy. Done. 

            bigCrowd <- rep(0,100)
            myWorld2 <-  myWorld
            bigCrowd2 <- colMeans(myWorld2)
          
            scoreBigCrowd <- judgeRecommender(judgeMe,bigCrowd2)

            # the best enemy. Done. 

            bestEnemy <-  myWorld[similarity2[length(similarity2)],]
            bestEnemy <- - bestEnemy
            scoreBestEnemy <- judgeRecommender(judgeMe,bestEnemy)

            # The best friend strategy. Done.

            bestFriend <- myWorld[similarity2[1],]
            scoreBestFriend <- judgeRecommender(judgeMe,bestFriend)

            #  small crowd strategy. Done.
           
            smallCrowd <- mySmallCrowdJester(myWorld,similarity2,10)
            scoreSmallCrowd <- judgeRecommender(judgeMe,smallCrowd)

            # use the joke length

            #transform the length to positive or negative with a small function.
            theLengthTraining <- lengthTraining[1:p]
            check <- cor(theLengthTraining,pickTheAgent)
            scoreLength <- judgeRecommender(judgeMe,theLength)
            if (check < 0){scoreLength <- 1 - scoreLength}

            # itemItemStrategy

            similarity6 <-  similarity5[,1:p]
            similarity7 <- as.matrix(similarity6)
            itemItem <- apply(similarity7,1,order)
            itemItem <- apply(itemItem,2,rev)
            theWeights <- matrix(0,ncol = 10,nrow = 5)
            theUtilities <- pickTheAgent[check]
            check <- itemItem[1:5,]
            for (k in 1:10){
                theWeights[,k] <- similarity6[k,check[,1]]       
            }
            itemItemCrowd <- theUtilities%*%theWeights
            scoreWeightedCrowd <- judgeRecommender(judgeMe,weightedCrowd)
         
            #  small enemy crowd strategy.
            
            #smallEnemyCrowd <- mySmallNegativeCrowdJester(myWorld,similarity2,10)
            #scoreSmallEnemyCrowd <- judgeRecommender(judgeMe,smallEnemyCrowd)

             #  small mixed crowd strategy. Check Again. 
            
            #smallMixedCrowd <- mySmallMixedCrowdJester(myWorld,similarity3,similarity4,10)
            #scoreSmallMixedCrowd <- judgeRecommender(judgeMe,smallMixedCrowd)

            
            onIndex <- shuffledIndex[(k-1)*agentsPerGroup + o]

            performanceStrategies[onIndex,p/5,1] <- performanceStrategies[onIndex,p/5,1] + scoreBestFriend
            performanceStrategies[onIndex,p/5,2] <- performanceStrategies[onIndex,p/5,2] + scoreBigCrowd
            performanceStrategies[onIndex,p/5,3] <- performanceStrategies[onIndex,p/5,3] + scoreSmallCrowd
            performanceStrategies[onIndex,p/5,4] <- performanceStrategies[onIndex,p/5,4] + scoreRandomAgent
            performanceStrategies[onIndex,p/5,5] <- performanceStrategies[onIndex,p/5,5] + scoreBestEnemy
            performanceStrategies[onIndex,p/5,6] <- performanceStrategies[onIndex,p/5,6] + scoreWeightedCrowd
            performanceStrategies[onIndex,p/5,7] <- performanceStrategies[onIndex,p/5,7] + scoreOverWeightedCrowd
            performanceStrategies[onIndex,p/5,8] <- performanceStrategies[onIndex,p/5,8] + scorePositiveCrowd
            performanceStrategies[onIndex,p/5,9] <- performanceStrategies[onIndex,p/5,9] + scoreNegativeCrowd
            performanceStrategies[onIndex,p/5,10] <- performanceStrategies[onIndex,p/5,10] + scoreNegativeCrowd2
            performanceStrategies[onIndex,p/5,11] <- performanceStrategies[onIndex,p/5,11] + scoreBinaryCrowd
            performanceStrategies[onIndex,p/5,12] <- performanceStrategies[onIndex,p/5,12] + scorePositivelyWeightedCrowd
            performanceStrategies[onIndex,p/5,13] <- performanceStrategies[onIndex,p/5,13] + scoreLength
            performanceStrategies[onIndex,p/5,14] <- performanceStrategies[onIndex,p/5,14] + scoreItemItem
               
        }
        
    }


    }

  memoryStrategies <- memoryStrategies + performanceStrategies 
 }


save.image("JesterTest250.Rdata")
