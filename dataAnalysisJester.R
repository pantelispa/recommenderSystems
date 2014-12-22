# Data analysis of the simulation results

setwd("/Users/pantelispa/Desktop/Datasets/Recommender_systems_data")
setwd("/Users/pantelispa/Desktop/Project-resources/Recommender-systems")

# Produce a simple graph comparing the basic strategies. 

load("JesterTest250.Rdata")
load("Jester6.Rdata")
load("bigCorrelations.Rdata")
library(ggplot2)
library(reshape)
library(RColorBrewer)


bestFriendPerformance <- colMeans(memoryStrategies[,,1])/10
bigCrowdPerformance <- colMeans(memoryStrategies[,,2])/10
smallCrowdPerformance <- colMeans(memoryStrategies[,,3])/10
randomAgentPerformance <-  colMeans(memoryStrategies[,,4])/10
bestEnemyPerformance <- colMeans(memoryStrategies[,,5])/10
weightedCrowdPerformance <- colMeans(memoryStrategies[,,6],na.rm = TRUE)/10
positiveCrowdPerformance <- colMeans(memoryStrategies[,,8])/10
overWeightedCrowdPerformance <- colMeans(memoryStrategies[,,7])/10
negativeCrowdPerformance <- colMeans(memoryStrategies[,,9],na.rm = TRUE)/10
negativeCrowdPerformance2 <- colMeans(memoryStrategies[,,10],na.rm = TRUE)/10
negativeBinaryCrowdPerformance <- colMeans(memoryStrategies[,,11])/10
smallMixedCrowdPerformance <- colMeans(memoryStrategies[,,12])/10
fiftyLine <- rep(0.5,18)

plot(bestFriendPerformance,main = "Strategy Performance",type = "l",col = "blue", ylim = c(0.45,0.7),xlim = c(1,18), ylab= "Performance",xlab = "Experience",xaxt = "n")
lines(bigCrowdPerformance, type = "l", col = "red")
lines(smallCrowdPerformance, type = "l", col = "green4")
lines(randomAgentPerformance, type = "l", col = "orange")
lines(bestEnemyPerformance, type = "l", col = "purple")
lines(weightedCrowdPerformance, type = "l", col = "pink")
lines(positiveCrowdPerformance, type = "l", col = "brown")
lines(fiftyLine, type = "l", col = "blue4")
lines(negativeCrowdPerformance,type = "l", col = "blue4")
lines(negativeCrowdPerformance2,type = "l", col = "brown4")
lines(negativeBinaryCrowdPerformance,type = "l", col = "red4")
lines(overWeightedCrowdPerformance, type = "l")

hist(memoryBigCrowd[,50])

axis(1, seq(1,18,by = 2), lwd = 1, cex.axis = 1)

# Bring the strategies to a comparison format. 

strategyComparison <- array(dim=c(18,7,280))
for (i in 1:280){strategyComparison[,1,i] <- memoryStrategies[i,,1]
                 strategyComparison[,2,i] <- memoryStrategies[i,,2]
                 strategyComparison[,3,i] <- memoryStrategies[i,,3]
                 strategyComparison[,4,i] <- memoryStrategies[i,,4]
                 strategyComparison[,5,i] <- memoryStrategies[i,,5]
                 strategyComparison[,6,i] <- memoryStrategies[i,,6]
                 strategyComparison[,7,i] <- memoryStrategies[i,,8]}

areaPlotMatrix <- data.frame(matrix(rep(0,18*7),nrow = 18, ncol = 7))
colnames(areaPlotMatrix) <- c("bestFriend","bigCrowd","smallCrowd","randomAgent","bestEnemy","weightedCrowd","positiveCrowd")
for (i in 1:280){
    for (k in 1:18){addIt <- which.max(strategyComparison[k,,i])
                    areaPlotMatrix[k,addIt] <- areaPlotMatrix[k,addIt] + 1}
}


newMatrix <- melt(areaPlotMatrix)
addOn <- as.vector(cbind(seq(1,18),seq(1,18),seq(1,18),seq(1,18),seq(1,18),seq(1,18),seq(1,18)))

socialStrategies <- data.frame(newMatrix,addOn)
colnames(socialStrategies)[1] <- "strategies"

# The set3 from colorbrewer is fine. Spectral looks even better.

strategiesGraph <- ggplot(socialStrategies, aes(x = addOn,y = value, xmin = 0, ymin= 0, fill = strategies)) + geom_area(position = "fill") + theme_bw() + scale_fill_brewer(palette = "Spectral") + scale_x_discrete(expand = c(0,0)) + scale_y_continuous(expand = c(0,0))

+ scale_y_continuous( expand = c(0,0), limits = c(0,1))


# Produce a nice histogram. Build a loop that creates the graphs for every agent. First produce a graph were 

load("jesterFull.Rdata")
pickTheAgent <- sample(length(bigLaugh[,1]),1)
pickTheAgent2 <- bigLaugh[pickTheAgent,]
allCorrelations <- cor(pickTheAgent2,t(bigLaugh))
hist(allCorrelations)

# rbind seems to be very slow as the matrix grows larger. I can create the matrix ex-ante and then change the individual lines. It produces a huge amount of data. It might be better to reduce that to 1000. 

i <- 0
bigCorrelations <- matrix(nrow=14116,ncol=14115)
for (i in 1:14116){
    pickTheAgent <- bigLaugh[i,]
    bigLaugh2 <- bigLaugh[-i,]
    allCorrelations <- cor(pickTheAgent,t(bigLaugh2))
    bigCorrelations[i,] <- allCorrelations}

# Calculating the correlations in a big subsample of the population.

testLaugh <- bigLaugh[sample(14116,1000),]
bigCorrelations <- matrix(nrow=1000,ncol=999)
for (i in 1:1000){
    pickTheAgent <- testLaugh[i,]
    bigLaugh2 <- testLaugh[-i,]
    allCorrelations <- cor(pickTheAgent,t(bigLaugh2))
    bigCorrelations[i,] <- allCorrelations}
save(bigCorrelations,file = "bigCorrelations.Rdata")

# Do the statistics for the entire population.

hist(bigCorrelations,prob = TRUE,breaks = 20)
positiveBigCorrelations <- bigCorrelations[bigCorrelations > 0]
nPositive <- length(positiveBigCorrelations)
negativeBigCorrelations <- bigCorrelations[bigCorrelations < 0]
nNegative <- length(negativeBigCorrelations)
mean(bigCorrelations)
sd(bigCorrelations)
curve(dnorm(x,mean = mean(bigCorrelations), sd = sd(bigCorrelations)), add = TRUE, col = "red")
shapiroSample <- as.vector(bigCorrelations)
shapiroSample <- shapiroSample[sample(length(shapiroSample),5000)]
shapiro.test(shapiroSample)

# Do the statistics for the means in the entire population.

meanCorrelations <- rowMeans(bigCorrelations)
hist(meanCorrelations,prob = TRUE,breaks = 40)
positiveMeanCorrelations <- meanCorrelations[meanCorrelations > 0]
nPositive <- length(positiveMeanCorrelations)
negativeMeanCorrelations <- meanCorrelations[meanCorrelations < 0]
nNegative <- length(negativeMeanCorrelations)
mean(meanCorrelations)
sd(meanCorrelations)
curve(dnorm(x,mean = mean(meanCorrelations), sd = sd(meanCorrelations)), add = TRUE, col = "red")
shapiroSample <- as.vector(meanCorrelations)
shapiro.test(shapiroSample)

# Product the histograms for all the population.

for (i in 1:1000){
    mypath <-  file.path("/Users/pantelispa/Desktop/Project-resources/Recommender-systems","SAVEHERE",paste("myplot_", names[i], ".jpg", sep = ""))
    jpg(file=mypath)
    mytitle = paste("my title is", names[i])
    hist(bigCorrelations[i,])
    dev.off()
     }
         












# produce a graph about that. Find a way to manipulate the community size.
                 
    
