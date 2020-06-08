library(MASS)
library(car)
library(userfriendlyscience)
library(BEST)
library(randomForest)
library(corrgram)
library(lime)       # ML local interpretation
library(vip)        # ML global interpretation
library(pdp)        # ML global interpretation
library(ggplot2)    # visualization pkg leveraged by above packages
library(caret)      # ML model building
library(h2o)        # ML model building
library(plot3D)
library(plotly)

options(scipen = 10000)


# construct diff data etc. ----

toDiff <- list(colnames(origResData)[c(4:63)])

resDiffData <- resData[0,]

for(orig in 1:nrow(origResData)){
  runData <- origResData$runData[orig]
  
  subRes <- resData[which(resData$runData==runData),]
  
  for(differ in toDiff){
    subRes[,differ] <- subRes[,differ] - c(origResData[orig,differ])
  }
  
  resDiffData <- rbind(resDiffData,subRes)
}

primeRows <- which(resDiffData$runData=="primes50k")
primeRandomOrderRows <- which(resDiffData$runData=="primesrandomorder50k")
primeRandomOrderBRows <- which(resDiffData$runData=="primesrandomorderB50k")
primeRandomOrderCRows <- which(resDiffData$runData=="primesrandomorderC50k")
randomNumbersRows <- which(resDiffData$runData=="random50k")


scaledResData <- resDiffData

for(stp in c(4:63,68)){
  scaledResData[,stp] <- (unlist(scaledResData[,stp])  - mean(unlist(scaledResData[,stp]) ))/sd(unlist(scaledResData[,stp]) )
}

mlResData <- resDiffData[,c(1,38,40,41,42,15,10,11,56,58,60,61,62,63,66)]
mlResData$runData <- as.factor(mlResData$runData)

# bayesian BEST ----
x <- scaledResData$recVmaxSpec[which(scaledResData$runData=="primes50k")]
y <- scaledResData$recVmaxSpec[which(scaledResData$runData=="primesrandomorder50k")]

priors <- list(muM = 6, muSD = 2)
BESTout <- BESTmcmc(x, y, priors=priors, parallel=FALSE)
plotAll(BESTout)


# using auto.ml ----
n_seed = 30116
h2o.init()
h2o.no_progress()
h2o.removeAll()
ldaDs = as.h2o(resDiffData[,c(1,38,40,41,15,10,11,56,62,63,66)])

# Split Train/Test
ldaDs.split = h2o.splitFrame(ldaDs, ratios = 0.75, seed = n_seed)
ldaDs.train = ldaDs.split[[1]] # 75% for modelling
ldaDs.test = ldaDs.split[[2]] # 25% for evaluation

ldaDs.x_train <- as.data.frame(ldaDs.train)
ldaDs.x_test <- as.data.frame(ldaDs.test)[,-1]

test.lda <- lda(runData ~ ., data=ldaDs.x_train)

explainer = lime::lime(x = ldaDs.x_train[,-1],
                       model = test.lda)
# Create explanations
explanations = lime::explain(x = ldaDs.x_test[sample(1:nrow(ldaDs.x_test),8),],
                             explainer = explainer,
                             n_permutations = 5000,
                             feature_select = "lasso_path",
                             dist_fun = "manhattan",
                             kernel_width = 3,
                             n_labels = 1,    # Binary classification
                             n_features = 5) # Look top x features
lime::plot_features(explanations)
plot_explanations(explanations)


# random froest -----
# Using random forest for variable selection
rfModel <-randomForest(runData ~ ., data = mlResData)

# Getting the list of important variables
importance(rfModel)

# plotting to evaluate some example combinations----
# 
irows <- primeRows
irows <- primeRandomOrderRows
irows <- primeRandomOrderBRows
irows <- primeRandomOrderCRows
irows <- randomNumbersRows

ix=scaledResData$recEntropyDiv[irows]
iy=scaledResData$piel_log_sl[irows]
iz =scaledResData$mean_div[irows]
interp_data <- interp(x=ix,
                      y=iy,
                      z=iz)

p <- plot_ly(x=interp_data$x, y=interp_data$y, z = interp_data$z) %>% add_surface()
p

lines3D(resData$max_div[primeRows],
        resData$largestEV[primeRows],
        resData$hurstCoeffDiv[primeRows])

# this is the combination of our choice
plot_ly(x=(resDiffData$pcount_dist_wiener - mean(resDiffData$pcount_dist_wiener))/sd(resDiffData$pcount_dist_wiener),
        y=(resDiffData$recLmeanDiv - mean(resDiffData$recLmeanDiv))/sd(resDiffData$recLmeanDiv),
        z = (resDiffData$recVmeanDiv - mean(resDiffData$recVmeanDiv))/sd(resDiffData$recVmeanDiv), 
        type = "scatter3d",mode="points",opacity=0.5, color = resDiffData$runData)

plot_ly(x=resData$max_div,
        y=resData$largestEV,
        z = resData$hurstCoeffDiv, 
        type = "scatter3d",mode="points",opacity=0.5, color = resData$runData) %>%
  layout(
    #title = "Layout options in a 3d scatter plot",
    scene = list(
      xaxis = list(title = "Max. TIC identifier set diversity"),
      yaxis = list(title = "Largest eigenvalue"),
      zaxis = list(title = "Hurst coefficient of identifier set diversity time series")
    ))

corrData <- resData[,c(38,40,41,42,15,10,11,56,58,60,61,62,63,66)]
corrgram(corrData, order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt,
         main="decomp")

meansComparisonDiamondPlot(scaledResData,
                           items=unlist(toDiff),
                           compareBy = 'runData',
                           conf.level = .99)
