# exploratory analysis -----


chart.Correlation(primesResData[,c(4,6,7,9,10,11,12,13,14,15,18,19,20,21,26,27,33,34,35,36,41,42)],histogram = T, pch=19, main="Primes")
chart.Correlation(randomResData[,c(4,6,7,9,10,11,12,13,14,15,18,19,20,21,26,27,33,34,35,36,41,42)],histogram = T, pch=19, main="Random numbers")
chart.Correlation(randomPrimesResData[,c(4,6,7,9,10,11,12,13,14,15,18,19,20,21,26,27,33,34,35,36,41,42)],histogram = T, pch=19, main="Random Primes")
chart.Correlation(randomOrderPrimesResData[,c(4,6,7,9,10,11,12,13,14,15,18,19,20,21,26,27,33,34,35,36,41,42)],histogram = T, pch=19, main="Random Order Primes")

chart.Correlation(primesResData[which(primesResData$runType=="headRemove"),c(4,6,7,9,10,11,12,16,17,19,24,25,31,32,39,40)],histogram = T, pch=19)
chart.Correlation(primesResData[which(primesResData$runType=="tailRemove"),c(4,6,7,9,10,11,12,16,17,19,24,25,31,32,39,40)],histogram = T, pch=19)

chart.Correlation(randomResData[which(randomResData$runType=="headRemove"),c(4,6,7,9,10,11,12,16,17,19,24,25,31,32,39,40)],histogram = T, pch=19)
chart.Correlation(randomResData[which(randomResData$runType=="tailRemove"),c(4,6,7,9,10,11,12,16,17,19,24,25,31,32,39,40)],histogram = T, pch=19)

chart.Correlation(randomPrimesResData[which(randomPrimesResData$runType=="headRemove"),c(4,6,7,9,10,11,12,16,17,19,24,25,31,32,39,40)],histogram = T, pch=19)
chart.Correlation(randomPrimesResData[which(randomPrimesResData$runType=="tailRemove"),c(4,6,7,9,10,11,12,16,17,19,24,25,31,32,39,40)],histogram = T, pch=19)

chart.Correlation(randomOrderPrimesResData[which(randomOrderPrimesResData$runType=="headRemove"),c(4,6,7,9,10,11,12,16,17,19,24,25,31,32,39,40)],histogram = T, pch=19)
chart.Correlation(randomOrderPrimesResData[which(randomOrderPrimesResData$runType=="tailRemove"),c(4,6,7,9,10,11,12,16,17,19,24,25,31,32,39,40)],histogram = T, pch=19)



ggpairs(primesResData[,c(4,6,7,9,10,11,12,13,14,15,18,19,20,21,26,27,33,34,35,36,41,42,50)],showStrips = T, title = "Primes")
ggpairs(randomResData[,c(4,6,7,9,10,11,12,13,14,15,18,19,20,21,26,27,33,34,35,36,41,42,50)],showStrips = T, title = "Random numbers")




# plot all correlations between random and primes data
par(mfrow=c(3,6))
chart.Correlation(cbind(primesResData$logNormSigma,randomResData$logNormSigma))
chart.Correlation(cbind(primesResData$logNormMu,randomResData$logNormMu))
chart.Correlation(cbind(primesResData$pLawAlpha,randomResData$pLawAlpha))
chart.Correlation(cbind(primesResData$conditionNumber,randomResData$conditionNumber))
chart.Correlation(cbind(primesResData$smallestEV,randomResData$smallestEV))
chart.Correlation(cbind(primesResData$largestEV,randomResData$largestEV))
chart.Correlation(cbind(primesResData$lapSpec,randomResData$lapSpec))
chart.Correlation(cbind(primesResData$recPercSpec,randomResData$recPercSpec))
chart.Correlation(cbind(primesResData$recPercDiv,randomResData$recPercDiv))
chart.Correlation(cbind(primesResData$recDetSpec,randomResData$recDetSpec))
chart.Correlation(cbind(primesResData$recDetDiv,randomResData$recDetDiv))
chart.Correlation(cbind(primesResData$recLamSpec,randomResData$recLamSpec))
chart.Correlation(cbind(primesResData$recLamDiv,randomResData$recLamDiv))
chart.Correlation(cbind(primesResData$recRatioSpec,randomResData$recRatioSpec))
chart.Correlation(cbind(primesResData$recRatioDiv,randomResData$recRatioDiv))
chart.Correlation(cbind(primesResData$recVmeanSpec,randomResData$recVmeanSpec))
chart.Correlation(cbind(primesResData$recVmeanDiv,randomResData$recVmeanDiv))
chart.Correlation(cbind(primesResData$recLmeanSpec,randomResData$recLmeanSpec))
chart.Correlation(cbind(primesResData$recLmeanDiv,randomResData$recLmeanDiv))
chart.Correlation(cbind(primesResData$recEntropySpec,randomResData$recEntropySpec))
chart.Correlation(cbind(primesResData$recEntropyDiv,randomResData$recEntropyDiv))
chart.Correlation(cbind(primesResData$recTrendSpec,randomResData$recTrendSpec))
chart.Correlation(cbind(primesResData$recTrendDiv,randomResData$recTrendDiv))
chart.Correlation(cbind(primesResData$recRateSpecMean,randomResData$recRateSpecMean))
chart.Correlation(cbind(primesResData$recRateDivMean,randomResData$recRateDivMean))

# colorblind_pal()(4)



# compare logNormSigma
ggplot(resData, 
       aes(x=axisTicks,y=scale(resData$logNormSigma), color=runData)) +
  #geom_line() +
  geom_vline(xintercept = 0, colour="grey", linetype="dashed") +
  scale_x_continuous(breaks = seq(from=-20, to=20, by=10), labels = c("bottom 1","bottom 10","","top 10", "top 1")) +
  geom_point() +
  geom_label_repel(aes(label = resData$setProb, fill=resData$runData), color = 'white', size = 2.5, label.padding = unit(0.15, "lines"), point.padding = unit(0.35, "lines"),show.legend=F, segment.colour = "grey") +
  scale_fill_manual(values=c("primes50k" = "#000000", "random50k" = "#F0E442", "randomprimes50k" = "#0072B2", "primesrandomorder50k" = "#E69F00", "primesrandomorderB50k" = "#56B4E9", "primesrandomorderC50k" = "#009E73")) +
  theme_clean() +
  scale_y_continuous(trans = 'asinh') +
  labs(x = "Identifier set probability rank", y = "Sigma (scaled)", colour="Dataset") +
  annotate(geom="text", x=-5.5, y=max(scale(resData$logNormSigma))*.8, label=TeX("$\\leftarrow$ tail removed"), color="grey") +
  annotate(geom="text", x=6, y=max(scale(resData$logNormSigma))*.8, label=TeX("head removed $\\rightarrow$"), color="grey") +
  scale_colour_colorblind() +
  NULL

ggplot(filter(resData,runType=="tailRemove"), 
       aes(x=runData,y=scale(logNormSigma), color=runData)) +
  geom_boxplot() +
  theme_clean() +
  #stat_compare_means(comparisons = my_comparisons) +
  #stat_compare_means(method = "t.test") +
  scale_colour_colorblind() +
  NULL

ggplot(filter(resData,runType=="headRemove"), 
       aes(x=runData,y=scale(logNormSigma), color=runData)) +
  geom_boxplot() +
  theme_clean() +
  #stat_compare_means(comparisons = my_comparisons) +
  #stat_compare_means(method = "t.test") +
  scale_colour_colorblind() +
  NULL

# compare logNormMu
# compare pLawAlpha
ggplot(resData, 
       aes(x=axisTicks,y=scale(resData$pLawAlpha), color=runData)) +
  #geom_line() +
  geom_vline(xintercept = 0, colour="grey", linetype="dashed") +
  scale_x_continuous(breaks = seq(from=-20, to=20, by=10), labels = c("bottom 1","bottom 10","","top 10", "top 1")) +
  geom_point() +
  geom_label_repel(aes(label = resData$setProb, fill=resData$runData), color = 'white', size = 2.5, label.padding = unit(0.15, "lines"), point.padding = unit(0.35, "lines"),show.legend=F, segment.colour = "grey") +
  scale_fill_manual(values=c("primes50k" = "#000000", "random50k" = "#F0E442", "randomprimes50k" = "#0072B2", "primesrandomorder50k" = "#E69F00", "primesrandomorderB50k" = "#56B4E9", "primesrandomorderC50k" = "#009E73")) +
  theme_clean() +
  labs(x = "Identifier set probability rank", y = "Alpha (scaled)", colour="Dataset") +
  annotate(geom="text", x=-5.5, y=max(scale(resData$pLawAlpha))*.8, label=TeX("$\\leftarrow$ tail removed"), color="grey") +
  annotate(geom="text", x=6, y=max(scale(resData$pLawAlpha))*.8, label=TeX("head removed $\\rightarrow$"), color="grey") +
  scale_colour_colorblind() +
  NULL

ggplot(filter(resData,runType=="tailRemove"), 
       aes(x=runData,y=scale(pLawAlpha), color=runData)) +
  geom_boxplot() +
  theme_clean() +
  #stat_compare_means(comparisons = my_comparisons) +
  #stat_compare_means(method = "t.test") +
  scale_colour_colorblind() +
  NULL

ggplot(filter(resData,runType=="headRemove"), 
       aes(x=runData,y=scale(pLawAlpha), color=runData)) +
  geom_boxplot() +
  theme_clean() +
  #stat_compare_means(comparisons = my_comparisons) +
  #stat_compare_means(method = "t.test") +
  scale_colour_colorblind() +
  NULL



# compare conditionNumber
ggplot(resData, 
       aes(x=axisTicks,y=resData$conditionNumber, color=runData)) +
  #geom_line() +
  geom_vline(xintercept = 0, colour="grey", linetype="dashed") +
  scale_x_continuous(breaks = seq(from=-20, to=20, by=10), labels = c("bottom 1","bottom 10","","top 10", "top 1")) +
  geom_point() +
  geom_label_repel(aes(label = resData$setProb, fill=resData$runData), color = 'white', size = 2.5, label.padding = unit(0.15, "lines"), point.padding = unit(0.35, "lines"),show.legend=F, segment.colour = "grey") +
  scale_fill_manual(values=c("primes50k" = "#000000", "random50k" = "#F0E442", "randomprimes50k" = "#0072B2", "primesrandomorder50k" = "#E69F00", "primesrandomorderB50k" = "#56B4E9", "primesrandomorderC50k" = "#009E73")) +
  theme_clean() +
  labs(x = "Identifier set probability rank", y = "Condition number", colour="Dataset") +
  annotate(geom="text", x=-5.5, y=max(resData$conditionNumber)*.8, label=TeX("$\\leftarrow$ tail removed"), color="grey") +
  annotate(geom="text", x=6, y=max(resData$conditionNumber)*.8, label=TeX("head removed $\\rightarrow$"), color="grey") +
  scale_colour_colorblind() +
  NULL

ggplot(filter(resData,runType=="tailRemove"), 
       aes(x=runData,y=scale(conditionNumber), color=runData)) +
  geom_boxplot() +
  theme_clean() +
  #stat_compare_means(comparisons = my_comparisons) +
  #stat_compare_means(method = "t.test") +
  scale_colour_colorblind() +
  NULL

ggplot(filter(resData,runType=="headRemove"), 
       aes(x=runData,y=scale(conditionNumber), color=runData)) +
  geom_boxplot() +
  theme_clean() +
  #stat_compare_means(comparisons = my_comparisons) +
  #stat_compare_means(method = "t.test") +
  scale_colour_colorblind() +
  NULL


# compare lapSpec (most promising because most variance)
ggplot(resData, 
       aes(x=axisTicks,y=resData$lapSpec, color=runData)) +
  #geom_line() +
  geom_vline(xintercept = 0, colour="grey", linetype="dashed") +
  scale_x_continuous(breaks = seq(from=-20, to=20, by=10), labels = c("bottom 1","bottom 10","","head 10", "head 1")) +
  geom_point() + geom_line() +
  geom_label_repel(aes(label = resData$setProb, fill=resData$runData), color = 'white', size = 2.5, label.padding = unit(0.15, "lines"), point.padding = unit(0.35, "lines"),show.legend=F, segment.colour = "grey") +
  scale_fill_manual(values=c("primes50k" = "#000000", "random50k" = "#F0E442", "randomprimes50k" = "#0072B2", "primesrandomorder50k" = "#E69F00", "primesrandomorderB50k" = "#56B4E9", "primesrandomorderC50k" = "#009E73")) +
  theme_clean() +
  labs(x = "Identifier set probability rank", y = "Lapyunov spectrum", colour="Dataset") +
  annotate(geom="text", x=-5.5, y=max(resData$lapSpec)*.8, label=TeX("$\\leftarrow$ tail removed"), color="grey") +
  annotate(geom="text", x=6, y=max(resData$lapSpec)*.8, label=TeX("head removed $\\rightarrow$"), color="grey") +
  scale_colour_colorblind() +
  NULL

ggplot(filter(resData,runType=="tailRemove"), 
       aes(x=runData,y=scale(lapSpec), color=runData)) +
  geom_boxplot() +
  theme_clean() +
  #stat_compare_means(comparisons = my_comparisons) +
  #stat_compare_means(method = "t.test") +
  scale_colour_colorblind() +
  NULL

ggplot(filter(resData,runType=="headRemove"), 
       aes(x=runData,y=scale(lapSpec), color=runData)) +
  geom_boxplot() +
  theme_clean() +
  #stat_compare_means(comparisons = my_comparisons) +
  #stat_compare_means(method = "t.test") +
  scale_colour_colorblind() +
  NULL

# compare recPercSpec + Div
ggplot(resData, 
       aes(x=axisTicks,y=scale(resData$recPercSpec), color=runData)) +
  #geom_line() +
  geom_vline(xintercept = 0, colour="grey", linetype="dashed") +
  scale_x_continuous(breaks = seq(from=-20, to=20, by=10), labels = c("bottom 1","bottom 10","","head 10", "head 1")) +
  geom_point() + geom_line() +
  geom_label_repel(aes(label = resData$setProb, fill=resData$runData), color = 'white', size = 2.5, label.padding = unit(0.15, "lines"), point.padding = unit(0.35, "lines"),show.legend=F, segment.colour = "grey") +
  scale_fill_manual(values=c("primes50k" = "#000000", "random50k" = "#F0E442", "randomprimes50k" = "#0072B2", "primesrandomorder50k" = "#E69F00", "primesrandomorderB50k" = "#56B4E9", "primesrandomorderC50k" = "#009E73")) +
  theme_clean() +
  labs(x = "Identifier set probability rank", y = "Recurrence percentage (scaled)", colour="Dataset") +
  annotate(geom="text", x=-5.5, y=min(scale(resData$recPercSpec))*.8, label=TeX("$\\leftarrow$ tail removed"), color="grey") +
  annotate(geom="text", x=6, y=min(scale(resData$recPercSpec))*.8, label=TeX("head removed $\\rightarrow$"), color="grey") +
  scale_colour_colorblind() +
  NULL

ggplot(resData, 
       aes(x=axisTicks,y=scale(resData$recPercDiv), color=runData)) +
  #geom_line() +
  geom_vline(xintercept = 0, colour="grey", linetype="dashed") +
  scale_x_continuous(breaks = seq(from=-20, to=20, by=10), labels = c("bottom 1","bottom 10","","head 10", "head 1")) +
  geom_point() + geom_line() +
  geom_label_repel(aes(label = resData$setProb, fill=resData$runData), color = 'white', size = 2.5, label.padding = unit(0.15, "lines"), point.padding = unit(0.35, "lines"),show.legend=F, segment.colour = "grey") +
  scale_fill_manual(values=c("primes50k" = "#000000", "random50k" = "#F0E442", "randomprimes50k" = "#0072B2", "primesrandomorder50k" = "#E69F00", "primesrandomorderB50k" = "#56B4E9", "primesrandomorderC50k" = "#009E73")) +
  theme_clean() +
  labs(x = "Identifier set probability rank", y = "Recurrence percentage (scaled)", colour="Dataset") +
  annotate(geom="text", x=-5.5, y=max(scale(resData$recPercDiv))*.8, label=TeX("$\\leftarrow$ tail removed"), color="grey") +
  annotate(geom="text", x=6, y=max(scale(resData$recPercDiv))*.8, label=TeX("head removed $\\rightarrow$"), color="grey") +
  scale_colour_colorblind() +
  NULL

ggplot(resData, 
       aes(x=runId,y=scale(resData$recPercSpec), color=runIdentifier)) +
  geom_line() +
  geom_point() +
  theme_clean() +
  scale_colour_colorblind() +
  NULL

ggplot(resData, 
       aes(x=runId,y=scale(resData$recPercDiv), color=runIdentifier)) +
  geom_line() +
  geom_point() +
  theme_clean() +
  scale_colour_colorblind() +
  NULL
# compare recDetSpec + Div
# compare recLamSpec + Div
# compare recRatioSpec + Div
# compare recLmaxSpec
# compare recEntropySpec + Div
# compare recTrendSpec + Div
# 
# compare hurst Spec + Div
ggplot(resData, 
       aes(x=setProb,y=scale(resData$hurstCoeffSpec), color=runData)) +
  #geom_line() +
  geom_vline(xintercept = 0, colour="grey", linetype="dashed") +
  scale_x_continuous(breaks = seq(from=-20, to=20, by=10), labels = c("bottom 1","bottom 10","","head 10", "head 1")) +
  geom_point() + geom_line() +
  geom_label_repel(aes(label = resData$setProb, fill=resData$runData), color = 'white', size = 2.5, label.padding = unit(0.15, "lines"), point.padding = unit(0.35, "lines"),show.legend=F, segment.colour = "grey") +
  scale_fill_manual(values=c("primes50k" = "#000000", "random50k" = "#F0E442", "randomprimes50k" = "#0072B2", "primesrandomorder50k" = "#E69F00", "primesrandomorderB50k" = "#56B4E9", "primesrandomorderC50k" = "#009E73")) +
  theme_clean() +
  labs(x = "Identifier set probability rank", y = "Hurst coefficient", colour="Dataset") +
  annotate(geom="text", x=-5.5, y=min(scale(resData$hurstCoeffSpec))*.8, label=TeX("$\\leftarrow$ tail removed"), color="grey") +
  annotate(geom="text", x=6, y=min(scale(resData$hurstCoeffSpec))*.8, label=TeX("head removed $\\rightarrow$"), color="grey") +
  scale_colour_colorblind() +
  NULL

ggplot(resData, 
       aes(x=axisTicks,y=scale(resData$hurstCoeffDiv), color=runData)) +
  #geom_line() +
  geom_vline(xintercept = 0, colour="grey", linetype="dashed") +
  scale_x_continuous(breaks = seq(from=-20, to=20, by=10), labels = c("bottom 1","bottom 10","","head 10", "head 1")) +
  geom_point() + geom_line() +
  geom_label_repel(aes(label = resData$setProb, fill=resData$runData), color = 'white', size = 2.5, label.padding = unit(0.15, "lines"), point.padding = unit(0.35, "lines"),show.legend=F, segment.colour = "grey") +
  scale_fill_manual(values=c("primes50k" = "#000000", "random50k" = "#F0E442", "randomprimes50k" = "#0072B2", "primesrandomorder50k" = "#E69F00", "primesrandomorderB50k" = "#56B4E9", "primesrandomorderC50k" = "#009E73")) +
  theme_clean() +
  labs(x = "Identifier set probability rank", y = "Recurrence percentage (scaled)", colour="Dataset") +
  annotate(geom="text", x=-5.5, y=min(scale(resData$hurstCoeffDiv))*.8, label=TeX("$\\leftarrow$ tail removed"), color="grey") +
  annotate(geom="text", x=6, y=min(scale(resData$hurstCoeffDiv))*.8, label=TeX("head removed $\\rightarrow$"), color="grey") +
  scale_colour_colorblind() +
  NULL

write_csv(resData,"~/OneDrive - Victoria University of Wellington - STAFF/RScripts/2019-12-letter-to-N-final-analysis/fullResData.csv")


#### nonlinear testing -----
#### 

#nonlinearityTest(coord_primes$specificity)
tak = buildTakens(coord_primes$specificity,3,1)
stp.test = spaceTimePlot(takens=tak,number.time.steps=10000,do.plot=TRUE)

z <- fractal::poincareMap(coord_primes$specificity, extrema="max")
z <- fractal::embedSeries(z$amplitude, tlag=1, dimension=2)
plot(z, pch=1, cex=1)

recquant <- rqa(time.series = coord_primes$specificity,radius = 2,time.lag = 1)

# data.cr <- crqa(ts1=coord_primes$specificity, ts2=coord_primes$diversity, 
#                 recpt=F, 
#                 normalize=0, 
#                 embed=5, #do 4 or 5, can cite JOn's work on cross convergence mapping
#                 delay=1, #this is TAU
#                 rescale=0, 
#                 radius=10, 
#                 mindiagline=2,
#                 minvertline=2,
#                 whiteline=F)

# working on the LR metric -----
# 
# 
plot(cumsum(primesResData$largestEV-mean(primesResData$largestEV)),cumsum(randomOrderPrimesResData$largestEV-mean(randomOrderPrimesResData$largestEV)),pch=20,log='xy')