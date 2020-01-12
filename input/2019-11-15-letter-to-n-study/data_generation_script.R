

if (!require(tidyverse)) install.packages('tidyverse')
if (!require(igraph)) install.packages('igraph')
if (!require(gtools)) install.packages('gtools')
if (!require(tools)) install.packages('tools')
if (!require(tuneR)) install.packages('tuneR')

setwd("~/OneDrive - Victoria University of Wellington - STAFF/Git/transcendental-information-cascades/input/2019-11-15-letter-to-n-study/")


# DATA GENERATION ---------------------------------------------------------


#white noise mamem
#
t<-14976

example <- read.csv("~/OneDrive - Victoria University of Wellington - STAFF/Git/transcendental-information-cascades/input/2019-11-15-letter-to-n-study/data/example-mamem.txt")
means <- apply(example,2,mean)
sds <- apply(example,2,sd)
X <- matrix(ncol = 14, nrow = 14976)
for(i in 1:14){
  white_noise <- arima.sim(model = list(order = c(0, 0, 0)), n = 14976, mean = means[i], sd = sds[i])
  X[,i] <- white_noise
}

plot(t, X[,1], xlab = "time", ylab = "signal", ylim = c(3000, 5200), type = "l")
apply(X[,2:14], 2, function(x, t) lines(t, x), t = t)

write_csv(as.data.frame(X),"~/Desktop/whitenoise-mamem.txt")


#simulated EEG
#
library(eegkit)
data(eegcoord)
chnames <- rownames(eegcoord)
chids <- c(17:24,51:53,55:79,84:86)#which(chnames %in% colnames(example))
tseq <- seq(0,1,length.out=t)


Y <- matrix(ncol = 87, nrow = 14976)
for(j in 1:t){
  Y[j,] <- eegsim(chnames,rep(tseq[j],87))[,1]
}

Y <- Y[,chids]

colnames(Y) <- chnames[chids]

write_csv(as.data.frame(Y),"~/Desktop/example-simulated.txt")

#white noise simulated
#
means <- apply(Y,2,mean)
sds <- apply(Y,2,sd)
X <- matrix(ncol = ncol(Y), nrow = 14976)
for(i in 1:ncol(Y)){
  white_noise <- arima.sim(model = list(order = c(0, 0, 0)), n = 14976, mean = means[i], sd = sds[i])
  X[,i] <- white_noise
}

plot(c(1:t), X[,1], xlab = "time", ylab = "signal", ylim = c(3000, 5200), type = "l")
apply(X[,2:ncol(Y)], 2, function(x, t) lines(t, x), t = t)
colnames(X) <- chnames[chids]
write_csv(as.data.frame(X),"~/Desktop/whitenoise-simulated.txt")

#brownian motion
t <- 0:14975  # time
sig2 <- 0.01
## first, simulate a set of random deviates
x <- rnorm(n = length(t) - 1, sd = sqrt(sig2))
## now compute their cumulative sum
x <- c(0, cumsum(x))
plot(t, x, type = "l", ylim = c(-2, 2))

nsim <- 100
X <- matrix(rnorm(n = nsim * (length(t) - 1), sd = sqrt(sig2)), nsim, length(t) - 
              1)
X <- cbind(rep(0, nsim), t(apply(X, 1, cumsum)))
plot(t, X[1, ], xlab = "time", ylab = "phenotype", ylim = c(-2, 2), type = "l")
apply(X[2:nsim, ], 1, function(x, t) lines(t, x), t = t)
X <- t(X)
colnames(X) <- paste0("V_",c(1:100))
write_csv(as.data.frame(X),"~/Desktop/brownian-motion.txt")

#jungle book
jb <- read_csv("~/OneDrive - Victoria University of Wellington - STAFF/Git/transcendental-information-cascades/input/2019-11-15-letter-to-n-study/data/jungle_book.csv")
jb <- apply(jb,2,function(x){
  gsub('\\"','',x)
})
#randomise sentences
jb_rand <- gsub('"',"'",jb[sample(1:length(jb))])
write_csv(as.data.frame(jb_rand),"~/OneDrive - Victoria University of Wellington - STAFF/Git/transcendental-information-cascades/input/2019-11-15-letter-to-n-study/data/jungle_book_random_sentences.csv",col_names = F)

words <- lapply(jb, function(x){
  unlist(strsplit(gsub("[[:punct:]]|[[:digit:]]","",x)," "))
})

words <- unique(unlist(words))
words <- words[-which(nchar(words)==0)]

avg_w <- c()
for(i in 1:length(jb)){
  avg_w <- c(avg_w,stringi::stri_stats_latex(jb[i])[4])
}
avg_w <- round(mean(avg_w))

jb_rand_words <- c()
for(i in 1:length(jb)){
  rsent <- paste(sample(words,avg_w,replace = FALSE),collapse = " ")
  jb_rand_words <- c(jb_rand_words,rsent)
}
write_csv(as.data.frame(jb_rand_words),"~/OneDrive - Victoria University of Wellington - STAFF/Git/transcendental-information-cascades/input/2019-11-15-letter-to-n-study/data/jungle_book_random_word_sentences.csv",col_names = F)

#primes base 32
library(bit64)
library(Rmpfr)
library(BBmisc)
options(scipen=10000)

base_convert <- function(x,base=10){
  out <- ""
  while(x>=base){
    out <- paste0(x %% base,out)
    x <- x %/% base
  }
  out <- paste0(x %% base,out)
  return(out)
}
seq <- 1

foo<-sapply(primes50k$X2,function(x){
  nextNum <- as.character(itostr(as.numeric(x),base=36))
  write.table(cbind(seq,x,
                    paste(sort(unique(unlist(strsplit(nextNum,split = "")))),
                          collapse=",")),"~/OneDrive - Victoria University of Wellington - STAFF/primes-base36-dataset.txt",sep=';',row.names = F,col.names = F,append = T,quote = F)
  seq <<- seq + 1
})


X50kprimes <- read_delim("~/Downloads/50kprimes.txt", 
                         ";", escape_double = FALSE, col_names = FALSE, 
                         trim_ws = TRUE)
X50kprimes <- X50kprimes[sample(1:50000, 50000, replace=FALSE),]
write.table(X50kprimes,"~/OneDrive - Victoria University of Wellington - STAFF/Git/transcendental-information-cascades/input/2019-11-15-letter-to-n-study/data/primes50k-random-order.txt",sep=';',row.names = F,col.names = F,append = T,quote = F)
