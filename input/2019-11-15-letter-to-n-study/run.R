

# PREAMBLE ----------------------------------------------------------------

if (!require(tidyverse)) install.packages('tidyverse')
if (!require(igraph)) install.packages('igraph')
if (!require(gtools)) install.packages('gtools')
if (!require(tools)) install.packages('tools')
if (!require(tuneR)) install.packages('tuneR')

# set available CPU cores
no_cores <- detectCores() - 1

# flag to use cuda GPU processing if available
cuda <- F

# CONFIG ------------------------------------------------------------------

# set the working directory to the desired project directory in input
setwd("~/OneDrive - Victoria University of Wellington - STAFF/Git/transcendental-information-cascades/input/2019-11-15-letter-to-n-study/")

# EEG examples

# PIPELINE 1 --------------------------------------------------------------

## create TIC
# get project name from current directory and create output directory for project
projectDir <- basename(getwd())
dir.create(file.path(paste0("../../output/"), projectDir), showWarnings = FALSE)

# data source
dataSource <- "example-mamem.txt"

# select tokeniser
tokeniser <- "continuous/signal-EEG-spectral-coherence"

# create output directories for the different modules in this pipeline
outputDir <- paste0(tools::file_path_sans_ext(dataSource),"-",gsub("/","-",tokeniser),"-",format(Sys.time(), "%Y-%m-%d-%H-%M-%S"))
dir.create(file.path(paste0("../../output/",projectDir), outputDir), showWarnings = T)
dir.create(file.path(paste0("../../output/",projectDir,"/",outputDir), "createTIC"), showWarnings = FALSE)
dir.create(file.path(paste0("../../output/",projectDir,"/",outputDir), "visualiseTIC"), showWarnings = FALSE)
dir.create(file.path(paste0("../../output/",projectDir,"/",outputDir), "postProcessTICNetwork"), showWarnings = FALSE)
dir.create(file.path(paste0("../../output/",projectDir,"/",outputDir), "postProcessTICMultiplex"), showWarnings = FALSE)

# run
source("../../src/createTIC/main.R")
tic <- createTIC(projectDir, outputDir, dataSource, tokeniser)

# run TIC network post processing
source("../../src/postProcessTICNetwork/main.R")
postProcessTICNetwork(tic, projectDir, outputDir, dataSource, tokeniser,no_cores=1)


# PIPELINE 2 --------------------------------------------------------------

## create TIC
# get project name from current directory and create output directory for project
projectDir <- basename(getwd())
dir.create(file.path(paste0("../../output/"), projectDir), showWarnings = FALSE)

# data source
dataSource <- "random-mamem.txt"

# select tokeniser
tokeniser <- "continuous/signal-EEG-spectral-coherence"

# create output directories for the different modules in this pipeline
outputDir <- paste0(tools::file_path_sans_ext(dataSource),"-",gsub("/","-",tokeniser),"-",format(Sys.time(), "%Y-%m-%d-%H-%M-%S"))
dir.create(file.path(paste0("../../output/",projectDir), outputDir), showWarnings = T)
dir.create(file.path(paste0("../../output/",projectDir,"/",outputDir), "createTIC"), showWarnings = FALSE)
dir.create(file.path(paste0("../../output/",projectDir,"/",outputDir), "visualiseTIC"), showWarnings = FALSE)
dir.create(file.path(paste0("../../output/",projectDir,"/",outputDir), "postProcessTICNetwork"), showWarnings = FALSE)
dir.create(file.path(paste0("../../output/",projectDir,"/",outputDir), "postProcessTICMultiplex"), showWarnings = FALSE)

# run
source("../../src/createTIC/main.R")
tic <- createTIC(projectDir, outputDir, dataSource, tokeniser)

# run TIC network post processing
source("../../src/postProcessTICNetwork/main.R")
postProcessTICNetwork(tic, projectDir, outputDir, dataSource, tokeniser,no_cores=1)


# PIPELINE 3 --------------------------------------------------------------

## create TIC
# get project name from current directory and create output directory for project
projectDir <- basename(getwd())
dir.create(file.path(paste0("../../output/"), projectDir), showWarnings = FALSE)

# data source
dataSource <- "example-simulated.txt"

# select tokeniser
tokeniser <- "continuous/signal-EEG-spectral-coherence"

# create output directories for the different modules in this pipeline
outputDir <- paste0(tools::file_path_sans_ext(dataSource),"-",gsub("/","-",tokeniser),"-",format(Sys.time(), "%Y-%m-%d-%H-%M-%S"))
dir.create(file.path(paste0("../../output/",projectDir), outputDir), showWarnings = T)
dir.create(file.path(paste0("../../output/",projectDir,"/",outputDir), "createTIC"), showWarnings = FALSE)
dir.create(file.path(paste0("../../output/",projectDir,"/",outputDir), "visualiseTIC"), showWarnings = FALSE)
dir.create(file.path(paste0("../../output/",projectDir,"/",outputDir), "postProcessTICNetwork"), showWarnings = FALSE)
dir.create(file.path(paste0("../../output/",projectDir,"/",outputDir), "postProcessTICMultiplex"), showWarnings = FALSE)

# run
source("../../src/createTIC/main.R")
tic <- createTIC(projectDir, outputDir, dataSource, tokeniser)

# run TIC network post processing
source("../../src/postProcessTICNetwork/main.R")
postProcessTICNetwork(tic, projectDir, outputDir, dataSource, tokeniser,no_cores=1)


# PIPELINE 4 --------------------------------------------------------------

## create TIC
# get project name from current directory and create output directory for project
projectDir <- basename(getwd())
dir.create(file.path(paste0("../../output/"), projectDir), showWarnings = FALSE)

# data source
dataSource <- "random-simulated.txt"

# select tokeniser
tokeniser <- "continuous/signal-EEG-spectral-coherence"

# create output directories for the different modules in this pipeline
outputDir <- paste0(tools::file_path_sans_ext(dataSource),"-",gsub("/","-",tokeniser),"-",format(Sys.time(), "%Y-%m-%d-%H-%M-%S"))
dir.create(file.path(paste0("../../output/",projectDir), outputDir), showWarnings = T)
dir.create(file.path(paste0("../../output/",projectDir,"/",outputDir), "createTIC"), showWarnings = FALSE)
dir.create(file.path(paste0("../../output/",projectDir,"/",outputDir), "visualiseTIC"), showWarnings = FALSE)
dir.create(file.path(paste0("../../output/",projectDir,"/",outputDir), "postProcessTICNetwork"), showWarnings = FALSE)
dir.create(file.path(paste0("../../output/",projectDir,"/",outputDir), "postProcessTICMultiplex"), showWarnings = FALSE)

# run
source("../../src/createTIC/main.R")
tic <- createTIC(projectDir, outputDir, dataSource, tokeniser)

# run TIC network post processing
source("../../src/postProcessTICNetwork/main.R")
postProcessTICNetwork(tic, projectDir, outputDir, dataSource, tokeniser,no_cores=1)


# PIPELINE 5 --------------------------------------------------------------

## create TIC
# get project name from current directory and create output directory for project
projectDir <- basename(getwd())
dir.create(file.path(paste0("../../output/"), projectDir), showWarnings = FALSE)

# data source
dataSource <- "brownian-motion.txt"

# select tokeniser
tokeniser <- "continuous/signal-EEG-spectral-coherence"

# create output directories for the different modules in this pipeline
outputDir <- paste0(tools::file_path_sans_ext(dataSource),"-",gsub("/","-",tokeniser),"-",format(Sys.time(), "%Y-%m-%d-%H-%M-%S"))
dir.create(file.path(paste0("../../output/",projectDir), outputDir), showWarnings = T)
dir.create(file.path(paste0("../../output/",projectDir,"/",outputDir), "createTIC"), showWarnings = FALSE)
dir.create(file.path(paste0("../../output/",projectDir,"/",outputDir), "visualiseTIC"), showWarnings = FALSE)
dir.create(file.path(paste0("../../output/",projectDir,"/",outputDir), "postProcessTICNetwork"), showWarnings = FALSE)
dir.create(file.path(paste0("../../output/",projectDir,"/",outputDir), "postProcessTICMultiplex"), showWarnings = FALSE)

# run
source("../../src/createTIC/main.R")
tic <- createTIC(projectDir, outputDir, dataSource, tokeniser)

# run TIC network post processing
source("../../src/postProcessTICNetwork/main.R")
postProcessTICNetwork(tic, projectDir, outputDir, dataSource, tokeniser,no_cores=1)



# TEXT EXAMPLES
# 
# PIPELINE 6 --------------------------------------------------------------

## create TIC
# get project name from current directory and create output directory for project
projectDir <- basename(getwd())
dir.create(file.path(paste0("../../output/"), projectDir), showWarnings = FALSE)

# data source
dataSource <- "jungle_book.txt"

# select tokeniser
tokeniser <- "discrete/text-POS-nouns"

# create output directories for the different modules in this pipeline
outputDir <- paste0(tools::file_path_sans_ext(dataSource),"-",gsub("/","-",tokeniser),"-",format(Sys.time(), "%Y-%m-%d-%H-%M-%S"))
dir.create(file.path(paste0("../../output/",projectDir), outputDir), showWarnings = T)

# run
source("../../src/createTIC/main.R")
tic <- createTIC(projectDir, outputDir, dataSource, tokeniser)

# run TIC network post processing
source("../../src/postProcessTICNetwork/main.R")
postProcessTICNetwork(tic, projectDir, outputDir, dataSource, tokeniser,no_cores=1)


# PIPELINE 7 --------------------------------------------------------------

## create TIC
# get project name from current directory and create output directory for project
projectDir <- basename(getwd())
dir.create(file.path(paste0("../../output/"), projectDir), showWarnings = FALSE)

# select tokeniser
tokeniser <- "discrete/text-POS-verbs"

# create output directories for this project pipeline
outputDir <- paste0(tools::file_path_sans_ext(dataSource),"-",gsub("/","-",tokeniser),"-",format(Sys.time(), "%Y-%m-%d-%H-%M-%S"))
dir.create(file.path(paste0("../../output/",projectDir), outputDir), showWarnings = T)

# run
source("../../src/createTIC/main.R")
tic <- createTIC(projectDir, outputDir, dataSource, tokeniser,no_cores)

# run TIC network post processing
source("../../src/postProcessTICNetwork/main.R")
postProcessTICNetwork(tic, projectDir, outputDir, dataSource, tokeniser,no_cores=1)


# PIPELINE 8 --------------------------------------------------------------

## create TIC
# get project name from current directory and create output directory for project
projectDir <- basename(getwd())
dir.create(file.path(paste0("../../output/"), projectDir), showWarnings = FALSE)

# select tokeniser
tokeniser <- "discrete/text-POS-adjectives"

# create output directory for this project
outputDir <- paste0(tools::file_path_sans_ext(dataSource),"-",gsub("/","-",tokeniser),"-",format(Sys.time(), "%Y-%m-%d-%H-%M-%S"))
dir.create(file.path(paste0("../../output/",projectDir), outputDir), showWarnings = T)

#dir.create(file.path(paste0("../../output/",projectDir,"/",outputDir), "visualiseTIC"), showWarnings = FALSE)

# run TIC creation
source("../../src/createTIC/main.R")
tic <- createTIC(projectDir, outputDir, dataSource, tokeniser)

# run TIC network post processing
source("../../src/postProcessTICNetwork/main.R")
postProcessTICNetwork(tic, projectDir, outputDir, dataSource, tokeniser,no_cores=1)


# random sentence

# PIPELINE 6 --------------------------------------------------------------

## create TIC
# get project name from current directory and create output directory for project
projectDir <- basename(getwd())
dir.create(file.path(paste0("../../output/"), projectDir), showWarnings = FALSE)

# data source
dataSource <- "jungle_book_random_sentences.txt"

# select tokeniser
tokeniser <- "discrete/text-POS-nouns"

# create output directories for the different modules in this pipeline
outputDir <- paste0(tools::file_path_sans_ext(dataSource),"-",gsub("/","-",tokeniser),"-",format(Sys.time(), "%Y-%m-%d-%H-%M-%S"))
dir.create(file.path(paste0("../../output/",projectDir), outputDir), showWarnings = T)

# run
source("../../src/createTIC/main.R")
tic <- createTIC(projectDir, outputDir, dataSource, tokeniser)

# run TIC network post processing
source("../../src/postProcessTICNetwork/main.R")
postProcessTICNetwork(tic, projectDir, outputDir, dataSource, tokeniser,no_cores=1)


# PIPELINE 7 --------------------------------------------------------------

## create TIC
# get project name from current directory and create output directory for project
projectDir <- basename(getwd())
dir.create(file.path(paste0("../../output/"), projectDir), showWarnings = FALSE)

# select tokeniser
tokeniser <- "discrete/text-POS-verbs"

# create output directories for this project pipeline
outputDir <- paste0(tools::file_path_sans_ext(dataSource),"-",gsub("/","-",tokeniser),"-",format(Sys.time(), "%Y-%m-%d-%H-%M-%S"))
dir.create(file.path(paste0("../../output/",projectDir), outputDir), showWarnings = T)

# run
source("../../src/createTIC/main.R")
tic <- createTIC(projectDir, outputDir, dataSource, tokeniser,no_cores)

# run TIC network post processing
source("../../src/postProcessTICNetwork/main.R")
postProcessTICNetwork(tic, projectDir, outputDir, dataSource, tokeniser,no_cores=1)


# PIPELINE 8 --------------------------------------------------------------

## create TIC
# get project name from current directory and create output directory for project
projectDir <- basename(getwd())
dir.create(file.path(paste0("../../output/"), projectDir), showWarnings = FALSE)

# select tokeniser
tokeniser <- "discrete/text-POS-adjectives"

# create output directory for this project
outputDir <- paste0(tools::file_path_sans_ext(dataSource),"-",gsub("/","-",tokeniser),"-",format(Sys.time(), "%Y-%m-%d-%H-%M-%S"))
dir.create(file.path(paste0("../../output/",projectDir), outputDir), showWarnings = T)

#dir.create(file.path(paste0("../../output/",projectDir,"/",outputDir), "visualiseTIC"), showWarnings = FALSE)

# run TIC creation
source("../../src/createTIC/main.R")
tic <- createTIC(projectDir, outputDir, dataSource, tokeniser)

# run TIC network post processing
source("../../src/postProcessTICNetwork/main.R")
postProcessTICNetwork(tic, projectDir, outputDir, dataSource, tokeniser,no_cores=1)



# random words
#

# PIPELINE 6 --------------------------------------------------------------

## create TIC
# get project name from current directory and create output directory for project
projectDir <- basename(getwd())
dir.create(file.path(paste0("../../output/"), projectDir), showWarnings = FALSE)

# data source
dataSource <- "jungle_book_random_word_sentences.txt"

# select tokeniser
tokeniser <- "discrete/text-POS-nouns"

# create output directories for the different modules in this pipeline
outputDir <- paste0(tools::file_path_sans_ext(dataSource),"-",gsub("/","-",tokeniser),"-",format(Sys.time(), "%Y-%m-%d-%H-%M-%S"))
dir.create(file.path(paste0("../../output/",projectDir), outputDir), showWarnings = T)

# run
source("../../src/createTIC/main.R")
tic <- createTIC(projectDir, outputDir, dataSource, tokeniser)

# run TIC network post processing
source("../../src/postProcessTICNetwork/main.R")
postProcessTICNetwork(tic, projectDir, outputDir, dataSource, tokeniser,no_cores=1)


# PIPELINE 7 --------------------------------------------------------------

## create TIC
# get project name from current directory and create output directory for project
projectDir <- basename(getwd())
dir.create(file.path(paste0("../../output/"), projectDir), showWarnings = FALSE)

# select tokeniser
tokeniser <- "discrete/text-POS-verbs"

# create output directories for this project pipeline
outputDir <- paste0(tools::file_path_sans_ext(dataSource),"-",gsub("/","-",tokeniser),"-",format(Sys.time(), "%Y-%m-%d-%H-%M-%S"))
dir.create(file.path(paste0("../../output/",projectDir), outputDir), showWarnings = T)

# run
source("../../src/createTIC/main.R")
tic <- createTIC(projectDir, outputDir, dataSource, tokeniser,no_cores)

# run TIC network post processing
source("../../src/postProcessTICNetwork/main.R")
postProcessTICNetwork(tic, projectDir, outputDir, dataSource, tokeniser,no_cores=1)


# PIPELINE 8 --------------------------------------------------------------

## create TIC
# get project name from current directory and create output directory for project
projectDir <- basename(getwd())
dir.create(file.path(paste0("../../output/"), projectDir), showWarnings = FALSE)

# select tokeniser
tokeniser <- "discrete/text-POS-adjectives"

# create output directory for this project
outputDir <- paste0(tools::file_path_sans_ext(dataSource),"-",gsub("/","-",tokeniser),"-",format(Sys.time(), "%Y-%m-%d-%H-%M-%S"))
dir.create(file.path(paste0("../../output/",projectDir), outputDir), showWarnings = T)

#dir.create(file.path(paste0("../../output/",projectDir,"/",outputDir), "visualiseTIC"), showWarnings = FALSE)

# run TIC creation
source("../../src/createTIC/main.R")
tic <- createTIC(projectDir, outputDir, dataSource, tokeniser)


# run TIC network post processing
source("../../src/postProcessTICNetwork/main.R")
postProcessTICNetwork(tic, projectDir, outputDir, dataSource, tokeniser,no_cores=1)


#word vec

# PIPELINE 1 --------------------------------------------------------------

## create TIC
# get project name from current directory and create output directory for project
projectDir <- basename(getwd())
dir.create(file.path(paste0("../../output/"), projectDir), showWarnings = FALSE)

# data source
dataSource <- "jungle_book.txt"

# select tokeniser
tokeniser <- "discrete/text-word2vec-cosinesim"

# create output directories for the different modules in this pipeline
outputDir <- paste0(tools::file_path_sans_ext(dataSource),"-",gsub("/","-",tokeniser),"-",format(Sys.time(), "%Y-%m-%d-%H-%M-%S"))
dir.create(file.path(paste0("../../output/",projectDir), outputDir), showWarnings = T)

# run
source("../../src/createTIC/main.R")
tic <- createTIC(projectDir, outputDir, dataSource, tokeniser)

# run TIC network post processing
source("../../src/postProcessTICNetwork/main.R")
postProcessTICNetwork(tic, projectDir, outputDir, dataSource, tokeniser,no_cores=1)


# PIPELINE 1 --------------------------------------------------------------

## create TIC
# get project name from current directory and create output directory for project
projectDir <- basename(getwd())
dir.create(file.path(paste0("../../output/"), projectDir), showWarnings = FALSE)

# data source
dataSource <- "jungle_book_random_sentences.txt"

# select tokeniser
tokeniser <- "discrete/text-word2vec-cosinesim"

# create output directories for the different modules in this pipeline
outputDir <- paste0(tools::file_path_sans_ext(dataSource),"-",gsub("/","-",tokeniser),"-",format(Sys.time(), "%Y-%m-%d-%H-%M-%S"))
dir.create(file.path(paste0("../../output/",projectDir), outputDir), showWarnings = T)

# run
source("../../src/createTIC/main.R")
tic <- createTIC(projectDir, outputDir, dataSource, tokeniser)

# run TIC network post processing
source("../../src/postProcessTICNetwork/main.R")
postProcessTICNetwork(tic, projectDir, outputDir, dataSource, tokeniser,no_cores=1)


# PIPELINE 1 --------------------------------------------------------------

## create TIC
# get project name from current directory and create output directory for project
projectDir <- basename(getwd())
dir.create(file.path(paste0("../../output/"), projectDir), showWarnings = FALSE)

# data source
dataSource <- "jungle_book_random_word_sentences.txt"

# select tokeniser
tokeniser <- "discrete/text-word2vec-cosinesim"

# create output directories for the different modules in this pipeline
outputDir <- paste0(tools::file_path_sans_ext(dataSource),"-",gsub("/","-",tokeniser),"-",format(Sys.time(), "%Y-%m-%d-%H-%M-%S"))
dir.create(file.path(paste0("../../output/",projectDir), outputDir), showWarnings = T)

# run
source("../../src/createTIC/main.R")
tic <- createTIC(projectDir, outputDir, dataSource, tokeniser)

# run TIC network post processing
source("../../src/postProcessTICNetwork/main.R")
postProcessTICNetwork(tic, projectDir, outputDir, dataSource, tokeniser,no_cores=1)






# PRIMES ------------------------------------------------------------------

# PIPELINE PRIME --------------------------------------------------------------

## create TIC
# get project name from current directory and create output directory for project
projectDir <- basename(getwd())
dir.create(file.path(paste0("../../output/"), projectDir), showWarnings = FALSE)

# data source
dataSource <- "primes50k.txt"

# select tokeniser
tokeniser <- "discrete/tokenised"

# create output directories for the different modules in this pipeline
outputDir <- paste0(tools::file_path_sans_ext(dataSource),"-",gsub("/","-",tokeniser),"-",format(Sys.time(), "%Y-%m-%d-%H-%M-%S"))
dir.create(file.path(paste0("../../output/",projectDir), outputDir), showWarnings = T)

# run
source("../../src/createTIC/main.R")
tic <- createTIC(projectDir, outputDir, dataSource, tokeniser)

# run TIC network post processing
source("../../src/postProcessTICNetwork/main.R")
postProcessTICNetwork(tic, projectDir, outputDir, dataSource, tokeniser,no_cores=1)


# PIPELINE RANDOM PRIME --------------------------------------------------------------

## create TIC
# get project name from current directory and create output directory for project
projectDir <- basename(getwd())
dir.create(file.path(paste0("../../output/"), projectDir), showWarnings = FALSE)

# data source
dataSource <- "primes50k-random.txt"

# select tokeniser
tokeniser <- "discrete/tokenised"

# create output directories for the different modules in this pipeline
outputDir <- paste0(tools::file_path_sans_ext(dataSource),"-",gsub("/","-",tokeniser),"-",format(Sys.time(), "%Y-%m-%d-%H-%M-%S"))
dir.create(file.path(paste0("../../output/",projectDir), outputDir), showWarnings = T)

# run
source("../../src/createTIC/main.R")
tic <- createTIC(projectDir, outputDir, dataSource, tokeniser)

# run TIC network post processing
source("../../src/postProcessTICNetwork/main.R")
postProcessTICNetwork(tic, projectDir, outputDir, dataSource, tokeniser,no_cores=1)


# PIPELINE PRIME BASE 4 --------------------------------------------------------------

## create TIC
# get project name from current directory and create output directory for project
projectDir <- basename(getwd())
dir.create(file.path(paste0("../../output/"), projectDir), showWarnings = FALSE)

# data source
dataSource <- "primes-base4-dataset-50k.txt"

# select tokeniser
tokeniser <- "discrete/tokenised"

# create output directories for the different modules in this pipeline
outputDir <- paste0(tools::file_path_sans_ext(dataSource),"-",gsub("/","-",tokeniser),"-",format(Sys.time(), "%Y-%m-%d-%H-%M-%S"))
dir.create(file.path(paste0("../../output/",projectDir), outputDir), showWarnings = T)

# run
source("../../src/createTIC/main.R")
tic <- createTIC(projectDir, outputDir, dataSource, tokeniser)

# run TIC network post processing
source("../../src/postProcessTICNetwork/main.R")
postProcessTICNetwork(tic, projectDir, outputDir, dataSource, tokeniser,no_cores=1)



# PIPELINE PRIME BASE 8 --------------------------------------------------------------

## create TIC
# get project name from current directory and create output directory for project
projectDir <- basename(getwd())
dir.create(file.path(paste0("../../output/"), projectDir), showWarnings = FALSE)

# data source
dataSource <- "primes-base8-dataset-50k.txt"

# select tokeniser
tokeniser <- "discrete/tokenised"

# create output directories for the different modules in this pipeline
outputDir <- paste0(tools::file_path_sans_ext(dataSource),"-",gsub("/","-",tokeniser),"-",format(Sys.time(), "%Y-%m-%d-%H-%M-%S"))
dir.create(file.path(paste0("../../output/",projectDir), outputDir), showWarnings = T)

# run
source("../../src/createTIC/main.R")
tic <- createTIC(projectDir, outputDir, dataSource, tokeniser)

# run TIC network post processing
source("../../src/postProcessTICNetwork/main.R")
postProcessTICNetwork(tic, projectDir, outputDir, dataSource, tokeniser,no_cores=1)



# PIPELINE PRIME BASE 16 --------------------------------------------------------------

## create TIC
# get project name from current directory and create output directory for project
projectDir <- basename(getwd())
dir.create(file.path(paste0("../../output/"), projectDir), showWarnings = FALSE)

# data source
dataSource <- "primes-base16-dataset-50k.txt"

# select tokeniser
tokeniser <- "discrete/tokenised"

# create output directories for the different modules in this pipeline
outputDir <- paste0(tools::file_path_sans_ext(dataSource),"-",gsub("/","-",tokeniser),"-",format(Sys.time(), "%Y-%m-%d-%H-%M-%S"))
dir.create(file.path(paste0("../../output/",projectDir), outputDir), showWarnings = T)

# run
source("../../src/createTIC/main.R")
tic <- createTIC(projectDir, outputDir, dataSource, tokeniser)

# run TIC network post processing
source("../../src/postProcessTICNetwork/main.R")
postProcessTICNetwork(tic, projectDir, outputDir, dataSource, tokeniser,no_cores=1)

# PIPELINE PRIME BASE 32 --------------------------------------------------------------

## create TIC
# get project name from current directory and create output directory for project
projectDir <- basename(getwd())
dir.create(file.path(paste0("../../output/"), projectDir), showWarnings = FALSE)

# data source
dataSource <- "primes-base32-dataset-50k.txt"

# select tokeniser
tokeniser <- "discrete/tokenised"

# create output directories for the different modules in this pipeline
outputDir <- paste0(tools::file_path_sans_ext(dataSource),"-",gsub("/","-",tokeniser),"-",format(Sys.time(), "%Y-%m-%d-%H-%M-%S"))
dir.create(file.path(paste0("../../output/",projectDir), outputDir), showWarnings = T)

# run
source("../../src/createTIC/main.R")
tic <- createTIC(projectDir, outputDir, dataSource, tokeniser)

# run TIC network post processing
source("../../src/postProcessTICNetwork/main.R")
postProcessTICNetwork(tic, projectDir, outputDir, dataSource, tokeniser,no_cores=1)

# PIPELINE PRIME BASE 36 --------------------------------------------------------------

## create TIC
# get project name from current directory and create output directory for project
projectDir <- basename(getwd())
dir.create(file.path(paste0("../../output/"), projectDir), showWarnings = FALSE)

# data source
dataSource <- "primes-base36-dataset-50k.txt"

# select tokeniser
tokeniser <- "discrete/tokenised"

# create output directories for the different modules in this pipeline
outputDir <- paste0(tools::file_path_sans_ext(dataSource),"-",gsub("/","-",tokeniser),"-",format(Sys.time(), "%Y-%m-%d-%H-%M-%S"))
dir.create(file.path(paste0("../../output/",projectDir), outputDir), showWarnings = T)

# run
source("../../src/createTIC/main.R")
tic <- createTIC(projectDir, outputDir, dataSource, tokeniser)

# run TIC network post processing
source("../../src/postProcessTICNetwork/main.R")
postProcessTICNetwork(tic, projectDir, outputDir, dataSource, tokeniser,no_cores=1)


# PIPELINE RANDOM NUMBERS --------------------------------------------------------------

## create TIC
# get project name from current directory and create output directory for project
projectDir <- basename(getwd())
dir.create(file.path(paste0("../../output/"), projectDir), showWarnings = FALSE)

# data source
dataSource <- "randomnumbers50k.txt"

# select tokeniser
tokeniser <- "discrete/tokenised"

# create output directories for the different modules in this pipeline
outputDir <- paste0(tools::file_path_sans_ext(dataSource),"-",gsub("/","-",tokeniser),"-",format(Sys.time(), "%Y-%m-%d-%H-%M-%S"))
dir.create(file.path(paste0("../../output/",projectDir), outputDir), showWarnings = T)

# run
source("../../src/createTIC/main.R")
tic <- createTIC(projectDir, outputDir, dataSource, tokeniser)

# run TIC network post processing
source("../../src/postProcessTICNetwork/main.R")
postProcessTICNetwork(tic, projectDir, outputDir, dataSource, tokeniser,no_cores=1)


# REMOVE TAIL FROM PRIME DATASETS FOR PERTUB ANALYSIS ----------------------

# PIPELINE PRIMES WITH k-th bottom removed by set probability --------------------------------------------------------------

## create TIC
# get project name from current directory and create output directory for project
for(i in 1:20){
  projectDir <- basename(getwd())
  dir.create(file.path(paste0("../../output/"), projectDir), showWarnings = FALSE)
  
  # data source
  dataSource <- paste0("primes50k_tailRemove_",i,".txt")

  # select tokeniser
  tokeniser <- "discrete/tokenised"
  
  # create output directories for the different modules in this pipeline
  outputDir <- paste0(tools::file_path_sans_ext(dataSource),"-",gsub("/","-",tokeniser),"-",format(Sys.time(), "%Y-%m-%d-%H-%M-%S"))
  dir.create(file.path(paste0("../../output/",projectDir), outputDir), showWarnings = T)
  
  # run
  source("../../src/createTIC/main.R")
  tic <- createTIC(projectDir, outputDir, dataSource, tokeniser)
  
  # run TIC network post processing
  source("../../src/postProcessTICNetwork/main.R")
  postProcessTICNetwork(tic, projectDir, outputDir, dataSource, tokeniser,no_cores=1)
}

# PIPELINE PRIMES WITH k-th head removed by combined token probability probability --------------------------------------------------------------

## create TIC
# get project name from current directory and create output directory for project
for(i in 1:20){
  projectDir <- basename(getwd())
  dir.create(file.path(paste0("../../output/"), projectDir), showWarnings = FALSE)
  
  # data source
  dataSource <- paste0("primes50k_headRemove_",i,".txt")
  
  # select tokeniser
  tokeniser <- "discrete/tokenised"
  
  # create output directories for the different modules in this pipeline
  outputDir <- paste0(tools::file_path_sans_ext(dataSource),"-",gsub("/","-",tokeniser),"-",format(Sys.time(), "%Y-%m-%d-%H-%M-%S"))
  dir.create(file.path(paste0("../../output/",projectDir), outputDir), showWarnings = T)
  
  # run
  source("../../src/createTIC/main.R")
  tic <- createTIC(projectDir, outputDir, dataSource, tokeniser)
  
  # run TIC network post processing
  source("../../src/postProcessTICNetwork/main.R")
  postProcessTICNetwork(tic, projectDir, outputDir, dataSource, tokeniser,no_cores=1)
}



## create TIC
# get project name from current directory and create output directory for project
for(i in 1:20){
  projectDir <- basename(getwd())
  dir.create(file.path(paste0("../../output/"), projectDir), showWarnings = FALSE)
  
  # data source
  dataSource <- paste0("random50k_tailRemove_",i,".txt")
  
  # select tokeniser
  tokeniser <- "discrete/tokenised"
  
  # create output directories for the different modules in this pipeline
  outputDir <- paste0(tools::file_path_sans_ext(dataSource),"-",gsub("/","-",tokeniser),"-",format(Sys.time(), "%Y-%m-%d-%H-%M-%S"))
  dir.create(file.path(paste0("../../output/",projectDir), outputDir), showWarnings = T)
  
  # run
  source("../../src/createTIC/main.R")
  tic <- createTIC(projectDir, outputDir, dataSource, tokeniser)
  
  # run TIC network post processing
  source("../../src/postProcessTICNetwork/main.R")
  postProcessTICNetwork(tic, projectDir, outputDir, dataSource, tokeniser,no_cores=1)
}

# PIPELINE RANDOMS WITH k-th head removed by combined token probability probability --------------------------------------------------------------

## create TIC
# get project name from current directory and create output directory for project
for(i in 1:20){
  projectDir <- basename(getwd())
  dir.create(file.path(paste0("../../output/"), projectDir), showWarnings = FALSE)
  
  # data source
  dataSource <- paste0("random50k_headRemove_",i,".txt")
  
  # select tokeniser
  tokeniser <- "discrete/tokenised"
  
  # create output directories for the different modules in this pipeline
  outputDir <- paste0(tools::file_path_sans_ext(dataSource),"-",gsub("/","-",tokeniser),"-",format(Sys.time(), "%Y-%m-%d-%H-%M-%S"))
  dir.create(file.path(paste0("../../output/",projectDir), outputDir), showWarnings = T)
  
  # run
  source("../../src/createTIC/main.R")
  tic <- createTIC(projectDir, outputDir, dataSource, tokeniser)
  
  # run TIC network post processing
  source("../../src/postProcessTICNetwork/main.R")
  postProcessTICNetwork(tic, projectDir, outputDir, dataSource, tokeniser,no_cores=1)
}




#### MULTIPLEX

# PIPELINE 4 --------------------------------------------------------------

## post process TIC multiplex from pipelines 1 - 3

#which TICs to comnbine?

#all
## load all TIC adjacency matrices
#allTICs <- list.dirs(paste0("../../output/",projectDir,"/"),full.names = F,recursive = F)
#allTICs <- allTICs[-which(allTICs=="postProcessTICMultiplex")]

#or just selected
allTICs <- c("primes50k-discrete-tokenised-2019-11-21-17-54-58",
             "primes-base4-dataset-50k-discrete-tokenised-2019-11-21-20-52-25",
             "primes-base8-dataset-50k-discrete-tokenised-2019-11-21-20-59-13",
             "primes-base16-dataset-50k-discrete-tokenised-2019-11-21-21-08-56",
             "primes-base32-dataset-50k-discrete-tokenised-2019-11-25-17-34-48",
             "primes-base36-dataset-50k-discrete-tokenised-2019-11-25-17-57-06")

source("../../src/postProcessTICMultiplex/main.R")
tnsr <- postProcessTICMultiplex(projectDir, outputDir, dataSource, allTICs)