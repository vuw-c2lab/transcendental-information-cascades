

# PREAMBLE ----------------------------------------------------------------

if (!require(tidyverse)) install.packages('tidyverse')
if (!require(igraph)) install.packages('igraph')
if (!require(gtools)) install.packages('gtools')
if (!require(tools)) install.packages('tools')
if (!require(parallel)) install.packages('parallel')

# set available CPU cores
no_cores <- detectCores() - 1

# flag to use cuda GPU processing if available
cuda <- F

# CONFIG ------------------------------------------------------------------

# set the working directory to the desired project directory in input
setwd("~/OneDrive - Victoria University of Wellington - STAFF/Git/transcendental-information-cascades/input/example-with-text-wordvectors/")

# PIPELINE 1 --------------------------------------------------------------

## create TIC
# get project name from current directory and create output directory for project
projectDir <- basename(getwd())
dir.create(file.path(paste0("../../output/"), projectDir), showWarnings = FALSE)

# data source
dataSource <- "example-short.txt"

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

## post process TIC network

## visualise process TIC network

# PIPELINE 2 --------------------------------------------------------------

## create TIC
# get project name from current directory and create output directory for project
projectDir <- basename(getwd())
dir.create(file.path(paste0("../../output/"), projectDir), showWarnings = FALSE)

# data source
dataSource <- "example-short.txt"

# select tokeniser
tokeniser <- "continuous/text-word2vec-cosinesim"

# create output directories for the different modules in this pipeline
outputDir <- paste0(tools::file_path_sans_ext(dataSource),"-",gsub("/","-",tokeniser),"-",format(Sys.time(), "%Y-%m-%d-%H-%M-%S"))
dir.create(file.path(paste0("../../output/",projectDir), outputDir), showWarnings = T)

# run
source("../../src/createTIC/main.R")
tic <- createTIC(projectDir, outputDir, dataSource, tokeniser)

# run TIC network post processing
source("../../src/postProcessTICNetwork/main.R")
postProcessTICNetwork(tic, projectDir, outputDir, dataSource, tokeniser,no_cores=1)

## post process TIC network

## visualise process TIC network
