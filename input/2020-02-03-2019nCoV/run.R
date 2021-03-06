

# PREAMBLE ----------------------------------------------------------------
set.seed(3011)
RNGkind("L'Ecuyer-CMRG")

if (!require(tidyverse)) install.packages('tidyverse')
if (!require(igraph)) install.packages('igraph')
if (!require(gtools)) install.packages('gtools')
if (!require(tools)) install.packages('tools')

# set available CPU cores
no_cores <- detectCores() - 1

# flag to use cuda GPU processing if available
cuda <- F

# CONFIG ------------------------------------------------------------------

# set the working directory to the desired project directory in input
setwd("~/OneDrive - Victoria University of Wellington - STAFF/Git/transcendental-information-cascades/input/2020-02-03-2019nCoV/")

# PIPELINE 1 --------------------------------------------------------------

## create TIC
# get project name from current directory and create output directory for project
projectDir <- basename(getwd())
dir.create(file.path(paste0("../../output/"), projectDir), showWarnings = FALSE)

# data source
dataSource <- "ordered.txt"

# select tokeniser
tokeniser <- "discrete/genes-CODON-samplesequence"

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

## post process TIC network

## visualise process TIC network


# PIPELINE 2 --------------------------------------------------------------

## create TIC
# get project name from current directory and create output directory for project
projectDir <- basename(getwd())
dir.create(file.path(paste0("../../output/"), projectDir), showWarnings = FALSE)

# data source
dataSource <- "unordered.txt"

# select tokeniser
tokeniser <- "discrete/genes-CODON-samplesequence"

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

## post process TIC network

## visualise process TIC network

# PIPELINE 3 --------------------------------------------------------------

## create TIC
# get project name from current directory and create output directory for project
projectDir <- basename(getwd())
dir.create(file.path(paste0("../../output/"), projectDir), showWarnings = FALSE)

# data source
dataSource <- "random.txt"

# select tokeniser
tokeniser <- "discrete/genes-CODON-samplesequence"

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

## post process TIC network

## visualise process TIC network



# PIPELINE 4 manual post process from python create --------------------------------------------------------------

# run TIC network post processing
source("../../src/postProcessTICNetwork/main.R")
postProcessTICNetwork(path="~/OneDrive - Victoria University of Wellington - STAFF/Python Scripts/tic/output/")

## post process TIC network

## visualise process TIC network

