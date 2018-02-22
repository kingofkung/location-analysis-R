rm(list = ls(all.names = TRUE))

## Read In Files
## For PC
## setwd("C:\\Users\\brogers\\Desktop\\SpatialFun\\pMed")
## For my mac
setwd("~/Dropbox/pMed")

DemandNames <- as.matrix(read.csv("CityClass_Names_CSV.csv", header = FALSE))

DemandNumbers <- as.matrix(read.table("DS_CityClass_Numbers_Text.txt"))

SupplyClassString <- as.matrix(read.csv("AHA_String_Class_CSV.csv",
                                        header = FALSE))

SupplyClassNumber <- as.matrix(read.table("AHA_Numbers_Class_Text.txt"))

DistanceMatrix <- as.matrix(read.table("FinalDistanceMatrix_Text.txt"))
#--------------------------------------------------------------
#Initialize important parameters
numberOptimized <- 8

totalDistance <- 0
optimalLocations <- numeric(numberOptimized)

numberOfSupplyNodes <- nrow(SupplyClassNumber)
numberOfDemandNodes <- nrow(DemandNumbers)

#--------------------------------------------------------------
#Start Timer
ptm <- proc.time()

#Find initial solution

solutionMatrix <- matrix(ncol = numberOptimized, nrow = numberOfDemandNodes)
demandDistances <- numeric(nrow(solutionMatrixTest))

## Choose first p clinics as initial solution
optimalLocations[1:numberOptimized] <- 1:numberOptimized

## Extract corresponding columns from the distance matrix
solutionMatrix[, 1:numberOptimized] <- DistanceMatrix[, optimalLocations]

##Determine which clinic a city will go to
## get the minimum of each row in the solution matrix
closestSupplyLocation <- apply(solutionMatrix, 1, min)
## and multiply by the first column of DemandNumbers
demandDistances <- closestSupplyLocation * DemandNumbers[, 1]

#Calculate the total distance
totalDistance <- sum(demandDistances)

#--------------------------------------------------------------

## develop indexing matrix
im <- matrix(data = c(
                     sort(rep(seq_len(numberOptimized), numberOfSupplyNodes)),
                     rep(seq_len(numberOfSupplyNodes), numberOptimized)),
             ncol = 2)
colnames(im) <- c('i', 'j')

## While loop for vertex substitutions until convergence
whileCounter <- 0
while(whileCounter < (numberOptimized * numberOfSupplyNodes))
{
  ## Initialize/re-initialize
  solutionMatrixTest <- matrix(ncol = numberOptimized, nrow = numberOfDemandNodes)
  demandDistancesTest <- numeric(nrow(solutionMatrixTest))
  totalDistanceTest <- 0
  whileCounter <- 0

  ## Begin vertex substitutions
    for(k in seq_len(nrow(im))){
        i <- im[k, 'i']
        j <- im[k, 'j']
        ## designate temporary locations
        temporaryLocations <- optimalLocations
        temporaryLocations[i] <- SupplyClassNumber[j, 3]

        ## Same steps to calculate totalDistance, this time with temporaryLocations
        solutionMatrixTest[, 1:numberOptimized] <- DistanceMatrix[, temporaryLocations]

        closestSupplyLocationTest <- apply(solutionMatrixTest, 1, min)
        demandDistancesTest <- closestSupplyLocationTest * DemandNumbers[, 1]

        totalDistanceTest <- sum(demandDistancesTest)

        ## Add to whileCounter if there is no improvement from a substitution
        if (totalDistanceTest >= totalDistance) whileCounter <- whileCounter + 1

        ## If there is improvement, complete the substitution
        if (totalDistanceTest < totalDistance)
        {
            totalDistance <- totalDistanceTest
            optimalLocations[i] <- SupplyClassNumber[j, 3]
        }

        totalDistanceTest <- 0
        solutionMatrixTest <- matrix(ncol = numberOptimized, nrow = numberOfDemandNodes)
        demandDistancesTest <- numeric(nrow(solutionMatrixTest))
    }

}
## 42457764867
totalDistance
##  42  449 1188  313  196  250 1046  128
optimalLocations
## Original Time:
##  user  system elapsed
## 69.50    0.00   77.19
proc.time() - ptm

