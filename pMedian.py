##---------------------------------------------------------------------
## Program: pMedian.py
## Version: 1.0
##
## Author: Nick Joslyn
## Institution: Simpson College
##
## Acknowledgements: Advisor Heidi Berger and research
##      partners Emma Christensen and Maddy Kersten.
##
## Purpose: Solve the p-median problem according to the Vertex
##	Substitution Algorithm proposed by Hakimi (and ??) in 1968.
##
## Instructions: Accumulate input and output files as specified in the 
##      readme. Compile and the printed indices correspond to the
##      optimal facility locations.
##
## Notes: Python is 0-indexed. Therefore, to identify the correct row
##      in the input Excel spreadsheet, add one to each index in the
##      optimal locations array.
##
##---------------------------------------------------------------------

##=====================================================================
import numpy as np
import time

#Read-in Files
demandNumbersExcel = 'DS_CityClass_Numbers_Text.txt'
supplyNumbersExcel = 'AHA_Numbers_Class_Text.txt'
distanceMatrixExcel = 'FinalDistanceMatrix_Text.txt'

DemandNumbers = np.genfromtxt(demandNumbersExcel)
SupplyNumbers = np.genfromtxt(supplyNumbersExcel)
DistanceMatrix = np.genfromtxt(distanceMatrixExcel)

#DemandNamesReadIn 
#SupplyNamesReadIn

numberOptimized = 8
totalDistance = 0
optimalLocations = np.arange(numberOptimized)

numberOfSupplyNodes = SupplyNumbers.shape[0]
numberOfDemandNodes = DemandNumbers.shape[0]

#---------------------------------------------
start = time.time()

solutionMatrix = np.ndarray(shape = (numberOfDemandNodes, numberOptimized))
demandDistances = np.arange(numberOfDemandNodes, dtype = 'int64')

for i in range(numberOptimized):
    optimalLocations[i] = SupplyNumbers[i,2]-1

for i in range(numberOptimized):
    solutionMatrix[:,i] = DistanceMatrix[:,optimalLocations[i]]

for i in range(numberOfDemandNodes):
    closestSupplyLocation = np.amin(solutionMatrix[i,:])
    demandDistances[i] = closestSupplyLocation * DemandNumbers[i,0]

for i in range(numberOfDemandNodes):
    totalDistance += demandDistances[i]

#---------------------------------------------

whileCounter = 0
while(whileCounter < (numberOptimized * numberOfSupplyNodes)):
    solutionMatrixTest = np.ndarray(shape = (numberOfDemandNodes, numberOptimized))
    demandDistancesTest = np.arange(numberOfDemandNodes, dtype = 'int64')
    totalDistanceTest = 0
    whileCounter = 0

    for i in range(numberOptimized):
        for j in range(numberOfSupplyNodes):
            temporaryLocations = np.copy(optimalLocations)
            temporaryLocations[i] = SupplyNumbers[j,2] - 1
            
            for k in range(numberOptimized):
                solutionMatrixTest[:,k] = DistanceMatrix[:,temporaryLocations[k]]

            for m in range(numberOfDemandNodes):
                closestSupplyLocationTest = np.amin(solutionMatrixTest[m,:])
                demandDistancesTest[m] = closestSupplyLocationTest * DemandNumbers[m,0]

            for n in range(numberOfDemandNodes):
                totalDistanceTest += demandDistancesTest[n]

            if totalDistanceTest >= totalDistance:
                whileCounter += 1
            
            if totalDistanceTest < totalDistance:
                totalDistance = totalDistanceTest
                optimalLocations[i] = SupplyNumbers[j,2]-1

            totalDistanceTest = 0
            solutionMatrixTest = np.ndarray(shape = (numberOfDemandNodes, numberOptimized))
            demandDistancesTest = np.arange(numberOfDemandNodes, dtype = 'int64')

#----------------------------------------------
end = time.time()

print(totalDistance)
print(optimalLocations)
print("Wall Time: " + str(round(end-start, 2)) + " seconds")

