library(dplyr)
library(tidyverse)
options(scipen = 999)


# equities <- readxl::read_xlsx("C:\\Users\\carlvinm\\Downloads\\AssetClass (6).xlsx")
# fixedIncome <- readxl::read_xlsx("C:\\Users\\carlvinm\\Downloads\\AssetClass (7).xlsx")
# moneymarket <- readxl::read_xlsx("C:\\Users\\carlvinm\\Downloads\\AssetClass (8).xlsx")
# summary <- readxl::read_xlsx("C:\\Users\\carlvinm\\Downloads\\DataGrid (1).xlsx")

###EQUITIES----
equities <- read.csv("C:\\Users\\carlvinm\\Desktop\\Analytics tests\\equities.csv", stringsAsFactors = FALSE)
timedeposits <- read.csv("C:\\Users\\carlvinm\\Desktop\\Analytics tests\\timedeposits.csv", stringsAsFactors = FALSE)
names(timedeposits)
names(equities)


#Weights----
difs <- equities %>%  group_by(HoldingsDate, AssetCategory) %>%

  #Units----
  mutate(test_Units = OpeningMarketValue + Creations + Redemptions ) %>%  #Units
  mutate(difUnits = test_Units - Units) %>% #DIff Units

#Unit Price----
  mutate(test_UnitPRice = ifelse(!ClosingMarketValue,0,ClosingMarketValue/Units )) %>% #Unit Price
  mutate(difUnitPrice = test_UnitPRice - UnitPrice ) %>% #Diff Unit Price

#Portfolio Perf----
  mutate(test_ActualPerformance = test_UnitPRice - 1 ) %>% #ActualPerf
  mutate(diffActualPerformance = test_ActualPerformance - ActualPerformance ) %>% #Diff ActualPerformance

  #Portfolio Weight----
  mutate(test_Weight = ifelse(!ClosingMarketValue,0,ClosingMarketValue/sum(ClosingMarketValue))) %>% #Weights
  mutate(diffWeights = test_Weight - Weight) %>% #Diff weights

#Weighted Perf----
  mutate(test_weightedPerformance = test_Weight * test_ActualPerformance) %>% #Weighted Performance
  mutate(diffweightedPerformance = test_weightedPerformance - WeightedPerformance) %>% #Diff Weighted Performance

#TO DO - custom benchmarks perf----
mutate(test_BenchmarkPerf = BenchmarkReturn) %>% #Benchmark Perf to do
mutate(diffBenchmarkPerf = test_BenchmarkPerf - BenchmarkReturn) %>% #diff benchmark perf

  #Benchmark Weights----
  mutate(test_BenchmarkWeight = BenchmarkWeight) %>% #BM Weight
  mutate(diffBenchmarkWeight = test_BenchmarkWeight - BenchmarkWeight) %>% #Diff  BM weight

#Weighted Benchmark Perf----
mutate(test_WeightedBMPerformance = test_BenchmarkWeight * test_BenchmarkPerf ) %>%
mutate(diffWeightedBMPerf = test_WeightedBMPerformance - WeightedBenchmarkReturn/100) %>%

#Active Weight----
mutate(test_ActiveWeight = test_Weight - test_BenchmarkWeight ) %>% #Active Weight
mutate(diffActiveWeight = test_ActiveWeight - ActiveWeight/100) %>% #Diff  Active weight

#Active Return----
mutate(test_ActiveReturn = test_ActualPerformance - test_BenchmarkPerf ) %>% #Active per
mutate(diffActiveReturn  = test_ActiveReturn - ActiveReturn/100) %>% #Diff  Active perf

#selection Effect----
mutate(test_SelectionEffect = (test_ActualPerformance - test_BenchmarkPerf) * test_BenchmarkWeight ) %>% #
mutate(diffSelectionEffect  = test_SelectionEffect - SelectionEffect/100) %>% #

#Allocation effect----
mutate(test_AllocationEffect = (test_BenchmarkPerf * test_Weight) - (test_BenchmarkWeight * test_BenchmarkPerf) ) %>% #
mutate(diffAllocationEffect  = test_AllocationEffect - AllocationEffect/100) %>% #

#Interaction effect----
mutate(test_InteractionEffect = test_ActiveReturn - test_AllocationEffect - test_SelectionEffect ) %>% #=
mutate(diffInteractionEffect  = test_InteractionEffect - InteractionEffect/100) %>% #=

#Active Management----
mutate(test_ActiveManagementEffect = test_AllocationEffect + test_SelectionEffect ) %>% #
mutate(diffActiveManagement  = test_ActiveManagementEffect - ActiveManagementEffect/100)  #

sum(difs$diffActiveWeight)

sum(difs$test_InteractionEffect - difs$InteractionEffect/100, na.rm = TRUE)

write.csv(difs,"difs.csv")

#
# mutate(weightCalc = ClosingMarketValue/sum(ClosingMarketValue))
# View(weights)
#
# SumweightsTest <- weights %>% group_by(HoldingsDate) %>% summarise( sum(weightCalc))
# SumweightsActual <- equities %>% group_by(HoldingsDate) %>% summarise( sum(Weight))
#
# SumweightsActual$`sum(Weight)` - SumweightsTest$`sum(weightCalc)`
#
#
# write.csv(weights, "equityweights.csv")
#



df <- (fixedIncome[-1,])


# # timeweighter2<- function(HPR){
# #   returns=1
# #   for(i in 1:length(as.numeric(levels(HPR$WeightedBondReturn))[HPR$WeightedBondReturn])){
# #     returns=returns*(1+as.numeric(levels(HPR$WeightedBondReturn))[HPR$WeightedBondReturn][[i]])
# #   }
# #   returns=(returns-1)*100
# #   as.numeric( returns)
# # }

timeweighter <- function(HPR){
  returns = 1
  for (i in 1:length(HPR)) {
    returns = returns * (1 + HPR[[i]]/(100))
  }
  returns = (returns - 1) * 100
  returns#as.numeric(returns)
}

timeweighter(df$`Weighted Benchmark Return`)




###############################---------------

#Weights----
depos <- timedeposits %>%  group_by(HoldingsDate, AssetCategory) %>%

  #Units----
mutate(test_Units = OpeningMarketValue + Creations + Redemptions ) %>%  #Units
  mutate(difUnits = test_Units - Units) %>% #DIff Units

  #Unit Price----
mutate(test_UnitPRice = ifelse(!ClosingMarketValue,0,ClosingMarketValue/Units )) %>% #Unit Price
  mutate(difUnitPrice = test_UnitPRice - UnitPrice ) %>% #Diff Unit Price

  #Portfolio Perf----
mutate(test_ActualPerformance = test_UnitPRice - 1 ) %>% #ActualPerf
  mutate(diffActualPerformance = test_ActualPerformance - ActualPerformance ) %>% #Diff ActualPerformance

  #Portfolio Weight----
mutate(test_Weight = ifelse(!ClosingMarketValue,0,ClosingMarketValue/sum(ClosingMarketValue))) %>% #Weights
  mutate(diffWeights = test_Weight - Weight) %>% #Diff weights

  #Weighted Perf----
mutate(test_weightedPerformance = test_Weight * test_ActualPerformance) %>% #Weighted Performance
  mutate(diffweightedPerformance = test_weightedPerformance - WeightedPerformance) %>% #Diff Weighted Performance

  #TO DO - custom benchmarks perf----
mutate(test_BenchmarkPerf = BenchmarkReturn) %>% #Benchmark Perf to do
  mutate(diffBenchmarkPerf = test_BenchmarkPerf - BenchmarkReturn) %>% #diff benchmark perf

  #Benchmark Weights----
mutate(test_BenchmarkWeight = BenchmarkWeight) %>% #BM Weight
  mutate(diffBenchmarkWeight = test_BenchmarkWeight - BenchmarkWeight) %>% #Diff  BM weight

  #Weighted Benchmark Perf----
mutate(test_WeightedBMPerformance = test_BenchmarkWeight * test_BenchmarkPerf ) %>%
  mutate(diffWeightedBMPerf = test_WeightedBMPerformance - WeightedBenchmarkReturn/100) %>%

  #Active Weight----
mutate(test_ActiveWeight = test_Weight - test_BenchmarkWeight ) %>% #Active Weight
  mutate(diffActiveWeight = test_ActiveWeight - ActiveWeight/100) %>% #Diff  Active weight

  #Active Return----
mutate(test_ActiveReturn = test_ActualPerformance - test_BenchmarkPerf ) %>% #Active per
  mutate(diffActiveReturn  = test_ActiveReturn - ActiveReturn/100) %>% #Diff  Active perf

  #selection Effect----
mutate(test_SelectionEffect = (test_ActualPerformance - test_BenchmarkPerf) * test_BenchmarkWeight ) %>% #
  mutate(diffSelectionEffect  = test_SelectionEffect - SelectionEffect/100) %>% #

  #Allocation effect----
mutate(test_AllocationEffect = (test_BenchmarkPerf * test_Weight) - (test_BenchmarkWeight * test_BenchmarkPerf) ) %>% #
  mutate(diffAllocationEffect  = test_AllocationEffect - AllocationEffect/100) %>% #

  #Interaction effect----
mutate(test_InteractionEffect = test_ActiveReturn - test_AllocationEffect - test_SelectionEffect ) %>% #=
  mutate(diffInteractionEffect  = test_InteractionEffect - InteractionEffect/100) %>% #=

  #Active Management----
mutate(test_ActiveManagementEffect = test_AllocationEffect + test_SelectionEffect ) %>% #
  mutate(diffActiveManagement  = test_ActiveManagementEffect - ActiveManagementEffect/100)  #

# sum(difs$diffActiveWeight)
#
# sum(difs$test_InteractionEffect - difs$InteractionEffect/100, na.rm = TRUE)

write.csv(depos,"difsdepos.csv")




















# geolink <- function(returns){
#   returns = 1
#   for (i in 1:length(returns)){
#     returns = returns * (1 + returns[[i]])
#   }
#   returns = (returns -1) * 100
#   returns
# }
#
#
# geolink(ret)
#
#
# ret <- c(1:10)
# ret[[5]]
#
# x = 2
# b= 3
# bb <- function(x,b){
#
# calc = (x + 2 * b)/6 ^ (x*8)
# calc
# }
#
# bb(2,3)
