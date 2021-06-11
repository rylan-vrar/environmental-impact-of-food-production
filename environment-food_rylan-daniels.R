# © Rylan Daniels. 2021. All Rights Reserved.

# 🍉 🌎
# The Environmental Impact of Food Production
# Data Science -- Final Project

# Rylan Daniels

#____________________________________________________________

# libraries

# libraries for data visualization
install.packages("lessR")
library(lessR)
install.packages("ggplot2")
library(ggplot2)
# library for machine learning
install.packages("randomForest")
library(randomForest)
install.packages("factoextra")
library(factoextra)
install.packages("dplyr")
library(dplyr)



# data access
foodProduction <- read.csv("./Food_Production.csv") 

# exploratory data analysis (EDA)
head(foodProduction)
dim(foodProduction) # dimensions
# [1] 43 23
names(foodProduction) # variable definitions
summary(foodProduction)

str(foodProduction)


# feature engineering
# remove unneeded variables about scarcity of water use
foodProduction <- subset(foodProduction, select= -c(Scarcity.weighted.water.use.per.kilogram..liters.per.kilogram.,
                                                    Scarcity.weighted.water.use.per.100g.protein..liters.per.100g.protein.,
                                                    Scarcity.weighted.water.use.per.1000kcal..liters.per.1000.kilocalories.))
names(foodProduction)


# data transformation

# new data set with foodProduction data and 5 major food groups: grains, vegetables, fruits, protein, dairy
foodProductionGrouped <- cbind(foodProduction, "Food.group")

for (i in 1:length(foodProductionGrouped[,21])) {
  if (i >= 1 && i <= 5) {
    foodProductionGrouped[i,21] <- "grain"
  }
  else if ((i >= 6 && i <= 11) || (i >= 16 && i <= 25)) {
    foodProductionGrouped[i,21] <- "vegetable"
  }
  else if ((i >= 12 && i <= 13) || i == 15 || (i >= 34 && i <= 38) || i >= 41) {
    foodProductionGrouped[i,21] <- "protein"
  }
  else if (i >= 26 && i <= 33) {
    foodProductionGrouped[i,21] <- "fruit"
  }
  else if (i == 14 || i == 39 || i == 40) {
    foodProductionGrouped[i,21] <- "dairy"
  }
}

head(foodProductionGrouped)


# more exploratory data analysis (EDA)

# there are 33 non-missing values of greenhouse gas emissions per 1000 kCals
sum(!is.na(foodProductionGrouped$Greenhouse.gas.emissions.per.1000kcal..kgCO.eq.per.1000kcal.))

# there are 43 types of food in the data set
levels(as.factor(foodProductionGrouped$Food.product))

# there is just 1 of each type of food
table(foodProductionGrouped$Food.product)

# percentages of food products
prop.table(table(foodProductionGrouped$Food.product))

# amount of foods in each food group
table(foodProductionGrouped$`"Food.group"`)
# dairy     fruit     grain   protein vegetable 
# 3         8         5        11        16 

# each food group's percentage
prop.table(table(foodProductionGrouped$`"Food.group"`)) 
# dairy      fruit      grain    protein  vegetable 
# 0.06976744 0.18604651 0.11627907 0.25581395 0.37209302 

# correlation between freshwater withdrawals emissions and greenhouse gas emissions
plot(foodProductionGrouped$Freshwater.withdrawals.per.1000kcal..liters.per.1000kcal.,
     foodProductionGrouped$Greenhouse.gas.emissions.per.1000kcal..kgCO.eq.per.1000kcal.,
     pch=19,
     xlab="Freshwater Withdrawals Emissions",
     ylab="Greenhouse Gas Emissions",
     main="Correlation Between Freshwater Withdrawals and Greenhouse Gases")



# data visualization

# setup of variables for data vis

# calculate total recorded emissions from all recorded foods
totalDatasetEmissions <- sum(foodProductionGrouped$Total_emissions)
totalDatasetEmissions
# [1] 256.8


# calculate total emissions of each food group

# total emissions from grains
totalGrainEmissions <- subset(foodProductionGrouped, `"Food.group"`=="grain", select=Total_emissions)
totalGrainEmissions <- sum(totalGrainEmissions)
totalGrainEmissions
# [1] 9.2

# total emissions from vegetables
totalVegetableEmissions <- subset(foodProductionGrouped, `"Food.group"`=="vegetable", select=Total_emissions)
totalVegetableEmissions <- sum(totalVegetableEmissions)
totalVegetableEmissions
# [1] 37.3

# total emissions from proteins
totalProteinEmissions <- subset(foodProductionGrouped, `"Food.group"`=="protein", select=Total_emissions)
totalProteinEmissions <- sum(totalProteinEmissions)
totalProteinEmissions
# [1] 145.5

# total emissions from fruits
totalFruitEmissions <- subset(foodProductionGrouped, `"Food.group"`=="fruit", select=Total_emissions)
totalFruitEmissions <- sum(totalFruitEmissions)
totalFruitEmissions
# [1] 39.8

# total emissions from dairy
totalDairyEmissions <- subset(foodProductionGrouped, `"Food.group"`=="dairy", select=Total_emissions)
totalDairyEmissions <- sum(totalDairyEmissions)
totalDairyEmissions
# [1] 25


# calculate percentage of each food group's emissions

grainEmissionsPercent <- (totalGrainEmissions / totalDatasetEmissions)*100
grainEmissionsPercent
# [1] 3.582555

vegetableEmissionsPercent <- (totalVegetableEmissions / totalDatasetEmissions)*100
vegetableEmissionsPercent
# [1] 14.52492

proteinEmissionsPercent <- (totalProteinEmissions / totalDatasetEmissions)*100
proteinEmissionsPercent
# [1] 56.65888

fruitEmissionsPercent <- (totalFruitEmissions / totalDatasetEmissions)*100
fruitEmissionsPercent
# [1] 15.49844

dairyEmissionsPercent <- (totalDairyEmissions / totalDatasetEmissions)*100
dairyEmissionsPercent
# [1] 9.735202


# Visualize emissions by food group with pie and bar charts

chartData <- data.frame(c(rep("Grain", round(grainEmissionsPercent)), 
                           rep("Vegetable", round(vegetableEmissionsPercent)),
                           rep("Protein", round(proteinEmissionsPercent)),
                           rep("Fruit", round(fruitEmissionsPercent)),
                           rep("Dairy", round(dairyEmissionsPercent))
                           ))

PieChart(c.rep..Grain...round.grainEmissionsPercent....rep..Vegetable..., 
         hole=0, 
         values="%", 
         data=chartData, 
         main="Emissions Per Food Group")

BarChart(c.rep..Grain...round.grainEmissionsPercent....rep..Vegetable..., 
         hole=0, 
         values="%", 
         data=chartData, 
         main="Emissions Per Food Group", 
         xlab="Food Groups", 
         ylab="% Emissions")


# heatmap of food production process direct emissions

names(foodProductionGrouped)
foodProdProcess <- subset(foodProductionGrouped, select=c("Farm", 
                                                          "Processing",
                                                          "Transport",
                                                          "Packging",
                                                          "Retail",
                                                          "Total_emissions"))
names(foodProdProcess)
foodProdProcess

matrix_foodProdProcess <- as.matrix(foodProdProcess)
rownames(matrix_foodProdProcess) <- paste0(foodProductionGrouped$Food.product)
matrix_foodProdProcess

heatmap(x=matrix_foodProdProcess, Colv = NA, Rowv = NA)


# heatmap of other effects of food production

names(foodProductionGrouped)
withoutFoodProduct <- subset(foodProductionGrouped, select= -c(Food.product,
                                                               Farm,
                                                               Processing,
                                                               Transport,
                                                               Packging,
                                                               Retail))
withoutFoodProduct <- sapply(withoutFoodProduct, as.numeric)
matrix_withoutFoodProduct <- as.matrix(withoutFoodProduct)
rownames(matrix_withoutFoodProduct) <- paste0(foodProductionGrouped$Food.product)
head(matrix_withoutFoodProduct)

heatmap(x=matrix_withoutFoodProduct, Colv = NA, Rowv = NA)


# distribution of freshwater withdrawals emissions based on food group
# must create subsets of each food group's freshwater data to be used in data visualizations
grainFreshwater <- subset(foodProductionGrouped, `"Food.group"`=="grain", select=Freshwater.withdrawals.per.1000kcal..liters.per.1000kcal.)
for (i in 1:length(grainFreshwater[,1])) {
  rownames(grainFreshwater)[i] <- foodProductionGrouped$Food.product[as.numeric(rownames(grainFreshwater)[i])]
}
vegetableFreshwater <- subset(foodProductionGrouped, `"Food.group"`=="vegetable", select=Freshwater.withdrawals.per.1000kcal..liters.per.1000kcal.)
for (i in 1:length(vegetableFreshwater[,1])) {
  rownames(vegetableFreshwater)[i] <- foodProductionGrouped$Food.product[as.numeric(rownames(vegetableFreshwater)[i])]
}
proteinFreshwater <- subset(foodProductionGrouped, `"Food.group"`=="protein", select=Freshwater.withdrawals.per.1000kcal..liters.per.1000kcal.)
for (i in 1:length(proteinFreshwater[,1])) {
  rownames(proteinFreshwater)[i] <- foodProductionGrouped$Food.product[as.numeric(rownames(proteinFreshwater)[i])]
}
fruitFreshwater <- subset(foodProductionGrouped, `"Food.group"`=="fruit", select=Freshwater.withdrawals.per.1000kcal..liters.per.1000kcal.)
for (i in 1:length(fruitFreshwater[,1])) {
  rownames(fruitFreshwater)[i] <- foodProductionGrouped$Food.product[as.numeric(rownames(fruitFreshwater)[i])]
}
dairyFreshwater <- subset(foodProductionGrouped, `"Food.group"`=="dairy", select=Freshwater.withdrawals.per.1000kcal..liters.per.1000kcal.)
for (i in 1:length(dairyFreshwater[,1])) {
  rownames(dairyFreshwater)[i] <- foodProductionGrouped$Food.product[as.numeric(rownames(dairyFreshwater)[i])]
}
# box plots displaying the frequency distribution of freshwater withdrawals emissions per food group
boxplot(grainFreshwater$Freshwater.withdrawals.per.1000kcal..liters.per.1000kcal., main="Grain Freshwater Emissions Distribution")
boxplot(vegetableFreshwater$Freshwater.withdrawals.per.1000kcal..liters.per.1000kcal., main="Vegetable Freshwater Emissions Distribution")
boxplot(proteinFreshwater$Freshwater.withdrawals.per.1000kcal..liters.per.1000kcal., main="Protein Freshwater Emissions Distribution")
boxplot(fruitFreshwater$Freshwater.withdrawals.per.1000kcal..liters.per.1000kcal., main="Fruit Freshwater Emissions Distribution")
boxplot(dairyFreshwater$Freshwater.withdrawals.per.1000kcal..liters.per.1000kcal., main="Dairy Freshwater Emissions Distribution")




# machine learning - prediction algorithm (supervised ML)

# low farm data variation to minimize extremes and outliers in the data
lowFarm <- foodProductionGrouped$Farm
lowFarm
lowFarm[1]
for (i in 1:length(lowFarm)) {
  if (lowFarm[i] > 10) {
    lowFarm[i] <- 0
  }
}
lowFarm

emissionPrediction <- lm(foodProductionGrouped$Freshwater.withdrawals.per.1000kcal..liters.per.1000kcal. ~ lowFarm) # running linear regression algorithm
emissionPrediction
# Call:
#   lm(formula = foodProductionGrouped$Freshwater.withdrawals.per.1000kcal..liters.per.1000kcal. ~ 
#        lowFarm)
# 
# Coefficients:
#   (Intercept)      lowFarm  
# 467.30        30.83 

summary(emissionPrediction)
# Call:
#   lm(formula = foodProductionGrouped$Freshwater.withdrawals.per.1000kcal..liters.per.1000kcal. ~ 
#        lowFarm)
# 
# Residuals:
#   Min     1Q Median     3Q    Max 
# -531.3 -395.7 -161.6  204.4 1483.9 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)
# (Intercept)   467.30     138.36   3.377  0.00216
# lowFarm        30.83      79.98   0.385  0.70284
# 
# Residual standard error: 547.2 on 28 degrees of freedom
# (13 observations deleted due to missingness)
# Multiple R-squared:  0.005277,	Adjusted R-squared:  -0.03025 
# F-statistic: 0.1485 on 1 and 28 DF,  p-value: 0.7028

plot(foodProductionGrouped$Freshwater.withdrawals.per.1000kcal..liters.per.1000kcal., 
     lowFarm, 
     pch=19)
abline(emissionPrediction, col="red", lwd=3)

par(mfrow=c(2,2))
plot(emissionPrediction)


# random forest - supervised machine learning
# split data into training and test sets

n <- nrow(foodProdProcess)
ntrain <- round(n*0.6)
set.seed(42)
tindex <- sample(n, ntrain)

train_foodProd <- foodProdProcess[tindex,]
test_foodProd <- foodProdProcess[-tindex,]

# train a random forest

rf <- randomForest(Farm~., data=train_foodProd, ntree=500, mtry=2, importance=TRUE)

prediction <- predict(rf, newdata=test_foodProd, type="class")
prediction
# 6         11         12         14         16         17         19         21         22 
# 0.2985518  0.2962637  0.7481418  1.4885933  2.2652933 10.4754800  1.9630017  0.8261216  0.2985518 
# 23         29         31         32         33         39         40         43 
# 0.2985518  0.7154909  0.2991028  8.9760900  8.6521100  1.8550367  9.4635329  2.6079432 

# confusion matrix
table(prediction, test_foodProd$Farm)

# miscalssification error rate
misclassificationErrorRate <- sum(test_foodProd$Farm != prediction) / 
  nrow(test_foodProd) * 100
misclassificationErrorRate
# [1] 100

# importance of random forest model (useful for indicating accruacy of algorithm)
importance(rf)
# %IncMSE IncNodePurity
# Processing      10.0876580     607.15090
# Transport       -1.1547986      72.17890
# Packging         0.4402374      37.68292
# Retail           1.2759353     115.83518
# Total_emissions 11.1295891     804.87391

print(rf)
# Call:
#   randomForest(formula = Farm ~ ., data = train_foodProd, ntree = 500,      mtry = 2, importance = TRUE) 
# Type of random forest: regression
# Number of trees: 500
# No. of variables tried at each split: 2
# 
# Mean of squared residuals: 32.37678
# % Var explained: 54.47

# variable importance plot visually illustrates accuracy of model with random forest
varImpPlot(rf)



# machine learning - prediction algorithm accuracy

# K-means algorithm

# clusters

set.seed(42)

kmeans_FoodProd <- kmeans(foodProductionGrouped$Farm, centers=3)
kmeans_FoodProd

kmeans_FoodProd$cluster

kmeans_FoodProd$centers

# visualize clusters from the K-means algorithm
fviz_cluster(kmeans_FoodProd, 
             data = data.frame(foodProductionGrouped$Farm, foodProductionGrouped$Freshwater.withdrawals.per.1000kcal..liters.per.1000kcal.),
             main = "Clusters",
             xlab = "Farm emissions",
             ylab = "Freshwater emissions")





