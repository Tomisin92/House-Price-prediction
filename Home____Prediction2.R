



install.packages("odbc")
install.packages("factoextra")
install.packages("dplyr")
install.packages("DBI")
install.packages("dbplyr")
install.packages("Hmisc")
library(factoextra)
library(Hmisc)
library(DBI)
library(odbc)
library(dplyr)
library(corrplot)
library(ggplot2) # Data visualization
library(gplots)
library(leaps) # helps with function StepAIC
library(MASS)
library(repr)

con<- dbConnect(odbc::odbc(),
                Driver = "SQL server",
                Server = "DESKTOP-ELOAIL5",
                Database = "HomePrediction",
                Port = 1433)

dbListTables(con) # Tables in DB
dbListFields(con, 'train_data')
traindata <- tbl(con, 'train_data')
traindata <-collect(traindata)
View(traindata)

dbListFields(con, 'test_data')
testdata <- tbl(con, 'test_data')
testdata <-collect(testdata)
View(testdata)

summary(traindata)
summary(testdata)

# Summary of Target Variable: Sale-Price
summary(traindata$SalePrice)


##################################################EXPLORATORY DATA ANALYSIS (EDA)


# Draw a histogram to figure out the distribution of Sale_Price
ggplot(traindata, aes(x =SalePrice)) +
  geom_histogram() + ggtitle("Histogram of Sale-Price")+
  labs(x= "Sales", y ="count")

#log term of Sale Price
traindata$SalePrice <- log(traindata$SalePrice)


options(repr.plot.width=5, repr.plot.height=4)
ggplot(traindata, aes(x = MSZoning, fill = MSZoning )) + geom_bar()+ 
  scale_fill_hue(c = 80)+ ggtitle(" Distribution of MSZoning")+
  theme(plot.title = element_text(hjust = 0.5),legend.position="right", legend.background = element_rect(fill="grey90",
                                                                                                         size=0.5, linetype="solid", colour ="black")) + geom_text(stat='count',aes(label=..count..),vjust=-0.25)

# Distribution of MSZoning
table(traindata$MSZoning)

# Change plot size to 9 x 6
options(repr.plot.width=9, repr.plot.height=6)

#boxplot of SalePrice by MSZoning
#add average value of SalePrice as red point
ggplot(traindata, aes(x=MSZoning, y=SalePrice, fill=MSZoning)) + 
  geom_boxplot(alpha=0.3) +
  stat_summary(fun.y=mean, geom="point", shape=20, size=4, color="red", fill="red")+
  theme(legend.position="none")+
  ggtitle("Boxplot of SalePrice by MSZoning")+
  theme(plot.title = element_text(hjust = 0.5))




###########Check Missing Data for train and test data
#### It was observed that were some missing values the data-set 
# list rows of data that have missing values 

missing_row <- traindata[!complete.cases(traindata),]
head(missing_row)
nrow(missing_row)
colSums(is.na(traindata)) ## number of NA in the data set
sum(is.na(traindata))


#Missing Values from the test_data
nrow(testdata)
colSums(is.na(testdata))
sum(is.na(testdata))


########I ran through some simple missing data code and found that yes, there are missing data.
#  From the Training Data, the  missing data are Linear feet of street connected to property (Lot_Frontage) of 259. From the Train Data we are missing the Lot_Frontage of 227 in the dataset.
# For the missing Lot_Frontage,  the median Lot_Frontage was used to replace missing Lot_Frontage values. 
##for both data-sets, the returned value was 69 for the train_data and 67 for the test_data. Thus the missing values with be replaced with these numbers.



#Filling missing values for Lot_Frontage: Linear feet of street connected to property

median(traindata$LotFrontage, na.rm=TRUE)
median(testdata$LotFrontage, na.rm=TRUE)
traindata$LotFrontage  <- ifelse(is.na(traindata$LotFrontage), 69, traindata$LotFrontage)
traindata$LotFrontage
testdata$LotFrontage  <- ifelse(is.na(testdata$LotFrontage), 67, testdata$LotFrontage)
testdata$LotFrontage



median(traindata$MasVnrArea, na.rm=TRUE)
median(testdata$MasVnrArea, na.rm=TRUE)
traindata$MasVnrArea  <- ifelse(is.na(traindata$MasVnrArea), 0, traindata$MasVnrArea)
traindata$MasVnrArea
testdata$MasVnrArea  <- ifelse(is.na(testdata$MasVnrArea), 0, testdata$MasVnrArea)
testdata$MasVnrArea



median(testdata$BsmtFinSF1, na.rm=TRUE)
testdata$BsmtFinSF1  <- ifelse(is.na(testdata$BsmtFinSF1), 350.5, testdata$BsmtFinSF1)
testdata$BsmtFinSF1

median(testdata$BsmtFinSF2, na.rm=TRUE)
testdata$BsmtFinSF2  <- ifelse(is.na(testdata$BsmtFinSF2), 0, testdata$BsmtFinSF2)
testdata$BsmtFinSF2


median(testdata$BsmtUnfSF, na.rm=TRUE)
testdata$BsmtUnfSF  <- ifelse(is.na(testdata$BsmtUnfSF), 460, testdata$BsmtUnfSF)
testdata$BsmtUnfSF


median(testdata$TotalBsmtSF, na.rm=TRUE)
testdata$TotalBsmtSF  <- ifelse(is.na(testdata$TotalBsmtSF), 988, testdata$TotalBsmtSF)
testdata$TotalBsmtSF


median(testdata$GarageCars, na.rm=TRUE)
testdata$GarageCars  <- ifelse(is.na(testdata$GarageCars), 2, testdata$TotalBsmtSF)
testdata$GarageCars


median(testdata$GarageArea, na.rm=TRUE)
testdata$GarageArea  <- ifelse(is.na(testdata$GarageArea), 480, testdata$GarageArea)
testdata$GarageArea



# correlation plot for numeric variables
Num_traindata <- subset( traindata, select = c(2,4,5,18,19,27,35,37:39,44:53,55,57,62,63,67:72,76,77,81) )
View(Num_traindata)
num <- unlist(lapply(Num_traindata, is.numeric), use.names = FALSE) 
corrplot(cor(Num_traindata[num]),method = 'number', tl.cex =0.5)

colSums(is.na(Num_traindata))


####confirming if there is still missing value 
colSums(is.na(traindata))
colSums(is.na(testdata))
View(traindata)


# Slicing columns
traindata <- subset( traindata)
View(traindata)

testdata <- subset( testdata)
View(testdata)

# convert factor to numeric

traindata$MSZoning <- as.numeric(factor(traindata$MSZoning, 
                                        levels = c("A", "C","FV", "I","RH", "RL","RP", "RM"),
                                        labels = c(1,2,3,4,5,6,7,8) ,ordered = TRUE))

testdata$MSZoning <- as.numeric(factor(testdata$MSZoning, 
                                       levels = c("A", "C","FV", "I","RH", "RL","RP", "RM"),
                                       labels = c(1,2,3,4,5,6,7,8) ,ordered = TRUE))


traindata$Street <- as.numeric(factor(traindata$Street, 
                                      levels = c("Grvl", "Pave"),
                                      labels = c(1,2) ,ordered = TRUE))


testdata$Street <- as.numeric(factor(testdata$Street, 
                                     levels = c("Grvl", "Pave"),
                                     labels = c(1,2) ,ordered = TRUE))

traindata$Alley <- as.numeric(factor(traindata$Alley, 
                                     levels = c("Grvl", "Pave","NA"),
                                     labels = c(1,2,3) ,ordered = TRUE))


testdata$Alley <- as.numeric(factor(testdata$Alley, 
                                    levels = c("Grvl", "Pave","NA"),
                                    labels = c(1,2,3) ,ordered = TRUE))

traindata$LotShape <- as.numeric(factor(traindata$LotShape, 
                                        levels = c("Reg", "IR1","IR2","IR3"),
                                        labels = c(1,2,3,4) ,ordered = TRUE))


testdata$LotShape <- as.numeric(factor(testdata$LotShape, 
                                       levels = c("Reg", "IR1","IR2","IR3"),
                                       labels = c(1,2,3,4) ,ordered = TRUE))


traindata$LandContour <- as.numeric(factor(traindata$LandContour, 
                                           levels = c("Lvl","BnK","HLS","Low"),
                                           labels = c(1,2,3,4) ,ordered = TRUE))

testdata$LandContour <- as.numeric(factor(testdata$LandContour, 
                                          levels = c("Lvl","BnK","HLS","Low"),
                                          labels = c(1,2,3,4) ,ordered = TRUE))


traindata$Utilities <- as.numeric(factor(traindata$Utilities, 
                                         levels = c("AllPub","NoSewr","NoSewa","Elo"),
                                         labels = c(1,2,3,4) ,ordered = TRUE))

testdata$Utilities <- as.numeric(factor(testdata$Utilities, 
                                        levels = c("AllPub","NoSewr","NoSewa","Elo"),
                                        labels = c(1,2,3,4) ,ordered = TRUE))



traindata$LotConfig <- as.numeric(factor(traindata$LotConfig, 
                                         levels = c("Inside","Corner","CulDSac","FR2","FR3"),
                                         labels = c(1,2,3,4,5) ,ordered = TRUE))


testdata$LotConfig <- as.numeric(factor(testdata$LotConfig, 
                                        levels = c("Inside","Corner","CulDSac","FR2","FR3"),
                                        labels = c(1,2,3,4,5) ,ordered = TRUE))


traindata$LandSlope <- as.numeric(factor(traindata$LandSlope, 
                                         levels = c("Gtl","Mod","Sev"),
                                         labels = c(1,2,3) ,ordered = TRUE))


testdata$LandSlope <- as.numeric(factor(testdata$LandSlope, 
                                        levels = c("Gtl","Mod","Sev"),
                                        labels = c(1,2,3) ,ordered = TRUE))


traindata$Neighborhood <- as.numeric(factor(traindata$Neighborhood, 
                                            levels = c("Blmngtn","Blueste","BrDale","BrkSide","ClearCr","CollgCr", "Crawfor","Edwards", "Gilbert", "IDOTRR", "MeadowV",
                                                       "Mitchel", "NAmes", "NoRidge", "NPkVill", "NridgHt", "NWAmes", "OldTown", "SWISU", "Sawyer", "SawyerW", "Somerst", "StoneBr", "Timber","Veenker"),
                                            labels = c(1,2,3, 4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25) ,ordered = TRUE))


testdata$Neighborhood <- as.numeric(factor(testdata$Neighborhood, 
                                           levels = c("Blmngtn","Blueste","BrDale","BrkSide","ClearCr","CollgCr", "Crawfor","Edwards", "Gilbert", "IDOTRR", "MeadowV",
                                                      "Mitchel", "NAmes", "NoRidge", "NPkVill", "NridgHt", "NWAmes", "OldTown", "SWISU", "Sawyer", "SawyerW", "Somerst", "StoneBr", "Timber","Veenker"),
                                           labels = c(1,2,3, 4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25) ,ordered = TRUE))



traindata$Condition1 <- as.numeric(factor(traindata$Condition1, 
                                          levels = c("Artery","Feedr","Norm","RRNn", "RRAn", "PosN", "PosA", "RRNe", "RRAe"),
                                          labels = c(1,2,3,4,5,6,7,8,9) ,ordered = TRUE))

testdata$Condition1 <- as.numeric(factor(testdata$Condition1, 
                                         levels = c("Artery","Feedr","Norm","RRNn", "RRAn", "PosN", "PosA", "RRNe", "RRAe"),
                                         labels = c(1,2,3,4,5,6,7,8,9) ,ordered = TRUE))


traindata$Condition2 <- as.numeric(factor(traindata$Condition2, 
                                          levels = c("Artery","Feedr","Norm","RRNn", "RRAn", "PosN", "PosA", "RRNe", "RRAe"),
                                          labels = c(1,2,3,4,5,6,7,8,9) ,ordered = TRUE))



testdata$Condition2 <- as.numeric(factor(testdata$Condition2, 
                                         levels = c("Artery","Feedr","Norm","RRNn", "RRAn", "PosN", "PosA", "RRNe", "RRAe"),
                                         labels = c(1,2,3,4,5,6,7,8,9) ,ordered = TRUE))


traindata$BldgType <- as.numeric(factor(traindata$BldgType, 
                                        levels = c("1Fam","2fmCon","Duplex", "TwnhsE","Twnhs"),
                                        labels = c(1,2,3,4,5) ,ordered = TRUE))

testdata$BldgType <- as.numeric(factor(testdata$BldgType, 
                                       levels = c("1Fam","2fmCon","Duplex", "TwnhsE","Twnhs"),
                                       labels = c(1,2,3,4,5) ,ordered = TRUE))


traindata$HouseStyle <- as.numeric(factor(traindata$HouseStyle, 
                                          levels = c("1Story","1.5Fin","1.5Unf", "2Story","2.5Fin", "2.5Unf", "SFoye", "SLvl"),
                                          labels = c(1,2,3,4,5,6,7,8) ,ordered = TRUE))

testdata$HouseStyle <- as.numeric(factor(testdata$HouseStyle, 
                                         levels = c("1Story","1.5Fin","1.5Unf", "2Story","2.5Fin", "2.5Unf", "SFoye", "SLvl"),
                                         labels = c(1,2,3,4,5,6,7,8) ,ordered = TRUE))



traindata$RoofStyle <- as.numeric(factor(traindata$RoofStyle, 
                                         levels = c("Flat","Gable","	Fair", "Gambrel","Hip", "Mansard", 
                                                    "Shed"),
                                         labels = c(1,2,3,4,5,6,7) ,ordered = TRUE))

testdata$RoofStyle <- as.numeric(factor(testdata$RoofStyle, 
                                        levels = c("Flat","Gable","Fair", "Gambrel","Hip", "Mansard", 
                                                   "Shed"),
                                        labels = c(1,2,3,4,5,6,7) ,ordered = TRUE))


traindata$RoofMatl <- as.numeric(factor(traindata$RoofMatl, 
                                        levels = c("ClyTile","CompShg","	Membran", "Metal","Roll", "Tar&Grv", 
                                                   "WdShake","WdShngl" ),
                                        labels = c(1,2,3,4,5,6,7,8) ,ordered = TRUE))


testdata$RoofMatl <- as.numeric(factor(testdata$RoofMatl, 
                                       levels = c("ClyTile","CompShg","	Membran", "Metal","Roll", "Tar&Grv", 
                                                  "WdShake","WdShngl" ),
                                       labels = c(1,2,3,4,5,6,7,8) ,ordered = TRUE))


traindata$Exterior1st <- as.numeric(factor(traindata$Exterior1st, 
                                           levels = c("AsbShng","AsphShn","BrkComm","BrkFace","CBlock","CemntBd","HdBoard","ImStucc","MetalSd","Other", "Plywood", "PreCas", "Stone", "Stucco", "VinylSd", "Wd Sdng", "WdShing" ),
                                           labels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17) ,ordered = TRUE))


testdata$Exterior1st <- as.numeric(factor(testdata$Exterior1st, 
                                          levels = c("AsbShng","AsphShn","BrkComm", "BrkFace","CBlock", "CemntBd", 
                                                     "HdBoard","ImStucc", "MetalSd","Other", "Plywood", "PreCas", "Stone", "Stucco", "VinylSd", "Wd Sdng", "WdShing" ),
                                          labels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17) ,ordered = TRUE))

traindata$Exterior2nd <- as.numeric(factor(traindata$Exterior2nd, 
                                           levels = c("AsbShng","AsphShn","BrkComm", "BrkFace","CBlock", "CemntBd", 
                                                      "HdBoard","ImStucc", "MetalSd","Other", "Plywood", "PreCas", "Stone", "Stucco", "VinylSd", "Wd Sdng", "WdShing" ),
                                           labels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17) ,ordered = TRUE))


testdata$Exterior2nd <- as.numeric(factor(testdata$Exterior2nd, 
                                          levels = c("AsbShng","AsphShn","BrkComm", "BrkFace","CBlock", "CemntBd", 
                                                     "HdBoard","ImStucc", "MetalSd", "Other", "Plywood", "PreCas", "Stone", "Stucco", "VinylSd", "Wd Sdng", "WdShing" ),
                                          labels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17) ,ordered = TRUE))



traindata$MasVnrType <- as.numeric(factor(traindata$MasVnrType, 
                                          levels = c("BrkCmn","BrkFace","CBlock", "None","Stone"),
                                          labels = c(1,2,3,4,5) ,ordered = TRUE))

testdata$MasVnrType <- as.numeric(factor(testdata$MasVnrType, 
                                         levels = c("BrkCmn","BrkFace","CBlock", "None","Stone"),
                                         labels = c(1,2,3,4,5) ,ordered = TRUE))



traindata$ExterQual <- as.numeric(factor(traindata$ExterQual, 
                                         levels = c("Po","Fa","TA", "Gd","Ex"),
                                         labels = c(1,2,3,4,5) ,ordered = TRUE))

testdata$ExterQual <- as.numeric(factor(testdata$ExterQual, 
                                        levels = c("Po","Fa","TA", "Gd","Ex"),
                                        labels = c(1,2,3,4,5) ,ordered = TRUE))


traindata$ExterCond <- as.numeric(factor(traindata$ExterCond, 
                                         levels = c("Po","Fa","TA", "Gd","Ex"),
                                         labels = c(1,2,3,4,5) ,ordered = TRUE))

testdata$ExterCond <- as.numeric(factor(testdata$ExterCond, 
                                        levels = c("Po","Fa","TA", "Gd","Ex"),
                                        labels = c(1,2,3,4,5) ,ordered = TRUE))


traindata$Foundation <- as.numeric(factor(traindata$Foundation, 
                                          levels = c("BrkTil","CBlock","PConc", "Slab","Stone", "Wood"),
                                          labels = c(1,2,3,4,5,6) ,ordered = TRUE))


testdata$Foundation <- as.numeric(factor(testdata$Foundation, 
                                         levels = c("BrkTil","CBlock","PConc", "Slab","Stone", "Wood"),
                                         labels = c(1,2,3,4,5,6) ,ordered = TRUE))

traindata$BsmtQual <- as.numeric(factor(traindata$BsmtQual, 
                                        levels = c("NA","Po","Fa", "TA","Gd", "Ex"),
                                        labels = c(1,2,3,4,5,6) ,ordered = TRUE))

testdata$BsmtQual <- as.numeric(factor(testdata$BsmtQual, 
                                       levels = c("NA","Po","Fa", "TA","Gd", "Ex"),
                                       labels = c(1,2,3,4,5,6) ,ordered = TRUE))


traindata$BsmtCond <- as.numeric(factor(traindata$BsmtCond, 
                                        levels = c("NA","Po","Fa", "TA","Gd", "Ex"),
                                        labels = c(1,2,3,4,5,6) ,ordered = TRUE))

testdata$BsmtCond <- as.numeric(factor(testdata$BsmtCond, 
                                       levels = c("NA","Po","Fa", "TA","Gd", "Ex"),
                                       labels = c(1,2,3,4,5,6) ,ordered = TRUE))


traindata$BsmtExposure <- as.numeric(factor(traindata$BsmtExposure, 
                                            levels = c("NA","No","Mn", "Av","Gd"),
                                            labels = c(1,2,3,4,5) ,ordered = TRUE))

testdata$BsmtExposure <- as.numeric(factor(testdata$BsmtExposure, 
                                           levels = c("NA","No","Mn", "Av","Gd"),
                                           labels = c(1,2,3,4,5) ,ordered = TRUE))


traindata$BsmtFinType1 <- as.numeric(factor(traindata$BsmtFinType1, 
                                            levels = c("NA","Unf","LwQ", "Rec","BLQ","ALQ","GLQ"),
                                            labels = c(1,2,3,4,5,6,7) ,ordered = TRUE))

testdata$BsmtFinType1 <- as.numeric(factor(testdata$BsmtFinType1, 
                                           levels = c("NA","Unf","LwQ", "Rec","BLQ","ALQ","GLQ"),
                                           labels = c(1,2,3,4,5,6,7) ,ordered = TRUE))


traindata$BsmtFinType2 <- as.numeric(factor(traindata$BsmtFinType2, 
                                            levels = c("NA","Unf","LwQ", "Rec","BLQ","ALQ","GLQ"),
                                            labels = c(1,2,3,4,5,6,7) ,ordered = TRUE))

testdata$BsmtFinType2 <- as.numeric(factor(testdata$BsmtFinType2, 
                                           levels = c("NA","Unf","LwQ", "Rec","BLQ","ALQ","GLQ"),
                                           labels = c(1,2,3,4,5,6,7) ,ordered = TRUE))


traindata$Heating <- as.numeric(factor(traindata$Heating, 
                                       levels = c("Floor","GasA","GasW", "Grav","OthW","Wall"),
                                       labels = c(1,2,3,4,5,6) ,ordered = TRUE))

testdata$Heating <- as.numeric(factor(testdata$Heating, 
                                      levels = c("Floor","GasA","GasW", "Grav","OthW","Wall"),
                                      labels = c(1,2,3,4,5,6) ,ordered = TRUE))

traindata$HeatingQC <- as.numeric(factor(traindata$HeatingQC, 
                                         levels = c("Po","Fa","TA", "Gd","Ex"),
                                         labels = c(1,2,3,4,5) ,ordered = TRUE))

testdata$HeatingQC <- as.numeric(factor(testdata$HeatingQC, 
                                        levels = c("Po","Fa","TA", "Gd","Ex"),
                                        labels = c(1,2,3,4,5) ,ordered = TRUE))


traindata$CentralAir <- as.numeric(factor(traindata$CentralAir, 
                                          levels = c("N", "Y"),
                                          labels = c(1,2) ,ordered = TRUE))


testdata$CentralAir <- as.numeric(factor(testdata$CentralAir, 
                                         levels = c("N", "Y"),
                                         labels = c(1,2) ,ordered = TRUE))

traindata$Electrical <- as.numeric(factor(traindata$Electrical, 
                                          levels = c("SBrkr","FuseA","FuseF", "FuseP","Mix"),
                                          labels = c(1,2,3,4,5) ,ordered = TRUE))


testdata$Electrical <- as.numeric(factor(testdata$Electrical, 
                                         levels = c("SBrkr","FuseA","FuseF", "FuseP","Mix"),
                                         labels = c(1,2,3,4,5) ,ordered = TRUE))


traindata$KitchenQual <- as.numeric(factor(traindata$KitchenQual, 
                                           levels = c("Po","Fa","TA", "Gd","Ex"),
                                           labels = c(1,2,3,4,5) ,ordered = TRUE))

testdata$KitchenQual <- as.numeric(factor(testdata$KitchenQual, 
                                          levels = c("Po","Fa","TA", "Gd","Ex"),
                                          labels = c(1,2,3,4,5) ,ordered = TRUE))


traindata$Functional <- as.numeric(factor(traindata$Functional, 
                                          levels = c("Typ","Min1","Min2", "Mod","Maj1", "Maj2", "Sev","Sal"),
                                          labels = c(1,2,3,4,5,6,7,8) ,ordered = TRUE))

testdata$Functional <- as.numeric(factor(testdata$Functional, 
                                         levels = c("Typ","Min1","Min2", "Mod","Maj1", "Maj2", "Sev","Sal"),
                                         labels = c(1,2,3,4,5,6,7,8) ,ordered = TRUE))

traindata$FireplaceQu <- as.numeric(factor(traindata$FireplaceQu, 
                                           levels = c("NA","Po","Fa","TA", "Gd","Ex"),
                                           labels = c(1,2,3,4,5,6) ,ordered = TRUE))

testdata$FireplaceQu <- as.numeric(factor(testdata$FireplaceQu, 
                                          levels = c("NA","Po","Fa","TA", "Gd","Ex"),
                                          labels = c(1,2,3,4,5,6) ,ordered = TRUE))

traindata$GarageType <- as.numeric(factor(traindata$GarageType, 
                                          levels = c("NA","Detchd","CarPort","BuiltIn", "Basment","Attchd", "2Types"),
                                          labels = c(1,2,3,4,5,6,7) ,ordered = TRUE))

testdata$GarageType <- as.numeric(factor(testdata$GarageType, 
                                         levels = c("NA","Detchd","CarPort","BuiltIn", "Basment","Attchd", "2Types"),
                                         labels = c(1,2,3,4,5,6,7) ,ordered = TRUE))


traindata$GarageFinish <- as.numeric(factor(traindata$GarageFinish, 
                                            levels = c("NA","Unf","RFn","Fin"),
                                            labels = c(1,2,3,4) ,ordered = TRUE))

testdata$GarageFinish <- as.numeric(factor(testdata$GarageFinish, 
                                           levels = c("NA","Unf","RFn","Fin"),
                                           labels = c(1,2,3,4) ,ordered = TRUE))



traindata$GarageQual <- as.numeric(factor(traindata$GarageQual, 
                                          levels = c("NA","Po","Fa","TA", "Gd","Ex"),
                                          labels = c(1,2,3,4,5,6) ,ordered = TRUE))

testdata$GarageQual <- as.numeric(factor(testdata$GarageQual, 
                                         levels = c("NA","Po","Fa","TA", "Gd","Ex"),
                                         labels = c(1,2,3,4,5,6) ,ordered = TRUE))

traindata$GarageCond <- as.numeric(factor(traindata$GarageCond, 
                                          levels = c("NA","Po","Fa","TA", "Gd","Ex"),
                                          labels = c(1,2,3,4,5,6) ,ordered = TRUE))

testdata$GarageCond <- as.numeric(factor(testdata$GarageCond, 
                                         levels = c("NA","Po","Fa","TA", "Gd","Ex"),
                                         labels = c(1,2,3,4,5,6) ,ordered = TRUE))


traindata$PavedDrive <- as.numeric(factor(traindata$PavedDrive, 
                                          levels = c("Y","P","N"),
                                          labels = c(1,2,3) ,ordered = TRUE))

testdata$PavedDrive <- as.numeric(factor(testdata$PavedDrive, 
                                         levels = c("Y","P","N"),
                                         labels = c(1,2,3) ,ordered = TRUE))


traindata$PoolQC <- as.numeric(factor(traindata$PoolQC, 
                                      levels = c("NA","Fa","TA", "Gd","Ex"),
                                      labels = c(1,2,3,4,5) ,ordered = TRUE))

testdata$PoolQC <- as.numeric(factor(testdata$PoolQC, 
                                     levels = c("NA","Fa","TA", "Gd","Ex"),
                                     labels = c(1,2,3,4,5) ,ordered = TRUE))

traindata$Fence <- as.numeric(factor(traindata$Fence, 
                                     levels = c("NA","MnWw","GdWo", "MnPrv","GdPrv"),
                                     labels = c(1,2,3,4,5) ,ordered = TRUE))

testdata$Fence <- as.numeric(factor(testdata$Fence, 
                                    levels = c("NA","MnWw","GdWo", "MnPrv","GdPrv"),
                                    labels = c(1,2,3,4,5) ,ordered = TRUE))


traindata$MiscFeature <- as.numeric(factor(traindata$MiscFeature, 
                                           levels = c("NA","TenC","Shed", "Othr","Gar2", "Elev"),
                                           labels = c(1,2,3,4,5,6) ,ordered = TRUE))

testdata$MiscFeature <- as.numeric(factor(testdata$MiscFeature, 
                                          levels = c("NA","TenC","Shed", "Othr","Gar2", "Elev"),
                                          labels = c(1,2,3,4,5,6) ,ordered = TRUE))



traindata$SaleType <- as.numeric(factor(traindata$SaleType, 
                                        levels = c("Oth","ConLD","ConLI", "ConLw","Con", "COD", "New","VWD","CWD","WD"),
                                        labels = c(1,2,3,4,5,6,7,8,9,10) ,ordered = TRUE))

testdata$SaleType <- as.numeric(factor(testdata$SaleType, 
                                       levels = c("Oth","ConLD","ConLI", "ConLw","Con", "COD", "New","VWD","CWD","WD"),
                                       labels = c(1,2,3,4,5,6,7,8,9,10) ,ordered = TRUE))


traindata$SaleCondition <- as.numeric(factor(traindata$SaleCondition , 
                                             levels = c("Partial","Family","Alloca", "AdjLand","Abnorml", "Normal"),
                                             labels = c(1,2,3,4,5,6) ,ordered = TRUE))

testdata$SaleCondition <- as.numeric(factor(testdata$SaleCondition , 
                                            levels = c("Partial","Family","Alloca", "AdjLand","Abnorml", "Normal"),
                                            labels = c(1,2,3,4,5,6) ,ordered = TRUE))



traindata<-traindata%>%mutate_at(c('YearBuilt'), as.numeric)
testdata<-testdata%>%mutate_at(c('YearBuilt'), as.numeric)

View(traindata)
View(testdata)

scale(Num_traindata, center = TRUE, scale = TRUE)


# Correlation Matrix
cor(Num_traindata)


##################################################################### 
# Data Cleaning
traindata<- na.omit(traindata) # drop NA
testdata<- na.omit(testdata)


colSums(is.na(traindata))
colSums(is.na(testdata))       


#use lm() to run linear regression of Sale-Price on all variables in model data-set
# multiple regression Model
Regression_Model <- lm(formula=SalePrice ~.,  data= traindata)
summary(Regression_Model)


# step wise regression
step_model <- stepAIC(Regression_Model, direction = "both", 
                      trace = FALSE)
summary(step_model)

Model3<- lm(formula=SalePrice ~ MSZoning+LotArea+BldgType+OverallQual+OverallCond+YearBuilt+Exterior1st+Exterior2nd+MasVnrType+
              BsmtExposure+BsmtFinSF1+BsmtFinSF2+BsmtUnfSF+ScreenPorch+KitchenQual+FireplaceQu, data=traindata)

summary(Model3)

Model4<- lm(formula=SalePrice ~ MSZoning+LotArea+BldgType+OverallQual+OverallCond+YearBuilt
            +BsmtFinSF1+BsmtFinSF2+BsmtUnfSF+ScreenPorch+KitchenQual+FireplaceQu, data=traindata)

summary(Model4)

# Plot of Residual vs Fitted Value
plot(Model4)



Pred<- predict(Model4, testdata)
Pred
View(Pred)

# Calculate Root Mean Square Error
# train RMSE
sqrt(mean((traindata$SalePrice - predict(Model4, traindata)) ^ 2))

# Generating CSV file for the result
prediction_test <- exp(predict(Model4, newdata= testdata))
dt.result <- NULL
dt.result$Id <- testdata$Id
dt.result$SalePrice <- prediction_test
dt.result <- as.data.frame(dt.result, row.names = NULL)
write.csv(dt.result,"House_Price_Predict.csv", row.names = FALSE)



