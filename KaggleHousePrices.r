#rm(list=ls())

# install.packages("Amelia")
# install.packages("mice")
# install.packages("dplyr")
# install.packages("e1071")
# install.packages("corrplot")
# install.packages("PCAmixdata")
# install.packages("randomForest")
# install.packages("FactoMineR")
# install.packages("rcompanion")
# install.packages("caret")
# install.packages("caretEnsemble")
# install.packages("mlbench")

library(Amelia)
library(mice)
library(dplyr)
library(e1071)
library(corrplot)
library(PCAmixdata)
library(randomForest)
library(FactoMineR)
library(rcompanion)
library(caret)
library(caretEnsemble)
library(mlbench)

set.seed(123)

#Chemin des fichiers de données
path <- "..."
setwd(path)

#Chemin pour la création du fichier de soumission 
path_soumission <- "..."

train <- read.csv(paste(path,"train.csv",sep=""), sep = ",",stringsAsFactors=FALSE)
test <- read.csv(paste(path,"test.csv",sep=""), sep = ",",stringsAsFactors=FALSE)

train_d <- subset(train, select = -c(Id,SalePrice))
test_d <- subset(test, select = -c(Id))

id_sale_prices <- data.frame(cbind(train$Id,train$SalePrice))
dim(id_sale_prices)
colnames(id_sale_prices) <- c('Id','SalePrice')

df_bind = rbind(train_d,test_d)

dim(df_bind)
colnames(df_bind)

############################################################################
#
#                       DONNEES MANQUANTES
#
#############################################################################
df_bind_by_var <- sapply(df_bind, function(x) sum(is.na(x)))

sort(df_bind_by_var[df_bind_by_var > 0], decreasing = TRUE)

missmap(df_bind)

#Pourcentage de valeur NA
sum(df_bind_by_var)*100/(dim(df_bind)[1]*dim(df_bind)[2])

############################################################################
#
#                       NA VALUE
#
#############################################################################

#---------  PoolQC  --------- 
table(df_bind$PoolQC, exclude = NULL)

#Voir s'il existe des maisons avec une superficie de piscine !=0 et NA en PoolQC : 3
df_bind[is.na(df_bind$PoolQC) & df_bind$PoolArea != 0,c('PoolQC','PoolArea')]

df_bind[,c('PoolQC','PoolArea')] %>%
  group_by(PoolQC) %>%
  summarise(mean = mean(PoolArea), counts = n()) 

#Imputation des trois maisons avec la modalité la plus proche de la moyenne
df_bind[2421,'PoolQC'] = 'Ex'
df_bind[2504,'PoolQC'] = 'Ex'
df_bind[2600,'PoolQC'] = 'Fa'

df_bind$PoolQC[is.na(df_bind$PoolQC)]='None'

#---------  MiscFeature | Alley | Fence --------- 
table(df_bind$MiscFeature, exclude = NULL)
df_bind$MiscFeature[is.na(df_bind$MiscFeature)]='None'

table(df_bind$Alley, exclude = NULL)
df_bind$Alley[is.na(df_bind$Alley)]='None'

table(df_bind$Fence, exclude = NULL)
df_bind$Fence[is.na(df_bind$Fence)]='None'

#---------  FireplaceQu  --------- 
table(df_bind$FireplaceQu, exclude = NULL)

# existe t'il une maison a une cheminée et une valeur manquante pour la qualité de la cheminée : non 
which((df_bind$Fireplaces > 0) & (is.na(df_bind$FireplaceQu)))

df_bind_t <- df_bind[df_bind$Fireplaces > 0,]
ggplot(df_bind_t, aes(x = Fireplaces )) + geom_bar(aes(fill = FireplaceQu)) + geom_text(aes(label = ..count..), stat='count', vjust=-0.5)

df_bind$FireplaceQu[is.na(df_bind$FireplaceQu)]='None'

#--------- GarageYrBlt |	GarageFinish | GarageQual | GarageCond | GarageType | GarageCars | GarageArea -------

#Nombre de maison aytn la même date de construction pour la maison et le garage
length(which((df_bind$GarageYrBlt == df_bind$YearBuilt)))

#   Pour voir la liste des années : attention, il y a une année à 2207 erronnée
table(df_bind$GarageYrBlt, exclude = NULL)

# Modification de l'année 2207 en 2007
id_house_2207 <- which(df_bind$GarageYrBlt == 2207)
df_bind[id_house_2207,'GarageYrBlt'] = 2007

# Liste des maisons pour les infos garages : deux maisons ayant GarageType différent de NA
garage_infos <- df_bind[,c('GarageYrBlt','GarageQual','GarageCond','GarageFinish','GarageType', 'GarageCars','GarageArea')]
garage_infos[is.na(garage_infos$GarageYrBlt),]

#Imputation 'NoGarage' pour les 157 maisons 
df_bind$GarageQual[is.na(df_bind$GarageQual) & is.na(df_bind$GarageType)] <- 'None'
df_bind$GarageFinish[is.na(df_bind$GarageFinish) & is.na(df_bind$GarageType)] <- 'None'
df_bind$GarageCond[is.na(df_bind$GarageCond) & is.na(df_bind$GarageType)] <- 'None'
df_bind$GarageType[is.na(df_bind$GarageType)] <- 'None'

#Imputation de la modalité la plus fréquente pour 2127
table(df_bind$GarageQual, exclude = NULL)
table(df_bind$GarageFinish, exclude = NULL)
table(df_bind$GarageCond, exclude = NULL)
table(df_bind$GarageYrBlt, exclude = NULL)

df_bind[2127,'GarageCond'] <- 'TA'
df_bind[2127,'GarageQual'] <- 'TA'
df_bind[2127,'GarageFinish'] <- 'Unf'
df_bind[2127,'GarageYrBlt'] <- df_bind[2127,'YearBuilt'] 

# Imputation de la modalité la plus fréquente pour 2577
#Iputation de l'année de construction de la maison pour l'année de construction du garage
df_bind[2577,'GarageYrBlt'] <- df_bind[2577,'YearBuilt'] 
df_bind[2577,'GarageQual'] <- 'TA'
df_bind[2577,'GarageCond'] <- 'TA'
df_bind[2577,'GarageFinish'] <- 'Unf'

df_bind$GarageQual[is.na(df_bind$GarageQual) & is.na(df_bind$GarageType)] <- 'None'

df_bind$GarageYrBlt[is.na(df_bind$GarageYrBlt)] <- 'None'

df_bind$GarageCars[is.na(df_bind$GarageCars)] = 0
df_bind$GarageArea[is.na(df_bind$GarageArea)] = 0

#--------- BsmtCond | BsmtExposure | BsmtQual | BsmtFinType2 | BsmtFinType1--------------------
#--------- BsmtFinSF1 | BsmtFinSF2 | BsmtUnfSF  | TotalBsmtSF -------------------------------
#---------- BsmtFullBath | BsmtHalfBath --------------------------------

Bsmt_infos <- df_bind[,c('BsmtCond','BsmtExposure','BsmtQual','BsmtFinType2','BsmtFinType1','BsmtFinSF1','BsmtFinSF2','BsmtUnfSF','TotalBsmtSF','BsmtFullBath','BsmtHalfBath')]

Bsmt_infos_ou <- Bsmt_infos[(is.na(Bsmt_infos$BsmtCond) | is.na(Bsmt_infos$BsmtExposure) 
                             | is.na(Bsmt_infos$BsmtQual) | is.na(Bsmt_infos$BsmtFinType2) 
                             | is.na(Bsmt_infos$BsmtFinType1)) & (Bsmt_infos$BsmtFinSF1 == 0
                                                                  | Bsmt_infos$BsmtFinSF2 == 0 | Bsmt_infos$BsmtUnfSF == 0 
                                                                  | Bsmt_infos$TotalBsmtSF == 0),]

dim(Bsmt_infos_ou) #87 maisons ayant des NA pour le sous-sol dans au moins une variable qualitative et des 0 pour les variables quantitatives

Bsmt_infos_et <- Bsmt_infos[is.na(Bsmt_infos$BsmtCond) & is.na(Bsmt_infos$BsmtExposure) 
                            & is.na(Bsmt_infos$BsmtQual) & is.na(Bsmt_infos$BsmtFinType2) 
                            & is.na(Bsmt_infos$BsmtFinType1) & Bsmt_infos$BsmtFinSF1 == 0
                            & Bsmt_infos$BsmtFinSF2 == 0 & Bsmt_infos$BsmtUnfSF == 0 
                            & Bsmt_infos$TotalBsmtSF == 0,]
dim(Bsmt_infos_et) # 79 maisons ayant des NA dans toutes les variables qualitatif et 0 pour les variables quantitatives

#Imputation de 'NoBasement' pour les maisons ayant des NA et O pour les variables sous-sol
# Suppression d'un nom de colonne au fur et à mesure de l'imputation

df_bind$BsmtFinSF1[is.na(df_bind$BsmtCond) & is.na(df_bind$BsmtExposure) 
                   & is.na(df_bind$BsmtQual) & is.na(df_bind$BsmtFinType2) 
                   & is.na(df_bind$BsmtFinType1) & (df_bind$BsmtFinSF1 == 0 | is.na(df_bind$BsmtFinSF1))
                   & (df_bind$BsmtFinSF2 == 0 | is.na(df_bind$BsmtFinSF2))
                   & (df_bind$BsmtUnfSF == 0 | is.na(df_bind$BsmtUnfSF))
                   & (df_bind$TotalBsmtSF == 0 | is.na(df_bind$TotalBsmtSF))
                   & (df_bind$BsmtFullBath == 0 | is.na(df_bind$BsmtFullBath))
                   & (df_bind$TotalBsmtSF == 0 | is.na(df_bind$TotalBsmtSF))] <- 0

df_bind$BsmtFinSF2[is.na(df_bind$BsmtCond) & is.na(df_bind$BsmtExposure) 
                   & is.na(df_bind$BsmtQual) & is.na(df_bind$BsmtFinType2) 
                   & is.na(df_bind$BsmtFinType1) & (df_bind$BsmtFinSF1 == 0 | is.na(df_bind$BsmtFinSF1))
                   & (df_bind$BsmtFinSF2 == 0 | is.na(df_bind$BsmtFinSF2))
                   & (df_bind$BsmtUnfSF == 0 | is.na(df_bind$BsmtUnfSF))
                   & (df_bind$TotalBsmtSF == 0 | is.na(df_bind$TotalBsmtSF))
                   & (df_bind$BsmtFullBath == 0 | is.na(df_bind$BsmtFullBath))
                   & (df_bind$TotalBsmtSF == 0 | is.na(df_bind$TotalBsmtSF))] <- 0

df_bind$BsmtUnfSF[is.na(df_bind$BsmtCond) & is.na(df_bind$BsmtExposure) 
                  & is.na(df_bind$BsmtQual) & is.na(df_bind$BsmtFinType2) 
                  & is.na(df_bind$BsmtFinType1) & (df_bind$BsmtFinSF1 == 0 | is.na(df_bind$BsmtFinSF1))
                  & (df_bind$BsmtFinSF2 == 0 | is.na(df_bind$BsmtFinSF2))
                  & (df_bind$BsmtUnfSF == 0 | is.na(df_bind$BsmtUnfSF))
                  & (df_bind$TotalBsmtSF == 0 | is.na(df_bind$TotalBsmtSF))] <- 0

df_bind$TotalBsmtSF[is.na(df_bind$BsmtCond) & is.na(df_bind$BsmtExposure) 
                    & is.na(df_bind$BsmtQual) & is.na(df_bind$BsmtFinType2) 
                    & is.na(df_bind$BsmtFinType1) & (df_bind$BsmtFinSF1 == 0 | is.na(df_bind$BsmtFinSF1))
                    & (df_bind$BsmtFinSF2 == 0 | is.na(df_bind$BsmtFinSF2))
                    & (df_bind$BsmtUnfSF == 0 | is.na(df_bind$BsmtUnfSF))
                    & (df_bind$TotalBsmtSF == 0 | is.na(df_bind$TotalBsmtSF))
                    & (df_bind$BsmtFullBath == 0 | is.na(df_bind$BsmtFullBath))
                    & (df_bind$TotalBsmtSF == 0 | is.na(df_bind$TotalBsmtSF))] <- 0

df_bind$BsmtFullBath[is.na(df_bind$BsmtCond) & is.na(df_bind$BsmtExposure) 
                     & is.na(df_bind$BsmtQual) & is.na(df_bind$BsmtFinType2) 
                     & is.na(df_bind$BsmtFinType1) & (df_bind$BsmtFinSF1 == 0 | is.na(df_bind$BsmtFinSF1))
                     & (df_bind$BsmtFinSF2 == 0 | is.na(df_bind$BsmtFinSF2))
                     & (df_bind$BsmtUnfSF == 0 | is.na(df_bind$BsmtUnfSF))
                     & (df_bind$TotalBsmtSF == 0 | is.na(df_bind$TotalBsmtSF))
                     & (df_bind$BsmtFullBath == 0 | is.na(df_bind$BsmtFullBath))
                     & (df_bind$BsmtHalfBath == 0 | is.na(df_bind$BsmtHalfBath))] <- 0

df_bind$BsmtHalfBath[is.na(df_bind$BsmtCond) & is.na(df_bind$BsmtExposure) 
                     & is.na(df_bind$BsmtQual) & is.na(df_bind$BsmtFinType2) 
                     & is.na(df_bind$BsmtFinType1) & (df_bind$BsmtFinSF1 == 0 | is.na(df_bind$BsmtFinSF1))
                     & (df_bind$BsmtFinSF2 == 0 | is.na(df_bind$BsmtFinSF2))
                     & (df_bind$BsmtUnfSF == 0 | is.na(df_bind$BsmtUnfSF))
                     & (df_bind$TotalBsmtSF == 0 | is.na(df_bind$TotalBsmtSF))
                     & (df_bind$BsmtFullBath == 0 | is.na(df_bind$BsmtFullBath))
                     & (df_bind$BsmtHalfBath == 0 | is.na(df_bind$BsmtHalfBath))] <- 0

df_bind$TotalBsmtSF[is.na(df_bind$BsmtCond) & is.na(df_bind$BsmtExposure) 
                    & is.na(df_bind$BsmtQual) & is.na(df_bind$BsmtFinType2) 
                    & is.na(df_bind$BsmtFinType1) & (df_bind$BsmtFinSF1 == 0 | is.na(df_bind$BsmtFinSF1))
                    & (df_bind$BsmtFinSF2 == 0 | is.na(df_bind$BsmtFinSF2))
                    & (df_bind$BsmtUnfSF == 0 | is.na(df_bind$BsmtUnfSF))
                    & (df_bind$TotalBsmtSF == 0 | is.na(df_bind$TotalBsmtSF))
                    & (df_bind$BsmtFullBath == 0 | is.na(df_bind$BsmtFullBath))
                    & (df_bind$TotalBsmtSF == 0 | is.na(df_bind$TotalBsmtSF))] <- 0

df_bind$BsmtCond[is.na(df_bind$BsmtCond) & is.na(df_bind$BsmtExposure) 
                 & is.na(df_bind$BsmtQual) & is.na(df_bind$BsmtFinType2) 
                 & is.na(df_bind$BsmtFinType1) & df_bind$BsmtFinSF1 == 0
                 & df_bind$BsmtFinSF2 == 0 & df_bind$BsmtUnfSF == 0 
                 & df_bind$TotalBsmtSF == 0] <- 'None'

df_bind$BsmtQual[is.na(df_bind$BsmtExposure) & is.na(df_bind$BsmtQual) 
                 & is.na(df_bind$BsmtFinType2) & is.na(df_bind$BsmtFinType1) 
                 & df_bind$BsmtFinSF1 == 0 & df_bind$BsmtFinSF2 == 0 
                 & df_bind$BsmtUnfSF == 0 & df_bind$TotalBsmtSF == 0] <- 'None'

df_bind$BsmtFinType2[is.na(df_bind$BsmtExposure) & is.na(df_bind$BsmtFinType2) 
                     & is.na(df_bind$BsmtFinType1) & df_bind$BsmtFinSF1 == 0
                     & df_bind$BsmtFinSF2 == 0 & df_bind$BsmtUnfSF == 0 
                     & df_bind$TotalBsmtSF == 0] <- 'None'

df_bind$BsmtFinType1[is.na(df_bind$BsmtExposure) & is.na(df_bind$BsmtFinType1) 
                     & df_bind$BsmtFinSF1 == 0 & df_bind$BsmtFinSF2 == 0 
                     & df_bind$BsmtUnfSF == 0 & df_bind$TotalBsmtSF == 0] <- 'None'

# Il reste 3 maisons avec BsmtExposure à NA et avec des valeurs différentes de 0 pour les variables quantitatives relatives au sous-sol
# d'où imputation avec 'No', pas d'exposition
df_bind$BsmtExposure[is.na(df_bind$BsmtExposure)] <- 'No'

#Il y a une maison avec une valeur NA à BsmtFinType2
which(is.na(df_bind$BsmtFinType2)) # Id = 333

# Il y a maintenant 5 maisons ayant des NA pour les variables qualitatives BsmtCond BsmtQual  BsmtFinType2

df_bind$BsmtQual[is.na(df_bind$BsmtQual)] <- 'TA'
df_bind$BsmtCond[is.na(df_bind$BsmtCond)] <- 'TA'
df_bind$BsmtFinType2[is.na(df_bind$BsmtFinType2)] <- 'Rec'

#---------  Electrical --------- 
table(df_bind$Electrical, exclude = NULL)

#Imputation avec la modalité la plus fréquente
df_bind$Electrical[is.na(df_bind$Electrical)]='SBrkr'

#--------------------- MasVnrType et MasVnrArea  ------------
MasVnr_infos <- df_bind[,c('MasVnrType','MasVnrArea')]

#Il y a 24 maisons avec des valeurs à NA pour MasVnrType et 23 pour MasVnrArea
MasVnr_infos[is.na(MasVnr_infos$MasVnrType) | is.na(MasVnr_infos$MasVnrArea),]

#Imputation de 'None' et O pour les variables 
df_bind$MasVnrType[is.na(df_bind$MasVnrType) & is.na(df_bind$MasVnrArea)] <- 'None'
df_bind$MasVnrArea[is.na(df_bind$MasVnrArea)] <- 0

# Il y a une maison avec des valeurs différentes de NA pour MasVnrArea : 2611
which(is.na(df_bind$MasVnrType) & df_bind$MasVnrArea !=0)

na.omit(df_bind[,c('MasVnrType','MasVnrArea')]) %>%
  group_by(na.omit(MasVnrType)) %>%
  summarise(MedianArea = median(MasVnrArea,na.rm = TRUE), counts = n()) %>%
  arrange(MedianArea)

#Imputation avec BrkFace
df_bind[2611,'MasVnrType'] <- 'BrkFace'

#-------------------  MSZoning  ------------------------
# Pour voir les informations de toutes les variables : 4 maisons
df_bind[is.na(df_bind$MSZoning),]

#Croisement entre les variables MSSubClass et MSZoning
table(df_bind$MSSubClass, df_bind$MSZoning)

# Les modalités des maisons avec ceux de la dernière maison : cela donne pour  MSZoning : RL
df_bind[df_bind$Neighborhood == 'Mitchel' & df_bind$Condition1=='Artery' & df_bind$Street=='Pave' & df_bind$MSSubClass == 20,]$MSZoning

# Imputation avec RL pour la maison 2905
df_bind[2905,'MSZoning']="RL"

# Utilisation du tableau des deux variables croisées en prenant la valeur la plus fréquente: 
# Id=1916 (MSSubClass=30)
# Id=2217 (MSSubClass=20)           
# Id=2251 (MSSubClass=70)  
df_bind[1916,'MSZoning']="RM"
df_bind[2217,'MSZoning']="RL"
df_bind[2251,'MSZoning']="RM"

#---------- Utilities -----------
table(df_bind$Utilities, exclude = NULL)
which(df_bind$Utilities == 'NoSeWa')

# La maison possédant la modalité 'NoSeWa' appartient au jeu d'd'entrainement
# Si une imputation est faite avec la valeur la plus frequente : AllPub, 
# alors la variable n'a pas d'utilité elle peut donc être supprimée.
df_bind <- subset(df_bind, select = -c(Utilities))

#---------- Functional -----------
table(df_bind$Functional, exclude = NULL)

#Imputation avec la plus frequente
df_bind$Functional[is.na(df_bind$Functional)]="Typ"

#-----  KitchenQual et KitchenAbvGr  --------------------------
table(df_bind$KitchenQual, exclude = NULL)
ggplot(df_bind, aes(x = KitchenAbvGr)) + geom_bar(aes(fill = KitchenQual)) + geom_text(aes(label = ..count..), stat='count', vjust=-0.5)

which(is.na(df_bind$KitchenQual))

test[test$Id == 1556,]

#Imputation avec la plus fréquente
df_bind$KitchenQual[is.na(df_bind$KitchenQual)]="TA"

#---------- SaleType ------------------
#Tableau croisé avec les conditions de vente
table(df_bind$SaleCondition, df_bind$SaleType)

#Imputation avec la valeur la plus fréquente
df_bind$SaleType[is.na(df_bind$SaleType)] = 'WD'

#------------- Exterior1st |  Exterior2nd  ----------
table(train$Exterior1st, exclude = NULL)
table(df_bind$Exterior2nd, exclude = NULL)

df_bind$Exterior1st= as.character(df_bind$Exterior1st)
df_bind$Exterior1st[is.na(df_bind$Exterior1st)]="Other"
df_bind$Exterior1st = as.factor(df_bind$Exterior1st)

df_bind$Exterior2nd= as.character(df_bind$Exterior2nd)
df_bind$Exterior2nd[is.na(df_bind$Exterior2nd)]="Other"
df_bind$Exterior2nd = as.factor(df_bind$Exterior2nd)

############################################################################
#
#                       IMPUTATION
#
#############################################################################

df_bind_imputed <- mice(df_bind, m=5, maxit = 50, method = 'cart', seed = 500)

df_bind_imputed$imp$LotFrontage$`5` <- rowMeans(df_bind_imputed$imp$LotFrontage, na.rm=TRUE)

df_bind <- complete(df_bind_imputed,5)

#Une sauvegarde pour ne pas refaire l'imputation
db_bind_backup <- df_bind
dim(db_bind_backup)
dim(df_bind)

df_bind <- db_bind_backup

############################################################################
#
#                       ANALYSE UNIDIMENTIONNELLE
#
#############################################################################

#------------ variable saleprice
hist(train$SalePrice, main=paste("Prix de vente - skewness : ",round(skewness(train$SalePrice),digits=3),sep = ''), xlab = "SalePrice")

hist(log(train$SalePrice), main=paste("Log Prix de vente - skewness : ",round(skewness(log(train$SalePrice)),digits=3),sep = ''), xlab = "SalePrice")

## -----------  les colonnes variables quantitatives
df_col_quant <- c("LotFrontage","LotArea","YearBuilt","YearRemodAdd","MasVnrArea","BsmtFinSF1"
                    ,"BsmtFinSF2","BsmtUnfSF","TotalBsmtSF","X1stFlrSF","X2ndFlrSF","LowQualFinSF","GrLivArea","BsmtFullBath"
                    ,"BsmtHalfBath","FullBath","HalfBath","BedroomAbvGr","KitchenAbvGr","TotRmsAbvGrd","Fireplaces"
                    ,"GarageCars","GarageArea","WoodDeckSF","OpenPorchSF","EnclosedPorch","X3SsnPorch","ScreenPorch"
                    ,"PoolArea","MiscVal","MoSold","YrSold")
length(df_col_quant)

#-------------  les colonnes variables qualitatives
df_col_qual <- c("MSSubClass","MSZoning","Street","Alley","LotShape","LandContour","LotConfig","LandSlope","Neighborhood"
                   ,"Condition1","Condition2","BldgType","HouseStyle","OverallQual","OverallCond","RoofStyle"
                   ,"RoofMatl","Exterior1st","Exterior2nd","MasVnrType","ExterQual","ExterCond","Foundation","BsmtQual"
                   ,"BsmtCond","BsmtExposure","BsmtFinType1","BsmtFinType2","Heating","HeatingQC","CentralAir","Electrical"
                   ,"KitchenQual","Functional","FireplaceQu","GarageType","GarageFinish","GarageQual","GarageCond","GarageYrBlt"
                   ,"PavedDrive","PoolQC","Fence","MiscFeature","SaleType","SaleCondition")
length(df_col_qual)


df_bind_quant <- df_bind[,df_col_quant]
dim(df_bind_quant)

df_bind_qual <- df_bind[,df_col_qual]
dim(df_bind_qual)


#------------------------------------ DONNEES DES VARIABLES QUALITATIVES : TRAIN + TEST
png(height=1200, width=1200, file="House_Prices_qual_all_1.1.png")
par(mfrow=c(5,5))  
for (i in 1:25) {
  pie(table(df_bind_qual[,df_col_qual[i]]), main=df_col_qual[i])
}
dev.off()

png(height=1200, width=1200, file="House_Prices_qual_all_1.2.png")
par(mfrow=c(5,5))  
for (i in 26:length(df_col_qual)) {
  pie(table(df_bind_qual[,df_col_qual[i]]), main=df_col_qual[i])
}
dev.off()

#------------------------------------ DONNEES DES VARIABLES quantitative : TRAIN + TEST

png(height=1200, width=1200, file="House_Prices_quant_all.png")
par(mfrow=c(8,4)) 
for (i in 1:length(df_col_quant)) {
  hist(df_bind_quant[,i], col="lavender", main=df_col_quant[i])
}
dev.off()

#------------------------------------ DONNEES DES VARIABLES quantitative : TRAIN 

df_bind_quant_plot <- df_bind_quant[1:1460,]

png(height=1200, width=1200, file="House_Prices_plot_quant_all.png")
par(mfrow=c(8,4))  
for (i in 1:length(df_col_quant)) {
  plot(df_bind_quant_plot[,i],id_sale_prices$SalePrice,main=df_col_quant[i])
  abline(lm(id_sale_prices$SalePrice ~ df_bind_quant_plot[,i]), col="blue", lwd=3, lty=2)
}
dev.off()

df_col <- c("LotFrontage","LotArea","YearBuilt","YearRemodAdd","MasVnrArea","BsmtFinSF1"
                  ,"BsmtFinSF2","BsmtUnfSF","TotalBsmtSF","X1stFlrSF","X2ndFlrSF","LowQualFinSF","GrLivArea","BsmtFullBath"
                  ,"BsmtHalfBath","FullBath","HalfBath","BedroomAbvGr","KitchenAbvGr","TotRmsAbvGrd","Fireplaces"
                  ,"GarageCars","GarageArea","WoodDeckSF","OpenPorchSF","EnclosedPorch","X3SsnPorch","ScreenPorch"
                  ,"PoolArea","MiscVal","MoSold","YrSold"
                  ,"MSSubClass","MSZoning","Street","Alley","LotShape","LandContour","LotConfig","LandSlope","Neighborhood"
                  ,"Condition1","Condition2","BldgType","HouseStyle","OverallQual","OverallCond","RoofStyle"
                  ,"RoofMatl","Exterior1st","Exterior2nd","MasVnrType","ExterQual","ExterCond","Foundation","BsmtQual"
                  ,"BsmtCond","BsmtExposure","BsmtFinType1","BsmtFinType2","Heating","HeatingQC","CentralAir","Electrical"
                  ,"KitchenQual","Functional","FireplaceQu","GarageType","GarageFinish","GarageQual","GarageCond","GarageYrBlt"
                  ,"PavedDrive","PoolQC","Fence","MiscFeature","SaleType","SaleCondition")

length(df_col)

############################################################################
#
#                       ANALYSE BIDIMENTIONNELLE
#
#############################################################################

#Plot de corrélation

#Suppression des 2 maisons ayant des valeurs de GrLivArea > 4000 et prix bas : Id=524,1299
#---- suppression dans le jeu de données train des données extremes (4 valeurs)
# ---- le plot des variables GrLivArea / SalePrice
plot(df_bind[1:1460,'GrLivArea'],id_sale_prices$SalePrice, xlab='GrLivArea', ylab='Prix de vente', main='GrLivArea/SalePrice')
abline(v=4000, col="blue")

#------------ que les variable quantitatives  -------------------------
df_bind_quant_bis <- df_bind[,df_col_quant]
dim(df_bind_quant_bis)

#Plot de corrélation
df_bind_quant_price = cbind(id_sale_prices$SalePrice,df_bind_quant_bis[1:dim(id_sale_prices)[1],])
dim(df_bind_quant_price)
#Renommage de la variable id_sale_prices$SalePrice en SalePrice
colnames(df_bind_quant_price)[which(names(df_bind_quant_price) == "id_sale_prices$SalePrice")] <- "SalePrice"

df_bind_quant_price_colname <- colnames(df_bind_quant_price)
new_col_name = character()

for(i in 1:length(df_bind_quant_price_colname)){
  name <- strsplit(df_bind_quant_price_colname[i],split = ".", fixed = TRUE)
  names <- unlist(name)
  new_col_name <- c(new_col_name,names[1])
}  
colnames(df_bind_quant_price) <- new_col_name

corr <- cor(df_bind_quant_price)

png(height=1200, width=1200, file="House_Prices_correlation.png")

corrplot(corr, type = 'upper', method='color', addCoef.col = 'black', tl.cex = .7,cl.cex = .7, number.cex=.7)
dev.off()

############################################################################
#
#                       ANALYSE MULTIDIMENTIONNELLE
#
#############################################################################

#----------------  ACP -------------------
df_bind_pca = PCA(df_bind_quant,graph=T, scale.unit=TRUE, ncp=dim(df_bind_quant)[2])
plot.PCA(df_bind_pca, axes=c(1, 2), choix="ind")

df_bind_coord_acp <- df_bind_pca$ind$coord
dim(df_bind_coord_acp)

plot(cumsum(df_bind_pca$eig$`percentage of variance`))

colnames(df_bind_coord_acp) <- df_col_quant

#----------------  Analyse factorielle mixte -------------------

library(PCAmixdata)

#?PCAmix
df_bind_qual[] <- lapply(df_bind_qual, factor)

#lancement de la procédure
df_bind_pcamix <- PCAmix(df_bind_quant,df_bind_qual,ndim=dim(df_bind_quant)[2]+dim(df_bind_qual)[2],graph=T, rename.level=TRUE)

#---- les coordonnees des individus
df_bind_coord_mixte <- df_bind_pcamix$ind$coord

colnames(df_bind_coord_mixte) <- c(df_col_quant, df_col_qual)
colnames(df_bind_coord_mixte)
#--------------------------- REGROUPEMENT DES MODALITES ET CODAGES------------------------

#Les modalités ayant un très faible nombre de maison sont regroupés puis un codage est effectué : 0 et 1

#----------------  Ajout d'une variable représentant le total de la superficie du sous-sol, du preùier et deuxième étage ----
df_bind$TotalSF = df_bind[,'TotalBsmtSF'] + df_bind[,'X1stFlrSF'] + df_bind[,'X2ndFlrSF']

hist(df_bind$TotalSF)
df_bind$TotalSF <- (df_bind$TotalSF - mean(df_bind$TotalSF))/var(df_bind$TotalSF)

#------ variable : Alley  ------------------------
table(df_bind$Alley, exclude = NULL)
#df_bind$Alley <- (df_bind$Alley == 'None') * 1

alleys <- c('None','Grvl','Pave')

df_bind$Alley <- as.numeric(factor(df_bind$Alley , levels=alleys))
table(df_bind$Alley, exclude = NULL)

#------ variable : Fence  ------------------------
table(df_bind$Fence)

fences <- c('None','GdPrv','GdWo','MnPrv','MnWw')

df_bind$Fence <- as.numeric(factor(df_bind$Fence , levels=fences))

#------ variable : MiscFeature  ------------------------
table(df_bind$MiscFeature)

miscFeatures <- c('None','Gar2','Othr','Shed','TenC')

df_bind$MiscFeature <- as.numeric(factor(df_bind$MiscFeature , levels=miscFeatures))

#------ variable : BldgType  ------------------------
table(df_bind$BldgType)

bldgTypes <- c('1Fam','2fmCon','Duplex','Twnhs','TwnhsE')
df_bind$BldgType <- as.numeric(factor(df_bind$BldgType , levels=bldgTypes))

#------ variable : Condition1  ------------------------
conditions <- c('Artery','Feedr','Norm','PosA','PosN','RRAe','RRAn','RRNe','RRNn')
df_bind$Condition1 <- as.numeric(factor(df_bind$Condition1 , levels=conditions))
table(df_bind$Condition1, exclude = NULL)

#------ variable : Condition2  ------------------------
table(df_bind$Condition2)

df_bind$Condition2 <- as.numeric(factor(df_bind$Condition2 , levels=conditions))
table(df_bind$Condition2, exclude = NULL)

#------ variable : Electrical  ------------------------
table(df_bind$Electrical)

electricals <- c('FuseA','FuseF','FuseP','Mix','SBrkr')
df_bind$Electrical <- as.numeric(factor(df_bind$Electrical , levels=electricals))

#------ variable : Heating ------------------------
table(df_bind$Heating)

heatings <- c('Floor','GasA','GasW','Grav','OthW','Wall')
df_bind$Heating <- as.numeric(factor(df_bind$Heating , levels=heatings))

#------ variable : LandContour ------------------------
table(df_bind$LandContour)

landContours <- c('Bnk','HLS','Low','Lvl')
df_bind$LandContour <- as.numeric(factor(df_bind$LandContour , levels=landContours))

#------ variable : LandSlope ------------------------
table(df_bind$LandSlope)

landSlopes <- c('Gtl','Mod','Sev')
df_bind$LandSlope <- as.numeric(factor(df_bind$LandSlope , levels=landSlopes))

#------ variable : LotConfig ------------------------
table(df_bind$LotConfig)
lotConfigs <- c('Corner','CulDSac','FR2','FR3','Inside')
df_bind$LotConfig <- as.numeric(factor(df_bind$LotConfig , levels=lotConfigs))

#------ variable : LotShape ------------------------
table(df_bind$LotShape)
lotShapes <- c('IR1','IR2','IR3','Reg')
df_bind$LotShape <- as.numeric(factor(df_bind$LotShape , levels=lotShapes))

#------ variable : RoofMatl ------------------------
table(df_bind$RoofMatl)

roofMatls <- c('ClyTile','CompShg','Membran','Metal','Roll','Tar&Grv','WdShake','WdShngl')
df_bind$RoofMatl <- as.numeric(factor(df_bind$RoofMatl , levels=roofMatls))

#------ variable : SaleType ------------------------
table(df_bind$SaleType)

saleTypes <- c('COD','Con','ConLD','ConLI','ConLw','CWD','New','Oth','WD')
df_bind$SaleType <- as.numeric(factor(df_bind$SaleType , levels=saleTypes))

#------ variable : SaleCondition ------------------------
table(df_bind$SaleCondition)

saleConditions <- c('Abnorml','AdjLand','Alloca','Family','Normal','Partial')
df_bind$SaleCondition <- as.numeric(factor(df_bind$SaleCondition , levels=saleConditions))

#------ variable : PavedDrive ------------------------
table(df_bind$PavedDrive)

pavedDrives <- c('N','P','Y')
df_bind$PavedDrive <- as.numeric(factor(df_bind$PavedDrive , levels=pavedDrives))

#------ Normalisation des variables dites continues  -----
hist(df_bind$GrLivArea)
df_bind$GrLivArea <- (df_bind$GrLivArea - mean(df_bind$GrLivArea))/var(df_bind$GrLivArea)

hist(df_bind$GarageArea)
df_bind$GarageArea <- (df_bind$GarageArea - mean(df_bind$GarageArea))/var(df_bind$GarageArea)

hist(df_bind$LotFrontage)
df_bind$LotFrontage <- (df_bind$LotFrontage - mean(df_bind$LotFrontage))/var(df_bind$LotFrontage)

hist(df_bind$LotArea)
df_bind$LotArea <- (df_bind$LotArea - mean(df_bind$LotArea))/var(df_bind$LotArea)

hist(df_bind$TotalBsmtSF)
df_bind$TotalBsmtSF <- (df_bind$TotalBsmtSF - mean(df_bind$TotalBsmtSF))/var(df_bind$TotalBsmtSF)

hist(df_bind$X1stFlrSF)
df_bind$X1stFlrSF <- (df_bind$X1stFlrSF - mean(df_bind$X1stFlrSF))/var(df_bind$X1stFlrSF)

hist(df_bind$X2ndFlrSF)
df_bind$X2ndFlrSF <- (df_bind$X2ndFlrSF - mean(df_bind$X2ndFlrSF))/var(df_bind$X2ndFlrSF)

hist(df_bind$WoodDeckSF)
df_bind$WoodDeckSF <- (df_bind$WoodDeckSF - mean(df_bind$WoodDeckSF))/var(df_bind$WoodDeckSF)

hist(df_bind$MasVnrArea)
df_bind$MasVnrArea <- (df_bind$MasVnrArea - mean(df_bind$MasVnrArea))/var(df_bind$MasVnrArea)

hist(df_bind$PoolArea)
df_bind$PoolArea <- (df_bind$PoolArea - mean(df_bind$PoolArea))/var(df_bind$PoolArea)

hist(df_bind$BsmtFinSF1)
df_bind$BsmtFinSF1 <- (df_bind$BsmtFinSF1 - mean(df_bind$BsmtFinSF1))/var(df_bind$BsmtFinSF1)

hist(df_bind$BsmtFinSF2)
df_bind$BsmtFinSF2 <- (df_bind$BsmtFinSF2 - mean(df_bind$BsmtFinSF2))/var(df_bind$BsmtFinSF2)

hist(df_bind$BsmtUnfSF)
df_bind$BsmtUnfSF <- (df_bind$BsmtUnfSF - mean(df_bind$BsmtUnfSF))/var(df_bind$BsmtUnfSF)

hist(df_bind$LowQualFinSF)
df_bind$LowQualFinSF <- (df_bind$LowQualFinSF - mean(df_bind$LowQualFinSF))/var(df_bind$LowQualFinSF)

hist(df_bind$EnclosedPorch)
df_bind$EnclosedPorch <- (df_bind$EnclosedPorch - mean(df_bind$EnclosedPorch))/var(df_bind$EnclosedPorch)

hist(df_bind$X3SsnPorch)
df_bind$X3SsnPorch <- (df_bind$X3SsnPorch - mean(df_bind$X3SsnPorch))/var(df_bind$X3SsnPorch)

hist(df_bind$ScreenPorch)
df_bind$ScreenPorch <- (df_bind$ScreenPorch - mean(df_bind$ScreenPorch))/var(df_bind$ScreenPorch)

hist(df_bind$OpenPorchSF)
df_bind$OpenPorchSF <- (df_bind$OpenPorchSF - mean(df_bind$OpenPorchSF))/var(df_bind$OpenPorchSF)

#------ Street  ------
table(df_bind$Street)

streets <- c('Grvl','Pave')

df_bind$Street <- as.numeric(factor(df_bind$Street , levels=streets))

#------ CentralAir  ------
table(df_bind$CentralAir)

centralAirs <- c('N','Y')

df_bind$CentralAir <- as.numeric(factor(df_bind$CentralAir , levels=centralAirs))

# ------ Les variables 'qualité'  et 'condition'
qualities <- c('None','Po','Fa','TA','Gd','Ex')

df_bind$PoolQC <- as.numeric(factor(df_bind$PoolQC , levels=qualities))
table(df_bind$PoolQC, exclude = NULL)

df_bind$BsmtCond <- as.numeric(factor(df_bind$BsmtCond , levels=qualities))
table(df_bind$BsmtCond, exclude = NULL)

df_bind$ExterCond <- as.numeric(factor(df_bind$ExterCond , levels=qualities))
table(df_bind$ExterCond, exclude = NULL)

df_bind$GarageCond <- as.numeric(factor(df_bind$GarageCond , levels=qualities))
table(df_bind$GarageCond, exclude = NULL)

df_bind$GarageQual <- as.numeric(factor(df_bind$GarageQual , levels=qualities))
table(df_bind$GarageQual, exclude = NULL)

df_bind$FireplaceQu <- as.numeric(factor(df_bind$FireplaceQu , levels=qualities))
table(df_bind$FireplaceQu, exclude = NULL)

df_bind$ExterQual <- as.numeric(factor(df_bind$ExterQual , levels=qualities))
table(df_bind$ExterQual, exclude = NULL)

df_bind$BsmtQual <- as.numeric(factor(df_bind$BsmtQual , levels=qualities))
table(df_bind$BsmtQual, exclude = NULL)

df_bind$HeatingQC <- as.numeric(factor(df_bind$HeatingQC , levels=qualities))
table(df_bind$HeatingQC, exclude = NULL)

df_bind$KitchenQual <- as.numeric(factor(df_bind$KitchenQual , levels=qualities))
table(df_bind$KitchenQual, exclude = NULL)

#----------------  BsmtExposure -----------------------
bsmts <- c('None','No','Mn','Av','Gd')

df_bind$BsmtExposure <- as.numeric(factor(df_bind$BsmtExposure , levels=bsmts))
table(df_bind$BsmtExposure, exclude = NULL)

#-------------   BsmtFinType1 BsmtFinType2 ----------------------------
bsmts_fins <- c('None','Unf','LwQ','Rec','BLQ','ALQ','GLQ')

df_bind$BsmtFinType1 <- as.numeric(factor(df_bind$BsmtFinType1 , levels=bsmts_fins))
table(df_bind$BsmtFinType1, exclude = NULL)

df_bind$BsmtFinType2 <- as.numeric(factor(df_bind$BsmtFinType2 , levels=bsmts_fins))
table(df_bind$BsmtFinType2, exclude = NULL)

#-------------------  GarageFinish  GarageType ---------------
garages <- c('None','Unf','RFn','Fin')

df_bind$GarageFinish <- as.numeric(factor(df_bind$GarageFinish , levels=garages))
table(df_bind$GarageFinish)

garages_types <- c('None','2Types','Attchd','Basment','BuiltIn','CarPort','Detchd')

df_bind$GarageType <- as.numeric(factor(df_bind$GarageType , levels=garages_types))
table(df_bind$GarageType)

#------------- YearRemodAdd de 1950 à 2010 --------------------------
plot(table(df_bind$YearRemodAdd), main='Années de remodelage de la maison')
df_bind$YearRemodAdd <- as.numeric(2011 - df_bind$YearRemodAdd)

#------- variable GarageYrBlt ------------------------
df_bind$GarageYrBlt_1 <- NULL

table(df_bind$GarageYrBlt_1, exclude = NULL)
plot(table(df_bind$GarageYrBlt), main='Années de construction des garages')

df_bind$GarageYrBlt_1[df_bind$GarageYrBlt %in% c(1895:2010)] <- as.numeric(2011 - as.numeric(df_bind$GarageYrBlt[df_bind$GarageYrBlt %in% c(1895:2010)]))
df_bind$GarageYrBlt_1[df_bind$GarageYrBlt == "None"] <- 117

#Suppression de la variable GarageYrBlt
df_bind$GarageYrBlt <- NULL

#Renommage de la variable GarageYrBlt_1 en GarageYrBlt
colnames(df_bind)[which(names(df_bind) == "GarageYrBlt_1")] <- "GarageYrBlt"

table(df_bind$GarageYrBlt)

#-------------- YearBuilt ------------------------
plot(table(df_bind$YearBuilt), main='Années de construction de la maison')

df_bind$YearBuilt <- as.numeric(2011 - df_bind$YearBuilt)

#-------------- YrSold ------------------------
df_bind$YrSold <- as.numeric(df_bind$YrSold - 2005)
table(df_bind$YrSold) 

#------------- MasVnrType  --------------------------
mavns <- c('BrkCmn','BrkFace','None','Stone')

df_bind$MasVnrType <- as.numeric(factor(df_bind$MasVnrType , levels=mavns))
table(df_bind$MasVnrType)

#------------- RoofStyle  --------------------------
roofs <- c('Flat','Gable','Gambrel','Hip','Mansard','Shed') 

df_bind$RoofStyle <- as.numeric(factor(df_bind$RoofStyle , levels=roofs))
table(df_bind$RoofStyle)

#------------- HouseStyle  --------------------------
houses <- c('1.5Fin','1.5Unf','1Story','2.5Fin','2.5Unf','2Story','SFoyer','SLvl')

df_bind$HouseStyle <- as.numeric(factor(df_bind$HouseStyle , levels=houses))
table(df_bind$HouseStyle)

#------------- Foundation  --------------------------
foundations <- c('BrkTil','CBlock','PConc','Slab','Stone','Wood')

df_bind$Foundation <- as.numeric(factor(df_bind$Foundation , levels=foundations))
table(df_bind$Foundation)

#------------- Exterior1st  Exterior2nd  --------------------------
exters1 <- c('AsbShng','AsphShn','BrkComm','BrkFace','CBlock','CemntBd','HdBoard','ImStucc','MetalSd','Other','Plywood','Stone','Stucco','VinylSd','Wd Sdng','WdShing')
exters2 <- c('AsbShng','AsphShn','Brk Cmn','BrkFace','CBlock','CmentBd','HdBoard','ImStucc','MetalSd','Other','Plywood','Stone','Stucco','VinylSd','Wd Sdng','Wd Shng')

df_bind$Exterior1st <- as.numeric(factor(df_bind$Exterior1st , levels=exters1))
df_bind$Exterior2nd <- as.numeric(factor(df_bind$Exterior2nd , levels=exters2))

table(df_bind$Exterior1st)
table(df_bind$Exterior2nd)

#------------- Neighborhood  --------------------------
neighborhoods <-  c('Blmngtn','Blueste','BrDale','BrkSide','ClearCr','CollgCr','Crawfor','Edwards','Gilbert','IDOTRR','MeadowV','Mitchel','NAmes','NoRidge','NPkVill','NridgHt','NWAmes'
                    ,'OldTown','Sawyer','SawyerW','Somerst','StoneBr','SWISU','Timber','Veenker')

df_bind$Neighborhood <- as.numeric(factor(df_bind$Neighborhood , levels=neighborhoods))
table(df_bind$Neighborhood)

#------------- MSZoning  --------------------------
mzonings <- c('C (all)','FV','RH','RL','RM')

df_bind$MSZoning <- as.numeric(factor(df_bind$MSZoning , levels=mzonings))
table(df_bind$MSZoning)

#------------- Functional  --------------------------
functionals <- c('Maj1','Maj2','Min1','Min2','Mod','Sev','Typ')

df_bind$Functional <- as.numeric(factor(df_bind$Functional , levels=functionals))
table(df_bind$Functional)

#---------------------- suppression d'observations extreme ------
df_bind <- df_bind[-c(524,1299),]

dim(df_bind)

#suppression de id_sale_price
id_sale_prices <- id_sale_prices[!(id_sale_prices$Id %in% c(524,1299)), ]
dim(id_sale_prices)

#Le nombre de maison est maintenant 1458
nb_row <- dim(df_bind)[1]-1459

#-----------------------  TEST  SKEWNESS  --------------------------------

for(i in 1:length(colnames(df_bind))){
  print(paste(colnames(df_bind)[i],skewness(df_bind[,i]),sep=':'))
}

df_bind_back_up <- df_bind

df_bind <- df_bind_back_up

df_bind$Street <- transformTukey(df_bind$Street, plotit=FALSE)
df_bind$Functional <- transformTukey(df_bind$Functional, plotit=FALSE)
df_bind$SaleType <- transformTukey(df_bind$SaleType, plotit=FALSE)
df_bind$BsmtCond <- transformTukey(df_bind$BsmtCond, plotit=FALSE)
df_bind$CentralAir <- transformTukey(df_bind$CentralAir, plotit=FALSE)
df_bind$GarageCond <- transformTukey(df_bind$GarageCond, plotit=FALSE)
df_bind$GarageQual <- transformTukey(df_bind$GarageQual, plotit=FALSE)
df_bind$LandContour <- transformTukey(df_bind$LandContour, plotit=FALSE)
df_bind$Electrical <- transformTukey(df_bind$Electrical, plotit=FALSE)
df_bind$PavedDrive <- transformTukey(df_bind$PavedDrive, plotit=FALSE)
df_bind$SaleCondition <- transformTukey(df_bind$SaleCondition, plotit=FALSE)
df_bind$MSZoning <- transformTukey(df_bind$MSZoning, plotit=FALSE)
df_bind$BsmtQual <- transformTukey(df_bind$BsmtQual, plotit=FALSE)
df_bind$LotConfig <- transformTukey(df_bind$LotConfig, plotit=FALSE)

df_bind$ExterQual <- transformTukey(df_bind$ExterQual, plotit=FALSE)
df_bind$X2ndFlrSF <- transformTukey(df_bind$X2ndFlrSF, plotit=FALSE)
df_bind$BsmtUnfSF <- transformTukey(df_bind$BsmtUnfSF, plotit=FALSE)
df_bind$GarageYrBlt <- transformTukey(df_bind$GarageYrBlt, plotit=FALSE)
df_bind$BsmtFinSF1 <- transformTukey(df_bind$BsmtFinSF1, plotit=FALSE)
df_bind$TotalSF <- transformTukey(df_bind$TotalSF, plotit=FALSE)
df_bind$LotFrontage <- transformTukey(df_bind$LotFrontage, plotit=FALSE)
df_bind$GrLivArea <- transformTukey(df_bind$GrLivArea, plotit=FALSE)
df_bind$BsmtExposure <- transformTukey(df_bind$BsmtExposure, plotit=FALSE)
df_bind$X1stFlrSF <- transformTukey(df_bind$X1stFlrSF, plotit=FALSE)
df_bind$ExterCond <- transformTukey(df_bind$ExterCond, plotit=FALSE)
df_bind$MSSubClass <- transformTukey(df_bind$MSSubClass, plotit=FALSE)
df_bind$RoofStyle <- transformTukey(df_bind$RoofStyle, plotit=FALSE)
df_bind$WoodDeckSF <- transformTukey(df_bind$WoodDeckSF, plotit=FALSE)
df_bind$Fence <- transformTukey(df_bind$Fence, plotit=FALSE)
df_bind$BldgType <- transformTukey(df_bind$BldgType, plotit=FALSE)
df_bind$OpenPorchSF <- transformTukey(df_bind$OpenPorchSF, plotit=FALSE)
df_bind$MasVnrArea <- transformTukey(df_bind$MasVnrArea, plotit=FALSE)
df_bind$Condition1 <- transformTukey(df_bind$Condition1, plotit=FALSE)
df_bind$BsmtFinType2 <- transformTukey(df_bind$BsmtFinType2, plotit=FALSE)
df_bind$BsmtHalfBath <- transformTukey(df_bind$BsmtHalfBath, plotit=FALSE)
df_bind$ScreenPorch <- transformTukey(df_bind$ScreenPorch, plotit=FALSE)
df_bind$EnclosedPorch <- transformTukey(df_bind$EnclosedPorch, plotit=FALSE)
df_bind$Alley <- transformTukey(df_bind$Alley, plotit=FALSE)
df_bind$BsmtFinSF2 <- transformTukey(df_bind$BsmtFinSF2, plotit=FALSE)
df_bind$KitchenAbvGr <- transformTukey(df_bind$KitchenAbvGr, plotit=FALSE)
df_bind$LandSlope <- transformTukey(df_bind$LandSlope, plotit=FALSE)
df_bind$MiscFeature <- transformTukey(df_bind$MiscFeature, plotit=FALSE)
df_bind$RoofMatl <- transformTukey(df_bind$RoofMatl, plotit=FALSE)
df_bind$X3SsnPorch <- transformTukey(df_bind$X3SsnPorch, plotit=FALSE)
df_bind$Heating <- transformTukey(df_bind$Heating, plotit=FALSE)
df_bind$LowQualFinSF <- transformTukey(df_bind$LowQualFinSF, plotit=FALSE)
df_bind$LotArea <- transformTukey(df_bind$LotArea, plotit=FALSE)
df_bind$Condition2 <- transformTukey(df_bind$Condition2, plotit=FALSE)
df_bind$PoolQC <- transformTukey(df_bind$PoolQC, plotit=FALSE)
df_bind$PoolArea <- transformTukey(df_bind$PoolArea, plotit=FALSE)
df_bind$MiscVal <- transformTukey(df_bind$MiscVal, plotit=FALSE)

############################################################################
#
#                       IMPORTANCE DES VARIABLES
#                  
#############################################################################
rf <- randomForest(x=df_bind[1:nb_row,], y = id_sale_prices$SalePrice, importance=TRUE, ntree=1000)

(VI_F=importance(rf))

### VISUALISER LES VARIABLES ET L'IMPORTANCE
importance <- varImp(rf, useModel='rf')
importance <- sort(importance$Overall)
importance
varImpPlot(rf,type=2, main='Variables et leurs importances')

############################################################################
#
#                       LES OBJETS POUR LES MODELES
#
#############################################################################

# sans pca
train_p <- cbind(df_bind[1:nb_row,],id_sale_prices$SalePrice)
colnames(train_p)[which(names(train_p) == "id_sale_prices$SalePrice")] <- "SalePrice"
colnames(train_p)
dim(train_p)

#-- solution avec toutes
nb_row_test <- nb_row+1

test_p <- cbind(df_bind[nb_row_test:dim(df_bind)[1],])
dim(test_p)
colnames(test_p)

############################################################################
#
#                       MODELES
#
#############################################################################

train_control <- trainControl(method="repeatedcv", number=5, savePredictions=TRUE)

algorithmList <- c('enet','gbm','lm','xgbLinear','rf')   

models <- caretList(log(SalePrice) ~ ., data=as.data.frame(train_p), trControl=train_control, verbose=TRUE, methodList=algorithmList)
results <- resamples(models)
summary(results)
dotplot(results)

# correlation between results
modelCor(results)
splom(results)

# stack using glm
stack_control <- trainControl(method="repeatedcv", number=10, repeats=3, savePredictions=TRUE)

stack_glm <- caretStack(models, method="lm", metric="RMSE", trControl=stack_control)

print(stack_glm)

# ----------------------------- PREDICTION --------------------------------------------------- 
dim(test_p)
dim(train_p)
test_pred_stack1 <- predict(stack_glm, data.frame(test_p))
length(test_pred_stack1)

test_pred_stack1  <- data.frame(c(1461:2919), exp(test_pred_stack1) )
head(test_pred_stack1)
colnames(test_pred_stack1) <- c('Id','SalePrice')

#--- Fichier pour la soumission
write.table(test_pred_stack1, file=paste(path_soumission,"kaggle_house_soumission_stack.csv", sep = ""), sep = ',', row.names = FALSE)
