
library(dplyr)
library(psych)

#Step 1: Import and prepare the data for analysis
# Reading the csv files and changing the column names
brooklyn_2016 <- read.csv("/Users/gedelasnigda/Desktop/Autumn 2022/Statistical Analysis/Final Assignment 1/2016_brooklyn.csv", skip=4)
head(brooklyn2016)
colnames(brooklyn2016) <- c('borough','neighborhood','bldclasscat','taxclasscurr','block','lot','easement','bldclasscurr','address','aptnum','zip','resunits','comunits','totunits','landsqft','grosssqft','yrbuilt','taxclasssale','bldclasssale','price','date')

brooklyn2017 <- read.csv("/Users/gedelasnigda/Desktop/Autumn 2022/Statistical Analysis/Final Assignment 1/2017_brooklyn.csv", skip=4)
head(brooklyn2017)
colnames(brooklyn2017) <- c('borough','neighborhood','bldclasscat','taxclasscurr','block','lot','easement','bldclasscurr','address','aptnum','zip','resunits','comunits','totunits','landsqft','grosssqft','yrbuilt','taxclasssale','bldclasssale','price','date')

brooklyn2018 <- read.csv("/Users/gedelasnigda/Desktop/Autumn 2022/Statistical Analysis/Final Assignment 1/2018_brooklyn.csv", skip=4)
head(brooklyn2018)
colnames(brooklyn2018) <- c('borough','neighborhood','bldclasscat','taxclasscurr','block','lot','easement','bldclasscurr','address','aptnum','zip','resunits','comunits','totunits','landsqft','grosssqft','yrbuilt','taxclasssale','bldclasssale','price','date')

brooklyn2019 <- read.csv("/Users/gedelasnigda/Desktop/Autumn 2022/Statistical Analysis/Final Assignment 1/2019_brooklyn.csv", skip=4)
head(brooklyn2019)
colnames(brooklyn2019) <- c('borough','neighborhood','bldclasscat','taxclasscurr','block','lot','easement','bldclasscurr','address','aptnum','zip','resunits','comunits','totunits','landsqft','grosssqft','yrbuilt','taxclasssale','bldclasssale','price','date')

brooklyn2020 <- read.csv("/Users/gedelasnigda/Desktop/Autumn 2022/Statistical Analysis/Final Assignment 1/2020_brooklyn.csv", skip=6)
head(brooklyn2020)
colnames(brooklyn2020) <- c('borough','neighborhood','bldclasscat','taxclasscurr','block','lot','easement','bldclasscurr','address','aptnum','zip','resunits','comunits','totunits','landsqft','grosssqft','yrbuilt','taxclasssale','bldclasssale','price','date')


### remove the first blank line in brooklyn2020
brooklyn2020 <- brooklyn2020[-1,] 
head(brooklyn2020)

#Combining the data
brooklyn_df <- rbind(brooklyn2016,brooklyn2017,brooklyn2018,brooklyn2019,brooklyn2020)

#Checking datatype
str(brooklyn_df)

#Cleaning the columns and fixing datatype
brooklyn_df$landsqft <- gsub(",", "", brooklyn_df$landsqft) 
brooklyn_df$landsqft <- as.numeric(brooklyn_df$landsqft)

brooklyn_df$grosssqft <- gsub(",", "", brooklyn_df$grosssqft)
brooklyn_df$grosssqft <- as.numeric(brooklyn_df$grosssqft) 

brooklyn_df$totunits[which(brooklyn_df$totunits == " -   ")] <- NA
brooklyn_df$totunits <- as.numeric(brooklyn_df$totunits)

brooklyn_df$comunits[which(brooklyn_df$comunits == " -   ")] <- NA
brooklyn_df$comunits <- as.numeric(brooklyn_df$comunits)

brooklyn_df$bldclasscat <- str_squish(brooklyn_df$bldclasscat)

brooklyn_df$block <-as.numeric(brooklyn_df$block)
brooklyn_df$lot <-as.numeric(brooklyn_df$lot)
brooklyn_df$zip<- as.numeric(brooklyn_df$zip)
brooklyn_df$yrbuilt <- as.numeric(brooklyn_df$yrbuilt)

brooklyn_df$resunits[which(brooklyn_df$resunits == " -   ")] <- NA
brooklyn_df$resunits <- as.numeric(brooklyn_df$resunits)

brooklyn_df$bldclasscurr[which(brooklyn_df$bldclasscurr == "  ")] <- NA
brooklyn_df$bldclasscurr[which(brooklyn_df$bldclasscurr == " ")] <- NA
brooklyn_df$bldclasscurr[which(brooklyn_df$bldclasscurr == "")] <- NA

brooklyn_df$bldclasscurr[which(brooklyn_df$bldclasscurr == "  ")] <- NA
brooklyn_df$bldclasscurr[which(brooklyn_df$bldclasscurr == " ")] <- NA
brooklyn_df$bldclasscurr[which(brooklyn_df$bldclasscurr == "")] <- NA

brooklyn_df$taxclasscurr[which(brooklyn_df$taxclasscurr == "  ")] <-  NA
brooklyn_df$taxclasscurr[which(brooklyn_df$taxclasscurr == " ")] <-  NA
brooklyn_df$taxclasscurr[which(brooklyn_df$taxclasscurr == "")] <-  NA

brooklyn_df$zip[which(brooklyn_df$zip == "")] <- NA

brooklyn_df$price <- gsub("[$]", "", brooklyn_df$price) 
brooklyn_df$price <- gsub(",", "", brooklyn_df$price)
brooklyn_df$price <- gsub("-", "", brooklyn_df$price)
brooklyn_df$price <- as.numeric(brooklyn_df$price)

brooklyn_df$date <- as.Date(brooklyn_df$date, "%m/%d/%Y")

#Converting variables into factor variables
brooklyn_df$yrbuilt <- as.factor(brooklyn_df$yrbuilt)
brooklyn_df$taxclasssale <- as.factor(brooklyn_df$taxclasssale)
brooklyn_df$bldclasssale <- as.factor(brooklyn_df$bldclasssale)
brooklyn_df$bldclasscurr <- as.factor(brooklyn_df$bldclasscurr)
brooklyn_df$taxclasscurr <- as.factor(brooklyn_df$taxclasscurr)
brooklyn_df$bldclasscat <- as.factor(brooklyn_df$bldclasscat)
brooklyn_df$neighborhood <- as.factor(brooklyn_df$neighborhood)
brooklyn_df$borough <-as.factor(brooklyn_df$borough)
brooklyn_df$zip <- as.factor(brooklyn_df$zip)
brooklyn_df$bldclasscat <- as.factor(substr(brooklyn_df$bldclasscat,1,2))

# Percentage of number of null values - columnwise
sapply(brooklyn_df, function(x) round(sum(is.na(x)*100/nrow(brooklyn_df))))

# Dropping easement column(100% null values) 
ncol(brooklyn_df) #21
brooklyn_df <- subset(brooklyn_df, select = -c(easement))
ncol(brooklyn_df) #20

# 1.3 Filter the data and make transformations specific to this analysis

#filtering data to purchases where the building class at the time of sale starts with ‘A’ or ‘R’
df1 <- brooklyn_df[grepl("A", brooklyn_df$bldclasssale),]
df2 <- brooklyn_df[grepl("R", brooklyn_df$bldclasssale),]
filteredData <- rbind(df1,df2)
nrow(filteredData) ## 44475

#filter data where the number of total units and the number of residential units are both 1
filteredData <- filteredData[filteredData$totunits == '1' & filteredData$resunits == '1', ]
nrow((filteredData))

#filter data where observations where gross square footage is more than 0
filteredData <- filteredData[filteredData$grosssqft > 0, ]
nrow((filteredData))

#filter data where sale price is non-missing
filteredData <- filteredData[!is.na(filteredData$price),]
dim((filteredData))
### Rows: 19640 Columns: 20


# Step 2: EDA and feature engineering 
# 2.1 Exploratory data analysis 
range(filteredData$price,na.rm=TRUE) # 0 25500000

# column : borough 
unique(filteredData$borough)
plot(filteredData$price,filteredData$borough)
str(factor(filteredData$borough)) # 1 level - useless column

# borough is an unecessary column as it only has one unique value 

# column : taxclasscurr 
unique(filteredData$taxclasscurr)
plot(filteredData$price,filteredData$taxclasscurr)

# column : block 
unique(filteredData$block)
plot(filteredData$price,filteredData$block)
#

# column : lot 
unique(filteredData$lot)
plot(filteredData$price,filteredData$lot)
# There is no strong pattern observed but as the price increases more than 5.0e+6 the value of lot seems to be less than 1000

# column : aptnum
plot(filteredData$price,filteredData$aptnum)
# seems to be a very low positive correlation

# column : resunits
unique(filteredData$resunits)
# Not a useful column for prediction as it has only one unique value

# column : totunits
unique(filteredData$totunits)
# Not a useful column for prediction as it has only one unique value


# column : landsqft
plot(filteredData$price,filteredData$landsqft)
# 

# column : grosssqft
plot(filteredData$price,filteredData$grosssqft)
# positve correlated

# Analysis using landsqft column
hist(filteredData$landsqft, breaks = 150, main = 'Land square feet Distribution', xlab = 'landsqft')
hist(log(filteredData$landsqft), breaks = 150, main = 'Log of Land square feet Distribution', xlab = 'log(landsqft)')
hist(sqrt(filteredData$landsqft), breaks = 150, main = 'Square root of Land square feet Distribution', xlab = 'sqrt(landsqft)')
plot(filteredData$landsqft, filteredData$price, main="price vs landsqft", 
     xlab="landsqft", ylab="price", pch=19)

# Analysis using grosssqft column
hist(filteredData$grosssqft, breaks = 50, main = 'Gross square feet Distribution', xlab = 'grosssqft')
hist(log(filteredData$grosssqft), breaks = 50, main = 'Log of Gross square feet Distribution', xlab = 'log(grosssqft)')
hist(sqrt(filteredData$grosssqft), breaks = 50, main = 'Square root of Gross square feet Distribution', xlab = 'sqrt(grosssqft)')
plot(filteredData$grosssqft, filteredData$price, main="price vs grosssqft", 
     xlab="grosssqft", ylab="price", pch=19)

# Analysis on block
hist(filteredData$block, breaks = 50, main = 'block distribution', xlab = 'block')

#Analysis on lot
hist(filteredData$lot, breaks = 50, main = 'lot distribution', xlab = 'lot')

#Analysis on year built
plot(filteredData$yrbuilt, filteredData$price, main="price vs yrbuilt", 
     xlab="yrbuilt", ylab="price", pch=19)

#Analysis on zip
plot(filteredData$zip, filteredData$price, main="price vs zip", 
     xlab="zip", ylab="price", pch=19)

# Analysis on Price
hist(filteredData$price, breaks = 50, main = 'Price Distribution', col = 'red', xlab = 'Price') 
hist(log(filteredData$price), breaks = 50, main = 'log price distribution', col = 'red', xlab = 'log(price)')
hist(sqrt(filteredData$price), breaks = 50, main = 'Square root price distribution', col = 'red', xlab = 'sqrt(price)')
## from the box plot, we have identified the outliers, we now need to remove them

#Removing outliers in price
boxplot(filteredData$price, main = 'Boxplot of Price')
describe(filteredData$price)
filteredData <- filteredData[filteredData$price>100000 & filteredData$price<7000000, ]

nrow(filteredData) #13420

hist(filteredData$price, breaks = 50, main = 'Price Distribution', col = 'red', xlab = 'Price') 
hist(log(filteredData$price), breaks = 50, main = 'log price distribution', col = 'black', xlab = 'log(price)')
plot(density(filteredData$price), main = 'Density of price', col = 'blue') 


##2.2

ModelVars <- dplyr::select(filteredData, c('price','bldclasscat', 'block', 'lot', 'bldclasscurr', 'zip', 'landsqft', 'grosssqft', 'yrbuilt', 'bldclasssale'))

# Finding best model using backward elimination
step(linear_model,direction='backward')
m1 <-lm(formula = price ~ lot + zip + landsqft + grosssqft + yrbuilt + 
          bldclasssale, data = ModelVars)
summary(m1) #Multiple R-squared:  0.6327,	Adjusted R-squared:  0.6282

# Finding best model usingforward selection   
step(lm(price~1,ModelVars),scope=formula(lin_mod),direction='forward')
m2 <-lm(formula = price ~ zip + grosssqft + yrbuilt + bldclasssale + 
          landsqft + lot, data = ModelVars)
summary(m2) #Multiple R-squared:  0.6327,	Adjusted R-squared:  0.6282 

RMSE2<-sqrt(mean((ModelVars$price - exp(predict(m2, ModelVars)))^2))
RMSE2 #inf

# Different model iterations - 

m3= lm(log(price)~zip+sqrt(landsqft)+sqrt(grosssqft)+yrbuilt, ModelVars)
summary(m3) #Multiple R-squared:  0.6348,	Adjusted R-squared:  0.6309 
RMSE3<-sqrt(mean((ModelVars$price - exp(predict(m3, ModelVars)))^2))
RMSE3 #438408.2

m4 = lm(sqrt(price)~zip+sqrt(landsqft)+log(grosssqft)+yrbuilt, ModelVars)
summary(m4) #Multiple R-squared:  0.6574,	Adjusted R-squared:  0.6538 
RMSE4<-sqrt(mean((ModelVars$price - exp(predict(m4, ModelVars)))^2))
RMSE4 #Inf

m5 = lm(log(price)~zip+sqrt(landsqft)+log(grosssqft), ModelVars)
summary(m5) #Multiple R-squared:  0.6226,	Adjusted R-squared:  0.6215 
RMSE5<-sqrt(mean((ModelVars$price - exp(predict(m5, ModelVars)))^2))
RMSE5 #444448.2
dim(filteredData) #Rows: 13420    Columns: 20

# Final Model is m5 

#Checking OLS Assumptions
require(lmtest)
hist(m5$residuals, breaks=50, xlab = 'Residuals', main='Distribution of residuals')
#Observation: The residual distribution looks normally distributed

ks.test(m5$residuals/summary(m5)$sigma, pnorm)
# ks test: The model does not pass normality check

# Step 3: Submit your model and your work.
saveRDS(list(model=m5, data=filteredData), file='/Users/gedelasnigda/Desktop/Autumn 2022/Statistical Analysis/Final Assignment 1/snigdagedela.RDS') 
