getwd()
setwd("/Users/Dell/Documents/R/customer_personality_regression")
getwd()

library(corrplot)

df = read.delim("marketing_campaign.csv")

response_count <- table(Response) # 0-1906 samples 1-334 samples 

sum(is.na(df)) # 24 nan values

# check for na values in each column
na_report <- sapply(df, function(x) sum(is.na(x))) # all missing values are for income
# delete missing values
df <- na.omit(df)
num_of_duplicates <- sum(duplicated(df))


# EDA
library(funModeling) 
library(tidyverse) 
library(Hmisc)

glimpse(df) # 27 int and 2 chr
print(status(df)) #number of unique values among columns
freq(df) # shows freq for char columns (Education and Marital status)
print(profiling_num(df)) # mean, std and other coef for all columns

options(scipen = 999) # to get rid of e+ num in plots
plot_num(df[,2:10])
plot_num(df[,11:19])#plots each int col
plot_num(df[,20:25])
plot_num(df[,26:29])
describe(df) # details on each column (missing, distinct, lowest, highest, freq 4 each value)

# we drop z_Revenue and Z_CostContact for having only one value
df$Z_CostContact <- NULL
df$Z_Revenue <- NULL

#replace char values 
library(fastDummies)
df <- dummy_cols(df, select_columns = c('Marital_Status'),
                 remove_selected_columns = TRUE)

df$Marital_Status_Absurd <- NULL
df$Marital_Status_Alone <- NULL
df$Marital_Status_YOLO <- NULL

# turn char into double
df$Dt_Customer <- as.Date(df$Dt_Customer, "%d-%m-%Y")
# split date - year, month, days
df <- df %>%
  dplyr::mutate(year = lubridate::year(Dt_Customer), 
                month = lubridate::month(Dt_Customer), 
                day = lubridate::day(Dt_Customer))

# delete date
df$Dt_Customer <- NULL
attach(df)
# explore date
table(year)
table(month)
table(day)
plot_num(df[,c('year','month','day')], bins = 40)

# replace education
table(df$Education)
df$Education[df$Education == "2n Cycle"] <- 1
df$Education[df$Education == "Basic"] <- 2
df$Education[df$Education == "Graduation"] <- 3
df$Education[df$Education == "Master"] <- 4
df$Education[df$Education == "PhD"] <- 5

df$Education <- as.numeric(df$Education)


# check for outliers with boxplot
for (i in 1:ncol(df)) {
  if (typeof(df[,i]) == "integer" || typeof(df[,i])=="num") {
    dev.new() #to create a new window
    boxplot(df[,i], main = paste("Histrogram of", names(df)[i]))
  }
  else {
    print("Feature is not numeric")
    print(typeof(df$i))
  }
}



# Check for correlation

features <- c("Income","Kidhome", "Teenhome",  "Year_Birth"     ,  "Education", "Recency"  , "MntWines"    ,"MntFruits"              
              , "MntMeatProducts"   ,      "MntFishProducts"         ,"MntSweetProducts"       
              , "MntGoldProds"       ,     "NumDealsPurchases"       ,"NumWebPurchases"        
              , "NumCatalogPurchases" ,    "NumStorePurchases"       ,"NumWebVisitsMonth"      
              , "AcceptedCmp3"         ,   "AcceptedCmp4"           , "AcceptedCmp5"           
              , "AcceptedCmp1"          ,  "AcceptedCmp2"           , "Complain"               
                   
              , "Marital_Status_Divorced", "Marital_Status_Married",  "Marital_Status_Single"  
              , "Marital_Status_Together" ,"Marital_Status_Widow"    
              ,"year"               ,     "month"      ,             "day" )

numeric_features <- c("Income","Kidhome", "Teenhome",  "Year_Birth"     ,  "Recency"  , "MntWines"    ,"MntFruits"              
                      , "MntMeatProducts"   ,      "MntFishProducts"         ,"MntSweetProducts"       
                      , "MntGoldProds"       ,     "NumDealsPurchases"       ,"NumWebPurchases"        
                      , "NumCatalogPurchases" ,    "NumStorePurchases"       ,"NumWebVisitsMonth"      
                      , "AcceptedCmp3"         ,   "AcceptedCmp4"           , "AcceptedCmp5"           
                      , "AcceptedCmp1"          ,  "AcceptedCmp2"           , "Complain"              
                      ,"year"               ,     "month"      ,             "day" )

factor_features <- c(    "Education"
                     , "Marital_Status_Divorced", "Marital_Status_Married",  "Marital_Status_Single"  
                     , "Marital_Status_Together" ,"Marital_Status_Widow"  )
str(df)

# scaling data, all numeric data
df[numeric_features] <- lapply(df[numeric_features], function(x) c(scale(x)))



# Step 1: Call the pdf command to start the plot
png(file = "/Users/Dell/Documents/R/customer_personality_regression/My_Plot.png",   # The directory you want to save the file in
    width = 2500, # The width of the plot in inches
    height = 2200) # The height of the plot in inches

# Step 2: Create the plot with R code
corrplot(cor(df[numeric_features]),na.label = " ", diag = T, type = "upper", addCoef.col="black" ,tl.cex = 2.5,number.cex = 1.5)

# Step 3: Run dev.off() to create the file!
dev.off()





#PCA ---------------------------------------------------------------------------------------------
# calculate principal components
y <- df[,c("ID","Response")]
y <- as.data.frame(y)

X <- df

X$Response <- NULL
X$ID <- NULL


#PCA
#install.packages('ggfortify')
library(ggfortify)

pca_res_numeric <- prcomp(df[,numeric_features], scale. = TRUE) 


autoplot(pca_res_numeric, data = df, colour = 'Response', loadings=TRUE,  loadings.label = TRUE, size=0.9)
summary(pca_res_numeric)


# how much princ. comp explain
library(factoextra)
fviz_eig(pca_res_numeric)


# clustering -------------------------------------------------------------------------------------
# check for numbers of clusters

# how many k we need
ncol(df)

fviz_nbclust(df[numeric_features], kmeans, method = 'wss')
fviz_nbclust(df[numeric_features], kmeans, method='silhouette')

set.seed(1)
autoplot(kmeans(df[numeric_features], 2), data = df[numeric_features])

library(cluster)
autoplot(fanny(df[numeric_features], 2), frame = TRUE)
autoplot(fanny(df[numeric_features], 2), frame = TRUE, frame.type='norm')


#glm --------------------------------------------------------------------------------------------

# choose based on different directions of variables based on pca

## following on from example(lm)
str(df)

# the lower the AIC the better the model
summary(glm1 <- glm(Response ~ ., data = df[2:33]), family=binomial(link="logit"))
final_model <- step(glm1, direction = "backward")
summary(glm1)
summary(final_model)


# splitting data to train and test
split1<- sample(c(rep(0, 0.7 * nrow(df[features])), rep(1, 0.3 * nrow(df[features]))))
table(split1) #o is train 1 is test

train <- df[split1 == 0, ]  
test <- df[split1== 1, ]    

str(train)

summary(glm_full <- glm(Response ~ ., data = train[2:33]), family=binomial(link="logit"))
final_model <- step(glm_full, direction = "backward")
summary(final_model)

# save the model
#save(final_model, file="final_model.Rdata")


y_pred <-predict(final_model, test)

# turning log into probability
logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}

y_prob <- logit2prob(y_pred)

y_final = list()

for (i in 1:length(y_prob)) {
  if (y_prob[[i]] > 0.60) {
    y_final[[i]] <- 1
  }
  else {
    y_final[[i]] <- 0 
  }
}

# check for accuracy
num_of_accurate_predictions <- sum(test$Response == y_final) / length(y_final)
print(num_of_accurate_predictions)


