library(readxl)
Air_France_Case_Spreadsheet_Supplement <- read_excel("C:/Users/Tony/Desktop/R/Business case study/Air France Case Spreadsheet Supplement.xls", 
                                                     sheet = "DoubleClick")
AF <- data.frame(Air_France_Case_Spreadsheet_Supplement)
summary(AF)

# Counting unique values in columns:

for (i in 1:ncol(AF)){
  if (is.character(AF[,i])){
    print(summary(as.factor(AF[,i])))
  }
}

AF$ROA <- AF$Amount/AF$Total.Cost # How to round AF$ROA

summary(as.factor(AF$Keyword))
summary(as.factor(AF$Campaign)) # 1214 unassigned
summary(as.factor(AF$Keyword.Group)) # 1214 unassigned
summary(as.factor(AF$Category)) # 1214 unassigned
#This column contain values of destinations, 'airflight gernal', 'discountr', and 'vaccation'.
#It does not seem to be valuable because of the quality of record. 
summary(as.factor(AF$Keyword.Type)) # Useless. This is columns has only one value "unassigned".
summary(as.factor(AF$Keyword.ID)) # Unique IDs for each observation
summary(as.factor(AF$Match.Type)) # 48 N/A. What is it?
summary(as.factor(AF$Bid.Strategy)) # 1224 NA's. What is it?
summary(as.factor(AF$Status)) # 1120 Unavalable 

unique(AF$Publisher.ID) # 7 IDs
unique(AF$Publisher.Name) # 7 names
unique(AF$Keyword.Type)




# Impute missing values with average
for( v in ncol(AF)){
  for(i in 1:nrow(AF)){
    if(is.null( AF$v[i] ) ) { AF$v[i] <- mean(as.numeric(AF$v[-which(is.na(AF$v))])) }
  }
}


# Extract data and subset by Publishers, using SQL 
library(sqldf)
Avg_Volume_SQL <- sqldf("SELECT `Publisher.Name`, AVG(`Total.Volume.of.Bookings`)
                        FROM AF
                        GROUP BY `Publisher.Name`
                        ORDER BY AVG(`Total.Volume.of.Bookings`) DESC;
                        ")

Engine_Click_SQL <- sqldf("SELECT `Publisher.Name`, SUM('Engine.Click.Thru.. ')
                        FROM AF
                          GROUP BY `Publisher.Name`
                          ORDER BY SUM('Engine.Click.Thru.. ') DESC;
                          ")

Trans_Conv_SQL <- sqldf("SELECT `Publisher.Name`, AVG('Trans..Conv... ')
                        FROM AF
                          GROUP BY `Publisher.Name`
                          ORDER BY AVG('Trans..Conv... ') DESC;
                          ")

Cost_Trans_SQL <- sqldf("SELECT `Publisher.Name`, AVG('Total.Cost..Trans ')
                        FROM AF
                        GROUP BY `Publisher.Name`
                        ORDER BY AVG('Total.Cost..Trans ') DESC;
                        ")

