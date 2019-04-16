library(bigrquery)
proj_id <- 'hult-r-project'
sql <- "SELECT * FROM `air_france.AF`"
AF <- bq_table_download(bq_project_query(proj_id, sql))
summary(AF)
AF$Profit <- (AF$Amount - AF$Total_Cost)
AF$ROA <- (AF$Amount / AF$Total_Cost)

##################### exploring ROA and Profits By Publisher, Global and US #####################

library(sqldf)
Scale_Performance <- sqldf("
                               SELECT
                               CASE WHEN Publisher_Name LIKE '%Global%' THEN 'Global'
                               WHEN Publisher_Name LIKE '%US%' THEN 'US'
                               ELSE 'Not defined'
                               END AS Scale,
                               Sum(Profit) AS sum_profit, 
                               SUM(Amount)/SUM(Total_Cost) AS ROA
                               FROM AF
                               GROUP BY Scale
                               ORDER BY sum(Profit) DESC;
                               ")
Global_Performance <- sqldf("
                               SELECT Publisher_Name,
                               Sum(Profit) AS sum_profit, 
                               SUM(Amount)/SUM(Total_Cost) AS ROA
                               FROM AF
                               WHERE Publisher_Name LIKE '%Global%'
                               GROUP BY Publisher_Name
                               ORDER BY sum(Profit) DESC;
                               ")
US_Performance <- sqldf("
                            SELECT Publisher_Name,
                            Sum(Profit) AS sum_profit, 
                            SUM(Amount)/SUM(Total_Cost) AS ROA
                            FROM AF
                            WHERE Publisher_Name LIKE '%US%'
                            GROUP BY Publisher_Name
                            ORDER BY sum(Profit) DESC;
                            ")


############################################################################

Publisher_KPI <- sqldf("
                              SELECT
                               CASE WHEN Publisher_Name LIKE '%Google%' THEN 'Google'
                               WHEN Publisher_Name LIKE '%MSN%' THEN 'MSN'
                               WHEN Publisher_Name LIKE '%Overture%' THEN 'Overture'
                               WHEN Publisher_Name LIKE '%Yahoo%' THEN 'Yahoo'
                               ELSE 'Not defined'
                               END AS Publisher,
                               SUM(Clicks) AS sum_clicks,
                               AVG(Engine_Click_Thru__) AS click_through_rate,
                               AVG(Trans__Conv___) AS conversion_rate,
                               SUM(Impressions) AS impression,
                               AVG(Avg__Pos_) AS average_position,
                               AVG(Total_Cost__Trans_) AS cost_per_conversion,
                               SUM(Total_Cost) AS sum_cost,
                               SUM(Total_Volume_of_Bookings) AS sum_bookings,
                               SUM(Amount) AS sum_revenue,
                               SUM(Amount)/SUM(Total_Cost) AS ROA
                               FROM AF
                               GROUP BY Publisher
                                ")
i <- 1.5 # goal to increase (i-1)*100 % 
d <- 1.25 # goal to decrease (1-i)*100 %
MD_obj <- c(i*mean(Publisher_KPI[,2]),
            i*mean(Publisher_KPI[,3]),
            i*mean(Publisher_KPI[,4]),
            i*mean(Publisher_KPI[,5]),
            i*mean(Publisher_KPI[,6]),
            i*mean(Publisher_KPI[,7]),
            d*mean(Publisher_KPI[,8]),
            i*mean(Publisher_KPI[,9]),
            i*mean(Publisher_KPI[,10]),
            i*mean(Publisher_KPI[,11]))
Google <- c(Publisher_KPI[1,2], Publisher_KPI[1,3], Publisher_KPI[1,4], Publisher_KPI[1,5], Publisher_KPI[1,6], Publisher_KPI[1,7], Publisher_KPI[1,8], Publisher_KPI[1,9], Publisher_KPI[1,10], Publisher_KPI[1,11]) 
MSN <- c(Publisher_KPI[2,2], Publisher_KPI[2,3], Publisher_KPI[2,4], Publisher_KPI[2,5], Publisher_KPI[2,6], Publisher_KPI[2,7], Publisher_KPI[2,8], Publisher_KPI[2,9], Publisher_KPI[2,10], Publisher_KPI[2,11])
Overture <- c(Publisher_KPI[3,2], Publisher_KPI[3,3], Publisher_KPI[3,4], Publisher_KPI[3,5], Publisher_KPI[3,6], Publisher_KPI[3,7], Publisher_KPI[3,8], Publisher_KPI[3,9], Publisher_KPI[3,10], Publisher_KPI[3,11])
Yahoo <- c(Publisher_KPI[4,2], Publisher_KPI[4,3], Publisher_KPI[4,4], Publisher_KPI[4,5], Publisher_KPI[4,6], Publisher_KPI[4,7], Publisher_KPI[4,8], Publisher_KPI[4,9], Publisher_KPI[4,10], Publisher_KPI[4,11])

################## Optimization Model ##############

linear_model <- lm(MD_obj ~ Google+MSN+Yahoo, data=Publisher_KPI) #linear model
summary(linear_model)
sum_coef <- sum(abs(linear_model$coefficient[2:4]))
coef_ratio_G <- linear_model$coefficient[2]/sum_coef
coef_ratio_M <- linear_model$coefficient[3]/abs(sum_coef)
#coef_ratio_OV <- linear_model$coefficient[4]/sum_coef
coef_ratio_Y <- linear_model$coefficient[4]/sum_coef

cost_change_G <- coef_ratio_G*Publisher_KPI$sum_cost[1]
cost_change_M <-coef_ratio_M*Publisher_KPI$sum_cost[2]
#cost_change_OV <-coef_ratio_OV*Publisher_KPI$sum_cost[3]
cost_change_Y <-coef_ratio_Y*Publisher_KPI$sum_cost[4]
cost_change <- round(c(cost_change_G, cost_change_M, cost_change_Y),2)

cost_alleg_G <- cost_change_G + Publisher_KPI$sum_cost[1]
cost_alleg_M <- cost_change_M + Publisher_KPI$sum_cost[2]
#cost_alleg_OV <- cost_change_OV + Publisher_KPI$sum_cost[3]
cost_alleg_Y <- cost_change_Y + Publisher_KPI$sum_cost[4]
cost_alleg <- round(c(cost_alleg_G, cost_alleg_M, cost_alleg_Y),2)

goal_budget <- data.frame(cost_change, cost_alleg)
colnames(goal_budget) <- c('Change in Budget', 'Total Budget')
library(readr)
write.csv(goal_budget, "C:/Users/Tony/Desktop/R/Business case study/goal_budget.csv")


############# Ploting the table ###############################################################

library(grid) 
library(gridExtra) 
plot_table <- goal_budget
grid.table(plot_table)

################### exploring variables for colinearity ################################################

library(ggplot2)
library(plotly)
plot_ly(data = Publis)
ggplot(data=AF, aes(Profit, ROA))+geom_point(aes(color = Publisher_Name)) # not linear relationship
ggplot(data=AF, aes(Amount, ROI))+geom_point(aes(color = Publisher_Name)) # not linear relationship
ggplot(data=AF, aes(Profit, ROI))+geom_point(aes(color = Publisher_Name)) # not linear relationship
ggplot(data=AF, aes(Amount, ROA))+geom_point(aes(color = Publisher_Name)) # not linear relationship
ggplot(data=AF, aes(Total_Volume_of_Bookings, Amount))+geom_point(aes(color = Publisher_Name)) #linear relationship
ggplot(data=AF, aes(Total_Volume_of_Bookings, Profit))+geom_point(aes(color = Publisher_Name)) #linear relationship 
ggplot(data=AF, aes(Amount, Profit))+geom_point(aes(color = Publisher_Name)) #linear relationship 
ggplot(data=AF, aes(ROA, ROI))+geom_point(aes(color = Publisher_Name)) # linear relationship
plot_ly(Publisher_Performance, x = ~ROA, y = ~ROI,  type="scatter", mode = "markers" , color = ~Publisher)
plot_ly(Publisher_Performance, x = ~sum_bookings, y = ~sum_profit,  type="scatter", mode = "markers" , color = ~Publisher)

