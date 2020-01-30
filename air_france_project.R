#Installing the necessary packages
install.packages("data.table")
install.packages("formattable")

#Loading the necessary packages
library(readxl)
library(tidyverse)
library(ggplot2)
library(data.table)
library(formattable)

#Loading the Air France data file into the environment
air_france_data_file <- "Air France Case Spreadsheet Supplement.xls"
air_france_data <- read_excel(air_france_data_file, sheet = "DoubleClick")

#Viewing the entire dataset
View(air_france_data)
glimpse(air_france_data)

#Looking at the first 5 rows of the dataset
head(air_france_data)

#Checking all the variable names
colnames(air_france_data)

#Finding the class of the dataset
class(air_france_data)

#Exploring the summary statistics of each variable
summary(air_france_data)

#Re-naming the variables by replacing the blank space with an underscore
names(air_france_data) <- gsub(" ", "_", names(air_france_data))
colnames(air_france_data)

#Converting the Publisher_Name column from character to factor
air_france_data$`Publisher_Name` <- as.factor(air_france_data$`Publisher_Name`)
class(air_france_data$`Publisher_Name`)

#Re-naming a few columns for easy recognition and consistency
colnames(air_france_data)[which(names(air_france_data) == "Total_Cost/_Trans.")] <- "Total_Cost_Per_Trans"
colnames(air_france_data)[which(names(air_france_data) == "Engine_Click_Thru_%")] <- "Engine_Click_Through_Perc"
colnames(air_france_data)[which(names(air_france_data) == "Trans._Conv._%")] <- "Trans_Conv_Perc"
colnames(air_france_data)[which(names(air_france_data) == "Avg._Pos.")] <- "Avg_Pos"
colnames(air_france_data)

#EXPLORATORY DATA ANALYYSIS

# Defining own function theme
theme_air_france <- function(){
  theme_minimal() +
    theme(
      text = element_text(color = "gray25"),
      plot.subtitle = element_text(size = 12),
      plot.caption = element_text(color = "gray30"),
      plot.background = element_rect(fill = "gray95"),
      plot.margin = unit(c(5, 10, 5, 10), units = "mm")
    )
}

#Bar plot of Search Engine Bid Vs. Publisher name
ggplot(air_france_data, aes(x = Publisher_Name, y = Search_Engine_Bid, fill = Publisher_Name)) + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 90)) 

#Bar plot of Average cost per click Vs. Publisher name. 
ggplot(air_france_data, aes(x = Publisher_Name, y = Avg._Cost_per_Click, fill = Publisher_Name)) + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 90)) 

#Bar plot of Clicks Vs. Publisher name. 
ggplot(air_france_data, aes(x = Publisher_Name, y = Clicks, fill = Publisher_Name)) + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 90))

#Bar plot of Impressions Vs. Publisher name. 
ggplot(air_france_data, aes(x = Publisher_Name, y = Impressions, fill = Publisher_Name)) + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 90))

#Bar plot of Engine_Click_Through_Perc Vs. Publisher name. 
ggplot(air_france_data, aes(x = Publisher_Name, y = Engine_Click_Through_Perc, fill = Publisher_Name)) + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 90))

#Bar plot of Avg_Pos Vs. Publisher name. 
ggplot(air_france_data, aes(x = Publisher_Name, y = Avg_Pos, fill = Publisher_Name)) + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 90))

#Bar plot of Total_Cost_Per_Trans Vs. Publisher name. 
ggplot(air_france_data, aes(x = Publisher_Name, y = Total_Cost_Per_Trans, fill = Publisher_Name)) + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 90))

#Bar plot of Total_Volume_of_Bookings Vs. Publisher name. 
ggplot(air_france_data, aes(x = Publisher_Name, y = Total_Volume_of_Bookings, fill = Publisher_Name)) + geom_col() + labs(x = "Publisher Name", y = "Total Volume of Bookings", title = "Total volume of bookings by publisher", caption = "Data source: Air France Internet Marketing, Case #KEL319") + theme_air_france() + theme(axis.text.x = element_text(angle = 90))

#Bar plot of Amount Vs. Publisher name. 
ggplot(air_france_data, aes(x = Publisher_Name, y = Amount, fill = Publisher_Name)) + geom_col() + labs(x = "Publisher Name", y = "Revenue", title = "Revenue generated from each publisher", caption = "Data source: Air France Internet Marketing, Case #KEL319") + theme_air_france() + theme(axis.text.x = element_text(angle = 90))

#Bar plot of Total_Cost Vs. Publisher name. 
ggplot(air_france_data, aes(x = Publisher_Name, y = Total_Cost, fill = Publisher_Name)) + geom_col() + labs(x = "Publisher Name", y = "Total Cost", title = "Total Cost by publisher", caption = "Data source: Air France Internet Marketing, Case #KEL319") + theme_air_france() + theme(axis.text.x = element_text(angle = 90))

by_Publisher_Name_CTR <- air_france_data %>%
  group_by(Publisher_Name) %>%
  summarize(mean_CTR = mean(Engine_Click_Through_Perc))
by_Publisher_Name_CTR
ggplot(by_Publisher_Name_CTR, aes(x = Publisher_Name, y = mean_CTR, fill = Publisher_Name)) + geom_col() + labs(x = "Publisher Name", y = "Average Click Through Rate", title = "Average Click Through Rate for each publisher", caption = "Data source: Air France Internet Marketing, Case #KEL319") + theme_air_france() + theme(axis.text.x = element_text(angle = 90))
 
by_Publisher_Name_Conv <- air_france_data %>%
  group_by(Publisher_Name) %>%
  summarize(mean_Trans_conv = mean(Trans_Conv_Perc))
by_Publisher_Name_Conv
ggplot(by_Publisher_Name_Conv, aes(x = Publisher_Name, y = mean_Trans_conv, fill = Publisher_Name)) + geom_col() + labs(x = "Publisher Name", y = "Average Transaction conversion %", title = "Average Transaction conversion % for each publisher", caption = "Data source: Air France Internet Marketing, Case #KEL319") + theme_air_france() + theme(axis.text.x = element_text(angle = 90))

by_Publisher_Name_vol <- air_france_data %>%
  group_by(Publisher_Name) %>%
  summarize(mean_Volume_of_Bookings = mean(Total_Volume_of_Bookings))
by_Publisher_Name_vol
ggplot(by_Publisher_Name_vol, aes(x = Publisher_Name, y = mean_Volume_of_Bookings, fill = Publisher_Name)) + geom_col() + theme(axis.text.x = element_text(angle = 90))

by_Publisher_Name_amn <- air_france_data %>%
  group_by(Publisher_Name) %>%
  summarize(mean_Amount = mean(Amount))
by_Publisher_Name_amn
ggplot(by_Publisher_Name_amn, aes(x = Publisher_Name, y = mean_Amount, fill = Publisher_Name)) + geom_col() + theme(axis.text.x = element_text(angle = 90))

by_Publisher_Name_CPC <- air_france_data %>%
  group_by(Publisher_Name) %>%
  summarize(mean_CPC = mean(Avg._Cost_per_Click))
by_Publisher_Name_CPC
ggplot(by_Publisher_Name_CPC, aes(x = Publisher_Name, y = mean_CPC, fill = Publisher_Name)) + geom_col() + theme(axis.text.x = element_text(angle = 90))

by_Publisher_Name_avgcost <- air_france_data %>%
  group_by(Publisher_Name) %>%
  summarize(mean_cost = mean(Total_Cost))
by_Publisher_Name_avgcost
ggplot(by_Publisher_Name_avgcost, aes(x = Publisher_Name, y = mean_cost, fill = Publisher_Name)) + geom_col() + theme(axis.text.x = element_text(angle = 90))

by_Publisher_Name_avgTotalcostperTrans <- air_france_data %>%
  group_by(Publisher_Name) %>%
  summarize(mean_TotalCostPerTrans = mean(Total_Cost_Per_Trans))
by_Publisher_Name_avgTotalcostperTrans
ggplot(by_Publisher_Name_avgTotalcostperTrans, aes(x = Publisher_Name, y = mean_TotalCostPerTrans, fill = Publisher_Name)) + geom_col() + labs(x = "Publisher Name", y = "Average Cost per Transaction", title = "Average Cost per Transaction by publisher", caption = "Data source: Air France Internet Marketing, Case #KEL319") + theme_air_france() + theme(axis.text.x = element_text(angle = 90))

ROA_by_Publisher_Name <- air_france_data %>%
  group_by(Publisher_Name) %>%
  summarize(ROA = mean(Amount) / mean(Total_Cost))
ROA_by_Publisher_Name
ggplot(ROA_by_Publisher_Name, aes(x = Publisher_Name, y = ROA, fill = Publisher_Name)) + geom_col() + labs(x = "Publisher Name", y = "ROA", title = "ROA by publisher", caption = "Data source: Air France Internet Marketing, Case #KEL319") + theme_air_france() + theme(axis.text.x = element_text(angle = 90))


#INSIGHTS: Revenue generating key metrics (Total volume of bookings and Amount) - Google US has the largest figure among all publishers. Kayak has generated higher figures compared to MSN-US and MSN-Global. But MSN has no search engine bid and works on Search Engine Optimization.
#          Non revenue genrating key metrics[Performance] (Engine click through % and Transaction conv. %) - Yahoo US has the highest figure.             
#          Cost key metrics (Total cost, total cost per transaction, Avg CPC) - Google US has the highest cost in each category. However it also brings in many users. It alone has 2071 records out of 4510.MSN global has the lowest total cost and Avg CPC, while MSN US has lowest total cost per transaction.
#          Key conclusion - Overture Global and US have the highest impressions but have the lowest engine CTR. On average, Overture Global and US have the least total volume of booking and Amount generated. Hence it is optimal to look at Kayak as an option and invest less in Overture.

####################################################################################################################################################################################################################################################################################################################

#Broad or focused keywords
by_match_type <- air_france_data %>%
  mutate(Profit = Amount - Total_Cost) %>%
  group_by(Match_Type) %>%
  summarize(total_Amount = sum(Amount), mean_Amount = mean(Amount), total_Profit = sum(Profit), mean_Profit = mean(Profit))
by_match_type
ggplot(by_match_type, aes(x = Match_Type, y = total_Profit, fill = Match_Type)) + geom_col() + labs(x = "Match Type", y = "Total Profit", title = "Total Profit by Match Type", caption = "Data source: Air France Internet Marketing, Case #KEL319") + theme_air_france() + theme(axis.text.x = element_text(angle = 90))
ggplot(by_match_type, aes(x = Match_Type, y = mean_Profit, fill = Match_Type)) + geom_col() + labs(x = "Match Type", y = "Average Profit", title = "Average Profit by Match Type", caption = "Data source: Air France Internet Marketing, Case #KEL319") + theme_air_france() + theme(axis.text.x = element_text(angle = 90))

#INSIGHTS: Althoguh the Broad keywords have brought in larger total profits, on average the focused keywords bring in 700 times more profits than broad keywords.

####################################################################################################################################################################################################################################################################################################################

#Branded or unbranded keywords
branded_keywords <- air_france_data$Keyword_Group[grepl("Brand", air_france_data$Keyword_Group)]
branded_keywords

unbranded_keywords <- air_france_data$Keyword_Group[str_detect(air_france_data$Keyword_Group, "Brand", negate = TRUE)]
unbranded_keywords

customGreen = "#71CA97"

customRed = "#ff7f7f"

improvement_formatter <- 
  formatter("span", style = x ~ style(font.weight = "bold", color = ifelse(x > 10000, customGreen, ifelse(x < 10000, customRed, "black"))))

by_branded_keywords <- air_france_data %>%
  filter(grepl("Brand", air_france_data$Keyword_Group)) %>%
  summarize(Total_Amount = sum(Amount), Avg_Amount = round(mean(Amount), 0))
formattable(by_branded_keywords, align = c("l", "r"))

by_unbranded_keywords <- air_france_data %>%
  filter(str_detect(air_france_data$Keyword_Group, "Brand", negate = TRUE)) %>%
  summarize(Total_Amount = sum(Amount), Avg_Amount = round(mean(Amount), 0))
formattable(by_unbranded_keywords, align = c("l", "r"))

non_zero_vol_of_booking <- air_france_data %>%
  filter(Total_Volume_of_Bookings != 0)
non_zero_vol_of_booking$Total_Volume_of_Bookings
class(non_zero_vol_of_booking)

by_branded_keywords_cc <- non_zero_vol_of_booking %>%
  mutate(click_conversion = Clicks / Total_Volume_of_Bookings) %>%
  filter(grepl("Brand", non_zero_vol_of_booking$Keyword_Group)) %>%
  summarize(Avg_bid = round(mean(Search_Engine_Bid), 1), Avg_click_charges = round(mean(Click_Charges), 0), Avg_click_conv = round(mean(click_conversion), 1))
formattable(by_branded_keywords_cc, align = c("l", "c", "r"))

by_unbranded_keywords_cc <- non_zero_vol_of_booking %>%
  mutate(click_conversion = Clicks / Total_Volume_of_Bookings) %>%
  filter(str_detect(non_zero_vol_of_booking$Keyword_Group, "Brand", negate = TRUE)) %>%
  summarize(Avg_bid = round(mean(Search_Engine_Bid), 1), Avg_click_charges = round(mean(Click_Charges), 0), Avg_click_conv = round(mean(click_conversion), 1))
formattable(by_unbranded_keywords_cc, align = c("l", "c", "r"))

#INSIGHTS: On average the use of branded keywords brought higher revenue when compared to unbranded keywords(13258 Vs 654). However the average click conversion rate for unbranded keywords is more than twice as big as branded keywords (189 Vs. 79.1). Also the average bid as well as avegrage click chharges are higher for branded keywords than unbranded keywords.

####################################################################################################################################################################################################################################################################################################################

colnames(air_france_data)
mean(air_france_data$Search_Engine_Bid)
median(air_france_data$Search_Engine_Bid)
mean(air_france_data$Avg._Cost_per_Click)
median(air_france_data$Avg._Cost_per_Click)
mean(air_france_data$Engine_Click_Through_Perc)
median(air_france_data$Engine_Click_Through_Perc)
mean(air_france_data$Amount)
median(air_france_data$Amount)
mean(air_france_data$Total_Cost_Per_Trans)
median(air_france_data$Total_Cost_Per_Trans)

air_france_data$Invest_or_not <- c()
for (i in 1:nrow(air_france_data)) {
  if (air_france_data$Search_Engine_Bid < mean(air_france_data$Search_Engine_Bid) & air_france_data$Engine_Click_Through_Perc > mean(air_france_data$Engine_Click_Through_Perc) & air_france_data$Trans_Conv_Perc > mean(air_france_data$Trans_Conv_Perc) & air_france_data$Amount > mean(air_france_data$Amount)){
    air_france_data$Invest_or_not[i] <- 1
  }else{
    air_france_data$Invest_or_not[i] <- 0
  }
}
class(air_france_data$Invest_or_not)
view(air_france_data$Invest_or_not)  

air_france_data$Invest_or_not <- c()
for (i in nrow(air_france_data)) {
  if (air_france_data$Amount > mean(air_france_data$Amount)) {
    air_france_data$Invest_or_not[i] = 1
  } else if (air_france_data$Total_Cost_Per_Trans < mean(air_france_data$Total_Cost_Per_Trans)) {
    air_france_data$Invest_or_not[i] = 1
  } else if (air_france_data$Trans_Conv_Perc > mean(air_france_data$Trans_Conv_Perc)) {
    air_france_data$Invest_or_not[i] = 1
  } else {
    air_france_data$Invest_or_not[i] = 0
  }
}
summary(air_france_data$Invest_or_not)
view(air_france_data$Invest_or_not)


air_france_data$Invest_or_not <- c()
for (i in nrow(air_france_data)) {
  if (air_france_data$Amount > mean(air_france_data$Amount)) {
    air_france_data$Invest_or_not[i] = 1
  } else {
    air_france_data$Invest_or_not[i] = 0
  }
}

air_france_data %>% 
  mutate(Invest_or_Dont_Invest = ifelse(Amount > mean(Amount) & Total_Cost_Per_Trans < mean(Total_Cost_Per_Trans), "1", "0"))

view(air_france_data$inv)
