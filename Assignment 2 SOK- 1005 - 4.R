


rm(list = ls())
library(dplyr)
library(readr)
library(ggplot2)
library(tydyr)
library(rvest)

#Task 1.1

url <- "https://raw.githubusercontent.com/uit-sok-1005-v23/uit-sok-1005-v23.github.io/main/storedata.csv"
df <- read_csv(url)

df$Order_Date <- as.Date(df$Order_Date, format="%Y-%m-%d")

df$Year <- format(df$Order_Date, "%Y")
df$Month <- format(df$Order_Date, "%m")

filtered_df <- df %>%
  filter(Year == "2017", Month %in% c("10", "11", "12"), 
         Region %in% c(1, 9), Customer_Segment %in% c("Corporate", "Consumer"))

table_1 <- filtered_df %>%
  group_by(Month, Region, Customer_Segment) %>%
  summarise(Total_Sales = sum(Sales, na.rm = TRUE))

print(table_1)


#Task 1.2 (note: for now not working, fix before finished!!!)

df$Month <- as.numeric(df$Month)

monthly_sales <- df %>%
  filter(Year %in% c("2015", "2016", "2017"), Region %in% c(1, 13)) %>%
  group_by(Year, Month, Region) %>%
  summarise(Total_Sales = sum(Sales, na.rm = TRUE), .groups = "drop") %>%
  mutate(Date = as.Date(paste(Year, Month, "01", sep="-"), format="%Y-%m-%d"))

ggplot(monthly_sales, aes(x = Date, y = Total_Sales, color = as.factor(Region))) +
  geom_line(size=1.2) +
  labs(title = "Monthly total sales from 2015 to 2017",
       x = "Date",
       y = "Total Sales",
       color = "Region") +
  theme_minimal()


#Task 1.3

sales_comparison <- monthly_sales %>%
  tidyr::pivot_wider(names_from = Region, values_from = Total_Sales, values_fill = list(Total_Sales = 0)) %>%
  rename(Sales_Region_1 = `1`, Sales_Region_13 = `13`) %>%
  filter(Sales_Region_13 > Sales_Region_1) %>%
  arrange(Year, Month)


#Task 1.4 (note: come back to, hard to start)


#Task 2 (note: problems with plot)

#task 2a (note: numbers too big)

url <- "https://www.motor.no/aktuelt/motors-store-vintertest-av-rekkevidde-pa-elbiler/217132"
webpage <- read_html(url)
table_data <- html_table(html_nodes(webpage, "div table")[[1]])

colnames(table_data) <- c("Car", "WLTP", "STOPP") 
table_data$WLTP <- as.numeric(gsub("[^0-9]", "", table_data$WLTP)) 
table_data$STOPP <- as.numeric(gsub("[^0-9]", "", table_data$STOPP)) 
table_data <- table_data %>% filter(!is.na(WLTP) & !is.na(STOPP))

model <- lm(STOPP ~ WLTP, data = table_data)
summary(model)

ggplot(table_data, aes(x = WLTP, y = STOPP)) +
  geom_point(color = "darkblue", size = 3) +
  geom_smooth(method = "lm", color = "black", se = FALSE) +
  geom_abline(slope = 1, intercept = 0, color = "darkred", linetype = "dashed", size = 1.2) +
  labs(title = "Electric Car Range: Expectations vs Actuality",
       x = "WLTP Stated Range in km",
       y = "Actual Tested Range in km") +
  theme_minimal()

#task 2b (note: not sure exactly how to go with this)


