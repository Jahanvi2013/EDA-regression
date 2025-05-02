#loading all the required libraries
library(dplyr)
library(ggplot2)
library(lubridate)

#reading in the data
data<- read.csv(file = 'MavenRail.csv', header = TRUE, sep= ",")
data

#getting a gist of the data
summary(data)
str(data)
colSums(is.na(data))

#converting Departure, Scheduled Arrival, and Actual Arrival into POSIXct
data$Departure <- as.POSIXct( data$Departure, format="%Y-%m-%d %H:%M" )
data$Scheduled.Arrival <- as.POSIXct( data$Scheduled.Arrival, format="%Y-%m-%d %H:%M" )
data$Actual.Arrival <- as.POSIXct( data$Actual.Arrival, format="%Y-%m-%d %H:%M" )
sapply( data, class )

#converting columns into factors for easier analysis
data$Journey.Status <- as.factor(data$Journey.Status)
data$Refund.Request <- as.factor(data$Refund.Request)
data$Payment.Method <- as.factor(data$Payment.Method)
data$Railcard <- as.factor(data$Railcard)
data$Ticket.Class <- as.factor(data$Ticket.Class)
data$Ticket.Type <- as.factor(data$Ticket.Type)
data$Reason.for.Delay <- as.factor(data$Reason.for.Delay)

#removing rows where departure is NA
data <- data[!is.na(data$Departure), ]

#removing rows where both Scheduled.Arrival and Actual.Arrival are NA
data <- data[!(is.na(data$Scheduled.Arrival) & is.na(data$Actual.Arrival)), ]

#2 EDA

#Chi square test on Journey.Status and Refund.Request
contingency_table <- table(data$Journey.Status, data$Refund.Request)
chi_square_test <- chisq.test(contingency_table)
print(chi_square_test)

#Bar Graph
refunds_by_status <- data %>%
  group_by(Journey.Status) %>%
  summarise(Refund.Count = sum(as.numeric(Refund.Request == "Yes"), na.rm = TRUE),
            Total.Count = n()) %>%
  mutate(Refund.Rate = Refund.Count / Total.Count)

ggplot(refunds_by_status, aes(x = Journey.Status, y = Refund.Rate, fill = Journey.Status)) +
  geom_bar(stat = "identity") +
  labs(title = "Refund Rate by Journey Status", y = "Refund Rate", x = "Journey Status")

#Chi square on Ticket.Type and Refund.Request
contingency_table1 <- table(data$Ticket.Type, data$Refund.Request)
chi_square_test1 <- chisq.test(contingency_table1)
print(chi_square_test1)

#Bar Graph
refunds_by_ticket <- data %>%
  group_by(Ticket.Type) %>%
  summarise(Refund.Count = sum(as.numeric(Refund.Request == "Yes"), na.rm = TRUE),
            Total.Count = n()) %>%
  mutate(Refund.Rate = Refund.Count / Total.Count)

ggplot(refunds_by_ticket, aes(x = Ticket.Type, y = Refund.Rate, fill = Ticket.Type)) +
  geom_bar(stat = "identity") +
  labs(title = "Refund Rate by Ticket Type", y = "Refund Rate", x = "Ticket Type")

#Chi-Square Test for Railcard and Refund Request
railcard_refund_table <- table(data$Railcard, data$Refund.Request)
chi_square_railcard <- chisq.test(railcard_refund_table)
chi_square_railcard

#Bar graph
ggplot(data, aes(x = Railcard, fill = Refund.Request)) +
  geom_bar(position = "dodge") +
  labs(title = "Railcard vs Refund Request",
       x = "Railcard",
       y = "Count",
       fill = "Refund Request") +
  scale_fill_manual(values = c("red", "lightgreen")) + 
  theme_minimal()

#Most and least popular routes
routes <- data %>%
  group_by(Departure.Station, Arrival.Station) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count))

top_routes <- head(routes, 10)
bottom_routes <- tail(routes, 10)

ggplot(top_routes, aes(x = reorder(paste(Departure.Station, Arrival.Station, sep = " -> "), Count), y = Count)) +
  geom_bar(stat = "identity", fill = "indianred") +
  coord_flip() +
  labs(title = "Top 10 Most Popular Routes", y = "Count", x = "Route")

#Frequency of causes for Cancelled Journey.Status
cancellations <- data %>%
filter(Journey.Status == "Cancelled") %>%
  count(Reason.for.Delay, sort = TRUE)

ggplot(cancellations, aes(x = reorder(Reason.for.Delay, n), y = n, fill = Reason.for.Delay)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Main Causes of Train Cancellations", y = "Count", x = "Reason for Delay")

#understanding peak hours
#extracting hours and coding it as AM/PM
data$Hour <- format(data$Departure, "%H") 
data$Hour <- as.numeric(data$Hour)  
data$TimeOfDay <- ifelse(data$Hour < 12, "AM", "PM")

#creating the plot
ggplot(data, aes(x = Hour, fill = TimeOfDay)) +
  geom_histogram(binwidth = 1, position = "dodge", color = "black") +
  scale_x_continuous(breaks = 0:23) +  # Set breaks for 24 hours
  labs(title = "Peak Hours of Train Journeys",
       x = "Hour of the Day",
       y = "Number of Journeys",
       fill = "Time of Day") +
  theme_minimal() +
  scale_fill_manual(values = c("AM" = "darkgoldenrod2", "PM" = "cadetblue4"))

#Heatmap for frequency of departures
#making new columns for hours and weekdays
#data$Hour <- hour(data$Departure)
data$Weekday <- wday(data$Departure, label = TRUE)

#Counting the number of departures for each combination of hour and weekday
departure_counts_heatmap <- data %>%
  group_by(Weekday, Hour) %>%
  summarise(Departures = n()) %>%
  ungroup()

#Creating a heatmap for frequency of departures by hour and weekday
ggplot(departure_counts_heatmap, aes(x = Hour, y = Weekday, fill = Departures)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(title = "Heatmap: Frequency of Departures by Hour and Weekday",
       x = "Hour of Day",
       y = "Day of Week",
       fill = "Departures") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#3 Adding the DelayInMinutes column
data$DelayInMinutes <- ifelse(
  data$Journey.Status == "Delayed", 
  as.numeric(difftime(data$Actual.Arrival, data$Scheduled.Arrival, units = "mins")),
  NA 
)

#4 Focusing on rows where Journey.Status is not "On Time"
filtered_df <- data[data$Journey.Status != "On Time", ]

#Adding the MediumPrice column
filtered_df$MediumPrice <- ifelse(filtered_df$Price > 10 & filtered_df$Price <= 30, TRUE, FALSE)
head(filtered_df)

#MODEL (logistic regression)
model <- glm(Refund.Request ~ MediumPrice, data = filtered_df, family = binomial)
summary(model)

#5

#MODEL 1
#keeping weather as the reference category
data$Reason.for.Delay <- relevel(data$Reason.for.Delay, ref = "Weather")
model1 <- glm(Refund.Request ~ Journey.Status + Reason.for.Delay, 
              data = data, family = binomial)
summary(model1)

#MODEL 2 
model2 <- glm(Refund.Request ~ Journey.Status + Payment.Method + Railcard, 
              data = data, family = binomial)
summary(model2)

#MODEL 3
model3 <- glm(Refund.Request ~ Journey.Status + Ticket.Type + Ticket.Class,
              data = data, family = binomial)
summary(model3)

#MODEl 4
model4 <- glm(Refund.Request ~ Journey.Status,
              data = data, family = binomial)
summary(model4)

#Using models with the ToPredict dataset
data1<- read.csv(file = 'ToPredict.csv', header = TRUE, sep= ",")
data1

#adding probabilities as a new column
data1$Refund_Probability <- predict(model4, newdata = data1, type = "response")
