#Shopify Challenge - Data Science
#We know that we have a total of 100 sneaker shops and each of them sell only one model of shoes
#importing the libraries that we need
install.packages("magrittr") # package installations are only needed the first time you use it
install.packages("dplyr")    # alternative installation of the %>%
install.packages("data.table")           # Install and load data.table
install.packages("lubridate")
install.packages("ggplot2")
library(magrittr)
library(magrittr) # needs to be run every time you start R and want to use %>%
library(dplyr)    # alternatively, this also loads %>%
library(FSA)
library(FSAdata)
library(magrittr)
library(plyr)
library(dplyr)
library(tidyr)
library(plyr)
library(tidyverse)
library("data.table")
library(lubridate)
library(ggplot2)
#import dataset in a variable called data
data<- read.csv("~/Documents/Shopify/Data Science/2019 Winter Data Science Intern Challenge Data Set - Sheet1.csv")
View(data)

#Check if values are missing
is.na(data)
#as in the output we see that there are no missing values, the data is continuous and we can calculate the AOV

#Average order value (AOV) means the total revenue divided by the number of orders. Thus we take the sum of all the order_amount and divide it by the sum of total_items and check if we get the answer as $3145.13
assumed_mean <- mean(data$order_amount)
assumed_mean
#As we can see, the assumed mean is actually showing up to be a 3145.128 or approximately 3145.13

#But, the summary statistics will tell us a better picture
summary(data$order_amount) #Median = 284; Minimum = 90 (that is okay); Maximum = 704,000 (? - very expensive sneakers)
sd(data$order_amount) #Standard Deviation = 41282.54
#the standard deviation is extremely high, which means that the data is heavily spread out and that some datapoints may be causing this as they are outliers

#lets plot a scatter plot to figure out which data is lying outside the limits
#summarising the months in a data
#data$order_amount %>% group_by(month=floor_date(data$created_at, "month")) %>%
#  summarize(amount=sum(data$order_amount))

#editing the created_at column to have only dates
data$created_at <- format(as.POSIXct(data$created_at,format='%Y-%m-%d %H:%M:%S'),format='%Y-%m-%d')
data$created_at<- as.numeric(data$created_at)

# plot the data using ggplot
options(scipen = 5)
q<- ggplot(data = data, aes(x = created_at, y = order_amount, color = shop_id, cex = 3)) +
  geom_point() +
  labs(x = "Date",
       y = "Orders",
       title = "Orders vs Date Chart",
       subtitle = "Outlier Detection")
q + theme(axis.text.x = element_text(angle = 90))

#from the data we can tell that there are outliers (mostly in shop number 42, 78)
#plot only for shop 42 and 78 to know better
shop42 <- data[data$shop_id == 42, ]
View(shop42)
summary(shop42)
#from the summary we can see that the order_amount is 704000

shop78 <- data[data$shop_id == 78, ]
View(shop78)
summary(shop78)
# we can see that the maximum is 154450, these are clearly our outliers and need to be evaluated
#This is the answer to the first question
# Create the function.
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
getmode(shop42$total_items)
getmode(shop78$total_items)

#AOV for shop 78 (total of amounts/ total items)
AOV_78 <- sum(shop78$order_amount)/sum(shop78$total_items)
AOV_78

#AOV for shop 42
AOV_42 <- sum(shop42$order_amount)/sum(shop42$total_items)
AOV_42

#AOV for the rest
data <- data[!data$shop_id == 42, ]
