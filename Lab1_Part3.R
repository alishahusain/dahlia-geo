# Author: Jin Chang
# Date: 1/25/2019

library(tidyverse)
library(ggplot2)

data <- read.csv("China_EO_49to17.csv",fileEncoding = "latin1")
data <- as_tibble(data)

Year <- seq(from=1949, to=2017)
data$Year <- Year

# Returns a dataframe with the calculated output ratio of the
# two specified columns. 
produce_ratios <- function(col1, col2) {
  # Create a data frame that contains the calculate the ratios.
  results <- data.frame(data$Year, data[[col1]], data[[col2]], data[[col1]] / data[[col2]])
  
  # Rename the column names
  colnames(results) <- c("Year", col1, col2, "Ratios")
  return(results)
}

# Ploting interesting comparisons
out1 <- produce_ratios("Liaoning_Output","Guangdong_Output")
out2 <- produce_ratios("Beijing_Output", "Jilin_Output")
out3 <- produce_ratios("InnerMongolia_Output","Hainan_Output")

## Creates a plot of the comparison between Liaoning and Guandong
plot1 <- ggplot(data=out1, aes(x=Year, y=Ratios)) +
  geom_line(color="Blue") +
  xlab("Year") +
  ylab("Output Ratio") +
  ggtitle("Ratio Comparison between Liaoning and Guandong") +
  theme(plot.title = element_text(hjust=.5))

## Creates a plot of the comparison between Beijing and Jilin
plot2 <- ggplot(data=out2, aes(x=Year, y=Ratios)) +
  geom_line(color="DarkGreen") +
  xlab("Year") +
  ylab("Output Ratio") +
  ggtitle("Ratio Comparison between Beijing and Jilin") +
  theme(plot.title = element_text(hjust=.5))

## Creates a plot of the comparison between InnerMongolia and Hainan
plot3 <- ggplot(data=out3, aes(x=Year, y=Ratios)) +
  geom_line(color="DarkOrange") +
  xlab("Year") +
  ylab("Output Ratio") +
  ggtitle("Ratio Comparison between InnerMongolia and Hainan") +
  theme(plot.title = element_text(hjust=.5))

# Step 2
# Retrieve all the enterprise columns 
sum_Enterprise <- data[,grep("_Enterprise", colnames(data))]

# Calculat the national total of Enterprises
data$Enterprise_Total <- rowSums(sum_Enterprise, na.rm = TRUE)

national_share <- data.frame(data$Year)
for(index in 1:ncol(sum_Enterprise)) {
  temp_df <- produce_ratios(colnames(sum_Enterprise)[index], "Enterprise_Total")
  
  # New column name
  name <- gsub("_\\w+","_Total_Ratio", colnames(sum_Enterprise)[index])
  national_share[[name]] <- temp_df$Ratios
}

national_share <- national_share[1:5]

national.plot <- ggplot(data=national_share, aes(x=Year)) +
  geom_line(aes(y=Beijing_Total_Ratio, colour="Beijing")) +
  geom_line(aes(y=Tianjin_Total_Ratio, colour="Tianjin")) +
  geom_line(aes(y=Hebei_Total_Ratio, colour="Hebei")) +
  geom_line(aes(y=Shanxi_Total_Ratio, colour="Shanxi")) +
  xlab("Year") +
  ylab("Percent of Enterprise Total") +
  labs(color="Provinces") +
  ggtitle("National Shares of The Total") +
  theme(plot.title = element_text(hjust=.5),legend.box = "horizontal")

# Step 3
## Selected several provinces to analyze
six_prov <- data %>%
  select(Year, Beijing_Enterprise, Beijing_Output, Tianjin_Enterprise, Tianjin_Output,
         Shaanxi_Enterprise, Shaanxi_Output, Shanghai_Enterprise, Shanghai_Output, Heilongjiang_Enterprise, Heilongjiang_Output)

## Create a new dataframe consisting of ratios
ratios <- data.frame(data$Year)
colnames(ratios) <- c("Year")

# Utilizing a counter as the index in a while loop as well as the position
i <- 2
while(i <= 10) {
  temp_df <- produce_ratios(colnames(six_prov)[i + 1], colnames(six_prov)[i])
  
  # New column name
  name <- gsub("_\\w+","_Ratios", colnames(six_prov)[i])
  ratios[[name]] <- temp_df$Ratios
  
  # Increase the counter
  i <- i + 2
}

output_enterprise <- ggplot(data=ratios, aes(x=Year)) +
  geom_line(aes(y=Beijing_Ratios, colour="Beijing")) +
  geom_line(aes(y=Tianjin_Ratios, colour="Tianjin")) +
  geom_line(aes(y=Shaanxi_Ratios, colour="Shaanxi")) +
  geom_line(aes(y=Shanghai_Ratios, colour="Shanghai")) +
  geom_line(aes(y=Heilongjiang_Ratios, colour="Heilongjiang")) +
  xlab("Year") +
  ylab("Ratio (Output per Enterprise)") +
  labs(color="Provinces") +
  ggtitle("Relationship Between Outputs and Enterprises") +
  theme(plot.title = element_text(hjust=.5),legend.box = "horizontal")

