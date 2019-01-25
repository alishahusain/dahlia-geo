library(tidyverse)
library(ggplot2)

# file
data <- read.csv("China_EO_49to17.csv",fileEncoding = "latin1")
data <- as_tibble(file)

Year <- seq(from=1949, to=2017)
data$Year <- Year

# Returns a dataframe with the calculated output ratio of the
# two specified columns. We did not modify or remove the NA values 
# during the calculation portion as we believe that there may be a loss
# in data and may impact the analysis negatively. 
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
ggplot(data=out1, aes(x=Year, y=Ratios)) +
  geom_point(color="Blue") +
  xlab("Year") +
  ylab("Output Ratio") +
  ggtitle("Ratio Comparison between Liaoning and Guandong") +
  theme(plot.title = element_text(hjust=.5), axis.text.x = element_text(angle = 90, hjust=1))

## Creates a plot of the comparison between Beijing and Jilin
ggplot(data=out2, aes(x=Year, y=Ratios)) +
  geom_tile(color="DarkGreen", size=1) +
  xlab("Year") +
  ylab("Output Ratio") +
  ggtitle("Ratio Comparison between Beijing and Jilin") +
  theme(plot.title = element_text(hjust=.5), axis.text.x = element_text(angle = 90, hjust=1))

## Creates a plot of the comparison between InnerMongolia and Hainan
ggplot(data=out3, aes(x=Year, y=Ratios)) +
  geom_point(color="DarkOrange", size=1) +
  xlab("Year") +
  ylab("Output Ratio") +
  ggtitle("Ratio Comparison between InnerMongolia and Hainan") +
  theme(plot.title = element_text(hjust=.5), axis.text.x = element_text(angle = 90, hjust=1))

# Step 2

# Step 3
## Selected several provinces to analyze
six_prov <- data %>%
  select(Year, Beijing_Enterprise, Beijing_Output, Tianjin_Enterprise, Tianjin_Output,
        Shaanxi_Enterprise, Shaanxi_Output, Shanghai_Enterprise, Shanghai_Output, Heilongjiang_Enterprise, Heilongjiang_Output)

## Create a new dataframe consisting of ratios
ratios <- data.frame(data$Year)
colnames(ratios) <- c("Year")

# Utilizing a counter in a while loop to calculate all 4 ratios for 4 different provinces
i <- 2
while(i <= 10) {
  temp_df <- produce_ratios(colnames(six_prov)[i + 1], colnames(six_prov)[i])
  
  # New column name
  name <- gsub("_\\w+","_Ratios", colnames(six_prov)[i])
  ratios[[name]] <- temp_df$Ratios
  
  # Increase the counter
  i <- i + 2
}

ggplot(data=ratios, aes(x=Year)) +
  geom_line(aes(y=Beijing_Ratios, colour="Beijing")) +
  geom_line(aes(y=Tianjin_Ratios, colour="Tianjin")) +
  geom_line(aes(y=Shaanxi_Ratios, colour="Shaanxi")) +
  geom_line(aes(y=Shanghai_Ratios, colour="Shanghai")) +
  geom_line(aes(y=Heilongjiang_Ratios, colour="Heilongjiang")) +
  xlab("Year") +
  ylab("Ratio (Output per Enterprise)") +
  labs(color="Provinces") +
  ggtitle("Relationship Between Outputs and Enterprises") +
  theme(plot.title = element_text(hjust=.5), axis.text.x = element_text(angle = 90, hjust=1),legend.box = "horizontal")

### Summarize
# Our team noticed that over time the price per enterprise was consistent until it drastically increased around 1997. Similarly, the 
# plot shows that all five provinces increase at a similar rate. However, around 2010 Shanghai showed a large drop in price per
# enterprise in contrast to the trend that Beijing and Tianjin exhibited. Conversely, both Heilongjiang and Shanghai showed an interesting
# occurence since both province's trend just stopped around the 2010. Overall, our team noticed that over time all five province's price per
# enterprise was consistent until 2010 where the ratios started to differ.

