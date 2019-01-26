# Authors: Jin Chang and Billy Wang
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

pop1 = read.csv("GrossDomPop_byProvince.csv")
pop2=as_tibble(pop1)

growthRate = read.csv("GrowthRate.csv")
growthRate2=as_tibble(growthRate)
natural_growthRate <- growthRate2[3,]
years <- c("1999","2000","2001","2002","2003","2004","2005","2006","2007","2008","2009", "2010","2011","2012","2013","2014","2015","2016","2017")

by_year_desc <- data[order(-data$Year),]
mergedTable <- merge(by_year_desc, pop2, by.x="Year")
province2000 <- function(province) {
  output <- filter(mergedTable, Year >= '2000') %>% select(Year,paste0(province,"_Enterprise"), paste0(province,"_GrossDomesticPopulation"))
  output2 <- mutate(output, ratio = output[,2]/output[,3])
  return(output2)
}
beijingEnterprise_Pop <- province2000("Beijing")
print(beijingEnterprise_Pop)

provinceList <- c("Beijing", "Tianjin", "Hebei", "Shanxi", "InnerMongolia", "Liaoning", "Jilin", "Heilongjiang", "Shanghai")
bigMerge <- function(names) {
  bigMergedTable <- filter(mergedTable, Year >= '2000')
  for (i in 1: length(names)) {
    currentEnter <- bigMergedTable %>% select(paste0(names[i],"_Enterprise"))
    currentPop <- bigMergedTable %>% select(paste0(names[i], "_GrossDomesticPopulation"))
    print(currentEnter/currentPop)
    print(ggplot(province2000(names[i]), aes(x = Year, y = ratio)) + 
            geom_line(color = "#660066") +
            geom_point(color = "#FF8000") +
            labs(x = "Year", y = "Ratio"))
  }
  return (bigMergedTable)
}

cpy <- data %>%
  filter(Year >= 2000)
cpy <- cbind(cpy, pop1)
i <- 2
dflist <- list()
while(i <= ncol(pop1)) {
  name <- gsub("_GrossDomesticPopulation","", colnames(pop1)[i])
  dflist[[i]] <- cpy[,grep(name, colnames(cpy))]
  i <- i + 1
}