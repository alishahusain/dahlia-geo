# Authors: Kevin Ho, Carmelita DeLeon, Alisha Husain
# 1/27/2019

library(tidyverse)
library(readr)
library(ggplot2)

object1 <- read.csv("China_EO_49to17.csv", fileEncoding = "latin1")
object2 <- as_tibble(object1)

Year <- seq(from=1949, to=2017)
object2$Year <- Year

# Author: Kevin Kho
# Filters dataset for years after 1975 first, then plots outputs against Year of several provinces
line_grph1 <- filter(object2, Year >= 1975) %>%
  ggplot() +
  geom_line(mapping = aes(x = Year, y = Guangdong_Output, color = "Guangdong"), size = 1.5, na.rm = TRUE) +
  geom_line(mapping = aes(x = Year, y = Fujian_Output, color = "Fujian"), size = 1.5, na.rm = TRUE) +
  geom_line(mapping = aes(x = Year, y = Qinghai_Output, color = "Qinghai"), size = 1.5, na.rm = TRUE) +
  geom_line(mapping = aes(x = Year, y = Xinjiang_Output, color = "Xinjiang"), size = 1.5, na.rm = TRUE) +
  scale_color_discrete(name = "Province") +
  scale_y_continuous(name = "Values in 100 Million RMB") +
  ggtitle("Output by Province, China, 1975 - 2017")

# Filters dataset for years after 1975 first, then plots Guandong Output + Enterpise with point and smooth geoms respectively
line_grph2 <- filter(object2, Year >= 1975) %>%
  ggplot() +
  geom_point(mapping = aes(x = Guangdong_Enterprise, y = Guangdong_Output), na.rm = TRUE) +
  geom_smooth(mapping = aes(x = Guangdong_Enterprise, y = Guangdong_Output), na.rm = TRUE) +
  scale_y_continuous(name = "Output (100 million RMB)") +
  scale_x_continuous(name = "Number of Enterprises") +
  ggtitle("Guangdong Province Enterprise & Output")

# Author: Carmelita DeLeon
# Filters dataset by selecting specific provinces and their output
selected_provinces <-
  object2 %>% select(Year, Shandong_Enterprise, Shandong_Output,
                        Beijing_Enterprise, Beijing_Output,
                        Shanghai_Enterprise, Shanghai_Output,
                        Jiangsu_Enterprise, Jiangsu_Output,
                        Liaoning_Enterprise, Liaoning_Output,
                        Guangdong_Enterprise, Guangdong_Output,
                        Yunnan_Enterprise, Yunnan_Output,
                        Gansu_Enterprise, Gansu_Output,
                        Xinjiang_Enterprise, Xinjiang_Output,
                        Qinghai_Enterprise, Qinghai_Output,
                        Ningxia_Enterprise, Ningxia_Output
                        )

master = ggplot(data=selected_provinces, 
                aes(x=Year, y=Shandong_Output,
                    color = "Shandong Enterprise" )) +
                    geom_line(size=1) +
                    xlab("Year") +
                    ylab("Output (100 million RMB)")

# Add each province to the plot
add_beijing <- geom_line(size = 1.5, data=selected_provinces,
                         aes(x=Year, y=Beijing_Output,
                             color = "Beijing Enterprise"))
add_shanghai <- geom_line(size = 1, data=selected_provinces,
                          aes(x=Year, y=Shanghai_Output,
                              color = "Shanghai Enterprise"))
add_jiangsu <- geom_line(size = 1.5, data=selected_provinces,
                         aes(x=Year, y=Jiangsu_Output,
                             color = "Jiangsu Enterprise"))
add_liaoning <- geom_line(size = 1.5, data=selected_provinces,
                          aes(x=Year, y=Liaoning_Output,
                              color = "Liaoning Enterprise"))

# Combine all the variables 
line_grph3 <- master + 
              add_beijing + 
              add_jiangsu + 
              add_liaoning + 
              add_shanghai +  
              ggtitle("China Province Enterprise Output Analysis (Based on Most Enterprises)") + 
              theme_update(plot.title = element_text(hjust = 0.5))

# Rename legend
legend <- line_grph3$labels$colour <- "Provinces" 

# Filters dataset by selecting specific provinces and their output
master2 = ggplot(data=selected_provinces, 
                 aes(x=Year, y=Yunnan_Output,
                     color =  "Yunnan" )) +
                     geom_line(size=1.5) + 
                     xlab("Year") + 
                     ylab("Output (100 million RMB)")

# Add each province to the plot, Beijing variable is already mentioned from above
add_guangdong <- geom_line(size = 1.5, data=selected_provinces,
                         aes(x=Year, y=Guangdong_Output,
                             color = "Guangdong"))
add_gansu <- geom_line(size = 1.5, data=selected_provinces,
                       aes(x=Year, y=Gansu_Output,
                           color = "Gansu"))
add_xinjiang <- geom_line(size = 1.5, data=selected_provinces,
                          aes(x=Year, y=Xinjiang_Output,
                              color = "Xinjiang"))
add_qinghai <- geom_line(size = 1.5, data=selected_provinces,
                         aes(x=Year, y=Qinghai_Output,
                             color = "Qinghai"))
add_ningxia <- geom_line(size = 1.5, data=selected_provinces,
                         aes(x=Year, y=Ningxia_Output,
                             color = "Ningxia"))

# Combine all the variables into one line graph
line_grph4 <- master2 +
              add_beijing +
              add_gansu +
              add_xinjiang +
              add_qinghai +
              add_ningxia + 
              ggtitle("China Province Enterprise Output Analysis (Starting Low Output)") +
              theme_update(plot.title = element_text(hjust = 0.5))

# Rename legend
legend <- line_grph4$labels$colour <- "Provinces" 

# Author: Alisha Husain
# Compares the two wealthiest provinces in China
line_grph5 <- ggplot(data = object2) +
  labs(title = "Differences in Economic Output in Beijing and Shanghai, China") +
  labs(subtitle = "1949-2017") +
  labs(y = "Output in units of 100 million RMB") +
  theme(axis.text.x = element_text(size = 6, angle = 90)) +
  geom_point(na.rm = TRUE, mapping = aes(x = Year, y = Beijing_Output), color = "blue") +
  geom_point(na.rm = TRUE, mapping = aes(x = Year, y = Shanghai_Output), color = "red") 