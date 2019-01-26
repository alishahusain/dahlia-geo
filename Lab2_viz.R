library(tidyverse)

object1 <- read.csv("China_EO_49to17.csv", fileEncoding = "latin1")
object2 <- as_tibble(object1)

Year <- seq(from=1949, to=2017)
object2$Year <- Year

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
