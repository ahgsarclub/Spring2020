


library("ggplot2")
library("tidyverse")


#### import data
covid <- read.csv("covid_data.csv")
covid$date <- as.Date(covid$date)


#### USA
US <- covid %>%
  filter(location == "United States") %>%
  gather("metric", "counts", -date, -location)

ggplot(US, aes(x = date, y = counts, color=metric)) +
  geom_point()


#### CHINA
China <- covid %>%
  filter(location == "China") %>%
  gather("metric", "counts", -date, -location)

ggplot(China, aes(x = date, y = counts, color=metric)) +
  geom_point()


#### World excluding China
World_ex_China <- covid %>%
  filter(!(location %in% c("World", "China"))) %>%
  select(-location) %>%
  group_by(date) %>%
  summarize_all(sum, na.rm = TRUE) %>%
  gather("metric", "counts", -date)
  
World_ex_China$metric <- factor(World_ex_China$metric, levels = c("total_cases", "new_cases","total_deaths", "new_deaths"))

ggplot(World_ex_China, aes(x = date, y = counts, color=metric)) +
  geom_point() +
  labs(title = "COVID-19 Cases (Outside China)") +
  theme_minimal() +
  theme(axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    legend.title=element_blank(),
    #legend.position = c(0.2, 0.8),
    legend.background = element_rect(fill = "#f9f9f9", colour = "#aaaaaa"))
  
  




