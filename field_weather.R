
library("tidyverse")
library("ggplot2")
library("zoo") # for rolling averages
library("patchwork") # to stitch plots together


w <- read.csv("weather_data.csv", header = TRUE)
w$year <- paste0("Y", as.character(w$year))
w$date <- as.character(w$date)
w$date <- as.Date(w$date, format = "%m/%d/%y")


weather_data <- w %>%
  select(location, year, date, soil_temp_F, precip_in) %>%
  group_by(year) %>%
  mutate(soil_temp_roll = rollmean(soil_temp_F, 7, na.pad = TRUE)) %>%
  mutate(precip_roll = rollmean(precip_in, 7, na.pad = TRUE))


w_2017 <- subset(weather_data, year == "Y2017") %>%
  subset(date >= as.Date("2017-05-01") & date <= as.Date("2017-10-31")) ##subset May 1st to October 31st


weather_plot_2017 <- ggplot(w_2017) + 
  geom_bar(mapping = aes(x = date, y = precip_roll * 100), stat = "identity", fill = "blue", color= "darkblue") +
  geom_line(mapping = aes(x = date, y = soil_temp_roll), color = "red", size = 1.5) +
  scale_y_continuous(name = expression("Soil temperature ("~degree~"F)"),
    sec.axis = sec_axis(~ . * 1/50 , name = "Precipitation (in)"), limits = c(0, 100)) +
  facet_wrap(~location) +
  theme_minimal() +
  theme(axis.title.x = element_blank()) +
  scale_x_date(date_labels = "%b-'%y", date_breaks="1 month") +
  ggtitle("Soil temperature and precipitation (7-day rolling avarage) at USDA field sites in Ithaca and Memphis NE")
  
#weather_plot_2017



w_2018 <- subset(weather_data, year == "Y2018") %>%
  subset(date >= as.Date("2018-05-01") & date <= as.Date("2018-10-31")) ##subset May 1st to October 31st


weather_plot_2018 <- ggplot(w_2018) + 
  geom_bar(mapping = aes(x = date, y = precip_roll * 100), stat = "identity", fill = "blue", color= "darkblue") +
  geom_line(mapping = aes(x = date, y = soil_temp_roll), color = "red", size = 1.5) +
  scale_y_continuous(name = expression("Soil temperature ("~degree~"F)"),
    sec.axis = sec_axis(~ . * 1/50 , name = "Precipitation (in)"), limits = c(0, 100)) +
  facet_wrap(~location) +
  theme_minimal() +
  theme(axis.title.x = element_blank()) +
  scale_x_date(date_labels = "%b-'%y", date_breaks="1 month")

#weather_plot_2018


# for info about date and time formatting see help page ?strptime

weather_plot_2017 + weather_plot_2018 + plot_layout(ncol = 1)
