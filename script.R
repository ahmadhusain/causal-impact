
# load libraries ----------------------------------------------------------

library(tidyverse)
library(CausalImpact)
library(readxl)
library(forecast)
library(TSstudio)
library(lubridate)


# read data ---------------------------------------------------------------

data <- read_csv("http://bit.ly/causal-impact-data")

glimpse(data)


# data wrangling ----------------------------------------------------------

actual <- data %>% 
  mutate(
    datetime = lubridate::as_datetime(as.character(datetime))
  ) %>% 
  dplyr::select(datetime, unique_views) %>% 
  na.omit()

head(actual)


# Exploratory Data --------------------------------------------------------

actual %>% 
  mutate(
    wdays = lubridate::wday(datetime, label = TRUE)
  ) %>% 
  group_by(wdays) %>% 
  summarise(total_views = sum(unique_views)) %>% 
  ungroup() %>% 
  mutate(
    label = scales::comma(total_views)
  ) %>% 
  ggplot(
    mapping = aes(x = wdays, y = total_views)
  ) +
  geom_col(fill = "steelblue", alpha  = 0.7) +
  labs(
    title = "Total Views Per Days",
    subtitle = "Period: May to July",
    y = NULL,
    x = "Day of Week"
  ) +
  geom_text(
    aes(label = label, y = total_views + max(total_views) * 0.075) , size = 3
  ) +
  theme_minimal()


# pre-post campaign -------------------------------------------------------

pre_campaign <- actual %>% 
  slice(1:37)


# time series estimation --------------------------------------------------

ts_campaign <- ts(pre_campaign$unique_views, frequency = 7)
fit_hw <- HoltWinters(ts_campaign)
forecast <- forecast(fit_hw, 16)

forecast_data <- data_frame(
  datetime = as_datetime(
    seq.Date(
      from = as.Date("2018-06-24"),
      by = "day",
      length.out = 16)
  ),
  unique_views = forecast$mean
)

append_data <- pre_campaign %>% bind_rows(forecast_data)


# forecast projection if not any 'intervention' ---------------------------

ggplot(data = append_data, mapping = aes(x = datetime, y = unique_views)) +
  geom_line(col = "steelblue", alpha = 0.5, size = 1.2) +
  geom_point(col = "black", size = 1.5) +
  labs(
    title = "Forecast Projection",
    y = "Total Unique Views"
  ) +
  theme_minimal()


# actual data -------------------------------------------------------------

actual %>% 
  ggplot(mapping = aes(x = datetime, y = unique_views)) + 
  geom_line(color = "steelblue", size = 1.2, alpha = 0.6) +
  geom_point(col = "black", size = 1.5) +
  labs(
    title = "Actual data on the number of website visitors",
    subtitle = "16 May to 17 July",
    y = NULL
  ) +
  theme_minimal()


# build causal impact analysis --------------------------------------------

pre <- c(1,37)
post <- c(38, 53)

pre <- as.Date(c("2018-05-16", "2018-06-24"))
post <- as.Date(c("2018-06-25", "2018-07-10"))

time.points <- seq.Date(as.Date("2018-05-16"), by = "days", length.out = 53)
data_ci <- zoo(
  cbind(actual$unique_views, append_data$unique_views), 
  time.points
)


# results -----------------------------------------------------------------

impact <- CausalImpact(data = data_ci, pre.period = pre, post.period = post)
plot(impact)


# summary anaylsis --------------------------------------------------------

summary(impact)


# reporting format --------------------------------------------------------

interpretation <- summary(impact, "report")
