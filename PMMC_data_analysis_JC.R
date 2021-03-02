# necessary package(s)
library(tidyverse)
library(lubridate)

# raw data
bight_data <- read_csv("SoCal 2006-2019.csv") # 18,720 entries total

# data tidying ------------------------------------------------------------

# create new dataframe of stranding events for living CA sea lions, with only interested variables (date, county, latitude/longitude, age group, sex, length, weight)
CSL <- bight_data %>% 
  rename(CommonName = 'Common Name', ID = 'National Database Number', LatUnits = 'Latitude Units', LongUnits = 'Longitude Units', Date = 'Observation Date', Status = 'Observation Status', Age = "Age Class", WeightUnits = 'Weight Units', LengthUnits = 'Length Units') %>% 
  filter(CommonName == "Sea lion, California", Status == "Alive") %>% 
  mutate(Date = mdy(Date), Year = year(Date), Month = months(Date, abbr = TRUE)) %>%
  select("County", "ID", "Latitude", "LatUnits", "Longitude", "LongUnits", "Date", "Month", "Year", "Sex", "Age", "Weight", "WeightUnits", "Length", "LengthUnits") # 11,712 entries total

# conforming units for length and weight (NOTE: still need to adjust lat/long units)
CSL_clean <- CSL %>% 
  mutate(Weight_kg = Weight * case_when(WeightUnits == "lb" ~ 0.45359,
                                      WeightUnits == "kg" ~ 1)) %>%
  mutate(Length_cm = Length * case_when(LengthUnits == "in" ~ 2.54,
                                      LengthUnits == "cm" ~ 1)) %>%
  select(-c(WeightUnits, LengthUnits, Weight, Length))


# data visualization ------------------------------------------------------

# abundance summary of stranding events by year, life stage, and sex
demo_summary <- CSL_clean %>% 
  count(Year, Sex, Age) %>% 
  na_if("UNKNOWN") %>% 
  na.omit() %>%
  mutate(Age = as_factor(Age)) %>% 
  mutate(Age = fct_relevel(Age, "PUP/CALF", "YEARLING", "SUBADULT", "ADULT")) # turns age into a factor and reorders

demo_summary2 <- CSL_clean %>% 
  count(Month, Year, Sex) %>% # includes months
  na_if("UNKNOWN") %>% 
  na.omit() %>%
  # mutate(Age = as_factor(Age)) %>% 
  # mutate(Age = fct_relevel(Age, "PUP/CALF", "YEARLING", "SUBADULT", "ADULT")) %>% 
  mutate(Month = as_factor(Month)) %>% 
  mutate(Month = fct_relevel(Month, "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))


# initial plot of observation counts between 2006-2019 across different life stages, between male and females
counts_plot <- ggplot(demo_summary, aes(x = Year, y = n, color = Sex)) +
  geom_line() +
  theme_bw() +
  labs(y = "Observation Count") +
  facet_wrap(~Age)
  # facet_wrap(~Age, scales = "free_y") # Dates are the same in each plot, but the y will have different scales

plot(counts_plot)
# ggsave("counts_plot.png")
# ggsave("counts_plot_free_y.png")


# plotting observation counts by month, per year, only looking at sex
yearly_plots_female <- demo_summary2 %>%
  ggplot(aes(Month, y = n)) +
  geom_path() +
   geom_point(size = 2) +
   theme_bw() +
   labs(y = "Observation Count")
   facet_wrap(~Year)

plot(yearly_plots)
# ggsave("yearly_plot.png")

# test
