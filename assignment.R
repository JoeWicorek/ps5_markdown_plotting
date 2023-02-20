library(tidyverse)


##1-2
gapdata <- read_delim("gapminder.csv")
nrow(gapdata)
ncol(gapdata)
##1-3
head(gapdata,5)
##2-1
distinct_count <- gapdata %>%
  summarise(iso3_distinct = n_distinct(iso3),
            iso2_distinct = n_distinct(iso2),
            name_distinct = n_distinct(name))
# print the results
print(distinct_count)
##2-2
##a
# print unique values in name for each iso2 value if there is more than one
gap_filtered <- gapdata %>%
  group_by(iso2) %>%
  summarize(unique_values = if_else(n_distinct(name) > 1, paste(unique(name), collapse = ", "), ""))
# only print rows where unique_values is not empty
print(gap_filtered[gap_filtered$unique_values != "",])
##b
# print unique values in name for each iso3 value if there is more than one
gap_filtered <- gapdata %>%
  group_by(name) %>%
  summarize(unique_values = if_else(n_distinct(iso3) > 1, paste(unique(iso3), collapse = ", "), ""))
# only print rows where unique_values is not empty
print(gap_filtered[gap_filtered$unique_values != "",])
##2-3
min_year <- min(gapdata$time, na.rm = TRUE)
max_year <- max(gapdata$time, na.rm = TRUE)
min_year
max_year
##3-1
missing_gap <- gapdata %>%
  group_by(time) %>%
  summarize(num_missing = sum(is.na(co2))) %>%
  arrange(desc(num_missing))
##print(missing_gap)
missing_gap <- gapdata %>%
  group_by(time) %>%
  summarize(num_missing = sum(is.na(co2_PC))) %>%
  arrange(desc(num_missing))
##print(missing_gap)
most_missing <- filter(missing_gap, num_missing == max(num_missing))
print(most_missing)
##3-2
# Filter the data to only include the U.S, China, and India, Russia, Canada
mydata_filtered <- gapdata %>% 
  filter(name %in% c("United States of America", "China", "India", "Russian Federation", "Canada"))
# Create the plot
ggplot(mydata_filtered, aes(x = time, y = co2, color = name)) +
  geom_line() +
  labs(x = "Year", y = "Total CO2 Emissions (Millions of metric tons)", 
       title = "Total CO2 Emissions over Time for the U.S, China, and India") +
  scale_color_manual(values = c("#0072B2", "#D55E00", "#009E73", "#E69F00", "56B4E9"))  # Set custom colors for each country
#china by far has the highest and most dramatic increase in emissions, US has been dropping since about 2005
##3-3
# Filter the data to only include the U.S, China, and India, Russia, Canada
mydata_filtered <- gapdata %>% 
  filter(name %in% c("United States of America", "China", "India", "Russian Federation", "Canada"))
# Create the plot
ggplot(mydata_filtered, aes(x = time, y = co2_PC, color = name)) +
  geom_line() +
  labs(x = "Year", y = "Total CO2 Emissions (Millions of metric tons)", 
       title = "Total CO2 Emissions over Time for the U.S, China, and India") +
  scale_color_manual(values = c("#0072B2", "#D55E00", "#009E73", "#E69F00", "56B4E9"))  # Set custom colors for each country
#Countries are much closer in emissions on a per capita basis. China is still rising per capital and other countries are falling.
##3-4
co2_per_capita_by_country <- gapdata %>% 
  select(time,name, region, co2_PC) %>% 
  filter(!is.na(region) & !is.na(co2_PC))
# Group the data by continent and calculate the mean of CO2_per_capita
avg_co2_per_capita_by_continent <- co2_per_capita_by_country %>% 
  group_by(region) %>% 
  summarize(avg_co2_per_capita = mean(co2_PC))
# Print the results
print(avg_co2_per_capita_by_continent)
##3-5
co2_per_capita_by_continent_1960_2016 <- co2_per_capita_by_country %>% 
  filter(time %in% c(1960, 2016))
# Group the data by year and continent and calculate the mean of CO2_per_capita
avg_co2_per_capita_by_continent_1960_2016 <- co2_per_capita_by_continent_1960_2016 %>% 
  group_by(time, region) %>% 
  summarize(avg_co2_per_capita = mean(co2_PC))
# Plot the results
ggplot(avg_co2_per_capita_by_continent_1960_2016, aes(x = region, y = avg_co2_per_capita, fill = factor(time))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Average CO2 Emissions per Capita by Continent", x = "Continet", y = "CO2 Emissions per Capita", fill="Year") +
  scale_fill_manual(values = c("blue", "red")) +
  theme_minimal()
##3-6


