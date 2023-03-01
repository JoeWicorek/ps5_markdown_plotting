library(tidyverse)


##1-2
gapdata <- read_delim("gapminder.csv")
nrow <- nrow(gapdata)
ncol <- ncol(gapdata)
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
# Select the relevant columns and remove rows with missing data
co2_per_capita_by_country <- gapdata %>%
  select(time, region, name, co2_PC) %>%
  filter(!is.na(region) & !is.na(name) & !is.na(co2_PC))
#co2_per_capita_by_country
# Filter for 2019
co2_per_capita_2019 <- co2_per_capita_by_country %>%
  filter(time == 2016)
#co2_per_capita_2019
# Group the data by continent and country and calculate the mean of CO2_per_capita
avg_co2_per_capita_by_continent_country <- co2_per_capita_2019 %>%
  group_by(region, name) %>%
  summarize(avg_co2_per_capita = mean(co2_PC))
#avg_co2_per_capita_by_continent_country
# Find the three largest CO2 emitters per continent
largest_emitters <- avg_co2_per_capita_by_continent_country %>%
  group_by(region) %>%
  top_n(3, avg_co2_per_capita)
# Find the three smallest CO2 emitters per continent
smallest_emitters <- avg_co2_per_capita_by_continent_country %>%
  group_by(region) %>%
  slice_tail(n = 3) %>%
  arrange(region, avg_co2_per_capita)
# Print the results
cat("The three largest CO2 emitters per continent in 2019 are:\n")
print(largest_emitters)
cat("\nThe three smallest CO2 emitters per continent in 2019 are:\n")
print(smallest_emitters)
#4-1
# Select the relevant columns and remove rows with missing data
data_1960 <- gapdata %>%
  filter(time == 1960 & !is.na(GDP_PC) & !is.na(lifeExpectancy) & !is.na(region) & !is.na(name))

# Create a named vector of colors for each continent
continent_colors <- c("Africa" = "red", "Asia" = "green", "Europe" = "blue", "Americas" = "purple", "Oceania" = "orange")

# Create a named vector of sizes for each country
country_sizes <- setNames(data_1960$totalPopulation / 1000000, data_1960$name)

# Create the plot
ggplot(data_1960, aes(x = GDP_PC, y = lifeExpectancy, color = region)) +
  geom_point(aes(size = country_sizes), alpha = 0.6) +
  scale_color_manual(values = continent_colors) +
  scale_size_continuous(range = c(1, 10)) +
  labs(title = "GDP per capita vs. life expectancy by country in 1960", x = "GDP per capita (USD)", y = "Life expectancy (years)") +
  theme_bw()
# the poorer countries (by GDP) have a lower life expectancy
#4-2
# Select the relevant columns and remove rows with missing data
data_1960 <- gapdata %>%
  filter(time == 2019 & !is.na(GDP_PC) & !is.na(lifeExpectancy) & !is.na(region) & !is.na(name))

# Create a named vector of colors for each continent
continent_colors <- c("Africa" = "red", "Asia" = "green", "Europe" = "blue", "Americas" = "purple", "Oceania" = "orange")

# Create a named vector of sizes for each country
country_sizes <- setNames(data_1960$totalPopulation / 1000000, data_1960$name)

# Create the plot
ggplot(data_1960, aes(x = GDP_PC, y = lifeExpectancy, color = region)) +
  geom_point(aes(size = country_sizes), alpha = 0.6) +
  scale_color_manual(values = continent_colors) +
  scale_size_continuous(range = c(1, 10)) +
  labs(title = "GDP per capita vs. life expectancy by country in 1960", x = "GDP per capita (USD)", y = "Life expectancy (years)") +
  theme_bw()
#4-3
#we are improving the life expectancy of lower GDP countries (with exception of Africa)
#4-4
# Compute the average life expectancy for each continent in 1960 and 2019
life_expectancy_avg <- gapdata %>%
  filter(!is.na(lifeExpectancy) & !is.na(time) & time %in% c(1960, 2019) & !is.na(region)) %>%
  group_by(region, time) %>%
  summarize(avg_life_expectancy = mean(lifeExpectancy))

# Print the results
print(life_expectancy_avg)
#Yes
#4-5
# Compute the average life expectancy growth from 1960 to 2019 by continent
life_expectancy_growth <- gapdata %>%
  filter(!is.na(lifeExpectancy) & !is.na(time) & time %in% c(1960, 2019) & !is.na(region)) %>%
  group_by(region) %>%
  mutate(avg_growth = (lifeExpectancy - lag(lifeExpectancy))/lag(lifeExpectancy)) %>%
  filter(time == 2019) %>%
  summarize(avg_growth = mean(avg_growth, na.rm = TRUE)) %>%
  arrange(desc(avg_growth))

# Print the results
print(life_expectancy_growth)
#the life expectancy growth is higher for low GDP countries
#4-6
gdp_1960_2019 <- gapdata %>% 
  filter(time %in% c(1960, 2019)) %>% 
  select(time, GDP_PC)
ggplot(gdp_1960_2019, aes(x=GDP_PC, fill=as.factor(time))) +
  geom_histogram(alpha=0.5, position="dodge", bins=20) +
  scale_fill_manual(values=c("blue", "red")) +
  labs(title="Histogram of GDP per capita for 1960 and 2019",
       x="GDP per capita", y="Frequency") +
  theme_classic()
#4-7
# Rank of the US in 1960
rank_1960 <- gapdata %>% 
  filter(time == 1960 & !is.na(name)) %>% 
  select(name, lifeExpectancy) %>% 
  arrange(desc(lifeExpectancy)) %>% 
  mutate(rank = rank(desc(lifeExpectancy))) %>% 
  filter(name == "United States of America") %>% 
  pull(rank)

# Rank of the US in 2019
rank_2019 <- gapdata %>% 
  filter(time == 2019 & !is.na(name)) %>% 
  select(name, lifeExpectancy) %>% 
  arrange(desc(lifeExpectancy)) %>% 
  mutate(rank = rank(desc(lifeExpectancy))) %>% 
  filter(name == "United States of America") %>% 
  pull(rank)

cat("Rank of the US in terms of life expectancy in 1960: ", rank_1960, "\n")
cat("Rank of the US in terms of life expectancy in 2019: ", rank_2019, "\n")
#4-8
# Calculate relative rank for 1960
rank_1960 <- gapdata %>% 
  filter(time == 1960 & !is.na(name) & !is.na(lifeExpectancy)) %>% 
  mutate(rank = rank(-lifeExpectancy)) %>% 
  filter(name == "United States of America") %>% 
  pull(rank)

# Calculate relative rank for 2019
rank_2019 <- gapdata %>% 
  filter(time == 2019 & !is.na(name) & !is.na(lifeExpectancy)) %>% 
  mutate(rank = rank(-lifeExpectancy)) %>% 
  filter(name == "United States of America") %>% 
  pull(rank)

# Calculate number of countries with data for each year
num_countries <- gapdata %>% 
  group_by(time) %>% 
  summarise(n_countries = sum(!is.na(lifeExpectancy)))

# Calculate relative rank per number of countries with data for each year
rel_rank <- c(rank_1960, rank_2019) / num_countries$n_countries
rank_1960
rank_2019
rel_rank

df_rank <- gapdata %>% 
  filter(!is.na(lifeExpectancy) & !is.na(name) & !is.na(time)) %>% 
  group_by(time) %>% 
  mutate(rank = rank(lifeExpectancy, na.last = "keep")) %>% 
  ungroup() %>% 
  group_by(time) %>% 
  mutate(rel_rank = rank / sum(!is.na(rank))) %>% 
  ungroup()

# Show the relative rank for the US
df_rank %>% 
  filter(name == "United States of America") %>% 
  select(time, rel_rank)
