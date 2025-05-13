library(ggplot2)
ggplot(data = mtcars) +  
  aes(x = mpg, y = hp, color = cyl) +  
  geom_point()

ggplot(data = diamonds) +  
  aes(x = cut) +  
  geom_bar()

ggplot(data = mpg) +  
  aes(x = class, fill = drv) +  
  geom_bar() +  
  facet_wrap(~ year) +  
  theme_minimal()

dataset <- read.csv("Greenhouse_gas_emissions_OECD.csv")
names(dataset)
library(dplyr)
dataset <- dataset %>% select(Country,Pollutant,Variable,Year,Value)
dataset$Variable

dataset<- dataset %>% filter(Variable=="Total  emissions excluding LULUCF")
dataset_year<- dataset%>% group_by(Year)%>% summarize(total_emissions=sum(Value))

ggplot(dataset_year, aes(Year,total_emissions)) + geom_point()
plot1 <- ggplot(dataset_year, aes(Year, total_emissions)) + geom_point() + 
  geom_smooth() + labs(title = "Total Annual Emissions", x= "Total emissions excluding LULUCF", 
                       y="Year", caption= "Source OECD") + theme_bw()
plot2<- ggplot(dataset_year, aes(total_emissions/1000)) + 
  geom_histogram(fill ="blue", color = "black" ) + 
  labs(title="Frequency of annual Emissions", x="total emissions expressed in 1000s", y="Frequency")

library("gridExtra")
grid.arrange(plot1, plot2, ncol = 2, nrow = 1)

library(maps)
library(viridis) # this if the colours used
# Select the latest available year
latest_year <- max(dataset$Year)

# Summarize emissions by country
dataset_country <- dataset %>%
  filter(Year == latest_year) %>%
  group_by(Country) %>%
  summarize(total_emissions = sum(Value, na.rm = TRUE))


world_map <- map_data("world")
# Rename columns for consistency
colnames(world_map)[colnames(world_map) == "region"] <- "Country"

# Merge emissions data with map data
map_data <- left_join(world_map, dataset_country, by = "Country")

ggplot(map_data, aes(x = long, y = lat, group = group, fill = total_emissions)) +
  geom_polygon(color = "gray") +
  scale_fill_viridis(option = "magma", na.value = "white") +  # Use a color scale
  labs(
    title = paste("Total Greenhouse Gas Emissions by Country in", latest_year),
    fill = "Emissions"
  ) +
  theme_minimal()





