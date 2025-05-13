library(ggplot2)
dim(diamonds)
names(diamonds)

ggplot(diamonds,aes(x=cut)) + geom_bar()
ggplot(diamonds,aes(x=cut)) + geom_histogram(stat = "count")
ggplot(diamonds,aes(x=carat)) + geom_histogram(binwidth = 0.1)
ggplot(diamonds,aes(x=carat)) + geom_histogram(binwidth = 0.1) + xlim(0,3)

ggplot(diamonds,aes(x=depth)) + geom_histogram()
ggplot(diamonds,aes(x=depth)) + geom_histogram(binwidth = 0.2)
ggplot(diamonds,aes(x=depth)) + geom_histogram(binwidth = 0.2) + xlim(55,70)
ggplot(diamonds,aes(x=depth, fill = cut)) + geom_histogram(binwidth = 0.2) + xlim(55,70)
ggplot(diamonds,aes(x=depth)) + geom_histogram(binwidth = 0.2) + xlim(55,70) + facet_wrap(~ cut)

ggplot(diamonds, aes(x=carat,y=price)) + geom_point()
ggplot(diamonds, aes(x=carat,y=price)) + geom_point(colour = "blue")
ggplot(diamonds, aes(x=carat,y=price)) + geom_point(colour = "blue") + geom_smooth()
ggplot(diamonds, aes(x=carat,y=price)) + geom_point(colour = "blue") + geom_smooth(colour = "red")
ggplot(diamonds, aes(x = carat, y = price)) + geom_point(aes(colour= cut)) + 
  geom_smooth()
ggplot(diamonds, aes(x = carat, y = price, colour =  cut)) + geom_point() + 
  geom_smooth()


library(plotly)
library(gapminder)

df <- gapminder 
fig <- df %>%
  plot_ly(
    x = ~gdpPercap, 
    y = ~lifeExp, 
    size = ~pop, 
    color = ~continent, 
    frame = ~year, 
    text = ~country, 
    hoverinfo = "text",
    type = 'scatter',
    mode = 'markers'
  )
fig <- fig %>% layout(
  xaxis = list(
    type = "log"
  )
)

fig
