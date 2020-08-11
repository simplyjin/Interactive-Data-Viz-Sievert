library(plotly)

data(diamonds, package = "ggplot2")
diamonds

#3 different visualizations using the diamond dataset 
plot_ly(diamonds, x = ~cut)
plot_ly(diamonds, x = ~cut, y = ~clarity)
plot_ly(diamonds, x = ~cut, color = ~clarity, colors = "Accent")

#doesn't produce black bars
plot_ly(diamonds, x = ~cut, color = "black")

#red bars with black outline
plot_ly(
  diamonds,
  x = ~cut,
  color = I("red"),
  stroke = I("black"),
  span = I(2)
)

#first argument is plotly, other arguments modify the various layout components
layout(
  plot_ly(diamonds, x = ~cut),
  title = "My beautiful histogram"
)

#using magrittr instead
diamonds %>% 
  plot_ly(x = ~cut) %>% 
  layout(title = "My beautiful histogram 2")

#specifying geometric object
diamonds %>% 
  plot_ly() %>% 
  add_histogram(x = ~cut)

#add_bars requires bar heights to be pre-specified
diamonds %>% 
  dplyr::count(cut) %>% 
  plot_ly() %>% 
  add_bars(x = ~cut, y = ~n)

#using globally assigned variables( this is very simliar to ggplot2)
library(dplyr)

diamonds %>% 
  plot_ly(x = ~cut) %>% 
  add_histogram() %>% 
  group_by(cut) %>% #to count the number of diamonds in each cut category
  summarise(n = n()) %>% 
  add_text( #add a layer of text using the summarized counts
    text = ~scales::comma(n), y = ~n,
    textposition = "top middle",
    cliponaxis = FALSE
  )

#can use plotly_data to return data at any point
diamonds %>% 
  plot_ly(x = ~cut) %>% 
  add_histogram() %>% 
  group_by(cut) %>% 
  summarise(n = n()) %>% 
  plotly_data()
  
#using ggplotly
p <- ggplot(diamonds, aes(x = log(carat), y = log(price))) +
  geom_hex(bins = 100)
ggplotly(p)

p <- ggplot(diamonds, aes(x = log(price), color = clarity)) +
  geom_freqpoly()
ggplotly(p)

#price variation by cut and clarity with relative frequencies
p <- ggplot(diamonds, aes(x = log(price), color = clarity)) +
  geom_freqpoly(stat = "density") +
  facet_wrap(~cut)
ggplotly(p)

#create a geom_sina graph using ggforce package, that adds jitter to the raw data allowing us to see where the majority of 
#observations fall within the group and across all groups. We also add a 95% confidence interval using the Hmisc package
p <- ggplot(diamonds, aes(x = clarity, y = log(price), color = clarity)) +
  ggforce::geom_sina(alpha = 0.1) +
  stat_summary(fun.data = "mean_cl_boot", color = "black") +
  facet_wrap(~cut)

toWebGL(ggplotly(p))

p <- ggplot(diamonds, aes(x=clarity, y=log(price), color=clarity)) +
  ggforce::geom_sina(alpha = 0.1) + 
  stat_summary(fun.data = "mean_cl_boot", color = "black") +
  facet_wrap(~cut)

# WebGL is a lot more efficient at rendering lots of points
#toWebGL(ggplotly(p)) 
ggplotly(p)

#adding carat to our model
m <- lm(log(price) ~ log(carat), data = diamonds)
diamonds <- modelr::add_residuals(diamonds, m)
p <- ggplot(diamonds, aes(x = clarity, y = resid, color = clarity)) +
  ggforce::geom_sina(alpha = 0.1) +
  stat_summary(fun.data = "mean_cl_boot", color = "black") +
  facet_wrap(~cut)
ggplotly(p)

library(GGally)
m <- lm(log(price) ~ log(carat) + cut, data = diamonds)
gg <- ggcoef(m)
# dynamicTicks means generate new axis ticks on zoom
ggplotly(gg, dynamicTicks = TRUE)

library(naniar) #working with missing data
# fake some missing data
diamonds$price_miss <- ifelse(diamonds$depth>60, diamonds$price, NA)

p <- ggplot(diamonds, aes(x = clarity, y = log(price_miss))) +
  geom_miss_point(alpha = 0.1) + 
  stat_summary(fun.data = "mean_cl_boot", colour = "black") +
  facet_wrap(~cut)
ggplotly(p) #this doesn't work. Can't find a solution
