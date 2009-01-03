library(tidyverse)
library(tidyr)
data(airquality)
data(chickwts)
data(Nile)
data(sleep)

# Air quality data
# use as.tibble to change the data format to something more compatible with tidyverse tools
tairquality <- as.tibble(airquality)
# use gglot2 to make a figure using the data set
ggplot(data = tairquality, mapping = aes(x = Ozone)) +
  geom_histogram(aes(y=..density..), color="black", fill="white") +
  geom_density(alpha=.2, fill="darkcyan") +
  facet_wrap(~ Month, nrow = 2) +
  xlab('Mean ozone in ppb') +
  ylab("Density") +
  ggtitle("Plot of ozone by month")

by_month <- group_by(tairquality, Month)
summarise(by_month, ozone = mean(Ozone, na.rm = T))

# Chicken weights data
tchickwts <- as.tibble(chickwts)
ggplot(data = tchickwts, mapping = aes( x = feed, y = weight)) +
  geom_boxplot() +
  coord_flip() +
  ylab('weight in grams') +
  xlab("feed type") +
  ggtitle("Boxplot of chicken weights by feed type")

by_feed <- group_by(tchickwts, feed)
summarise(by_feed, weight = median(weight, na.rm = T))

# Nile river data
tNile <- as.tibble(Nile) %>%
  mutate(year = 1871:1970) 

ggplot(data = tNile, mapping = aes(x = as.numeric(year), y = as.numeric(x))) +
  geom_line() +
  geom_smooth() +
  ylab('flow of the river Nile in 10^8 m^3') +
  xlab("year") +
  ggtitle("Plot of annual flow of the river Nile 1871-1970")

before_1898 <- tNile %>%
  filter(year < 1898)
after_1898 <- tNile %>%
  filter(year > 1898)
mean(before_1898$x)
mean(after_1898$x)

# Student sleep data
tsleep <- as.tibble(sleep)
ggplot(data = tsleep, mapping = aes(x = group, y = extra)) +
  geom_boxplot() +
  xlab('Drug given') +
  ylab("Increase in hours of sleep") +
  ggtitle("Boxplot of increase in sleep by drug type")

sleep_wide <- spread(sleep,group,extra) %>%
  mutate(diff = `2` - `1`)
sleep_wide$diff

