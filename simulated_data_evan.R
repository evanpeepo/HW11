
library(tidyverse)

set.seed(123)

n <- 50
beta0_1 <- 3
beta1 <- 2
beta0_2 <- 9

x <- rnorm(n, mean = 10, sd = 2)

y1_labels <- rep('pinyon', n)
y2_labels <- rep('juniper', n)

# Response with normal errors
epsilon <- rnorm(n, mean = 0, sd = 2)  

y1 <- beta0_1 + beta1 * x + epsilon
y2 <- beta0_2 + beta1 * x + epsilon

sim_data <- data.frame(
  vwc_percent = c(x, x),
  growth_mm = c(y1, y2),
  species = c(y1_labels, y2_labels)
)

ggplot(sim_data, aes(vwc_percent, growth_mm, color = species)) +
  geom_point() + 
  geom_smooth(method = 'lm')

ancova_species <- lm(growth_mm ~ vwc_percent + species, data = sim_data)
summary(ancova_species)

ancova_slope <- lm(growth_mm ~ vwc_percent * species, data = sim_data)
summary(ancova_slope)

anova(ancova_species, ancova_slope)

write.csv(sim_data, 'pinyon_juniper_growth.csv', row.names = FALSE)
#Researchers monitored yearly diameter growth (mm) for 50 pinyon and 50 juniper across 
#different soil moistures (measured as volumetric water content, %). 
#How does tree growth (y) in response to soil moisture (x) compare between pinyon and juniper trees?