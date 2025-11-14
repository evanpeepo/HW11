#Researchers monitored Temperature (in Celsius) and relative humidity (%) across two sites (Auburn and Merced, CA)  
#to look at abiotic conditions tied to bee fitness. 
#How does relative humidity (%) in response to temperature (C) compare between two different research sites?

library(tidyverse)

rel_humidity <- read.csv("VAS_oCAM_TempRelHum_subset.csv")

#Filter and clean up
rel_humidity_data <- rel_humidity %>% 
  select(-X, -X.1, -X.2) %>% 
  rename(
    temp_C = Temperature..C.,
    relative_humidity = Relative.Humidity....
  )

#Graph data
ggplot(rel_humidity_data, aes(x = temp_C, y = relative_humidity, shape = Site, color = Site)) +
  geom_point() +
  geom_smooth(method = 'lm')

#Sites appear to have different slopes and intercepts

# Find best fit model -----------------------------------------------------

lm_1 <- lm(relative_humidity ~ temp_C * Site, data = rel_humidity_data)
summary(lm_1)
#This shows that slopes are significantly different between sites (p = .0247). Intercepts don't appear to differ (p = .317)

#Comparing models with anova
lm_2 <- lm(relative_humidity ~ temp_C + Site, data = rel_humidity_data)
anova(lm_1, lm_2)

#At zero degrees C, both sites are expected to have about the same relative humidity. (intercept not significantly different)
#As temperature increases, the relative humidity at the two sites responds differently (slopes differ). Relative humidity
#decreases more rapidly at the Auburn site compared to the Merced site. 