#                                                                                           #
#       Effects of environmental variables on salamander abundance in the Bruce Peninsula   #
#                                                                                           #
#############################################################################################

# Packages required ------------------------------------------

library(tidyverse)
source("https://raw.githubusercontent.com/UofTCoders/rcourse/master/resources/HighstatLibV6.R")
library(PerformanceAnalytics)
library(MuMIn)

# Download dataset -------------------------------------------

salamander <- "https://github.com/eeb313-2019/git-out/raw/master/salamander.csv"
download.file(salamander, "salamander.csv")
salamander <- read.csv("salamander.csv", header = TRUE)

# Data cleaning ===============================================================================================

# Rename columns -----------------------------------------------

colnames(salamander) = c("site", "number", "cover","year", "date", "time", "survey",
                         "eastern_redback_count", "redback_count", "leadback_count", 
                         "cover_age","observer","precipitation", "air_temp","soil_temp",
                         "sky_class", "wind_class", "soil_moisture", "soil_ph")


# Make variables numeric -----------------------------------------

salamander <- salamander %>% 
  mutate(soil_temp = as.numeric(soil_temp)) %>% 
  mutate(air_temp = as.numeric(air_temp)) %>% 
  mutate(precipitation = as.numeric(precipitation)) %>% 
  mutate(eastern_redback_count = as.numeric(eastern_redback_count))

# Standardize the salamander abundance based on number of surveys and counts of salamanders --------------

df1 <- salamander %>% 
  filter(year>=2009) %>% 
  group_by(site, year) %>% 
  mutate(n=n()) %>% 
  select(site, year, n, eastern_redback_count) %>% 
  tally(eastern_redback_count)

df2 <- salamander %>% 
  filter(year>=2009) %>% 
  group_by(site, year) %>% 
  mutate(n=n()) %>% 
  select(site, year, n) %>% 
  distinct() %>% 
  arrange(site)

df1$count <- df2$n
df1$average <- df1$n/df1$count

df1 <- as_tibble(df1)

# Calculate the mean rainfall, soil, and air to ensure rows match up with standardized salamander counts

salamander_vars <- salamander %>% 
  filter(year>=2009) %>% 
  group_by(site, year) %>% 
  summarise(mean_rainfall = mean(precipitation), mean_soil = mean(soil_temp), mean_air = mean(air_temp))

# Combine the dataframes --------------------------------------------

salamander_new <- cbind(df1, salamander_vars[,c(3:5)])

salamander_new <- salamander_new %>% 
  filter(!is.na(mean_soil)) 

# Now look at salamander abundance over time using a linear model ======================================================

abundance <- salamander_new %>%
  group_by(year) %>% 
  summarise(average = sum(average))
  
abun_model <- lm(average ~ year, data = abundance)

summary(abun_model)

# Plot the model 

abundance %>% 
  ggplot(aes(x = year, y = average)) +
  geom_point() +
  geom_smooth(method = "lm") +
  scale_x_discrete(limits=c(2009:2017)) +
  theme_classic() +
  labs(y = "Relative Salamander Abundance", x = "Year")


# Now we will test how the environmental variables affect the salamander abundance ======================================

# Check the correlation between the variables --------------------------------------------

salamander_new %>% 
  dplyr::select(c(mean_rainfall, mean_air, mean_soil)) %>%
  chart.Correlation(histogram = TRUE, pch = 19)

# Next, look at the variance inflation factors (VIF) to double check multicollinearity -----------------

salamander_new %>% 
  dplyr::select(c(mean_rainfall, mean_air, mean_soil)) %>% 
  corvif()
# All values are < 3, so do not need to remove any of them or separate them. 

# Test all possible models with the 3 variables -------------------------------------------

model1 <- lm(average ~ mean_rainfall * mean_air * mean_soil, data = salamander_new)
model2 <- lm(average ~ mean_rainfall * mean_air, data = salamander_new)
model3 <- lm(average ~ mean_rainfall * mean_soil, data = salamander_new)
model4 <- lm(average ~ mean_soil * mean_air, data = salamander_new)

model5 <- lm(average ~ mean_rainfall + mean_air + mean_soil, data = salamander_new)
model6 <- lm(average ~ mean_rainfall + mean_air, data = salamander_new)
model7 <- lm(average ~ mean_rainfall + mean_soil, data = salamander_new)
model8 <- lm(average ~ mean_air + mean_soil, data = salamander_new)

model9 <- lm(average ~ mean_rainfall, data = salamander_new)
model10 <- lm(average ~ mean_air, data = salamander_new)
model11 <- lm(average ~ mean_soil, data = salamander_new)

model12 <- lm(average ~ mean_rainfall * mean_air + mean_soil, data = salamander_new)
model13 <- lm(average ~ mean_rainfall * mean_soil + mean_air, data = salamander_new)
model14 <- lm(average ~ mean_soil * mean_air + mean_rainfall, data = salamander_new)

model15 <- lm(average ~ 1, data = salamander_new)

# Compare the models using AICc (n/k < 40, so best to use AICc) -------------------------------

nrow(salamander_new)/3

AICc(model1, model2, model3, model4, model5, 
     model6, model7, model8, model9, model10, 
     model11, model12, model13, model14, model15)

# Summarise lowest models/ models of interest ------------------------------------------------
summary(model9)
summary(model10)
summary(model11)
summary(model15)

# Make plots for the predictor variables over time ===============================================================

# Mean air temperature over time
salamander_new %>% 
  group_by(year) %>% 
  summarise(mean_air = mean(mean_air)) %>% 
  ggplot(aes(y = mean_air, x = year)) +
  geom_point() +
  geom_line() +
  scale_x_discrete(limits = c(2009:2017)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme_classic()+
  labs(y = "Air Temperature (ºC)", x = "Year")

# Mean soil temperature over time 
salamander_new %>% 
  group_by(year) %>% 
  summarise(mean_soil = mean(mean_soil)) %>% 
  ggplot(aes(y = mean_soil, x = year)) +
  geom_point() +
  geom_line() +
  scale_x_discrete(limits = c(2009:2017)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme_classic()+
  labs(y = "Soil Temperature (ºC)", x = "Year")

# Mean precipitation over time 
salamander_new %>% 
  group_by(year) %>% 
  summarise(mean_rainfall = mean(mean_rainfall)) %>% 
  ggplot(aes(y = mean_rainfall, x = year)) +
  geom_point() +
  geom_line() +
  scale_x_discrete(limits = c(2009:2017)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme_classic()+
  labs(y = "Precipitation in the Last 24 hrs (mm)", x = "Year")

# Make plots for the linear regression models =======================================================================

# Salamander abundance over mean air temperature
salamander_new %>% 
  ggplot(aes(x = mean_air, y = average)) +
  geom_point() +
  geom_smooth(method = lm, formula = y ~ x) +
  theme_classic() +
  labs(x = "Air Temperature (ºC)", y = "Salamander Abundance") 

# Salamander abundance over mean soil temperature
salamander_new %>% 
  ggplot(aes(x = mean_soil, y = average)) +
  geom_point() +
  geom_smooth(method = lm, formula = y ~ x) +
  theme_classic() +
  labs(x = "Soil Temperature (ºC)", y = "Salamander Abundance") 

# Salamander abundance over mean precipitation
salamander_new %>% 
  ggplot(aes(x = mean_rainfall, y = average)) +
  geom_point() +
  geom_smooth(method = lm, formula = y ~ x) +
  theme_classic() +
  labs(x = "Precipitation in the Last 24 hrs (mm)", y = "Salamander Abundance") 



# Supplementary Material ==============================================================================

# Identify and remove outliers 

outlier_test <- salamander_out %>% 
  select(year, site, air_temp, precipitation, soil_temp) %>% 
  distinct()

# for air temperature
outliers_air <- boxplot((outlier_test$air_temp ~ outlier_test$year), plot = FALSE)$out

(outlier_test[which(outlier_test$air_temp %in% outliers_air), ])

# for precipitation
outliers_rain <- boxplot(outlier_test$precipitation ~ outlier_test$year, plot = FALSE)$out

(outlier_test[which(outlier_test$precipitation %in% outliers_rain), ])

# for soil temperature
outliers_soil2 <- boxplot(outlier_test$soil_temp ~ outlier_test$year, plot = F)$out

(outlier_test[which(outlier_test$soil_temp %in% outliers_soil2), ])

# remove the identified outliers from the data
remove_outliers <- subset(outlier_test[-c(16, 33, 34, 35, 71, 101), ])

# then can repeat same procedure as above with this new dataset 






