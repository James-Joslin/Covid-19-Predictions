library(readr)
library(dplyr)
library(gt)
library(ggforce)
library(ggplot2)
library(ggpubr)
library(lubridate)
library(broom)
library(knitr)
library(plyr)
library(reshape2)
library(timetk)
library(basictabler)
library(tidymv)
library(mgcv)
library(sjPlot)
library(car)
library(rgdal)
library(raster)
library(rgeos)
library(sp)
library(tidyverse)

# Adjust Shapefiles ####
# boundary <- 
#   readOGR("Indices_of_Multiple_Deprivation_(IMD)_2019.shp", layer = "Indices_of_Multiple_Deprivation_(IMD)_2019")
# 
# plot(boundary)
# summary(boundary)
# Depriv <- spTransform(boundary, CRS("+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +units=m +no_defs"))
# summary(Depriv)
# writeOGR(Depriv, ".", "DeprivBNG", 
#          driver = "ESRI Shapefile")

# National Data Import and Prep #### 
UK <- read_csv("C:/Users/james/Desktop/Deaths_Vs_Cases_UK.csv")
UK <- na.omit(UK)
UK <- UK[order(as.Date(UK$date, format="%d/%m/%Y")),]
UK$newdate <- strptime(as.character(UK$date), "%d/%m/%Y")
UK <- UK %>%
  filter(newdate > as.Date("2020-03-12"))
UK <- UK[c(1,2,7,5,6)]
colnames(UK)<-c("Code", "Area", "Date", "Cases", "Deaths")
UK$Cases_Rolling <- zoo::rollmean(UK$Cases, 7, na.pad = TRUE)
UK$Deaths_Rolling <- zoo::rollmean(UK$Deaths, 7, na.pad = TRUE)
UK$Date <- as.Date(UK$Date)

Surrey <- read_csv("C:/Users/james/Desktop/Deaths_Vs_Cases_Surrey.csv")
Surrey <- na.omit(Surrey)
Surrey <- Surrey[order(as.Date(Surrey$date, format="%d/%m/%Y")),]
Surrey$newdate <- strptime(as.character(Surrey$date), "%d/%m/%Y")
Surrey <- Surrey %>%
  filter(newdate < as.Date("2021-04-12"))
Surrey <- Surrey[c(1,2,7,5,6)]
colnames(Surrey)<-c("Code", "Area", "Date", "Cases", "Deaths")
Surrey$Cases_Rolling <- zoo::rollmean(Surrey$Cases, 7, na.pad = TRUE)
Surrey$Deaths_Rolling <- zoo::rollmean(Surrey$Deaths, 7, na.pad = TRUE)
Surrey$Date <- as.Date(Surrey$Date)

df_UK_Surrey <- rbind(UK,Surrey)
df_UK_Surrey$Date <- as.Date(df_UK_Surrey$Date)

# National Plots ####
NationalCases_plot <- ggplot(data = df_UK_Surrey, aes(colour = Area, x = Date, y = Cases)) +
  geom_point(shape = 1) +
  scale_x_date(name = "Date") +
  facet_zoom(ylim = c(0, 2000), zoom.data = ifelse(Area == "Surrey", NA, FALSE)) +
  theme_bw()+
  theme(text = element_text((size = 20))) +
  geom_line(data = df_UK_Surrey, 
            aes(y = Cases_Rolling, colour = Area), lty = 1, size = 1) +
  scale_colour_manual(values = c("orange", "grey50"))

NationalDeaths_plot <- ggplot(data = df_UK_Surrey, aes(colour = Area, x = Date, y = Deaths)) +
  geom_point(shape = 1) +
  scale_x_date(name = "Date") +
  scale_y_continuous(name = "Deaths") +
  facet_zoom(ylim = c(0, 60), zoom.data = ifelse(Area == "Surrey", NA, FALSE)) +
  theme_bw() +
  theme(axis.text = element_text((size = 20))) +
  geom_line(data = df_UK_Surrey, 
            aes(y = Deaths_Rolling, colour = Area), lty = 1, size = 1) +
  scale_colour_manual(values = c("orange", "grey50"))

National_Plots <- ggarrange(NationalCases_plot, NationalDeaths_plot, nrow = 2, labels = c("A", "B"))
print(National_Plots)

# Cases By Age ####
Complete_Demographic <- read_csv("C:/Users/james/Desktop/specimenDate_ageDemographic-stacked.csv")
Surrey_UK_Demo <- Complete_Demographic %>% 
  filter(areaName == "Surrey" | areaName == "United Kingdom")
Surrey_UK_Demo <- Surrey_UK_Demo %>% 
  filter(age != "0_59" & age != "60+" & age != "unassigned")
Surrey_UK_Demo <- Surrey_UK_Demo[c(3,5,6)]
colnames(Surrey_UK_Demo) <- c("Area", "Age", "Cases")

Surrey_UK_Demo <- Surrey_UK_Demo %>%
  mutate(Age = gsub('_.*','',Age))
Surrey_UK_Demo$Age <- as.numeric(Surrey_UK_Demo$Age)
Surrey_UK_Demo <- na.omit(Surrey_UK_Demo)

Surrey_Proportion <- Surrey_UK_Demo %>% 
  filter(Area == "Surrey")
UK_Proportion <- Surrey_UK_Demo %>% 
  filter(Area == "United Kingdom") 

UK_Proportion <- UK_Proportion %>%                                       
  group_by(Age) %>%                         # Specify group indicator
  summarise_at(vars(Cases),              # Specify column
               list(name = sum))

Surrey_Proportion <- Surrey_Proportion %>%                                        # Specify data frame
  group_by(Age) %>%                         # Specify group indicator
  summarise_at(vars(Cases),              # Specify column
               list(name = sum))

UK_Proportion$Area <- "United Kingdom"
Surrey_Proportion$Area <- "Surrey"

UK_Proportion$Proportion <- UK_Proportion$name/sum(UK_Proportion$name)
Surrey_Proportion$Proportion <- Surrey_Proportion$name/sum(Surrey_Proportion$name)

Age_Proportions <- rbind(UK_Proportion, Surrey_Proportion)

Age_Proportion_Plot <- ggplot(data = Age_Proportions, aes(x = Age, y = Proportion, colour = Area)) +
  geom_point(shape = 1) +
  scale_x_continuous(breaks = scales::pretty_breaks(6)) +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 8), se = FALSE) +
  scale_colour_manual(values = c("orange", "grey50"))
print(Age_Proportion_Plot)

# Cases Vs Deaths UK ####
# create columns for deaths led 0 to 40 days ahead
max_lead <- 50
UK_lags <- UK %>%
  # create lags by day
  tk_augment_lags(Deaths_Rolling, .lags = 0:-max_lead, .names = "auto")
# fix names to remove minus sign
names(UK_lags) <- names(UK_lags) %>% str_replace_all("lag-|lag", "lead")

cutoff_end <- max(UK$Date) - 7

# use only case dates where we have complete future knowledge of deaths for all lead times.
UK_lags$Date <- as.Date(UK_lags$Date)
UK_lags <- UK_lags %>% filter(UK_lags$Date < cutoff_end - max_lead)

UK_models <- UK_lags %>%
  ungroup() %>%
  pivot_longer(
    cols = contains("lead"),
    names_to = "lead",
    values_to = "led_deaths"
  ) %>%
  select(Date, Cases_Rolling, lead, led_deaths) %>%
  mutate(lead = as.numeric(str_remove(lead, "Deaths_Rolling_lead"))) %>%
  nest(data = c(Date, Cases_Rolling, led_deaths)) %>%
  # Run a regression on lagged cases and date vs deaths
  mutate(model = map(
    data,
    function(df) {
      lm(led_deaths ~ Cases_Rolling + poly(Date, 2), data = df)
    }
  ))

# Add regression coefficient
# get adjusted r squared
UK_models <- UK_models %>%
  mutate(adj_r = map(model, function(x) {
    glance(x) %>%
      pull(adj.r.squared)
  })
  %>% unlist())
print(UK_models)

UKbest_fit <- UK_models %>%
  summarize(adj_r = max(adj_r)) %>%
  left_join(UK_models, by = "adj_r")

UK_Leads_Plot <- UK_models %>%
  ggplot(aes(lead, adj_r)) +
  geom_line(colour = "grey50") +
  geom_line(aes(x = UKbest_fit$lead),linetype = "dashed", colour = "red")+
  geom_segment(linetype = "dashed", x = 0, xend = UKbest_fit$lead, y = max(UK_models$adj_r), yend = max(UK_models$adj_r), colour = "red")+
  theme_bw() +
  labs(
    subtitle = paste("Best fit =", UKbest_fit$lead, "days"),
    x = "Lag Time (Days)",
    y = "Adjusted R-squared"
  )
#print(UK_Leads_Plot)

# Cases Vs Deaths Surrey ####
# create columns for deaths led 0 to 40 days ahead

Surrey_lags <- Surrey %>%
  # create lags by day
  tk_augment_lags(Deaths_Rolling, .lags = 0:-max_lead, .names = "auto")
# fix names to remove minus sign
names(Surrey_lags) <- names(Surrey_lags) %>% str_replace_all("lag-|lag", "lead")

cutoff_end <- max(Surrey$Date) - 7

# use only case dates where we have complete future knowledge of deaths for all lead times.
Surrey_lags$Date <- as.Date(Surrey_lags$Date)
Surrey_lags <- Surrey_lags %>% filter(Surrey_lags$Date < cutoff_end - max_lead)

Surrey_models <- Surrey_lags %>%
  ungroup() %>%
  pivot_longer(
    cols = contains("lead"),
    names_to = "lead",
    values_to = "led_deaths"
  ) %>%
  select(Date, Cases_Rolling, lead, led_deaths) %>%
  mutate(lead = as.numeric(str_remove(lead, "Deaths_Rolling_lead"))) %>%
  nest(data = c(Date, Cases_Rolling, led_deaths)) %>%
  # Run a regression on lagged cases and date vs deaths
  mutate(model = map(
    data,
    function(df) {
      lm(led_deaths ~ Cases_Rolling + poly(Date, 2), data = df)
    }
  ))

# Add regression coefficient
# get adjusted r squared
Surrey_models <- Surrey_models %>%
  mutate(adj_r = map(model, function(x) {
    glance(x) %>%
      pull(adj.r.squared)
  })
  %>% unlist())
print(Surrey_models)

Surreybest_fit <- Surrey_models %>%
  summarize(adj_r = max(adj_r)) %>%
  left_join(Surrey_models, by = "adj_r")

Surrey_Lags_Plot <- Surrey_models %>%
  ggplot(aes(lead, adj_r)) +
  geom_line(colour = "orange") +
  geom_line(aes(x = Surreybest_fit$lead), colour = "red", linetype = "dashed")+
  geom_segment(linetype = "dashed", x = 0, xend = Surreybest_fit$lead, y = max(Surrey_models$adj_r), yend = max(Surrey_models$adj_r), colour = "red")+
  theme_bw() +
  labs(
    subtitle = paste("Best fit =", Surreybest_fit$lead, "days"),
    x = "Lag Time (Days)",
    y = "Adjusted R-squared"
  )
#print(Surrey_Lags_Plot)

# Print Lag Plots ####
Lag_Plots <- ggarrange(UK_Leads_Plot, Surrey_Lags_Plot, ncol = 2, labels = c("A", "B"))
print(Lag_Plots)
# UK Fatalities ####
UKfatality <- UKbest_fit$data[[1]] %>%
  filter(Cases_Rolling > 0) %>%
  filter(Date > as.Date("2020-04-01")) %>%
  mutate(rate = led_deaths / Cases_Rolling)

UKg <- UKfatality %>% ggplot(aes(Date, rate)) +
  geom_line() +
  geom_smooth(se = FALSE, colour = "grey50") +
  theme_bw() +
  labs(
    x = "Date", y = "Fatality Rate")+
  scale_y_continuous(labels = scales::percent)
#print(UKg)

# Surrey Fatalities ####
Sfatality <- Surreybest_fit$data[[1]] %>%
  filter(Cases_Rolling > 0) %>%
  filter(Date > as.Date("2020-04-01")) %>%
  mutate(rate = led_deaths / Cases_Rolling)

Sg <- Sfatality %>% ggplot(aes(Date, rate)) +
  geom_line() +
  geom_smooth(se = FALSE, colour = "orange") +
  theme_bw() +
  labs(x = "Date", y = "Fatality Rate") +
  scale_y_continuous(labels = scales::percent)
#print(Sg)

# Print Fatalities ####
Fatalities_Plots <- ggarrange(UKg, Sg, ncol = 2, labels = c("A", "B"))
print(Fatalities_Plots)

# Predictions Surrey !BAD! ####
Surreyshow_predictions <- function(Surreysingle_model, n.ahead) {
  Predicted_Deaths <- predict(Surreysingle_model$model[[1]], newdata = Surrey)
  Date <- seq.Date(from = min(Surrey$Date) + n.ahead, to = max(Surrey$Date) + n.ahead, by = 1)
  display <- full_join(Surrey, tibble(Date, Predicted_Deaths))
  
  gg <- display %>%
    pivot_longer(cols = where(is.numeric)) %>%
    filter(name %in% c("Deaths_Rolling", "Predicted_Deaths")) %>%
    ggplot(aes(Date, value, color = name)) +
    geom_line() +
    labs(
      title = "Surrey - Actual vs. Predicted Deaths",
      x = "Date",
      y = "Count"
    )
  gg
}
print(Surreyshow_predictions(Surreybest_fit, Surreybest_fit$lead))

# Deprivation and Sex ####
Deprivation_df <- read_csv("C:/Users/james/Desktop/Deprivation_Data_sex_Deaths.csv")
Deprivation_df <- melt(Deprivation_df, id.vars = c("Cause", "Sex", "Deprivation_Decile"))
colnames(Deprivation_df) <- c("Cause", "Sex", "Dep", "Date", "Deaths")
Deprivation_df$Dep <- as.factor(Deprivation_df$Dep)
Deprivation_df$Date <- as.Date(Deprivation_df$Date)
head(Deprivation_df)

dep_death_Plot <- ggplot(data = Deprivation_df %>% filter(Cause != "All causes" & Sex != "People"), aes(colour = Cause, x = as.numeric(Dep), y = Deaths)) +
  geom_point() +
  geom_smooth(method = "glm")
print(dep_death_Plot)

Mdep_death <- lm(data = Deprivation_df %>% filter(Cause != "All causes" & Sex != "People")
                 , Deaths ~ Dep*Sex)
#plot(Mdep_death)
Anova(Mdep_death)

Multi_Dep_Deathsplot<-ggplot(data = Deprivation_df %>% filter(Cause != "All causes" & Sex != "People"), aes(colour = Sex, x = Date, y = Deaths)) +
  geom_point(shape = 1, alpha = 1) +
  facet_wrap(~Cause) +
  scale_y_continuous(breaks = scales::pretty_breaks(10)) +
  stat_smooth(method = "gam", formula = y ~ s(x, k = 4, bs = "cr"), size = 1, se = FALSE) +
  theme_bw()
print(Multi_Dep_Deathsplot)

dep_cov <- Deprivation_df %>% filter(Cause != "All causes" & Sex != "People")
dep_cov$Dep <- as.numeric(dep_cov$Dep)
dep_cov$Date <- as.numeric(dep_cov$Date)
dep_cov$Sex <- gsub("Males", 1, dep_cov$Sex)
dep_cov$Sex <- gsub("Females", 2, dep_cov$Sex)

dep_cov$Cause <- gsub("Due to other causes", 1, dep_cov$Cause)
dep_cov$Cause <- gsub("Due to COVID-19", 2, dep_cov$Cause)

dep_cov$Sex <- as.numeric(dep_cov$Sex)
dep_cov$Cause <- as.numeric(dep_cov$Cause)

mod2da <- gam(Deaths ~ s(Date, by = Cause, bs = "cr") + Sex + Dep, by = Cause, 
              data = dep_cov)

summary(mod2da)
anova.gam(mod2da)

set_theme(base = theme_bw())
plot_model(model = mod2da, type = "est") + ylim(-80, 0)

# Inspect the model

