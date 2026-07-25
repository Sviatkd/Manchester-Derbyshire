library(tidyverse)
library(gratia)
library(mgcv)
gmppanel_prop <- ns_socioeconomic_classification %>%
  group_by(Area, Year) %>%
  mutate(Proportion = Count / sum(Count, na.rm = TRUE),Area=Location) %>%
  ungroup()
gmppanel_prop <- read_csv("C:/Users/alexa/Downloads/gmppanel_prop.csv")
gmppanel_prop
high_peak_areas <- c(
  "New Mills West Ward",
  "New Mills East Ward",
  "Buxton Central Ward",
  "Dinting Ward",
  "Old Glossop Ward",
  "Hadfield South Ward",
  "Hadfield North Ward"
)
unemployed <- na.omit(filter(gmppanel_prop,Classification=="Never worked & long-term unemployed"))
unemployed <- unemployed %>% 
  filter(!(Location %in% c("Tameside District","High Peak District")),Year>=1971) %>% 
  mutate(treatment = factor(ifelse(Location %in%high_peak_areas, 1, 0)),
         established = ifelse(Year>=2011,1,0),
         did= established*treatment)
ggplot(data = unemployed,mapping = aes(x=Year,y=Proportion,colour = factor(Location)))+geom_line()+ggtitle(unique(unemployed$Classification))
ggplot(data = unemployed,mapping = aes(x=Year,y=Proportion,colour = factor(treatment)))+stat_summary(
  fun.data = "mean_cl_boot", lwd=1,geom = "line"
)+ggtitle(unique(unemployed$Classification))
spline1 <- gam(
  Proportion ~                        # Smooth trend for Reference Group (reduced k)
    s(Year, by = treatment, k = 3) + # Difference spline (reduced k)
    factor(Location),          # Location fixed effects
  data = unemployed,
  method = "ML"
)
anova(spline1)
spliner <- gam(
  Proportion ~            # Parametric intercept difference
    s(Year, k = 3) +           # Smooth trend for Reference Group (reduced k)
    factor(Location),          # Location fixed effects
  data = unemployed,
  method = "ML"
)
model_comparisontable =tibble(SIC=c(BIC(spliner),BIC(spline1)),
                              Akaike=c(AICc(spliner),
                                       AICc(spline1)))
model_comparisontable = model_comparisontable %>% mutate(,ChangeSchwarz=SIC-min(SIC),ChangeAkaike=Akaike-min(Akaike),RL=exp(-0.5*(ChangeAkaike)),BF=(exp(-0.5*(ChangeSchwarz))))
gt::gt(model_comparisontable)
diff_fit <- difference_smooths(spline1, smooth = "s(Year)")

# Plot the difference curve across time
draw(diff_fit) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Smooth Difference (Derbyshire-GManchester)")
unemployed <- na.omit(filter(gmppanel_prop,Classification=="Intermediate occupations"))
unemployed <- unemployed %>% 
  filter(Location != "Tameside District",Year>=1971) %>% 
  mutate(treatment = factor(ifelse(Location == "High Peak District", 1, 0)),
         established = ifelse(Year>=2011,1,0),
         did= established*treatment)
ggplot(data = unemployed,mapping = aes(x=Year,y=Proportion,colour = factor(Location)))+geom_line()+ggtitle(unique(unemployed$Classification))
ggplot(data = unemployed,mapping = aes(x=Year,y=Proportion,colour = factor(treatment)))+geom_smooth(,se=FALSE,span=0.7)+ggtitle(unique(unemployed$Classification))
ggplot(data = unemployed,mapping = aes(x=Year,y=Proportion,colour = factor(treatment)))+stat_summary(
  fun.data = "mean_cl_boot", lwd=1,geom = "line"
)+ggtitle(unique(unemployed$Classification))
spline1 <- gam(
  Proportion ~                       # Smooth trend for Reference Group (reduced k)
    s(Year, by = treatment, k = 3) + # Difference spline (reduced k)
    factor(Location),          # Location fixed effects
  data = unemployed,
  method = "ML"
)
anova(spline1)
spliner <- gam(
  Proportion ~            # Parametric intercept difference
    s(Year, k = 3) +           # Smooth trend for Reference Group (reduced k)
    factor(Location),          # Location fixed effects
  data = unemployed,
  method = "ML"
)
model_comparisontable =tibble(SIC=c(BIC(spliner),BIC(spline1)),
                              Akaike=c(AICc(spliner),
                                       AICc(spline1)))
model_comparisontable = model_comparisontable %>% mutate(,ChangeSchwarz=SIC-min(SIC),ChangeAkaike=Akaike-min(Akaike),RL=exp(-0.5*(ChangeAkaike)),BF=(exp(-0.5*(ChangeSchwarz))))
gt::gt(model_comparisontable)
diff_fit <- difference_smooths(spline1, smooth = "s(Year)")

# Plot the difference curve across time
draw(diff_fit) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Smooth Difference (Derbyshire - GManchester)")
unemployed <- na.omit(filter(gmppanel_prop,Classification=="Routine & manual occupations" ))
unemployed <- unemployed %>% 
  filter(Location != "Tameside District",Year>=1971) %>% 
  mutate(treatment = factor(ifelse(Location == "High Peak District", 1, 0)),
         established = ifelse(Year>=2011,1,0),
         did= established*treatment)
ggplot(data = unemployed,mapping = aes(x=Year,y=Proportion,colour = factor(Location)))+geom_line()+ggtitle(unique(unemployed$Classification))
ggplot(data = unemployed,mapping = aes(x=Year,y=Proportion,colour = factor(treatment)))+geom_smooth(,se=FALSE,span=0.7)+ggtitle(unique(unemployed$Classification))
ggplot(data = unemployed,mapping = aes(x=Year,y=Proportion,colour = factor(treatment)))+stat_summary(
  fun.data = "mean_cl_boot", lwd=1,geom = "line"
)+ggtitle(unique(unemployed$Classification))
spline1 <- gam(
  Proportion ~                       # Smooth trend for Reference Group (reduced k)
    s(Year, by = treatment, k = 3) + # Difference spline (reduced k)
    factor(Location),          # Location fixed effects
  data = unemployed,
  method = "ML"
)
anova(spline1)
spliner <- gam(
  Proportion ~           # Parametric intercept difference
    s(Year, k = 3) +           # Smooth trend for Reference Group (reduced k)
    factor(Location),          # Location fixed effects
  data = unemployed,
  method = "ML"
)
model_comparisontable =tibble(SIC=c(BIC(spliner),BIC(spline1)),
                              Akaike=c(AICc(spliner),
                                       AICc(spline1)))
model_comparisontable = model_comparisontable %>% mutate(,ChangeSchwarz=SIC-min(SIC),ChangeAkaike=Akaike-min(Akaike),RL=exp(-0.5*(ChangeAkaike)),BF=(exp(-0.5*(ChangeSchwarz))))
gt::gt(model_comparisontable)
diff_fit <- difference_smooths(spline1, smooth = "s(Year)")

# Plot the difference curve across time
draw(diff_fit) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Smooth Difference (Derbyshire - GManchester)")
unemployed <- na.omit(filter(gmppanel_prop,Classification=="Higher managerial, administrative & professional occupations" ))
unemployed <- unemployed %>% 
  filter(Location != "Tameside District",Year>=1971) %>% 
  mutate(treatment = factor(ifelse(Location == "High Peak District", 1, 0)),
         established = ifelse(Year>=2011,1,0),
         did= established*treatment)
ggplot(data = unemployed,mapping = aes(x=Year,y=Proportion,colour = factor(Location)))+geom_line()+ggtitle(unique(unemployed$Classification))
ggplot(data = unemployed,mapping = aes(x=Year,y=Proportion,colour = factor(treatment)))+geom_smooth(,se=FALSE,span=0.7)+ggtitle(unique(unemployed$Classification))
ggplot(data = unemployed,mapping = aes(x=Year,y=Proportion,colour = factor(treatment)))+stat_summary(
  fun.data = "mean_cl_boot", lwd=1,geom = "line"
)+ggtitle(unique(unemployed$Classification))
spline1 <- gam(
  Proportion ~                        # Smooth trend for Reference Group (reduced k)
    s(Year, by = treatment, k = 3) + # Difference spline (reduced k)
    factor(Location),          # Location fixed effects
  data = unemployed,
  method = "ML"
)
anova(spline1)
spliner <- gam(
  Proportion ~             # Parametric intercept difference
    s(Year, k = 3) +           # Smooth trend for Reference Group (reduced k)
    factor(Location),          # Location fixed effects
  data = unemployed,
  method = "ML"
)
model_comparisontable =tibble(SIC=c(BIC(spliner),BIC(spline1)),
                              Akaike=c(AICc(spliner),
                                       AICc(spline1)))
model_comparisontable = model_comparisontable %>% mutate(,ChangeSchwarz=SIC-min(SIC),ChangeAkaike=Akaike-min(Akaike),RL=exp(-0.5*(ChangeAkaike)),BF=(exp(-0.5*(ChangeSchwarz))))
gt::gt(model_comparisontable)
diff_fit <- difference_smooths(spline1, smooth = "s(Year)")

# Plot the difference curve across time
draw(diff_fit) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Smooth Difference (Derbyshire - GManchester)")

