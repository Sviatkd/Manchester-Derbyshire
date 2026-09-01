library(rstanarm)
library(marginaleffects)
library(bayestestR)
set.seed(123)
spline_random <- stan_glmer(
  Proportion ~ factor(treatment)*factor(Year)+(1 | Location),
  data = unemployed,
  family = gaussian(),cores=8,chains=8
)
grid = unemployed[,c(2,7)]
grid$Year = as.factor(grid$Year)
paths = predictions(spline_random,ndraws=4000,re.form=NA)
pathpred = paths[,c(2,3,4,6,10)]
pathpred = data.frame(pathpred)
ggplot(pathpred, aes(x = Year, y = estimate, color = treatment, group = treatment)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  # Add Bayesian uncertainty ribbons
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = treatment), 
              alpha = 0.15, color = NA) + scale_color_manual(
                name = "Boundary Side", 
                values = c("orange","darkgreen"), 
                labels = c("Greater Manchester","High Peak") # <--- UPDATE THIS
              ) +
  scale_fill_manual(
    name = "Boundary Side", 
    values = c("orange","darkgreen"), 
    labels = c("Greater Manchester","High Peak") # <--- MUST MATCH EXACTLY
  ) +ggtitle(unique(unemployed$Classification))+labs(subtitle = "95 percent probability that the true value lies in shaded area(posterior interval)")
describe_posterior(spline_random)
