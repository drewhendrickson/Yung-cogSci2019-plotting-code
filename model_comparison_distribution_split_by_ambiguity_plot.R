library(dplyr)
library(tidyr)
library(ggplot2)

results <- read.csv("../updated_lowest_mllh_aic_2models.csv",
                    stringsAsFactors = F)

results <- results[,2:ncol(results)]

aic_2models_all_wide2 <- results[, c(1:3, 6:12)] %>%
  spread(model, aic)
aic_2models_all_wide2$words.screen <- factor(aic_2models_all_wide2$words.screen)

aic_2models_all_wide2$Distribution <- ifelse(aic_2models_all_wide2$condition == "uniform", "Uniform", "Zipfian")


ggplot(aic_2models_all_wide2,
       aes(x = kach_llh,
           y = pbv_llh,
           color = Distribution)) +
  geom_point(size = 1, alpha=0.3) +
  geom_abline(slope = 1) +
  theme_bw(22) +
  labs(x = "AIC Associative model",
       y = "AIC PbV model") +
  theme(legend.justification=c(0.02, 0.98), 
        legend.position=c(0.02, 0.98),
        panel.grid.minor=element_blank(), 
        panel.grid.major=element_blank()) + 
  xlim(c(0, 400)) + 
  facet_wrap( ~ words.screen, scales = "fixed" )
ggsave("distribution.pdf", width=10, height=4, units="in")

