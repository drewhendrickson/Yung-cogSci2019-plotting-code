library(dplyr)
library(tidyr)
library(ggplot2)

results <- read.csv("../updated_lowest_mllh_aic_2models.csv",
                    stringsAsFactors = F)

results <- results[,2:ncol(results)]

aic_2models_all_wide2 <- results[, c(1:3, 6:12)] %>%
  spread(model, aic)
aic_2models_all_wide2$words.screen <- factor(aic_2models_all_wide2$words.screen)

ggplot(aic_2models_all_wide2,
       aes(x = kach_llh,
           y = pbv_llh,
           color = condition)) +
  geom_point(size = 1) +
  geom_abline(slope = 1) +
  theme_bw(32) +
  xlim(c(1, 90)) +
  ylim(c(1, 90)) +
  labs(x = "Kachergis",
       y = "PbV") +
  theme(legend.justification=c(0.02, 0.98), 
        legend.position=c(0.02, 0.98)) +
  scale_color_discrete(breaks=c("uniform", "zipfian_correlated", "zipfian_shuffled"),
                       labels=c("uniform", "zipfian (correlated length)",
                                "zipfian (uncorrelated length"))
#ggsave("main_plot.pdf", width=10, height=10, units="in")


#length(which(aic_2models_all_wide2$kach_llh > aic_2models_all_wide2$pbv_llh))
#length(which(aic_2models_all_wide2$pbv_llh > aic_2models_all_wide2$kach_llh))



ggplot(aic_2models_all_wide2,
       aes(x = kach_llh,
           y = pbv_llh,
           color = words.screen)) +
  geom_point(size = 1, alpha=0.5) +
  geom_abline(slope = 1) +
  theme_bw(22) +
  xlim(c(1, 420)) +
  ylim(c(1, 420)) +
  labs(x = "Associative",
       y = "Hypothesis testing",
       color = "Training\nAmbiguity") +
  theme(legend.justification=c(0.98, 0.02), 
        legend.position=c(0.98, 0.02),
        panel.grid.minor=element_blank(), 
        panel.grid.major=element_blank())
ggsave("main_plot.pdf", width=5, height=5, units="in")

