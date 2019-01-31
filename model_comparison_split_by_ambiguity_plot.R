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
           color = words.screen)) +
  geom_point(size = 1, alpha=0.5) +
  geom_abline(slope = 1) +
  theme_bw(22) +
  labs(x = "AIC Associative model",
       y = "AIC PbV model") +
  theme(legend.position = "none",
        panel.grid.minor=element_blank(), 
        panel.grid.major=element_blank()) + 
  xlim(c(0, 400)) + 
  facet_wrap( ~ words.screen, scales = "fixed" )
ggsave("ambiguity.pdf", width=10, height=4, units="in")

