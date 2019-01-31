library(dplyr)
library(tidyr)
library(ggplot2)

results <- read.csv("../updated_lowest_mllh_aic_2models.csv",
                    stringsAsFactors = F)

results <- results[,2:ncol(results)]


results$guessing.[results$experiment == "m4"] <- "yes"
results$guessing.[results$experiment == "o"] <- "yes"
results$guessing.[results$experiment == "n"] <- "yes"



aic_2models_all_wide2 <- results[, c(1:3, 6:12)] %>%
  spread(model, aic)
aic_2models_all_wide2$words.screen <- factor(aic_2models_all_wide2$words.screen)


aic_2models_all_wide2 <- aic_2models_all_wide2[aic_2models_all_wide2$words.screen == 4,]

aic_2models_all_wide2$Guessing <- factor(ifelse(aic_2models_all_wide2$guessing. == "yes", 
                                                "Yes", "No"),
                                         levels=c("Yes", "No"))


ggplot(aic_2models_all_wide2,
       aes(x = kach_llh,
           y = pbv_llh,
           color = Guessing)) +
  geom_point(size = 1, alpha=0.3) +
  geom_abline(slope = 1) +
  theme_bw(22) +
  labs(x = "AIC Associative model",
       y = "AIC PbV model",
       color = "Guessing") +
  theme(legend.justification=c(0.98,0.02), 
        legend.position=c(0.98,0.02),
        panel.grid.minor=element_blank(), 
        panel.grid.major=element_blank()) + 
  xlim(c(0, 400)) + 
  ylim(c(0, 400))
ggsave("guessing.pdf", width=10, height=10, units="in")

