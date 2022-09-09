library(tidyverse)
library(brms)
library(caret)
library(InformationValue)
library(janitor)

# Incremental update
increm <- read_csv("data/incremental_lookbacks.csv")

# "Off-line" model
fit <- readRDS(file = "stanout/lookback_single_ppt.rda")

increm$residuals_offline <- residuals(fit, newdata = increm)[,1]

increm <- increm %>% mutate(extreme_online = residual > 2 * sd(residual),
                            extreme_manual = sum_lookbacks > 6,
                            extreme_offline = residuals_offline > 2 * sd(residuals_offline),
                            extreme_online_p = p_obs_given_prior < 0.05)

increm %>% 
  mutate(across(starts_with("p_"), ~ . < .05)) %>% 
  pivot_longer(c(starts_with("extreme"), starts_with("p_"))) %>% 
  ggplot(aes(x = increment, y = sum_lookbacks, colour = value, group = name)) +
  geom_point(size = 3) +
  geom_line(colour = "black") +
  facet_wrap(~name, scales = "free_x")

test <- increm %>% 
  select(increment, sum_lookbacks, starts_with("extreme"))

(optimal <- optimalCutoff(test$extreme_offline, test$extreme_online)[1])
(optimal <- optimalCutoff(test$extreme_manual, test$extreme_online)[1])
(optimal <- optimalCutoff(test$extreme_offline, test$extreme_online_p)[1])
(optimal <- optimalCutoff(test$extreme_manual, test$extreme_online_p)[1])
(optimal <- optimalCutoff(test$extreme_manual, test$extreme_offline)[1])

(cm <- confusionMatrix(test$extreme_offline, test$extreme_online))
# Correct classification accuracy rate
# Precision: TP / (TP + FP)
(precision <- cm[2,2] / (cm[2,2] + cm[1,2]))
sensitivity(test$extreme_offline, test$extreme_online)

# Recall: TP / (TP / FN)
(recall <- cm[2,2] / (cm[2,2] + cm[2,1]))

specificity(test$extreme_offline, test$extreme_online) # true negative rate
misClassError(test$extreme_offline, test$extreme_online, threshold = optimal)

# F1 score: (2 * Precicion * Recall) / (precision + Recall)
(2 * precision * recall) / (precision + recall)

(cm <- confusionMatrix(test$extreme_manual, test$extreme_online))
# Correct classification accuracy rate
# Precision: TP / (TP + FP)
(precision <- cm[2,2] / (cm[2,2] + cm[1,2]))
sensitivity(test$extreme_offline, test$extreme_online)

# Recall: TP / (TP / FN)
(recall <- cm[2,2] / (cm[2,2] + cm[2,1]))

specificity(test$extreme_offline, test$extreme_online) # true negative rate
misClassError(test$extreme_offline, test$extreme_online, threshold = optimal)

# F1 score: (2 * Precicion * Recall) / (precision + Recall)
(2 * precision * recall) / (precision + recall)

(cm <- confusionMatrix(test$extreme_offline, test$extreme_online_p))
# Correct classification accuracy rate
# Precision: TP / (TP + FP)
(precision <- cm[2,2] / (cm[2,2] + cm[1,2]))
sensitivity(test$extreme_offline, test$extreme_online)

# Recall: TP / (TP / FN)
(recall <- cm[2,2] / (cm[2,2] + cm[2,1]))

specificity(test$extreme_offline, test$extreme_online) # true negative rate
misClassError(test$extreme_offline, test$extreme_online, threshold = optimal)

# F1 score: (2 * Precicion * Recall) / (precision + Recall)
(2 * precision * recall) / (precision + recall)

(cm <- confusionMatrix(test$extreme_manual, test$extreme_online_p))
# Correct classification accuracy rate
# Precision: TP / (TP + FP)
(precision <- cm[2,2] / (cm[2,2] + cm[1,2]))
sensitivity(test$extreme_offline, test$extreme_online)

# Recall: TP / (TP / FN)
(recall <- cm[2,2] / (cm[2,2] + cm[2,1]))

specificity(test$extreme_offline, test$extreme_online) # true negative rate
misClassError(test$extreme_offline, test$extreme_online, threshold = optimal)

# F1 score: (2 * Precicion * Recall) / (precision + Recall)
(2 * precision * recall) / (precision + recall)

(cm <- confusionMatrix(test$extreme_manual, test$extreme_offline))
# Correct classification accuracy rate
# Precision: TP / (TP + FP)
(precision <- cm[2,2] / (cm[2,2] + cm[1,2]))
sensitivity(test$extreme_offline, test$extreme_online)

# Recall: TP / (TP / FN)
(recall <- cm[2,2] / (cm[2,2] + cm[2,1]))

specificity(test$extreme_offline, test$extreme_online) # true negative rate
misClassError(test$extreme_offline, test$extreme_online, threshold = optimal)

# F1 score: (2 * Precicion * Recall) / (precision + Recall)
(2 * precision * recall) / (precision + recall)


#test %>% 
#  tabyl(extreme_offline, extreme_online) %>% 
#  adorn_totals(c("row", "col"))

