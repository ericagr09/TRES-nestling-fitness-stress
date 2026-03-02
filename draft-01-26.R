library(lme4)
library(lmerTest)
library(dplyr)

nestling_df <- nestling_df %>% # keep only nestlings that are 5-12 days
  filter(
    nestling_stage %in% c("mid"))


nestling_df <- nestling_df %>% # center and scale temp - the avg. is not 0 
  mutate(
    avgC_capture_day_c = scale(avgC_capture_day, center = TRUE, scale = FALSE)
  )


model1 <- glmer( # run initial base model
  nestling_fate_bin ~ avgC_capture_day_c + (1 | nest_key) + (1 | band),
  data = nestling_df,
  family = binomial
)
summary(model1)


model2 <- glmer( # run context dependent  model
  nestling_fate_bin ~ avgC_capture_day * avgC_fledge_day + (1 | nest_key) + (1 | band),
  data = nestling_df,
  family = binomial
)
summary(model2)

