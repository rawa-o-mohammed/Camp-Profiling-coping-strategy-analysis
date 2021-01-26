rm(list = ls(all = T))
library(dplyr)
library(openxlsx)

df20 <- read.xlsx("input/REACH_IRQ_Dataset_Camp-profiling_RoundXIV_Aug2020_published-1.xlsx", sheet = "HH level dataset")
df19 <- read.xlsx("input/irq_dataset_camp_profile_round_xii_july_2019.xlsx",  sheet = "HH data")
df18 <- read.xlsx("input/reach_irq_cccm_dataset_camp_profiling_round_x_julyaug2018_0.xlsx", sheet = "HH Dataset")

names(df20) <- gsub("/", ".", names(df20))
names(df19) <- gsub("/", ".", names(df19))
names(df18) <- gsub("/", ".", names(df18))


df20$emergency <-
  ifelse(
    df20$child_dropout_school %in% c("no_already_did", "yes") |
      df20$adult_risky  %in% c("no_already_did", "yes") |
      df20$family_migrating %in% c("no_already_did", "yes") |
      df20$child_forced_marriage %in% c("no_already_did", "yes"),
    1,
    0
  )
df20$crisis <-
  ifelse(
    (
      df20$selling_transportation_means %in% c("no_already_did", "yes") |
        df20$change_place  %in% c("no_already_did", "yes") |
        df20$child_work %in% c("no_already_did", "yes")
    ) &
      df20$emergency == 0,
    1,
    0
  )

df20$stress <-
  ifelse(
    (
      df20$selling_assets %in% c("no_already_did", "yes") |
        df20$borrow_debt  %in% c("no_already_did", "yes") |
        df20$reduce_spending %in% c("no_already_did", "yes") |
        df20$spending_savings %in%  c("no_already_did", "yes")
    ) &
      df20$emergency ==  0 & df20$crisis == 0,
    1,
    0
  )
df20$no_cop_start <-
  ifelse(df20$emergency == 0 & df20$crisis == 0 & df20$stress == 0, 1, 0)

camp_aggr <- df20 %>%
  group_by(camp_name) %>%
  summarise(
    perc_no_cop_strat = sum(no_cop_start, na.rm = TRUE) / n(),
    perc_stress = sum(stress,  na.rm = TRUE) / n(),
    perc_crisis = sum(crisis, na.rm = TRUE) / n(),
    perc_emergency = sum(emergency, na.rm = TRUE) / n(),
    n = n()
  )

write.csv(camp_aggr, "output/coping_strategy_findings_2020.csv")


df19$emergency <-
  ifelse(
    df19$coping_strategy_last_month.child_droput_school %in% c("No, because we already did it (so cannot continue to do it)", "Yes") |
      df19$coping_strategy_last_month.adults_illigal_acts  %in% c("No, because we already did it (so cannot continue to do it)", "Yes") |
      df19$coping_strategy_last_month.family_migrating %in% c("No, because we already did it (so cannot continue to do it)", "Yes") |
      df19$coping_strategy_last_month.forced_marriage %in% c("No, because we already did it (so cannot continue to do it)", "Yes"),
    1,
    0
  )
df19$crisis <-
  ifelse(
    (
      df19$coping_strategy_last_month.selling_transportation_means %in% c("No, because we already did it (so cannot continue to do it)", "Yes") |
        df19$coping_strategy_last_month.change_place  %in% c("No, because we already did it (so cannot continue to do it)", "Yes") |
        df19$coping_strategy_last_month.child_work %in% c("No, because we already did it (so cannot continue to do it)", "Yes")
    ) &
      df19$emergency == 0,
    1,
    0
  )

df19$stress <-
  ifelse(
    (
      df19$coping_strategy_last_month.selling_assets %in% c("No, because we already did it (so cannot continue to do it)", "Yes") |
        df19$coping_strategy_last_month.borrow_debt  %in% c("No, because we already did it (so cannot continue to do it)", "Yes") |
        df19$coping_strategy_last_month.reduce_spending %in%  c("No, because we already did it (so cannot continue to do it)", "Yes") |
        df19$coping_strategy_last_month.spent_savings %in%  c("No, because we already did it (so cannot continue to do it)", "Yes")
    ) &
      df19$emergency ==  0 & df19$crisis == 0,
    1,
    0
  )
df19$no_cop_start <-
  ifelse(df19$emergency == 0 & df19$crisis == 0 & df19$stress == 0, 1, 0)

sub <- select(df19, Camp_name, no_cop_start, stress, crisis, emergency, weight_camp)

camp_aggr <- sub %>%
  group_by(Camp_name) %>%
  summarise(
    perc_no_cop_strat = sum(no_cop_start, na.rm = TRUE) / n(),
    perc_stress = sum(stress,  na.rm = TRUE) / n(),
    perc_crisis = sum(crisis, na.rm = TRUE) / n(),
    perc_emergency = sum(emergency, na.rm = TRUE) / n(),
    n = n()
  )

write.csv(camp_aggr, "output/coping_strategy_findings_2019.csv")

df18$stress <-
  ifelse(
    df18$food_security.coping_strategies_food2.selling_assets %in% c("No_already_did", "yes") |
      df18$food_security.coping_strategies_food2.borrow_debt  %in% c("No_already_did", "yes") |
      df18$food_security.coping_strategies_food2.reduce_spending %in% c("No_already_did", "yes") |
      df18$food_security.coping_strategies_food2.spent_savings %in% c("No_already_did", "yes")   ,
    1,
    0
  )
df18$crisis <-
  ifelse(
    df18$food_security.coping_strategies_food2.selling_transportation_means %in% c("No_already_did", "yes") |
      df18$food_security.coping_strategies_food2.change_place  %in% c("No_already_did", "yes") |
      df18$food_security.coping_strategies_food2.child_work %in% c("No_already_did", "yes"),
    1,
    0
  )
df18$emergency <-
  ifelse(
    df18$food_security.coping_strategies_food2.child_droput_school %in% c("No_already_did", "yes") |
      df18$food_security.coping_strategies_food2.male_illigal_acts  %in% c("No_already_did", "yes") |
      df18$food_security.coping_strategies_food2.female_illigal_acts  %in% c("No_already_did", "yes") |
      df18$food_security.coping_strategies_food2.family_migrating %in% c("No_already_did", "yes") |
      df18$food_security.coping_strategies_food2.forced_marriage %in% c("No_already_did", "yes") |
      df18$food_security.coping_strategies_food2.child_marriage %in% c("No_already_did", "yes"),
    1,
    0
  )
df18$no_cop_start <-
  ifelse(df18$emergency == 0 & df18$crisis == 0 & df18$stress == 0, 1, 0)


camp_aggr <- df18 %>%
  group_by(Camp_name) %>%
  summarise(
    perc_no_cop_strat = sum(no_cop_start, na.rm = TRUE) / n(),
    perc_stress = sum(stress,  na.rm = TRUE) / n(),
    perc_crisis = sum(crisis, na.rm = TRUE) / n(),
    perc_emergency = sum(emergency, na.rm = TRUE) / n(),
    n = n()
  )

write.csv(camp_aggr, "output/coping_strategy_findings_2018.csv")


