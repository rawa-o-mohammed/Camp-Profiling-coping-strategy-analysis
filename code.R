rm(list = ls(all = T))
library(dplyr)
library(openxlsx)
library(expss)

df20 <-
  read.xlsx(
    "input/REACH_IRQ_Dataset_Camp-profiling_RoundXIV_Aug2020_published-1.xlsx",
    sheet = "HH level dataset"
  )
df19 <-
  read.xlsx("input/irq_dataset_camp_profile_round_xii_july_2019.xlsx",
            sheet = "HH data")
df18 <-
  read.xlsx(
    "input/reach_irq_cccm_dataset_camp_profiling_round_x_julyaug2018_0.xlsx",
    sheet = "HH Dataset"
  )

names(df20) <- gsub("/", ".", names(df20))
names(df19) <- gsub("/", ".", names(df19))
names(df18) <- gsub("/", ".", names(df18))


df20$emergency <-
  case_when(
    df20$child_dropout_school %in% c("no_already_did", "yes") |
      df20$adult_risky  %in% c("no_already_did", "yes") |
      df20$family_migrating %in% c("no_already_did", "yes") |
      df20$child_forced_marriage %in% c("no_already_did", "yes") ~ 1,
    (
      is.na(df20$child_dropout_school) |
        df20$child_dropout_school == "not_applicable"
    ) &
      (is.na(df20$adult_risky) |
         df20$adult_risky == "not_applicable") &
      (
        is.na(df20$family_migrating) |
          df20$family_migrating == "not_applicable"
      ) &
      (
        is.na(df20$child_forced_marriage) |
          df20$child_forced_marriage == "not_applicable"
      ) ~ NA_real_,
    TRUE ~ 0
  )

df20$crisis <-
  case_when(
    (
      df20$selling_transportation_means %in% c("no_already_did", "yes") |
        df20$change_place  %in% c("no_already_did", "yes") |
        df20$child_work %in% c("no_already_did", "yes")
    ) & df20$emergency == 0  ~ 1,
    (
      is.na(df20$selling_transportation_means) |
        df20$selling_transportation_means == "not_applicable"
    ) &
      (
        is.na(df20$change_place) | df20$change_place == "not_applicable"
      ) &
      (is.na(df20$child_work) |
         df20$child_work == "not_applicable") ~ NA_real_,
    TRUE ~ 0
  )


df20$stress <-
  case_when(
    (
      df20$selling_assets %in% c("no_already_did", "yes") |
        df20$borrow_debt  %in% c("no_already_did", "yes") |
        df20$reduce_spending %in% c("no_already_did", "yes") |
        df20$spending_savings %in% c("no_already_did", "yes")
    ) & df20$emergency == 0 & df20$crisis == 0  ~ 1,
    (
      is.na(df20$selling_assets) |
        df20$selling_assets == "not_applicable"
    ) &
      (is.na(df20$borrow_debt) |
         df20$borrow_debt == "not_applicable") &
      (
        is.na(df20$reduce_spending) |
          df20$reduce_spending == "not_applicable"
      ) &
      (
        is.na(df20$spending_savings) |
          df20$spending_savings == "not_applicable"
      ) ~ NA_real_,
    TRUE ~ 0
  )

df20$no_cop_start <-
  ifelse(sum_row(df20$emergency,
                 df20$crisis, df20$stress, na.rm = TRUE) == 0,
         1,
         0)

df20$emergency_no_na <- ifelse(is.na(df20$emergency), 0, 1)
df20$crisis_no_na <- ifelse(is.na(df20$crisis), 0, 1)
df20$stress_no_na <- ifelse(is.na(df20$stress), 0, 1)

camp_aggr <- df20 %>%
  group_by(camp_name) %>%
  summarise(
    perc_no_cop_strat = sum(no_cop_start, na.rm = TRUE) / n(),
    perc_stress = sum(stress,  na.rm = TRUE) /  sum(stress_no_na),
    perc_crisis = sum(crisis, na.rm = TRUE) /  sum(crisis_no_na),
    perc_emergency = sum(emergency, na.rm = TRUE) /  sum(emergency_no_na),
    n = n()
  )

write.csv(camp_aggr, "output/coping_strategy_findings_2020.csv")

df19$emergency <-
  case_when(
    df19$coping_strategy_last_month.child_droput_school %in% c("No, because we already did it (so cannot continue to do it)", "Yes") |
      df19$coping_strategy_last_month.adults_illigal_acts  %in% c("No, because we already did it (so cannot continue to do it)", "Yes") |
      df19$coping_strategy_last_month.family_migrating %in% c("No, because we already did it (so cannot continue to do it)", "Yes") |
      df19$coping_strategy_last_month.forced_marriage %in% c("No, because we already did it (so cannot continue to do it)", "Yes") ~ 1,
    (
      is.na(df19$coping_strategy_last_month.child_droput_school) |
        df19$coping_strategy_last_month.child_droput_school == "Not applicable (I don't have)"
    ) &
      (
        is.na(df19$coping_strategy_last_month.adults_illigal_acts) |
          df19$coping_strategy_last_month.adults_illigal_acts == "Not applicable (I don't have)"
      ) &
      (
        is.na(df19$coping_strategy_last_month.family_migrating) |
          df19$coping_strategy_last_month.family_migrating == "Not applicable (I don't have)"
      ) &
      (
        is.na(df19$coping_strategy_last_month.forced_marriage) |
          df19$coping_strategy_last_month.forced_marriage == "Not applicable (I don't have)"
      ) ~ NA_real_,
    TRUE ~ 0
  )

df19$crisis <-
  case_when(
    (
      df19$coping_strategy_last_month.selling_transportation_means %in% c("No, because we already did it (so cannot continue to do it)", "Yes") |
        df19$coping_strategy_last_month.change_place  %in% c("No, because we already did it (so cannot continue to do it)", "Yes") |
        df19$coping_strategy_last_month.child_work %in% c("No, because we already did it (so cannot continue to do it)", "Yes")
    ) & df19$emergency == 0  ~ 1,
    (
      is.na(
        df19$coping_strategy_last_month.selling_transportation_means
      ) |
        df19$coping_strategy_last_month.selling_transportation_means == "Not applicable (I don't have)"
    ) &
      (
        is.na(df19$coping_strategy_last_month.change_place) |
          df19$coping_strategy_last_month.change_place == "Not applicable (I don't have)"
      ) &
      (
        is.na(df19$coping_strategy_last_month.child_work) |
          df19$coping_strategy_last_month.child_work == "Not applicable (I don't have)"
      ) ~ NA_real_,
    TRUE ~ 0
  )


df19$stress <-
  case_when(
    (
      df19$coping_strategy_last_month.selling_assets %in% c("No, because we already did it (so cannot continue to do it)", "Yes") |
        df19$coping_strategy_last_month.borrow_debt  %in% c("No, because we already did it (so cannot continue to do it)", "Yes") |
        df19$coping_strategy_last_month.reduce_spending %in%  c("No, because we already did it (so cannot continue to do it)", "Yes") |
        df19$coping_strategy_last_month.spent_savings %in%  c("No, because we already did it (so cannot continue to do it)", "Yes")
    ) & df19$emergency == 0 & df19$crisis == 0  ~ 1,
    (
      is.na(df19$coping_strategy_last_month.selling_assets) |
        df19$coping_strategy_last_month.selling_assets == "Not applicable (I don't have)"
    ) &
      (
        is.na(df19$coping_strategy_last_month.borrow_debt) |
          df19$coping_strategy_last_month.borrow_debt == "Not applicable (I don't have)"
      ) &
      (
        is.na(df19$coping_strategy_last_month.reduce_spending) |
          df19$coping_strategy_last_month.reduce_spending == "Not applicable (I don't have)"
      ) &
      (
        is.na(df19$coping_strategy_last_month.spent_savings) |
          df19$coping_strategy_last_month.spent_savings == "Not applicable (I don't have)"
      ) ~ NA_real_,
    TRUE ~ 0
  )

df19$no_cop_start <-
  ifelse(sum_row(df19$emergency,
                 df19$crisis, df19$stress, na.rm = TRUE) == 0,
         1,
         0)

df19$emergency_no_na <- ifelse(is.na(df19$emergency), 0, 1)
df19$crisis_no_na <- ifelse(is.na(df19$crisis), 0, 1)
df19$stress_no_na <- ifelse(is.na(df19$stress), 0, 1)

sub <-
  select(
    df19,
    Camp_name,
    no_cop_start,
    stress,
    crisis,
    emergency,
    emergency_no_na,
    crisis_no_na,
    stress_no_na
  )

camp_aggr <- sub %>%
  group_by(Camp_name) %>%
  summarise(
    perc_no_cop_strat = sum(no_cop_start, na.rm = TRUE) / n(),
    perc_stress = sum(stress,  na.rm = TRUE) /  sum(stress_no_na),
    perc_crisis = sum(crisis, na.rm = TRUE) /  sum(crisis_no_na),
    perc_emergency = sum(emergency, na.rm = TRUE) /  sum(emergency_no_na),
    n = n()
  )

write.csv(camp_aggr, "output/coping_strategy_findings_2019.csv")


df18$emergency <-
  case_when(
    df18$food_security.coping_strategies_food2.child_droput_school %in% c("No_already_did", "yes") |
      df18$food_security.coping_strategies_food2.male_illigal_acts  %in% c("No_already_did", "yes") |
      df18$food_security.coping_strategies_food2.female_illigal_acts  %in% c("No_already_did", "yes") |
      df18$food_security.coping_strategies_food2.family_migrating %in% c("No_already_did", "yes") |
      df18$food_security.coping_strategies_food2.forced_marriage %in% c("No_already_did", "yes") |
      df18$food_security.coping_strategies_food2.child_marriage %in% c("No_already_did", "yes") ~ 1,
    (
      is.na(
        df18$food_security.coping_strategies_food2.child_droput_school
      ) |
        df18$food_security.coping_strategies_food2.child_droput_school == "Not_applicable"
    ) &
      (
        is.na(
          df18$food_security.coping_strategies_food2.male_illigal_acts
        ) |
          df18$food_security.coping_strategies_food2.male_illigal_acts == "Not_applicable"
      ) &
      (
        is.na(
          df18$food_security.coping_strategies_food2.female_illigal_acts
        ) |
          df18$food_security.coping_strategies_food2.female_illigal_acts == "Not_applicable"
      ) &
      (
        is.na(
          df18$food_security.coping_strategies_food2.family_migrating
        ) |
          df18$food_security.coping_strategies_food2.family_migrating == "Not_applicable"
      ) &
      (
        is.na(df18$food_security.coping_strategies_food2.forced_marriage) |
          df18$food_security.coping_strategies_food2.forced_marriage == "Not_applicable"
      ) &
      (
        is.na(df18$food_security.coping_strategies_food2.child_marriage) |
          df18$food_security.coping_strategies_food2.child_marriage == "Not_applicable"
      ) ~ NA_real_,
    TRUE ~ 0
  )

df18$crisis <-
  case_when(
    (
      df18$food_security.coping_strategies_food2.selling_transportation_means %in% c("No_already_did", "yes") |
        df18$food_security.coping_strategies_food2.change_place  %in% c("No_already_did", "yes") |
        df18$food_security.coping_strategies_food2.child_work %in% c("No_already_did", "yes")
    ) & df18$emergency == 0  ~ 1,
    (
      is.na(
        df18$food_security.coping_strategies_food2.selling_transportation_means
      ) |
        df18$food_security.coping_strategies_food2.selling_transportation_means == "Not_applicable"
    ) &
      (
        is.na(df18$food_security.coping_strategies_food2.change_place) |
          df18$food_security.coping_strategies_food2.change_place == "Not_applicable"
      ) &
      (
        is.na(df18$food_security.coping_strategies_food2.child_work) |
          df18$food_security.coping_strategies_food2.child_work == "Not_applicable"
      ) ~ NA_real_,
    TRUE ~ 0
  )


df18$stress <-
  case_when(
    (
      df18$food_security.coping_strategies_food2.selling_assets %in% c("No_already_did", "yes") |
        df18$food_security.coping_strategies_food2.borrow_debt  %in% c("No_already_did", "yes") |
        df18$food_security.coping_strategies_food2.reduce_spending %in% c("No_already_did", "yes") |
        df18$food_security.coping_strategies_food2.spent_savings %in% c("No_already_did", "yes")
    ) & df18$emergency == 0 & df18$crisis == 0  ~ 1,
    (
      is.na(df18$food_security.coping_strategies_food2.selling_assets) |
        df18$food_security.coping_strategies_food2.selling_assets == "Not_applicable"
    ) &
      (is.na(df18$food_security.coping_strategies_food2.borrow_debt) |
         df18$food_security.coping_strategies_food2.borrow_debt == "Not_applicable") &
      (
        is.na(df18$food_security.coping_strategies_food2.reduce_spending) |
          df18$food_security.coping_strategies_food2.reduce_spending == "Not_applicable"
      ) &
      (
        is.na(df18$food_security.coping_strategies_food2.spent_savings) |
          df18$food_security.coping_strategies_food2.spent_savings == "Not_applicable"
      ) ~ NA_real_,
    TRUE ~ 0
  )

df18$no_cop_start <-
  ifelse(sum_row(df18$emergency,
                 df18$crisis, df18$stress, na.rm = TRUE) == 0,
         1,
         0)

df18$emergency_no_na <- ifelse(is.na(df18$emergency), 0, 1)
df18$crisis_no_na <- ifelse(is.na(df18$crisis), 0, 1)
df18$stress_no_na <- ifelse(is.na(df18$stress), 0, 1)

camp_aggr <- df18 %>%
  group_by(Camp_name) %>%
  summarise(
    perc_no_cop_strat = sum(no_cop_start, na.rm = TRUE) / n(),
    perc_stress = sum(stress,  na.rm = TRUE) /  sum(stress_no_na),
    perc_crisis = sum(crisis, na.rm = TRUE) /  sum(crisis_no_na),
    perc_emergency = sum(emergency, na.rm = TRUE) /  sum(emergency_no_na),
    n = n()
  )



write.csv(camp_aggr, "output/coping_strategy_findings_2018.csv")
