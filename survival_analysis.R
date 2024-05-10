library(ggplot2)
library(data.table)
library(tidyverse)
library(ggplot2)
library(stringr)
library(reshape2)
library(survival)
library(dplyr)
library(bit64)
library(dbarts)
library(knitr)
library(ROCR)
library(pROC)
library(bartMachine)
library(caret)


# Read
core_2018_0B11 <- read.csv("/restricted/projectnb/ma679/ProcessedData/ngtszwai/filter_0B11_core_2018.csv")
core_2019_0B11 <- read.csv("/restricted/projectnb/ma679/ProcessedData/ngtszwai/filter_0B11_core_2019.csv")
core_2018_0CTS <- read.csv("/restricted/projectnb/ma679/ProcessedData/ngtszwai/filter_0CTS_core_2018.csv")
core_2019_0CTS <- read.csv("/restricted/projectnb/ma679/ProcessedData/ngtszwai/filter_0CTS_core_2019.csv")

# Clean
## Filter out died = 0
core_2018_0B11 <- core_2018_0B11 %>% filter(DIED == 0)
core_2019_0B11 <- core_2019_0B11 %>% filter(DIED == 0)
core_2018_0CTS <- core_2018_0CTS %>% filter(DIED == 0)
core_2019_0CTS <- core_2019_0CTS %>% filter(DIED == 0)
merged_2018 <- bind_rows(core_2018_0B11, core_2018_0CTS)
merged_2019 <- bind_rows(core_2019_0B11, core_2019_0CTS)
## Filter out revisit
filter_2018 <- merged_2018 %>% filter(NRD_DaysToEvent>0) %>% group_by(NRD_VisitLink) %>% filter(n() > 1)
filter_2019 <- merged_2019 %>% filter(NRD_DaysToEvent>0) %>% group_by(NRD_VisitLink) %>% filter(n() > 1)
## Extract unique visit link
visit_link_2018 <- filter_2018 %>% select(NRD_VisitLink, NRD_DaysToEvent,LOS)
revisit_2018 <- visit_link_2018 %>% group_by(NRD_VisitLink) %>%
  mutate(row_id = row_number()) %>% ungroup() %>%
  pivot_wider(names_from = row_id,values_from = c(NRD_DaysToEvent, LOS),names_sep = "_")
visit_link_2019 <- filter_2019 %>% select(NRD_VisitLink, NRD_DaysToEvent,LOS)
revisit_2019 <- visit_link_2019 %>% group_by(NRD_VisitLink) %>%
  mutate(row_id = row_number()) %>% ungroup() %>%
  pivot_wider(names_from = row_id,values_from = c(NRD_DaysToEvent, LOS),names_sep = "_")
## Calculate the readmission time
revisit_2018 <- revisit_2018 %>%
  mutate(revisit_1 =  case_when(NRD_DaysToEvent_2 > NRD_DaysToEvent_1 ~  NRD_DaysToEvent_2 - LOS_1 -NRD_DaysToEvent_1,
                                NRD_DaysToEvent_2 < NRD_DaysToEvent_1 ~ NRD_DaysToEvent_1 - LOS_2 - NRD_DaysToEvent_2),
         revisit_2 = case_when(NRD_DaysToEvent_3 > NRD_DaysToEvent_2 ~  NRD_DaysToEvent_3 - LOS_2 - NRD_DaysToEvent_2,
                               NRD_DaysToEvent_3 < NRD_DaysToEvent_2 ~  NRD_DaysToEvent_2 - LOS_3 - NRD_DaysToEvent_3))
revisit_2019 <- revisit_2019 %>%
  mutate(revisit_1 =  case_when(NRD_DaysToEvent_2 > NRD_DaysToEvent_1 ~  NRD_DaysToEvent_2 - LOS_1 -NRD_DaysToEvent_1,
                                NRD_DaysToEvent_2 < NRD_DaysToEvent_1 ~ NRD_DaysToEvent_1 - LOS_2 - NRD_DaysToEvent_2),
         revisit_2 = case_when(NRD_DaysToEvent_3 > NRD_DaysToEvent_2 ~  NRD_DaysToEvent_3 - LOS_2 - NRD_DaysToEvent_2,
                               NRD_DaysToEvent_3 < NRD_DaysToEvent_2 ~  NRD_DaysToEvent_2 - LOS_3 - NRD_DaysToEvent_3))
# Merge
## Merge with merged_2018/2019
revisit_2018 <- merged_2018 %>%
  left_join(revisit_2018, by = "NRD_VisitLink") %>%
  mutate(revisit_1 = coalesce(revisit_1, NA))

revisit_2019 <- merged_2019 %>%
  left_join(revisit_2019, by = "NRD_VisitLink") %>%
  mutate(revisit_1 = coalesce(revisit_1, NA))
##
select_cloums <- revisit_2018 %>%
  select(revisit_1, NRD_VisitLink)
core_2018_distinct <- merged_2018 %>% distinct(NRD_VisitLink, .keep_all = TRUE)
core_2018 <- left_join(select_cloums, core_2018_distinct, by = "NRD_VisitLink")
core_2018 <- filter(core_2018, !is.na(revisit_1))
core_2018 <- core_2018[order(core_2018$KEY_NRD), ]

select_cloums <- revisit_2019 %>%
  select(revisit_1, NRD_VisitLink)
core_2019_distinct <- merged_2019 %>% distinct(NRD_VisitLink, .keep_all = TRUE)
core_2019 <- left_join(select_cloums, core_2019_distinct, by = "NRD_VisitLink")
core_2019 <- filter(core_2019, !is.na(revisit_1))
core_2019 <- core_2019[order(core_2019$KEY_NRD), ]
## Severity Data
command <- "awk 'NR > 19 {print substr($0, 41, 29)}' '/restricted/projectnb/ma679/Data/FileSpecifications/FileSpecifications_NRD_2018_Severity.TXT'"
severity_colnames_2018 <- trimws(system(command, intern = TRUE))
severity_2018 <- fread('/restricted/projectnb/ma679/Data/NRD_2018_Severity.CSV',  col.names = severity_colnames_2018)
severity_2018 <- severity_2018[order(severity_2018$KEY_NRD), ]
command <- "awk 'NR > 19 {print substr($0, 41, 29)}' '/restricted/projectnb/ma679/Data/FileSpecifications/FileSpecifications_NRD_2019_Severity.TXT'"
severity_colnames_2019 <- trimws(system(command, intern = TRUE))
severity_2019 <- fread('/restricted/projectnb/ma679/Data/NRD_2019_Severity.CSV',  col.names = severity_colnames_2019)
severity_2019 <- severity_2019[order(severity_2019$KEY_NRD), ]
## Merge Severity
# Convert KEY_NRD in core_2018_0B11 to integer64 before merging
core_2018$KEY_NRD <- as.integer64(core_2018$KEY_NRD)
core_2018 <- core_2018 %>%
  left_join(severity_2018, by = "KEY_NRD")
# Convert KEY_NRD in core_2019_0B11 to integer64 before merging
core_2019$KEY_NRD <- as.integer64(core_2019$KEY_NRD)
core_2019 <- core_2019 %>%
  left_join(severity_2019, by = "KEY_NRD")

## Filter out revisit or not
# Calculate max_obs_time for each dataset
max_obs_time_2018 <- max(core_2018$revisit_1, na.rm = TRUE) + 1
max_obs_time_2019 <- max(core_2019$revisit_1, na.rm = TRUE) + 1
# Now use these in your mutations
# Adjusting revisit_2018 and revisit_2019 according to the new logic
core_2018 <- core_2018 %>%
  mutate(
    Time = coalesce(revisit_1, max_obs_time_2018),  # Replace NA with max_obs_time + 1
    Event = ifelse(is.na(revisit_1), 0, 1)          # Censored if revisit_1 was NA
  )

core_2019 <- core_2019 %>%
  mutate(
    Time = coalesce(revisit_1, max_obs_time_2019),
    Event = ifelse(is.na(revisit_1), 0, 1)
  )
## Count Disease
core_2018 <- core_2018 %>%
  mutate(Diagnosis_Count = rowSums(!is.na(select(., starts_with("I10_DX"))) & select(., starts_with("I10_DX")) != "", na.rm = TRUE)) %>%
  mutate(Procedure_Count = rowSums(!is.na(select(., starts_with("I10_PR"))) & select(., starts_with("I10_PR")) != "", na.rm = TRUE))
core_2019 <- core_2019 %>%
  mutate(Diagnosis_Count = rowSums(!is.na(select(., starts_with("I10_DX"))) & select(., starts_with("I10_DX")) != "", na.rm = TRUE)) %>%
  mutate(Procedure_Count = rowSums(!is.na(select(., starts_with("I10_PR"))) & select(., starts_with("I10_PR")) != "", na.rm = TRUE))


#######################################################################
#######################################################################
############################ Survival Analysis#########################
#######################################################################
#######################################################################

# Kaplan-Meier survival estimate
# Fit Kaplan-Meier for 2018 data

km_fit1 <- survfit(Surv(time = core_2018$Time, event = core_2018$Event, type = "right") ~ 1, 
                   data = core_2018)
# Fit Kaplan-Meier for 2019 data
km_fit2 <- survfit(Surv(time = core_2019$Time, event = core_2019$Event, type = "right") ~ 1, 
                   data = core_2019)
# Create data frames from each survfit object and add a 'Group' column
km_data1 <- data.frame(
  time = km_fit1$time,
  surv = km_fit1$surv,
  upper = km_fit1$upper,
  lower = km_fit1$lower,
  group = "2018"
)

km_data2 <- data.frame(
  time = km_fit2$time,
  surv = km_fit2$surv,
  upper = km_fit2$upper,
  lower = km_fit2$lower,
  group = "2019"
)
# Combine the data frames
km_data <- rbind(km_data1, km_data2)
# Plotting the Kaplan-Meier survival curve using ggplot2
km_plot <- ggplot(km_data, aes(x = time, y = surv, color = group, linetype = group)) +
  geom_step(size = 1) +  # Use geom_step instead of geom_line for step functions
  geom_point(aes(shape = group), size = 0.2, stroke = 2) +  # Adjust point style and size
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = group), alpha = 0.1) +  # Make CI more transparent
  scale_color_manual(values = c("blue", "red")) +
  scale_fill_manual(values = c("lightblue", "pink")) +
  scale_linetype_manual(values = c("solid", "dashed")) +
  labs(x = "Time in days", y = "Cumulative Readmit Rate", 
       title = "Readmission Rate for 2018 vs 2019") +
  theme_minimal() +
  theme(legend.title = element_blank(),  # Improve theme settings for clarity
        legend.position = "bottom",
        legend.text = element_text(size = 12),  # Increase text size for better readability
        plot.title = element_text(hjust = 0.5, size = 14))

# Print the plot
print(km_plot)
#### TABLE
summary_2018 <- core_2018 %>%
  summarise(
    Number_of_patients = n_distinct(NRD_VisitLink),
    Mean_Age = mean(AGE, na.rm = TRUE),
    Mean_Time = mean(Time, na.rm = TRUE)
  )

summary_2019 <- core_2019 %>%
  summarise(
    Number_of_patients = n_distinct(NRD_VisitLink),
    Mean_Age = mean(AGE, na.rm = TRUE),
    Mean_Time = mean(Time, na.rm = TRUE)
  )

# Combine the summaries into a single data frame for display
summary_table <- rbind(summary_2018, summary_2019)
rownames(summary_table) <- c("2018", "2019")
print(kable(summary_table, format = "simple"))
#######################################################################

# cox_model
cox_model <- coxph(Surv(time = core_2018$Time, event = core_2018$Event, type = "right") 
                   ~ AGE + FEMALE + PAY1 + ZIPINC_QRTL+Diagnosis_Count +
                     Procedure_Count + HCUP_ED + APRDRG_Severity + DMONTH + DISPUNIFORM,
                   data = core_2018)
# SCHOENFELD
schoenfeld_test <- cox.zph(cox_model)
print(schoenfeld_test)
# Generating survival estimates
fit <- survfit(cox_model)
fit_df <- data.frame(time = fit$time, surv = fit$surv)
fit_df$loglog_surv <- log(-log(fit_df$surv))

# Log-log plot
cloglog_plot <- ggplot(fit_df, aes(x = time, y = loglog_surv)) +
  geom_line() +
  labs(title = "C-loglog plot for model check", x = "Time in days", y = "Cumulative hazard (log-log scale)") +
  theme_minimal()
print(cloglog_plot)
## Model Check

#Residual Check
schoenfeld_res <- cox.zph(cox_model)
plot(schoenfeld_res)
print(schoenfeld_res)
# Martingale residuals - can help identify non-linear effects
martingale_res <- residuals(cox_model, type = "martingale")
plot(core_2018$Time, martingale_res, xlab = "Time", ylab = "Martingale Residuals", main = "Residual Check")

# Calculate concordance using the cox_model
c_index <- concordance(cox_model)$concordance

# Plot survival curve
plot(fit, xlab = "Days", ylab = "Survival Probability", col = 1:2, main = "Survival Curve")

# Add legend for levels of FEMALE variable
legend("bottomleft", legend = levels(core_2018$FEMALE), col = 1:2, lty = 1)
