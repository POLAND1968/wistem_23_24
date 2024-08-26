# Data Cleaning ----
library(tidyverse) # loads all necessary packages to write basic code
library(readr) # helps read in data files (csv)
library(readxl) # helps read in data files (xls and xlxs)
library(skimr) # help skim data and see its general distribution
library(janitor) # helps with cleaning the data (variable names)

# Student Demographics ----
student_dem <- read_excel(path = "data/raw/student_dem_raw.xlsx") %>% 
  janitor::clean_names()

student_dem <- student_dem %>% 
  rename(race = federal_race_name) %>% 
  select(-gender, -activity) %>% 
  mutate(name = paste(first_name, last_name)) %>% 
  mutate(grade = case_when(
    grad_year == 2027 ~ 9,
    grad_year == 2026 ~ 10,
    grad_year == 2025 ~ 11,
    grad_year == 2024 ~ 12,
    TRUE ~ NA_integer_  
  )) %>% 
  unique()

student_dem$race <- recode(student_dem$race, "Two or More Races" = "ToM") 
student_dem$race <- recode(student_dem$race, "Hispanic or Latino" = "Hispanic") 
student_dem$race <- recode(student_dem$race, "Black or African American" = "Black")
student_dem$race <- recode(student_dem$race, "American Indian or Alaska Native" = "Native")

write_csv(student_dem, file = "data/processed/student_dem_processed.csv")
student_dem <- read_csv(file = "data/processed/student_dem_processed.csv")

## Checking Missing Student Information ----
### WiSTEM ---- 
wistem_attendance_data$student_present <- wistem_attendance_data$full_name %in% student_dem$name

wistem_needed_students <- wistem_attendance_data %>% 
  filter(student_present == "FALSE") %>% 
  filter(!is.na(full_name)) %>% 
  select(id, first_name, last_name, full_name)

write_csv(wistem_needed_students, file = "wistem_needed_students.csv")

### WiENG ----
wieng_attendance_data$student_present <- wieng_attendance_data$full_name %in% student_dem$name

wieng_needed_students <- wieng_attendance_data %>% 
  filter(student_present == "FALSE") %>% 
  filter(!is.na(full_name)) %>% 
  select(id, first_name, last_name, full_name)

write_csv(wieng_needed_students, file = "wieng_needed_students.csv")

# WiSTEM Student Entry Forms ----
## Load in Data Set ----
entry_form <- read_csv(file = "data/raw/wistem_2324_entry_form_raw.csv")

## Change Variable Names ----
entry_form_new <- entry_form %>%
  row_to_names(row_number = 1) # make first row the variable names

entry_form_new = entry_form_new[-1,] # remove first row

## Make Factors ----
entry_form_new <- entry_form_new %>% 
  mutate(
    survey_type = as_factor(survey_type), 
    involved = as_factor(involved), 
    work_hard = as_factor(work_hard), 
    courses = as_factor(courses), 
    succesful = as_factor(succesful), 
    member = as_factor(member), 
    career = as_factor(career), 
    identity = as_factor(identity), 
    meaningful = as_factor(meaningful), 
    everyday = as_factor(everyday)
  )

## Ordering the Date ----
entry_form_new$date <- factor(entry_form_new$date, levels = c("Sep 29th", "Oct 6th", "May 3rd"))
entry_form_new$survey_type <- factor(entry_form_new$survey_type, levels = c("Pre", "Post"))

## Renaming Observation ----
entry_form_new$career_eight <- gsub("Wed Developer", "Web Developer", entry_form_new$career_eight)
entry_form_new$career_eight <- gsub("Architecture", "Architect", entry_form_new$career_eight)

## Save Processed Data Set ----
write_csv(entry_form_new, file = "data/processed/wistem_2324_entry_form_processed.csv")
entry_form_data <- read_csv(file = "data/processed/wistem_2324_entry_form_processed.csv")

## Split Data Set ----
pre_entry_form <- entry_form_data %>% 
  filter(survey_type == "Pre")

write_csv(pre_entry_form, file = "data/processed/wistem_2324_pre_entry_form_processed.csv")
pre_entry_form_data <- read_csv(file = "data/processed/wistem_2324_pre_entry_form_processed.csv")

post_entry_form <- entry_form_data %>% 
  filter(survey_type == "Post")

write_csv(post_entry_form, file = "data/processed/wistem_2324_post_entry_form_processed.csv")
post_entry_form_data <- read_csv(file = "data/processed/wistem_2324_post_entry_form_processed.csv")

# WiSTEM Exit Slips ----
## Load in Data Set ----
wistem_exit <- read_csv(file = "data/raw/wistem_2324_exit_slips_raw.csv")

## Change Variable Names ----
wistem_exit_new <- wistem_exit %>%
  row_to_names(row_number = 1) # make first row the variable names

wistem_exit_new = wistem_exit_new[-1,] # remove first row

## Make Factors ----
wistem_exit_new <- wistem_exit_new %>% 
  mutate(
    topic = as_factor(topic), 
    interesting = as_factor(interesting), 
    included = as_factor(included), 
    belong = as_factor(belong)
  )

## Ordering the Date ----
wistem_exit_new$date <- factor(wistem_exit_new$date, levels = c("Sep 29th", "Oct 13th", "Oct 27th", "Nov 17th", "Dec 1st", "Dec 15th", "Jan 18th", "Jan 19th", "Feb 16th", "Mar 1st", "Mar 15th", "Apr 5th", "Apr 19th", "May 3rd"))

## Save Processed Data Set ----
write_csv(wistem_exit_new, file = "data/processed/wistem_2324_exit_slips_processed.csv")
wistem_exit_slip_data <- read_csv(file = "data/processed/wistem_2324_exit_slips_processed.csv")

### Pivoting Longer ----
wistem_exit_longer_data <- pivot_longer( # this will help put all the questions into one graph
  data = wistem_exit_slip_data, 
  cols = c(interesting, included, belong), 
  names_to = "question", 
  values_to = "response"
)

### Save Processed Data Set ----
write_csv(wistem_exit_longer_data, file = "data/processed/wistem_2324_exit_slips_longer_processed.csv")
wistem_exit_longer_data <- read_csv(file = "data/processed/wistem_2324_exit_slips_longer_processed.csv")

# WiSTEM Attendance ----
## Load in Data Sets ----
wistem_01_19_2024 <- read_excel(path = "data/raw/wistem_attendance/wistem_01_19_2024.xlsx") 
wistem_02_16_2024 <- read_excel(path = "data/raw/wistem_attendance/wistem_02_16_2024.xlsx") 
wistem_03_01_2024 <- read_excel(path = "data/raw/wistem_attendance/wistem_03_01_2024.xlsx") 
wistem_03_15_2024 <- read_excel(path = "data/raw/wistem_attendance/wistem_03_15_2024.xlsx")
wistem_04_05_2024 <- read_excel(path = "data/raw/wistem_attendance/wistem_04_05_2024.xlsx")
wistem_04_19_2024 <- read_excel(path = "data/raw/wistem_attendance/wistem_04_19_2024.xlsx")
wistem_05_03_2024 <- read_excel(path = "data/raw/wistem_attendance/wistem_05_03_2024.xlsx")
wistem_09_29_2023 <- read_excel(path = "data/raw/wistem_attendance/wistem_09_29_2023.xlsx")
wistem_10_13_2023 <- read_excel(path = "data/raw/wistem_attendance/wistem_10_13_2023.xlsx")
wistem_10_27_2023 <- read_excel(path = "data/raw/wistem_attendance/wistem_10_27_2023.xlsx")
wistem_11_15_2023 <- read_excel(path = "data/raw/wistem_attendance/wistem_11_15_2023.xlsx")
wistem_11_17_2023 <- read_excel(path = "data/raw/wistem_attendance/wistem_11_17_2023.xlsx")
wistem_12_01_2023 <- read_excel(path = "data/raw/wistem_attendance/wistem_12_01_2023.xlsx")
wistem_12_15_2023 <- read_excel(path = "data/raw/wistem_attendance/wistem_12_15_2023.xlsx")

## Clean Variables ----
wistem_03_01_2024  <- wistem_03_01_2024 %>% 
  janitor::clean_names() %>% # puts all variable names in snake case (all lower case letters with underscores in between words)
  select(-period, -user, -user_room, -activity)  # removes these variables that aren't helpful

wistem_01_19_2024  <- wistem_01_19_2024 %>% 
  janitor::clean_names() %>%
  select(-period, -user, -user_room, -activity)

wistem_02_16_2024  <- wistem_02_16_2024 %>% 
  janitor::clean_names() %>%
  select(-period, -user, -user_room, -activity)

wistem_03_15_2024  <- wistem_03_15_2024 %>% 
  janitor::clean_names() %>%
  select(-period, -user, -user_room, -activity)

wistem_04_05_2024  <- wistem_04_05_2024 %>% 
  janitor::clean_names() %>%
  select(-period, -user, -user_room, -activity)

wistem_04_19_2024  <- wistem_04_19_2024 %>% 
  janitor::clean_names() %>%
  select(-period, -user, -user_room, -activity)

wistem_05_03_2024  <- wistem_05_03_2024 %>% 
  janitor::clean_names() %>%
  select(-period, -user, -user_room, -activity)

wistem_09_29_2023  <- wistem_09_29_2023 %>% 
  janitor::clean_names() %>%
  select(-period, -user, -user_room, -activity)

wistem_10_13_2023 <- wistem_10_13_2023 %>% 
  janitor::clean_names() %>%
  select(-period, -user, -user_room, -activity)

wistem_10_27_2023 <- wistem_10_27_2023 %>% 
  janitor::clean_names() %>%
  select(-period, -user, -user_room, -activity)

wistem_11_17_2023  <- wistem_11_17_2023 %>% 
  janitor::clean_names() %>%
  select(-period, -user, -user_room, -activity)

wistem_11_15_2023  <- wistem_11_15_2023 %>% 
  janitor::clean_names() %>%
  select(-period, -activity)

wistem_12_01_2023  <- wistem_12_01_2023 %>% 
  janitor::clean_names() %>%
  select(-period, -user, -user_room, -activity)

wistem_12_15_2023 <- wistem_12_15_2023 %>% 
  janitor::clean_names() %>%
  select(-period, -user, -user_room, -activity)

## Create New Full Name Variable ----
wistem_01_19_2024$full_name <- paste(wistem_01_19_2024$first_name,wistem_01_19_2024$last_name)
wistem_02_16_2024$full_name <- paste(wistem_02_16_2024$first_name,wistem_02_16_2024$last_name)
wistem_03_01_2024$full_name <- paste(wistem_03_01_2024$first_name,wistem_03_01_2024$last_name)
wistem_03_15_2024$full_name <- paste(wistem_03_15_2024$first_name,wistem_03_15_2024$last_name)
wistem_04_05_2024$full_name <- paste(wistem_04_05_2024$first_name,wistem_04_05_2024$last_name)
wistem_04_19_2024$full_name <- paste(wistem_04_19_2024$first_name,wistem_04_19_2024$last_name)
wistem_05_03_2024$full_name <- paste(wistem_05_03_2024$first_name,wistem_05_03_2024$last_name)
wistem_09_29_2023$full_name <- paste(wistem_09_29_2023$first_name,wistem_09_29_2023$last_name)
wistem_10_13_2023$full_name <- paste(wistem_10_13_2023$first_name,wistem_10_13_2023$last_name)
wistem_10_27_2023$full_name <- paste(wistem_10_27_2023$first_name,wistem_10_27_2023$last_name)
wistem_11_17_2023$full_name <- paste(wistem_11_17_2023$first_name,wistem_11_17_2023$last_name)
wistem_11_15_2023$full_name <- paste(wistem_11_15_2023$first_name,wistem_11_15_2023$last_name)
wistem_12_01_2023$full_name <- paste(wistem_12_01_2023$first_name,wistem_12_01_2023$last_name)
wistem_12_15_2023$full_name <- paste(wistem_12_15_2023$first_name,wistem_12_15_2023$last_name)

## Include Vague Attendance ----
wistem_01_18_2024 <- data.frame(
  id = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA), 
  first_name = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA), 
  last_name = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA), 
  date = c("Jan 18th", "Jan 18th", "Jan 18th", "Jan 18th", "Jan 18th", "Jan 18th", "Jan 18th", "Jan 18th", "Jan 18th", "Jan 18th", "Jan 18th", "Jan 18th", "Jan 18th", "Jan 18th", "Jan 18th", "Jan 18th", "Jan 18th", "Jan 18th", "Jan 18th", "Jan 18th", "Jan 18th", "Jan 18th", "Jan 18th", "Jan 18th"),
  full_name = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
)

## Combining All Data Sets ----
wistem_attendance <- rbind(wistem_03_01_2024, wistem_03_15_2024)
wistem_attendance <- rbind(wistem_attendance, wistem_04_05_2024)
wistem_attendance <- rbind(wistem_attendance, wistem_04_19_2024)
wistem_attendance <- rbind(wistem_attendance, wistem_05_03_2024)
wistem_attendance <- rbind(wistem_attendance, wistem_09_29_2023)
wistem_attendance <- rbind(wistem_attendance, wistem_10_13_2023)
wistem_attendance <- rbind(wistem_attendance, wistem_10_27_2023)
wistem_attendance <- rbind(wistem_attendance, wistem_11_17_2023)
wistem_attendance <- rbind(wistem_attendance, wistem_12_01_2023)
wistem_attendance <- rbind(wistem_attendance, wistem_12_15_2023)
wistem_attendance <- rbind(wistem_attendance, wistem_11_15_2023)
wistem_attendance <- rbind(wistem_attendance, wistem_01_18_2024)
wistem_attendance <- rbind(wistem_attendance, wistem_01_19_2024)
wistem_attendance <- rbind(wistem_attendance, wistem_02_16_2024)

## Ordering the Date ----
wistem_attendance$date <- gsub("May 6", "May 3rd", wistem_attendance$date)
wistem_attendance$date <- factor(wistem_attendance$date, levels = c("Sep 29th", "Oct 13th", "Oct 27th", "Nov 15th", "Nov 17th", "Dec 1st", "Dec 15th", "Jan 18th", "Jan 19th", "Feb 16th", "Mar 1st", "Mar 15th", "Apr 5th", "Apr 19th", "May 3rd"))

## Adding Topic ----
wistem_topics <- wistem_exit_slip_data %>% 
  select(date, topic) %>% 
  unique() %>% 
  filter(!is.na(topic))

new_observation <- data.frame(
  date = "Nov 15th",
  topic = "Outside School Event"
)

wistem_topics <- bind_rows(wistem_topics, new_observation)
wistem_attendance <- left_join(wistem_attendance, wistem_topics)

## Adding Demographics ----
student_charac <- student_dem %>%
  select(-name) %>%
  unique()

wistem_attendance <- wistem_attendance %>% 
  mutate(id = ifelse(first_name == "Izzah" & last_name == "Shah", 627199, id)) %>% 
  mutate(id = ifelse(first_name == "Hannah" & last_name == "McLeod", 999010, id)) %>% 
  mutate(id = ifelse(first_name == "Yulian" & last_name == "Ramos", 119413, id))

wistem_attendance <- left_join(wistem_attendance, student_charac)

wistem_attendance <- wistem_attendance %>%
  mutate(white_non_white = ifelse(race == "White", "White", "Non-White"))

## Adding Number of Meetings ----
indiv_atten <- wistem_attendance %>%
  filter(!is.na(full_name)) %>% 
  count(full_name) %>% 
  rename(num_meetings = n) %>%
  select(full_name, num_meetings)

wistem_attendance <- left_join(wistem_attendance, indiv_atten)

## Save Processed Data Set ----
write_csv(wistem_attendance, file = "data/processed/wistem_2324_attendance_processed.csv")
wistem_attendance_data <- read_csv(file = "data/processed/wistem_2324_attendance_processed.csv")

# WiENG Attendance ----
## Load in Data Sets ----
wieng_01_19_2024 <- read_excel(path = "data/raw/wieng_attendance/wieng_01_19_2024.xlsx") 
wieng_01_26_2024 <- read_excel(path = "data/raw/wieng_attendance/wieng_01_26_2024.xlsx")
wieng_02_02_2024 <- read_excel(path = "data/raw/wieng_attendance/wieng_02_02_2024.xlsx")
wieng_02_09_2024 <- read_excel(path = "data/raw/wieng_attendance/wieng_02_09_2024.xlsx")
wieng_02_16_2024 <- read_excel(path = "data/raw/wieng_attendance/wieng_02_16_2024.xlsx")
wieng_02_23_2024 <- read_excel(path = "data/raw/wieng_attendance/wieng_02_23_2024.xlsx")
wieng_03_01_2024 <- read_excel(path = "data/raw/wieng_attendance/wieng_03_01_2024.xlsx")
wieng_03_08_2024 <- read_excel(path = "data/raw/wieng_attendance/wieng_03_08_2024.xlsx")
wieng_03_15_2024 <- read_excel(path = "data/raw/wieng_attendance/wieng_03_15_2024.xlsx")
wieng_03_22_2024 <- read_excel(path = "data/raw/wieng_attendance/wieng_03_22_2024.xlsx")
wieng_04_05_2024 <- read_excel(path = "data/raw/wieng_attendance/wieng_04_05_2024.xlsx")
wieng_04_12_2024 <- read_excel(path = "data/raw/wieng_attendance/wieng_04_12_2024.xlsx")
wieng_04_19_2024 <- read_excel(path = "data/raw/wieng_attendance/wieng_04_19_2024.xlsx")
wieng_04_26_2024 <- read_excel(path = "data/raw/wieng_attendance/wieng_04_26_2024.xlsx")
wieng_05_03_2024 <- read_excel(path = "data/raw/wieng_attendance/wieng_05_03_2024.xlsx")
wieng_05_10_2024 <- read_excel(path = "data/raw/wieng_attendance/wieng_05_10_2024.xlsx")
wieng_05_17_2024 <- read_excel(path = "data/raw/wieng_attendance/wieng_05_17_2024.xlsx")
wieng_10_06_2023 <- read_excel(path = "data/raw/wieng_attendance/wieng_10_06_2023.xlsx")
wieng_10_13_2023 <- read_excel(path = "data/raw/wieng_attendance/wieng_10_13_2023.xlsx")
wieng_10_20_2023 <- read_excel(path = "data/raw/wieng_attendance/wieng_10_20_2023.xlsx")
wieng_11_03_2023 <- read_excel(path = "data/raw/wieng_attendance/wieng_11_03_2023.xlsx")
wieng_11_17_2023 <- read_excel(path = "data/raw/wieng_attendance/wieng_11_17_2023.xlsx")
wieng_12_01_2023 <- read_excel(path = "data/raw/wieng_attendance/wieng_12_01_2023.xlsx")
wieng_12_08_2023 <- read_excel(path = "data/raw/wieng_attendance/wieng_12_08_2023.xlsx")
wieng_12_15_2023 <- read_excel(path = "data/raw/wieng_attendance/wieng_12_15_2023.xlsx")
wieng_12_16_2023 <- read_excel(path = "data/raw/wieng_attendance/wieng_12_16_2023.xlsx")

## Clean Variables ----
wieng_01_19_2024 <- wieng_01_19_2024 %>% 
  janitor::clean_names() %>% # puts all variable names in snake case (all lower case letters with underscores in between words)
  select(-period, -user, -user_room, -activity) # removes these variables that aren't helpful

wieng_01_26_2024  <- wieng_01_26_2024 %>% 
  janitor::clean_names() %>%
  select(-period, -user, -user_room, -activity)

wieng_02_02_2024  <- wieng_02_02_2024 %>% 
  janitor::clean_names() %>%
  select(-period, -user, -user_room, -activity)

wieng_02_09_2024 <- wieng_02_09_2024 %>% 
  janitor::clean_names() %>%
  select(-period, -user, -user_room, -activity)

wieng_02_16_2024  <- wieng_02_16_2024 %>% 
  janitor::clean_names() %>%
  select(-period, -user, -user_room, -activity)

wieng_02_23_2024  <- wieng_02_23_2024 %>% 
  janitor::clean_names() %>%
  select(-period, -user, -user_room, -activity)

wieng_03_01_2024  <- wieng_03_01_2024 %>% 
  janitor::clean_names() %>%
  select(-period, -user, -user_room, -activity)

wieng_03_08_2024  <- wieng_03_08_2024 %>% 
  janitor::clean_names() %>%
  select(-period, -user, -user_room, -activity)

wieng_03_15_2024  <- wieng_03_15_2024 %>% 
  janitor::clean_names() %>%
  select(-period, -user, -user_room, -activity)

wieng_03_22_2024  <- wieng_03_22_2024 %>% 
  janitor::clean_names() %>%
  select(-period, -user, -user_room, -activity)

wieng_04_05_2024 <- wieng_04_05_2024 %>% 
  janitor::clean_names() %>%
  select(-period, -user, -user_room, -activity)

wieng_04_12_2024 <- wieng_04_12_2024 %>% 
  janitor::clean_names() %>%
  select(-period, -user, -user_room, -activity)

wieng_04_19_2024  <- wieng_04_19_2024 %>% 
  janitor::clean_names() %>%
  select(-period, -user, -user_room, -activity)

wieng_04_26_2024  <- wieng_04_26_2024 %>% 
  janitor::clean_names() %>%
  select(-period, -user, -user_room, -activity)

wieng_05_03_2024 <- wieng_05_03_2024 %>% 
  janitor::clean_names() %>%
  select(-period, -user, -user_room, -activity)

wieng_05_10_2024 <- wieng_05_10_2024 %>% 
  janitor::clean_names() %>%
  select(-period, -user, -user_room, -activity)

wieng_05_17_2024  <- wieng_05_17_2024 %>% 
  janitor::clean_names() %>%
  select(-period, -user, -user_room, -activity)

wieng_10_06_2023  <- wieng_10_06_2023 %>% 
  janitor::clean_names() %>%
  select(-period, -user, -user_room, -activity)

wieng_10_13_2023  <- wieng_10_13_2023 %>% 
  janitor::clean_names() %>%
  select(-period, -user, -user_room, -activity)

wieng_10_20_2023  <- wieng_10_20_2023 %>% 
  janitor::clean_names() %>%
  select(-period, -user, -user_room, -activity)

wieng_11_03_2023  <- wieng_11_03_2023 %>% 
  janitor::clean_names() %>%
  select(-period, -user, -user_room, -activity)

wieng_11_17_2023  <- wieng_11_17_2023 %>% 
  janitor::clean_names() %>%
  select(-period, -user, -user_room, -activity)

wieng_12_01_2023  <- wieng_12_01_2023 %>% 
  janitor::clean_names() %>%
  select(-period, -user, -user_room, -activity)

wieng_12_08_2023  <- wieng_12_08_2023 %>% 
  janitor::clean_names() %>%
  select(-period, -user, -user_room, -activity)

wieng_12_15_2023  <- wieng_12_15_2023 %>% 
  janitor::clean_names() %>%
  select(-period, -user, -user_room, -activity)

wieng_12_16_2023  <- wieng_12_16_2023 %>% 
  janitor::clean_names() %>%
  select(-activity)

## Create New Full Name Variable ----
wieng_01_19_2024$full_name <- paste(wieng_01_19_2024$first_name,wieng_01_19_2024$last_name)
wieng_01_26_2024$full_name <- paste(wieng_01_26_2024$first_name,wieng_01_26_2024$last_name)
wieng_02_02_2024$full_name <- paste(wieng_02_02_2024$first_name,wieng_02_02_2024$last_name)
wieng_02_09_2024$full_name <- paste(wieng_02_09_2024$first_name,wieng_02_09_2024$last_name)
wieng_02_16_2024$full_name <- paste(wieng_02_16_2024$first_name,wieng_02_16_2024$last_name)
wieng_02_23_2024$full_name <- paste(wieng_02_23_2024$first_name,wieng_02_23_2024$last_name)
wieng_03_01_2024$full_name <- paste(wieng_03_01_2024$first_name,wieng_03_01_2024$last_name)
wieng_03_08_2024$full_name <- paste(wieng_03_08_2024$first_name,wieng_03_08_2024$last_name)
wieng_03_15_2024$full_name <- paste(wieng_03_15_2024$first_name,wieng_03_15_2024$last_name)
wieng_03_22_2024$full_name <- paste(wieng_03_22_2024$first_name,wieng_03_22_2024$last_name)
wieng_04_05_2024$full_name <- paste(wieng_04_05_2024$first_name,wieng_04_05_2024$last_name)
wieng_04_12_2024$full_name <- paste(wieng_04_12_2024$first_name,wieng_04_12_2024$last_name)
wieng_04_19_2024$full_name <- paste(wieng_04_19_2024$first_name,wieng_04_19_2024$last_name)
wieng_04_26_2024$full_name <- paste(wieng_04_26_2024$first_name,wieng_04_26_2024$last_name)
wieng_05_03_2024$full_name <- paste(wieng_05_03_2024$first_name,wieng_05_03_2024$last_name)
wieng_05_10_2024$full_name <- paste(wieng_05_10_2024$first_name,wieng_05_10_2024$last_name)
wieng_05_17_2024$full_name <- paste(wieng_05_17_2024$first_name,wieng_05_17_2024$last_name)
wieng_10_06_2023$full_name <- paste(wieng_10_06_2023$first_name,wieng_10_06_2023$last_name)
wieng_10_13_2023$full_name <- paste(wieng_10_13_2023$first_name,wieng_10_13_2023$last_name)
wieng_10_20_2023$full_name <- paste(wieng_10_20_2023$first_name,wieng_10_20_2023$last_name)
wieng_11_03_2023$full_name <- paste(wieng_11_03_2023$first_name,wieng_11_03_2023$last_name)
wieng_11_17_2023$full_name <- paste(wieng_11_17_2023$first_name,wieng_11_17_2023$last_name)
wieng_12_01_2023$full_name <- paste(wieng_12_01_2023$first_name,wieng_12_01_2023$last_name)
wieng_12_08_2023$full_name <- paste(wieng_12_08_2023$first_name,wieng_12_08_2023$last_name)
wieng_12_15_2023$full_name <- paste(wieng_12_15_2023$first_name,wieng_12_15_2023$last_name)
wieng_12_16_2023$full_name <- paste(wieng_12_16_2023$first_name,wieng_12_16_2023$last_name)

## Combining All Data Sets ----
wieng_attendance <- rbind(wieng_01_19_2024, wieng_01_26_2024)
wieng_attendance <- rbind(wieng_attendance, wieng_02_02_2024)
wieng_attendance <- rbind(wieng_attendance, wieng_02_09_2024)
wieng_attendance <- rbind(wieng_attendance, wieng_02_16_2024)
wieng_attendance <- rbind(wieng_attendance, wieng_02_23_2024)
wieng_attendance <- rbind(wieng_attendance, wieng_03_01_2024)
wieng_attendance <- rbind(wieng_attendance, wieng_03_08_2024)
wieng_attendance <- rbind(wieng_attendance, wieng_03_15_2024)
wieng_attendance <- rbind(wieng_attendance, wieng_03_22_2024)
wieng_attendance <- rbind(wieng_attendance, wieng_04_05_2024)
wieng_attendance <- rbind(wieng_attendance, wieng_04_12_2024)
wieng_attendance <- rbind(wieng_attendance, wieng_04_19_2024)
wieng_attendance <- rbind(wieng_attendance, wieng_04_26_2024)
wieng_attendance <- rbind(wieng_attendance, wieng_05_03_2024)
wieng_attendance <- rbind(wieng_attendance, wieng_05_10_2024)
wieng_attendance <- rbind(wieng_attendance, wieng_05_17_2024)
wieng_attendance <- rbind(wieng_attendance, wieng_10_06_2023)
wieng_attendance <- rbind(wieng_attendance, wieng_10_13_2023)
wieng_attendance <- rbind(wieng_attendance, wieng_10_20_2023)
wieng_attendance <- rbind(wieng_attendance, wieng_11_03_2023)
wieng_attendance <- rbind(wieng_attendance, wieng_11_17_2023)
wieng_attendance <- rbind(wieng_attendance, wieng_12_01_2023)
wieng_attendance <- rbind(wieng_attendance, wieng_12_08_2023)
wieng_attendance <- rbind(wieng_attendance, wieng_12_15_2023)
wieng_attendance <- rbind(wieng_attendance, wieng_12_16_2023)

## Ordering the Date ----
wieng_attendance$date <- gsub("1715014500", "May 3rd", wieng_attendance$date)
wieng_attendance$date <- gsub("1715014560", "May 3rd", wieng_attendance$date)
wieng_attendance$date <- factor(wieng_attendance$date, levels = c("Oct 6th", "Oct 13th", "Oct 20th", "Nov 3rd", "Nov 17th", "Dec 1st", "Dec 8th", "Dec 15th", "Dec 16th", "Jan 19th", "Jan 26th", "Feb 2nd", "Feb 9th", "Feb 16th", "Feb 23rd", "Mar 1st", "Mar 8th", "Mar 15th", "Mar 22nd", "Apr 5th", "Apr 12th", "Apr 19th", "Apr 26th", "May 3rd", "May 10th", "May 17th"))

## Adding Topic ----
wieng_topics <- wieng_exit_slip_data %>% 
  select(date, topic) %>% 
  unique() %>% 
  filter(!is.na(topic))

new_observation <- data.frame(
  date = c("Nov 15th", "Dec 16th", "Jan 12th", "Feb 9th", "Mar 16th", "May 17th"), 
  topic = c("Outside School Event", "Outside School Event", "Activity", "Activity", "Outside of School Event", "Activity")
)

wieng_topics <- bind_rows(wieng_topics, new_observation)
wieng_attendance <- left_join(wieng_attendance, wieng_topics)

## Adding Demographics ----
student_charac <- student_dem %>%
  select(-name) %>%
  mutate(id = as.numeric(id)) %>% 
  unique()

wieng_attendance <- wieng_attendance %>% 
  mutate(id = as.numeric(id)) %>% 
  mutate(id = ifelse(first_name == "Ellen" & last_name == "Pepsnik", 857052, id))

wieng_attendance <- left_join(wieng_attendance, student_charac)

wieng_attendance <- wieng_attendance %>%
  mutate(white_non_white = ifelse(race == "White", "White", "Non-White"))

## Adding Number of Meetings ----
indiv_atten <- wieng_attendance %>%
  filter(!is.na(full_name)) %>% 
  count(full_name) %>% 
  rename(num_meetings = n) %>%
  select(full_name, num_meetings)

wieng_attendance <- left_join(wieng_attendance, indiv_atten)

## Save Processed Data Set ----
write_csv(wieng_attendance, file = "data/processed/wieng_2324_attendance_processed.csv")
wieng_attendance_data <- read_csv(file = "data/processed/wieng_2324_attendance_processed.csv")

# WiENG Exit Slips ----
## Load in Data Set ----
wieng_exit <- read_csv(file = "data/raw/wieng_2324_exit_slips_raw.csv")

## Change Variable Names ----
wieng_exit_new <- wieng_exit %>%
  row_to_names(row_number = 1) # make first row the variable names

wieng_exit_new = wieng_exit_new[-1,] # remove first row

## Make Factors ----
wieng_exit_new <- wieng_exit_new %>% 
  mutate(
    topic = as_factor(topic), 
    interesting = as_factor(interesting), 
    included = as_factor(included), 
    belong = as_factor(belong)
  )

## Ordering the Date ----
wieng_exit_new$date <- factor(wieng_exit_new$date, levels = c("Oct 6th", "Oct 13th", "Oct 20th", "Nov 3rd", "Nov 17th", "Dec 1st", "Dec 5th", "Dec 8th", "Dec 15th", "Jan 19th", "Jan 26th", "Feb 2nd", "Feb 9th", "Feb 16th", "Feb 23rd", "Mar 1st", "Mar 8th", "Mar 15th", "Mar 22nd", "Apr 5th", "Apr 12th", "Apr 19th", "Apr 26th", "May 3rd", "May 10th", "May 17th"))
wieng_exit_new$date <- gsub("Dec 5th", "Dec 1st", wieng_exit_new$date)

## Save Processed Data Set ----
write_csv(wieng_exit_new, file = "data/processed/wieng_2324_exit_slips_processed.csv")
wieng_exit_slip_data <- read_csv(file = "data/processed/wieng_2324_exit_slips_processed.csv")
