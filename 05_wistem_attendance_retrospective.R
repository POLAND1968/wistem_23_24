# Data Cleaning ----
library(tidyverse) # loads all necessary packages to write basic code
library(readr) # helps read in data files (csv)
library(readxl) # helps read in data files (xls and xlxs)
library(skimr) # help skim data and see its general distribution
library(janitor) # helps with cleaning the data (variable names)

wistem_attendance_2324 <- read_csv(file = "data/processed/wistem_2324_attendance_processed.csv")

wistem_attendance_2324 <- wistem_attendance_2324 |> 
  distinct(id, .keep_all = TRUE)

wistem_attendance_2223_raw <- read_csv(file = "data/raw/wistem_attendance_2223_raw.csv")

wistem_attendance_2223 <- wistem_attendance_2223_raw |> 
  distinct(student_id, .keep_all = TRUE)

wistem_attendance_2122_raw <- read_csv(file = "data/raw/wistem_attendance_2122_raw.csv")

wistem_attendance_2122 <- wistem_attendance_2122_raw |> 
  drop_na(Name)

counts <- data.frame(
  Year = c("2021-2022", "2022-2023", "2023-2024"),
  Observations = c(
    nrow(wistem_attendance_2122),
    nrow(wistem_attendance_2223),
    nrow(wistem_attendance_2324)
  )
)

# Plot the line chart
ggplot(counts, aes(x = Year, y = Observations, group = 1)) +
  geom_line(color = "#B6ACD1", size = 1) +
  geom_point(color = "#B6ACD1", size = 3) +
  geom_text(aes(label = Observations), hjust = 1.5, vjust = -0.5, color = "black", size = 4) +
  labs(
    title = "WiSTEM Attendance Across the Years",
    x = "School Year",
    y = "Number of Unique Students in WiSTEM"
  ) +
  theme_minimal()
