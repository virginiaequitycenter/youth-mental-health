# Download and clean the 2023 Virginia Youth Survey data 
# Homepage: https://www.vdh.virginia.gov/virginia-youth-survey/data-tables/

# Libraries ----
library(boxr)
library(janitor)
library(readxl)
library(tidyverse)

# Instructions for setting up Box developer account and connecting it to RStudio: 
# https://r-box.github.io/boxr/articles/boxr-app-interactive.html#create
box_auth()

# Download & unzip ----

vys_dl_dir <- "data/raw/va_youth_survey/"
if (!dir.exists("data/raw/va_youth_survey/")) {
  dir.create("data/raw/va_youth_survey/")
}

box_dl(file_id = 2072282017850, local_dir = vys_dl_dir)

unzip(list.files(vys_dl_dir, pattern = "\\.zip$", full.names = T), exdir = vys_dl_dir)

# Middle school ----

## Variables of interest ---- 
# -QN10: Percentage of students who were ever in a physical fight
# -QN13: Percentage of students who were ever bullied on school property
# -QN14: Percentage of students who were ever electronically bullied (counting being 
# bullied through texting, Instagram, Facebook, or other social media)	
# -QN15: Percentage of students who ever seriously thought about killing themselves															
# -QN16: Percentage of students who ever made a plan about how they would kill themselves
# -QN17: Percentage of students who ever tried to kill themselves
# -QN45: Percentage of students who reported that their mental health was most of 
# the time or always not good (including stress, anxiety, and depression, during the 30 days before the survey)	

## Summary Table ----

# Read in:
m_sum_raw <- read_excel("data/raw/va_youth_survey/VYS 2023 Excel Tables/Middle School/2023VAM-Summary-Tables-1.xlsx", 
                        col_names = FALSE)

# Filter to variables of interest:
# Each variable of interest followed by 19 rows of additional information that is unique to each question. 
# We need to extract just the questions of interest and their supplemental information. 

m_vars <- str_c(paste0("QN", c(10, 13:17, 45)), collapse = "|")

match_rows <- which(str_detect(replace_na(m_sum_raw[[1]], ""), m_vars))

rows_to_keep <- match_rows %>%
  map(~ .x:(.x + 19)) %>%
  flatten_int() %>%
  unique()

m_sum <- m_sum_raw %>% slice(rows_to_keep)

# Tidy:
m_sum <- m_sum %>%
  select(question = ...1, pct_tot = ...3, n_tot = ...7, pct_m = ...8, n_m = ...11, pct_f = ...12, n_f = ...15) %>%
  filter(!question %in% c(NA, "Age", "Grade", "Race/Ethnicity"))

# Now every question is followed by 14 rows of information unique to each question. 
# As a result, we need to iterate our tidying based on the row index of each question using
# a for loop for ease of understanding, though this could be done with map. 

q_indices <- grepl("^Q", m_sum$question) %>%
  which()

wide_list <- vector("list", length(q_indices))

for (i in seq_along(q_indices)) {
  
  q_i <- q_indices[i]
  q_dat <- m_sum[(q_i + 1):(q_i + 14), ]

  q_dat_wide <- q_dat %>%
    pivot_wider(
      names_from = question,
      values_from = pct_tot:n_f) %>%
    clean_names() %>%
    mutate(question = m_sum$question[q_i])

  wide_list[[i]] <- q_dat_wide
}

# Prep for combining with regional data:
m_sum_clean <- bind_rows(wide_list) %>% 
  mutate(geography = "State",
         across(where(is.character) & !c(geography, question),
                ~ na_if(., "-") %>% as.numeric())) %>%
  select(question, geography, everything())


## Regions ----

# Read in:
reg_paths <- paste0("data/raw/va_youth_survey/VYS 2023 Excel Tables/Middle School/2023-MS-VYS-Regional-Results-", 
                    c("Central-Summary-Tables.xlsx",
                      "Eastern-Summary-Tables.xlsx",
                      "Northern-Summary-Tables.xlsx",
                      "Northwestern-Summary-Tables.xlsx",
                      "Southwestern-Summary-Tables.xlsx"))


m_reg_raw <- map_dfr(reg_paths, function(file) {
  read_excel(file, col_names = FALSE) %>%
    mutate(geography = basename(file))
})

# Filter to variables of interest:
# Each variable of interest followed by 20 rows of additional information that is unique to each question. 
# We need to extract just the questions of interest and their supplemental information.

match_rows_reg <- which(str_detect(replace_na(m_reg_raw[[1]], ""), m_vars))

rows_to_keep_reg <- match_rows_reg %>%
  map(~ .x:(.x + 20)) %>%
  flatten_int() %>%
  unique()

m_reg <- m_reg_raw %>% 
  mutate(
    geography = str_remove_all(geography, "2023-MS-VYS-Regional-Results-|\\-Summary-Tables\\.xlsx")) %>%
  slice(rows_to_keep_reg)

# Tidy:
m_reg <- m_reg %>%
  select(question = ...1, pct_tot = ...2, n_tot = ...4, pct_m = ...5, n_m = ...7, 
         pct_f = ...8, n_f = ...10, geography) %>%
  filter(!question %in% c(NA, "Age", "Gender", "Male", "Female", "Grade", "Race/Ethnicity")) %>%
  mutate(question = str_replace_all(question, "Overall", "Total"))

# Now every question is followed by 12 rows of information unique to each question. 
# As a result, we need to iterate our tidying based on the row index of each question using
# a for loop for ease of understanding, though this could be done with map. 

q_indices <- grepl("^Q", m_reg$question) %>%
  which()

wide_list <- vector("list", length(q_indices))

for (i in seq_along(q_indices)) {
  
  q_i <- q_indices[i]
  q_dat <- m_reg[(q_i + 1):(q_i + 12), ]
  
  q_dat_wide <- q_dat %>%
    pivot_wider(
      names_from = question,
      values_from = pct_tot:n_f) %>%
    clean_names() %>%
    mutate(question = m_reg$question[q_i])
  
  wide_list[[i]] <- q_dat_wide
}

# Prep for combining with statewide summary data:
m_reg_clean <- bind_rows(wide_list) %>%
  rename_with(~ .x %>%
                str_remove_all("_years_old") %>%
                str_remove_all("_grade")) %>%
  mutate(across(where(is.character) & !c(geography, question),
                ~ na_if(., "-") %>% as.numeric()),
         across(starts_with("pct"), ~. * 100)) %>%
  select(question, geography, everything())

# Combine regional and state data frames:
vys_middle <- bind_rows(m_sum_clean, m_reg_clean) %>%
  mutate(year = "2023")

# Save
write_csv(vys_middle, "data/vys_middle.csv")

# High School ----

## Variables of interest ----
# -QN12: Percentage of students who carried a weapon on school property (such as a gun, 
# knife, or club, on at least 1 day during the 30 days before the survey)		
# -QN13: Percentage of students who carried a gun (not counting the days when they 
# carried a gun only for hunting or for a sport such as target shooting, on at 
# least 1 day during the 12 months before the survey)	
# -QN14: Percentage of students who did not go to school because they felt unsafe at 
# school or on their way to or from school (on at least 1 day during the 30 days before the survey)	
# -QN15: Percentage of students who were threatened or injured with a weapon on school 
# property (such as a gun, knife, or club, one or more times during the 12 months before the survey)
# -QN16: Percentage of students who were in a physical fight (one or more times 
# during the 12 months before the survey)	
# -QN24: Percentage of students who were bullied on school property (ever during 
# the 12 months before the survey)
# -QN25: Percentage of students who were electronically bullied (counting being bullied 
# through texting, Instagram, Facebook, or other social media, ever during the 12 months before the survey)			
# -QN26: Percentage of students who felt sad or hopeless (almost every day for >=2 weeks 
# in a row so that they stopped doing some usual activities, ever during the 12 months before the survey)
# -QN27: Percentage of students who seriously considered attempting suicide (during 
# the 12 months before the survey)	
# -QN28: Percentage of students who made a plan about how they would attempt suicide 
# (during the 12 months before the survey)		
# -QN29: Percentage of students who actually attempted suicide (one or more times during 
# the 12 months before the survey)
# -QN30: Percentage of students who had a suicide attempt that resulted in an injury, 
# poisoning, or overdose that had to be treated by a doctor or nurse (during the 12 months before the survey)		
# -QN84: Percentage of students who reported that their mental health was most of 
# the time or always not good (including stress, anxiety, and depression, during the 30 days before the survey)																		

## Summary Table ----

# Read in:
h_sum_raw <- read_excel("data/raw/va_youth_survey/VYS 2023 Excel Tables/High School/2023VAH-Summary-Tables.xlsx",
                        col_names = FALSE)

# Filter to variables of interest:
# Each variable of interest followed by 18 rows of additional information that is unique to each question. 
# We need to extract just the questions of interest and their supplemental information. 

h_vars <- str_c(paste0("QN", c(12:16, 24:30, 84)), collapse = "|")

match_rows <- which(str_detect(replace_na(h_sum_raw[[1]], ""), h_vars))

rows_to_keep <- match_rows %>%
  map(~ .x:(.x + 19)) %>%
  flatten_int() %>%
  unique()

h_sum <- h_sum_raw %>% slice(rows_to_keep)

# Tidy:
h_sum <- h_sum %>%
  select(question = ...1, pct_tot = ...4, n_tot = ...8, pct_m = ...9, n_m = ...12, pct_f = ...14, n_f = ...17) %>%
  filter(!question %in% c(NA, "Age", "Grade", "Race/Ethnicity"))

# Now every question is followed by 14 rows of information unique to each question. 
# As a result, we need to iterate our tidying based on the row index of each question using
# a for loop for ease of understanding, though this could be done with map. 

q_indices <- grepl("^Q", h_sum$question) %>%
  which()

wide_list <- vector("list", length(q_indices))

for (i in seq_along(q_indices)) {
  
  q_i <- q_indices[i]
  q_dat <- h_sum[(q_i + 1):(q_i + 14), ]
  
  q_dat_wide <- q_dat %>%
    pivot_wider(
      names_from = question,
      values_from = pct_tot:n_f) %>%
    clean_names() %>%
    mutate(question = h_sum$question[q_i])
  
  wide_list[[i]] <- q_dat_wide
}

h_sum_clean <- bind_rows(wide_list) %>% 
  mutate(geography = "State",
         across(where(is.character) & !c(geography, question),
                ~ na_if(., "-") %>% as.numeric())) %>%
  select(question, geography, everything())

## Regions ----

# Read in:
reg_paths <- paste0("data/raw/va_youth_survey/VYS 2023 Excel Tables/High School/2023-HS-VYS-Regional-Results-", 
                    c("Central-Summar.xlsx",
                      "Eastern-Summar.xlsx",
                      "Northern-Summa.xlsx",
                      "Northwestern-S.xlsx",
                      "Southwestern-S.xlsx"))

h_reg_raw <- map_dfr(reg_paths, function(file) {
  read_excel(file, col_names = FALSE) %>%
    mutate(geography = basename(file))
})

# Filter to variables of interest: 
# Each variable of interest followed by 21 rows of additional information that is unique to each question. 
# We need to extract just the questions of interest and their supplemental information.

match_rows_reg <- which(str_detect(replace_na(h_reg_raw[[1]], ""), h_vars))

rows_to_keep_reg <- match_rows_reg %>%
  map(~ .x:(.x + 21)) %>%
  flatten_int() %>%
  unique()

h_reg <- h_reg_raw %>% 
  mutate(
    geography = str_remove_all(geography, "2023-HS-VYS-Regional-Results-|\\-Summar|umma|\\-S|\\.xlsx")) %>%
  slice(rows_to_keep_reg)

# Tidy:
h_reg <- h_reg %>%
  select(question = ...1, pct_tot = ...2, n_tot = ...4, pct_m = ...5, n_m = ...7, 
         pct_f = ...8, n_f = ...10, geography) %>%
  filter(!question %in% c(NA, "Age", "Grade", "Race/Ethnicity", "Sexual Identity")) %>%
  mutate(question = str_replace_all(question, "Overall", "Total"))

# Now every question is followed by 15 rows of information unique to each question. 
# As a result, we need to iterate our tidying based on the row index of each question using
# a for loop for ease of understanding, though this could be done with map. 

q_indices <- grepl("^Q", h_reg$question) %>%
  which()

wide_list <- vector("list", length(q_indices))

for (i in seq_along(q_indices)) {
  
  q_i <- q_indices[i]
  q_dat <- h_reg[(q_i + 1):(q_i + 15), ]
  
  q_dat_wide <- q_dat %>%
    pivot_wider(
      names_from = question,
      values_from = pct_tot:n_f) %>%
    clean_names() %>%
    mutate(question = h_reg$question[q_i])
  
  wide_list[[i]] <- q_dat_wide
}

# Prep for combining with statewide summary data:
h_reg_clean <- bind_rows(wide_list) %>%
  rename_with(~ .x %>%
                str_remove_all("_years_old") %>%
                str_remove_all("_grade")) %>%
  mutate(across(where(is.character) & !c(geography, question),
                ~ na_if(., "-") %>% as.numeric()),
         across(starts_with("pct"), ~. * 100)) %>%
  select(question, geography, everything())

# Combine regional and state data frames:
vys_high <- bind_rows(h_sum_clean, h_reg_clean) %>%
  mutate(year = "2023")

# Save
write_csv(vys_high, "data/vys_high.csv")


# Notes ----
# - NA means there were fewer than 10 students in a subgroup so the exact value is suppressed. 
# - There are some fields where the regional and state columns don't match up perfectly. For example, 
# sexual identity is not collected in the state summary data, but is collected in the regional data. As 
# a result all of the sexual identity fields for the state grouping are NA because they are not available, 
# not because they are <10.
# - Fields available statewide, but not regional: *_13, *_14_or_older, *_multiple_races, *_all_other_races
# - Fields available regionally, but not statewide: *_other_multiple_races (this is the combined multiple and other,
# *_heterosexual, *_gay_lesbian_or_bisexual)
# - Many of the responses are optional, so there might be areas where the data was not collected (and as
# a result subgroup counts do not add up to the total.


# Example viz ----


# 1. Middle school students reporting poor mental health by geography:

vys_middle <- read_csv("data/vys_middle.csv")

mh <- vys_middle %>%
  filter(str_detect(question, "QN45"))

mh_state_pct <- mh %>% filter(geography == "State") %>% select(pct_tot_total) %>% pull()
m_labs <- mh %>% filter(geography != "State") %>% select(geography, n_tot_total) %>%
  mutate(lab = paste0("N = ", n_tot_total))

mh %>%
  filter(geography != "State") %>%
  ggplot(aes(x = geography, y = pct_tot_total)) +
  geom_col(fill = "#253568") +
  geom_hline(yintercept = mh %>% filter(geography == "State") %>% select(pct_tot_total) %>% pull()) +
  geom_text(aes(label = m_labs$lab), vjust = -.5, size = 3) +
  annotate("text", x = "Central", y = 19, label = "State Summary", size = 3) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(x = NULL,
       y = "Percent of Students",
       title = "Middle school students who reported poor mental health most of the time",
       subtitle = "2023",
       caption = "Note that these are voluntary, self-reported results that are randomly administered")


# 2. High school students who did not go to school because they felt unsafe, by grade:

vys_high <- read_csv("data/vys_high.csv")

unsafe_long_pct <- vys_high %>%
  filter(str_detect(question, "QN14"),
         geography == "State") %>%
  select(pct_tot_9th:pct_tot_12th) %>%
  pivot_longer(
    everything(),
    names_to = "grade",
    values_to = "pct_tot") %>%
  mutate(
    grade = factor(
      sub("^pct_tot_", "", grade),
      levels = unique(sub("^pct_tot_", "", grade))))

unsafe_long_n <- vys_high %>%
  filter(str_detect(question, "QN14"),
         geography == "State") %>%
  select(n_tot_9th:n_tot_12th) %>%
  pivot_longer(
    everything(),
    names_to = "grade",
    values_to = "n_tot") %>%
  mutate(
    grade = factor(
      sub("^n_tot_", "", grade),
      levels = unique(sub("^n_tot_", "", grade))))

plt_dat <- left_join(unsafe_long_pct, unsafe_long_n) %>%
  mutate(lab = paste0("N = ", n_tot))

unsafe_avg <- vys_high %>% 
  filter(str_detect(question, "QN14"),
         geography == "State") %>%
  select(pct_tot_total) %>% 
  pull()


plt_dat %>%
  ggplot(aes(x = grade, y = pct_tot)) +
  geom_col(fill = "#253568") +
  geom_hline(yintercept = unsafe_avg) +
  geom_text(aes(label = lab), vjust = -.5, size = 3) +
  #annotate("text", x = "Central", y = 19, label = "State Summary", size = 3) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(x = NULL,
       y = "Percent of Students",
       title = "High school students who did not go to school because they felt unsafe",
       subtitle = "2023",
       caption = "Note that these are voluntary, self-reported results that are randomly administered")

