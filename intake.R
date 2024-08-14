# ------------------- Importing Libraries and Cleaning Data -----------

# Loading Libraries
library(tidyverse)
library(janitor)
library(stringdist)
library(tigris)

# Loading in city names 
cities_louisiana <- places(state = "LA") %>%
  pull(NAME)

# Loading county names
parishes_louisiana <- counties(state = "LA") %>%
  pull(NAME)

# Importing data 
df <- read_csv("data/louisiana_police_misconduct_data_collection.csv") %>%
  clean_names()

# Renaming variables
names(df) <- substr(names(df), 1, 20)

# Cleaning our dataframe
df_clean <- df %>%
  
  # Filtering to only authorized responses
  filter(i_authorize_the_aclu == "X") %>%
  mutate(
    
    # Turning incidents into their categories
    location_of_incident = case_when(
      location_of_incident == 1 ~ "School or University",
      location_of_incident == 2 ~ "Public Street",
      location_of_incident == 3 ~ "Public Transit",
      location_of_incident == 4 ~ "Place of Worship",
      location_of_incident == 5 ~ "Public Park",
      location_of_incident == 6 ~ "Private Residence",
      location_of_incident == 7 ~ "Store",
      location_of_incident == 8 ~ "Protest",
      location_of_incident == 9 ~ "Traffic",
      location_of_incident == 10 ~ "Jail",
      location_of_incident == 11 ~ "Prison",
      TRUE ~ location_of_incident
    ),
    
    # Cleaning parish names
    cleaned_parish = gsub(" parish", "", tolower(parish_of_incident)),
    
    # Classifying parishes with our parish list with a max distance of 10
    cleaned_parish = parishes_louisiana[amatch(parish_of_incident, parishes_louisiana, maxDist = 10)],
    
    # Classifying cities with our city list with a max distance of 3
    cleaned_city = cities_louisiana[amatch(str_to_title(city_of_incident), cities_louisiana, maxDist = 3)],
    
    # Reformatting dates
    date = as.Date(str_sub(time, 1, 10), "%m/%d/%Y"),
    incident_date = as.Date(date_of_incident, "%m/%d/%y")
  ) %>%
  
  # Removing fake submissions
  filter(last_name != "Appelson") %>%
  filter(!(first_name == last_name)) %>%
  
  # Selecting / Renaming variables
  select(submit_date = date, 
         first_name,
         last_name,
         incident_date,
         parish = cleaned_parish,
         city = cleaned_city,
         location = location_of_incident,
         location_other = other_23,
         police = police_department_s_,
         victim = this_incident_happen,
         victim_other = other_26,
         wrongful_stop = wrongfully_stopped,
         wrongful_search = wrongfully_searched,
         wrongful_arrest = wrongfully_arrested,
         wrongful_force = wrongfully_subjected,
         wrongful_property = property_wrongfully_,
         wrongful_killing = a_loved_one_was_kill,
         wrongful_other = other_34,
         charges = is_the_victim_of_thi,
         narrative = in_my_words_this_is_,
         dob = the_victims_date_of_,
         race = the_victim_identifie,
         perceived_race = the_victim_is_usuall,
         gender = the_victims_gender_i,
         disability = the_victim_has_a_phy,
         targeted_race = race,
         targeted_gender = gender,
         targeted_sexuality = sexuality,
         targeted_disability = disability_or_mental,
         targeted_none = none,
         targeted_other = other_48,
         targeted_other_desc = other_49,
         impact = the_incident_s_negat
         )

# ----------------------- Data Analysis -----------------------------------

# How many submission received?
n_submission <- nrow(df_clean)

# How many submission received per month?
sub_per_month <- df_clean %>%
  mutate(month = format(submit_date, "%Y-%m")) %>%
  tabyl(month) %>%
  arrange(-n)

# Where are these incidents occurring?
parish <- df_clean %>%
  tabyl(parish) %>%
  arrange(-n)

city <- df_clean %>%
  tabyl(city) %>%
  arrange(-n)

location <- df_clean %>%
  tabyl(location)

# What police department is committing these misconduct?
police_involved <- df_clean %>%
  mutate(police = strsplit(tolower(police), ",| and | or|;")) %>%
  unnest(police) %>%
  mutate(police = case_when(
    police == "jpso" ~ "jefferson parish",
    TRUE ~ police
  )) %>%
  tabyl(police) %>%
  arrange(police)

# Who is Reporting?
victim <- df_clean %>%
  tabyl(victim) %>%
  arrange(-n)

# What occurred?
violence_type <- df_clean %>% 
  mutate(across(c("wrongful_stop":"wrongful_other"), ~ ifelse(. == "X", cur_column(), NA))) %>%
  pivot_longer(
    cols = c("wrongful_stop":"wrongful_other"),
    names_to = "category",
    values_drop_na = TRUE
  ) %>%
  tabyl(category) %>%
  arrange(-n)

# What are their demographics?

## Race
race <- df_clean %>%
  tabyl(race) %>%
  arrange(-n)

## Perceived Race
perceived_race <- df_clean %>%
  tabyl(perceived_race) %>%
  arrange(-n)

## Gender
gender <- df_clean %>%
  tabyl(gender) %>%
  arrange(-n)

## Disability
disability <- df_clean %>%
  tabyl(disability) %>%
  arrange(-n)

# Why were they targeted?
target_reason <- df_clean %>% 
  mutate(across(c("targeted_race":"targeted_other"), ~ ifelse(. == "X", cur_column(), NA))) %>%
  pivot_longer(
    cols = c("targeted_race":"targeted_other"),
    names_to = "category",
    values_drop_na = TRUE
  ) %>%
  tabyl(category) %>%
  arrange(-n)

# What impact has this had on their well-being?
impact <- df_clean %>%
  tabyl(impact)

# What happened?
all_narratives <- df_clean$narrative
all_sentences <- unlist(map(df_clean$narrative, ~ str_split(.x, "\\.")))