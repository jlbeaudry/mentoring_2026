#title: "Researcher Mentoring Program for Early-Career Researchers (2026)"
# subtitle: "Data Script for Preprocessing the EOIs"
#author: "Jen Beaudry"
#date: "November 2025"


# This script cleans the data and gives us a quick report of how many people
# applied from each College. 

# This info is the same as the 00_.Rmd file, but I don't know which will be needed
# next year to run the script. Will need to play with it. 

# Ideally, I want to build this in to be run in the 02 Rmd file, but I don't have
# time in 2025. Review this at the end of the year. 

# I think that we should be able to pull from this script so the numbers are in 
# the reports and we don't have the same chunk in two (or three??) scripts 
# that are identical. [check if we need to use any of this in 01 Rmd too]
# Removes errors. 
# one way of doing this: https://bookdown.org/yihui/rmarkdown-cookbook/read-chunk.html

# If I can't figure out how to pull it, then I think I can do the analysis in '00_'
# then run that in '01_' and then just count it in '02_' for the report. There's no 
# point in running the same code repeatedly across the different files.


# Function to load or install packages
load_or_install <- function(packages) {
  for (package in packages) {
    if (!require(package, character.only = TRUE)) {
      install.packages(package, dependencies = TRUE)
      library(package, character.only = TRUE)
    }
  }
}

# Required packages 
packages <- c("here"
              , "tidyverse"
              , "xlsx"
)

# Load or install the packages
load_or_install(packages)


source(here("..", "functions", "read_qualtrics.R"))
source(here("..", "functions", "meta_rename.R"))

#### LOAD DATA ####

# mentor data

df_tor <- here::here("00_data", "raw_data", "eoi_mentor.csv") %>%
  read_qualtrics(legacy = FALSE) %>%
  select(-c("start_date":"user_language", "cv_file_id":"cv_file_type")) %>% 
  filter(!is.na(surname))  %>% # remove anyone that didn't provide their last name
  mutate(id = 1:n()) %>% 
  relocate(id, .before = first_name) 


# mentee data
df_tee <- here::here("00_data", "raw_data", "eoi_mentee.csv") %>%
  read_qualtrics(legacy = FALSE) %>%
  select(-c("start_date":"user_language", "cv_file_id":"cv_file_type")) %>% 
  filter(!is.na(surname)) %>% # remove anyone that didn't provide their last name
  mutate(id = 1:n()) %>%  
  relocate(id, .before = first_name) 

# load metadata for mentor
meta_tor <- read_csv(here::here("00_data", "raw_data", "metadata_eoi_mentor.csv"), lazy = FALSE) %>%
  filter(old_variable != "NA", old_variable != "exclude") # remove the instruction variables

# load metadata for mentee
meta_tee <- read_csv(here::here("00_data", "raw_data", "metadata_eoi_mentee.csv"), lazy = FALSE) %>%
  filter(old_variable != "NA", old_variable != "exclude") # remove the instruction variables

#### RECODE VARIABLES ####    

# recode variable labels according to metadata for mentor data

df_tor <- meta_rename(df = df_tor, metadata = meta_tor, old = old_variable, new = new_variable)

# recode variable labels according to metadata for mentee data

df_tee <- meta_rename(df = df_tee, metadata = meta_tee, old = old_variable, new = new_variable)  


#### CREATE NEW VARIABLES ####

df_tor <- df_tor %>% 
  unite ("name", preferred_name:surname, sep = " ", remove = FALSE)

df_tee <- df_tee %>% 
  unite ("name", preferred_name:surname, sep = " ", remove = FALSE)


# create a role variable for the two tibbles

df_tor$role <- "mentor"
df_tee$role <- "mentee"

# identify the mentor and mentee IDs 
df_tor$id <- paste("mentor", df_tor$id, sep = "_")
df_tee$id <- paste("mentee", df_tee$id, sep = "_")


### CHANGE VARIABLE TYPES ####

# change the type of variables from numbers to characters

df_tee <- df_tee %>% 
  mutate_if(is.numeric, as.character)

df_tor <- df_tor %>% 
  mutate_if(is.numeric, as.character)

#### CHECK FOR DUPLICATES ####

# [note: the reason this is here rather than preprocessing is so I can use the 
# data for reporting; if this chunk is updated, also update it in 00_ file]

# get our starting numbers

original_n_tee <- nrow(df_tee)
original_n_tor <- nrow(df_tor)

# we then need to create values for 'dup_tee' & 'dup_tor' that we can use in the
# report, if there are any duplicates. Don't need if there are no duplicates.

# MENTORS

# check for duplicates in the data
# integer(0) means there are no duplicates
# any other number reflects a duplicate. Two or more numbers indicates two or
# more duplicates. 

which(duplicated(df_tor$surname))

# if no duplicate, add a duplicate variable so it matches the mentee data

# five possible duplicate flagged; XXX real duplicates 
# delete mentor_27; mentor_23 was the complete application
# delete mentor_1 & mentor_29; mentor_26 was the complete application
# delete mentor_31; mentor_36 was the complete application


# after reviewing the raw data, indicate duplicates in the df
# if we have more than one, add them in like this:  ((id %in% "mentee_25") | (id %in% "mentee_23")


df_tor <- df_tor %>%
  mutate (duplicate = ifelse ((id %in% "mentor_27") |
                                (id %in% "mentor_1")|
                                (id %in% "mentor_29")|
                              (id %in% "mentor_31"),
                              "yes",
                              "no"
  )) 

# create object indicating how many mentors submitted duplicate EOIs(needed for report)

dup_tor <- df_tor %>% 
  filter(duplicate == "yes") %>% 
  nrow()

# remove duplicate from working df

df_tor <- df_tor %>% 
  filter(duplicate == "no")

# double check by looking at first name duplicates before moving forward
which(duplicated(df_tor$first_name))

# four people share the same first names (two pairs)


# MENTEES [BREADCRUMB: COME BACK TO THIS FOR 2026]

which(duplicated(df_tee$surname))

# five possible duplicates flagged; five real duplicates 
# delete mentee_23; mentee_24 was the complete application
# delete mentee_11; mentee_41 was the complete application
# delete mentee_19; mentee_46 was the complete application
# delete mentee_27; mentee_52 was the complete application
# delete mentee_33; mentee_53 was the complete application

# after reviewing the raw data, indicate duplicates in the df

df_tee <- df_tee %>%
  mutate (duplicate = ifelse ((id %in% "mentee_11") | 
                                (id %in% "mentee_19") | 
                                (id %in% "mentee_23") |
                                (id %in% "mentee_27") | 
                                (id %in% "mentee_33"),
                              "yes",
                              "no"
  )) 

# create object indicating how many mentees submitted duplicate EOIs(needed for report)

dup_tee <- df_tee %>% 
  filter(duplicate == "yes") %>% 
  nrow()

# remove duplicate from working df

df_tee <- df_tee %>% 
  filter(duplicate == "no")

# double check by looking at first name duplicates before moving forward. No shared names. 
which(duplicated(df_tee$first_name))


#### CREATE COMBINED TIBBLE ####


#combine the mentor and mentee tibbles into one

df <- full_join(df_tor, df_tee)


#### SAVE SIMPLE DATA FOR MENTORING EVALUATION & COLLEGES ####


# I need to check with the Colleges before I go ahead with the matching process.
# row.names gets rid of the first column from the dataframe.

df_by_college <- df %>% 
  select(c(college, role, name, email))

# we also need the names of mentors, so when we send out the final 
  # evaluation, we can remind those who hadn't completed an eoi to do so.

# now write the mentors names so we can send them the right targeted email

df_tor <- df_tor %>% 
  select(c(name, email))

write.xlsx(
  as.data.frame(df_tor),
  here::here("00_data", "processed_data", "mentors_26.xlsx"),
  row.names = FALSE,
  sheetName = "Sheet1",
  col.names = TRUE,
  append = FALSE
)

# now write it for the mentees & mentors with college info. We can then create 
  # a pivot table for the colleges

write.xlsx(
  as.data.frame(df_by_college),
  here::here("00_data", "processed_data", "applicants_by_college_with_email_25.xlsx"),
  row.names = FALSE,
  sheetName = "Sheet1",
  col.names = TRUE,
  append = FALSE
)



