#title: "Researcher Mentoring Program for Early-Career Researchers (2026)"
# subtitle: "Data Script for Preprocessing the EOIs"
#author: "Jen Beaudry"
#date: "November 2025"


# This script cleans the data and gives us a quick report of how many people
# applied from each College. 

# Ideally, I want to build this in to be run in the 02 Rmd file, but I don't have
# time in 2026. Review this at the end of the year. I think I have figured out
# how to bring this data into 02.Rmd (2026 attempt).

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
  mutate(first_name = str_to_title(first_name), 
         preferred_name = str_to_title(preferred_name), 
         surname = str_to_title(surname)) %>% 
  mutate(id = 1:n()) %>% 
  relocate(id, .before = first_name) %>% 
  filter(surname != "Fgh") # remove a test case, do it now because I didn't see it until I worked with the data & id numbers


# mentee data
df_tee <- here::here("00_data", "raw_data", "eoi_mentee.csv") %>%
  read_qualtrics(legacy = FALSE) %>%
  select(-c("start_date":"user_language", "cv_file_id":"cv_file_type")) %>% 
  filter(!is.na(surname)) %>% # remove anyone that didn't provide their last name
  mutate(first_name = str_to_title(first_name), 
         preferred_name = str_to_title(preferred_name), 
         surname = str_to_title(surname)) %>% 
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
df_tor [df_tor == "O'dea"] <- "O'Dea"

# recode variable labels according to metadata for mentee data

df_tee <- meta_rename(df = df_tee, metadata = meta_tee, old = old_variable, new = new_variable)  


#### CREATE NEW VARIABLES ####

df_tor <- df_tor %>% 
  unite ("name", preferred_name:surname, sep = " ", remove = FALSE)

df_tee <- df_tee %>% 
  unite ("name", preferred_name:surname, sep = " ", remove = FALSE)

df_tor <- df_tor %>% 
  unite ("name and college", c(name,college), sep = " - ", remove = FALSE)

df_tee <- df_tee %>% 
  unite ("name and college", c(name,college), sep = " - ", remove = FALSE)

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
# data for reporting]

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

# six possible duplicate flagged; 5 real duplicates 
# delete mentor_27; mentor_23 was the complete application
# delete mentor_1 & mentor_29; mentor_26 was the complete application
# delete mentor_31; mentor_36 was the complete application
# delete mentor_37; mentor_28 was the complete application
# delete mentor_18; mentor_42 was teh complete application

# mark the duplicates in the data; if none, add a duplicate column so it matches the 
  # mentee data and the rest of the code

df_tor <- df_tor %>%
  mutate (duplicate = ifelse ((id %in% "mentor_27") |
                                (id %in% "mentor_1")|
                                (id %in% "mentor_29")|
                                (id %in% "mentor_31")|
                                (id %in% "mentor_37")|
                                (id %in% "mentor_42"),
                              "yes",
                              "no"
  )) 



# double check by looking at first name duplicates before moving forward
which(duplicated(df_tor$first_name))

# four people share the same first names (two pairs)


# MENTEES 

which(duplicated(df_tee$surname))

# three possible duplicates flagged; 3 real duplicates
# delete mentee_39; mentee_36 was the complete application
# delete mentee_38; mentee_37 was the complete application
# delete mentee_17; mentee_43 was the complete application

# after reviewing the raw data, indicate duplicates in the df

df_tee <- df_tee %>%
  mutate (duplicate = ifelse ((id %in% "mentee_39")|
                                (id %in% "mentee_38")|
                              (id %in% "mentee_17"),
                              "yes",
                              "no"
  )) 



# double check by looking at first name duplicates before moving forward. 1 shared name. 
which(duplicated(df_tee$first_name))


#### CREATE COMBINED TIBBLES & WRITE NEW FILES ####



# when done preprocessing, write the data to new files
# row.names gets rid of the first column from the dataframe.

write.csv(df_tor, here::here("00_data", "processed_data", "eoi_mentor_preprocessed.csv"), row.names = FALSE)

write.csv(df_tee, here::here("00_data", "processed_data", "eoi_mentee_preprocessed.csv"), row.names = FALSE)


#### SAVE SIMPLE DATA FOR MENTORING EVALUATION & COLLEGES ####

# remove the duplicates & combine the mentor and mentee tibbles into one

df <- full_join(df_tor, df_tee) %>% 
  filter(duplicate == "no")
  

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
  here::here("00_data", "processed_data", "applicants_by_college_with_email_26.xlsx"),
  row.names = FALSE,
  sheetName = "Sheet1",
  col.names = TRUE,
  append = FALSE
)



