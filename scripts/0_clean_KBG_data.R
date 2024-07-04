##############################################
######## DATA AGGREGATION AND CLEANING #######
##############################################

# This script loads the two datasets (primary and update), cleans their date columns
# and column names individually to allow the two sets to be combined, combines 
# the two datasets, and then cleans all other columns of the combined dataset.

# clear working directory

rm(list = ls())

WD_DIR # <- insert working directory here

setwd(WD_DIR)


# load packages
source("./scripts/0_install_packages.R")

# load in both datasets
primary_data <- read_csv("./data/raw/20240629_KBG_data_primary_anonymised.csv",
                         locale = locale(encoding = "latin1"))
update_data <- read_csv("./data/raw/20240629_KBG_data_update_anonymised.csv",
                        locale = locale(encoding = "latin1"))

### PRIMARY DATASET ###
# move column names from row 2 to colnames
colnames(primary_data) <- primary_data[2,]
primary_data <- primary_data[-c(1,2),]

# prepare column names for merging (add comment column names, make lower case, remove spaces)
primary_data %<>% rename("comment1" = 16,
                         "comment2" = 17,
                         "comment3" = 18) %>%
  rename_with(~tolower(gsub(" ", "_", .x)))


# clean date column where possible and convert output to date format
# loop cycles through all elements of the raw_date column and trys to parse
# according to formats found in the data. If not possible, NA is returned.

primary_data %<>%
  rename("raw_date" = "date") %>%
  mutate("clean_date" = NA, .before = "raw_date")

for (i in 1:nrow(primary_data)) {
  date_value <- tryCatch(as.Date(primary_data$raw_date[i], 
                                 tryFormats = c("%d/%m/%Y","%d-%m-%Y",
                                                "%Y-%m-%d", "%d.%m.%Y",
                                                "%d/%m %Y")),
                         error = function(e) return(NA))
  primary_data$clean_date[i] <- date_value
}

# converts from numeric dates to human-parsable Date class
primary_data$clean_date <- as.Date(primary_data$clean_date)

sum(is.na(primary_data$clean_date))

# 112 rows have only a date range or year

### UPDATE DATASET ###
# first two rows are removed as contain metadata only
update_data <- update_data[-c(1,2),]

# columns after 16 are removed as they are empty for all rows
update_data <- update_data[,-c(17:251)]

# clean date column is added (these dates are already cleaner than the other set)
update_data %<>%
  rename("raw_date" = "DATE") %>%
  mutate("clean_date" = NA, .before = "raw_date")

update_data$clean_date <- as.Date(update_data$raw_date, "%d/%m/%Y")

# columns are renamed to match original dataset
update_data %<>%
  rename("comment1" = "COMMENTS") %>%
  rename_with(~tolower(gsub(" ", "_", .x)))

### COMBINE AND CHECK OVERLAPPING DATA
# primary and update datasets are combined, with source column to identify
# which dataset an entry is from
# all data is reindexed and an id number added in the id column so that filtered
# values can be traced back to entries in main dataset
# all entries with dates between the end of the first dataset and the start
# of the second dataset are manually checked for duplicate records (none are found)

combined_data <- bind_rows(primary_data, update_data, .id="source_data")
combined_data <- combined_data %>%
  mutate("id" = 1:nrow(combined_data), .before = "source_data")

overlap_max <- range(primary_data$clean_date, na.rm = TRUE)[2]
overlap_min <- range(update_data$clean_date, na.rm = TRUE)[1]

date_overlap_check <- combined_data %>%
  filter(clean_date > overlap_min & clean_date < overlap_max) %>%
  arrange(clean_date)

rm(date_overlap_check)

######### COMBINED DATASET ###########

str(combined_data)
# many columns include non-Unix entries

# there are large numbers of grammar/formatting differences across the dataset
# in all columns except osgridref and address, convert all values to lowercase
# and convert osgridref to uppercase and address to sentence case


combined_data %<>%
  mutate(across(is.character, ~ gsub("’", "'", .))) %>%
  mutate(across(c(where(is.character), -c("osgridref", "address","raw_date")), 
                tolower)) %>%
  mutate(osgridref = toupper(osgridref)) %>%
  mutate(address = str_to_title(address)) %>%
  # remove trailing whitespace
  mutate(across(is.character, ~ gsub(" $", "",.))) %>%
  # clean categories in bat species
  rename("common_name" = "bat_species_(common_name)") %>%
  mutate(across(common_name, ~ gsub("no bats", "no bat", .))) %>%
  mutate(across(common_name, ~ gsub("pipistrellus", "pipistrelle", .))) %>%
  mutate(across(common_name, ~ gsub("vespertilionidae", "bat", .))) %>%
  mutate(across(common_name, ~ gsub("nathusius' pipistrelle bat",
                                    "pipistrelle nathusius'", .))) %>% 
  mutate(across(common_name, ~ gsub("brown long-eared", "long-eared bat brown",.))) %>%
  mutate(common_name = ifelse(common_name=="", NA, common_name)) %>%
  # where 'count' gives a number of passes, append 'passes' to 'count_type'
  # column, and remove 'passes' from count column
  mutate(count_type = ifelse(str_detect(count, "pass"), "passes", count_type)) %>%
  mutate(across(count, ~gsub(" pass| passes", "", .))) %>%
  # where 'count' gives a number and sex, append sex to sex column
  mutate(sex = case_when(count == "1m" ~ "male",
                         count == "2f" ~ "female",
                         .default = sex)) %>%
  mutate(count = case_when(count == "1m" ~ "1",
                           count == "2f" ~ "2",
                           .default = count)) %>%
  # remove ambiguous count values (1 to 5, up to 10, etc), and "n/a"
  mutate(count = case_when(str_detect(count, "\\D") ~ NA,
                           count == "" ~ NA,
                           .default = count))

# manually check entries where 'record_type' gives species
check_record_type <- combined_data %>%
  filter(record_type %in% c("pipistrelle bat", "no bats", "natterers",
                            "whiskered bat"))

# all have data recorded in species column. remove values in record type
combined_data %<>%
  mutate(record_type = ifelse(record_type %in% c("pipistrelle bat",
                                                 "no bats", "natterers",
                                                 "whiskered bat", "",
                                                 "unknown"), 
                              NA, record_type))
# combine two variations of 'feeding and droppings' in identification column
# add NA, including to record of '20:27' (unclear information)
combined_data %<>%
  mutate(identification = case_when(identification %in% c("feeding & droppings",
                                                          "feeding remains and droppings")
                                    ~ "feeding and droppings",
                                    identification %in% c("", "20:27", "unknown")
                                    ~ NA,
                                    .default = identification))

# correct grammar error in habitat type, add NA
combined_data %<>%
  mutate(habitat = case_when(habitat == "woodland - ixed" ~ "woodland - mixed",
                             habitat == "" ~ NA,
                             .default = habitat),
         # remove ! in structure, add NA
         structure = case_when(structure == "roundabout!" ~ "roundabout",
                               structure == "" ~ NA,
                               .default = structure),
         # fix spelling in count_type, add NA
         count_type = case_when(count_type == "estiate" ~ "estimate",
                                count_type %in% c("", "unknown") ~ NA,
                                .default = count_type),
         # fix spelling in sex, add NA
         sex = case_when(sex == "ale" ~ "male",
                         sex == "feale" ~ "female",
                         sex %in% c("", "unknown", "?", "-") ~ NA,
                         .default = sex))

# manually check entries where 'sex' column has other data
check_sex <- combined_data %>%
  filter(!(sex %in% c("male", "female", NA)))

# where 'sex' gives range of numbers or 'estimate', give 'count_type' as estimate
# all other unusual values recorded elsewhere, remove
combined_data %<>%
  mutate(count_type = ifelse(sex %in% c("2-3", "estimate"), "estimate", count_type),
         sex = ifelse(sex %in% c("male", "female"), sex, NA),
         # add NAs to age column
         age = ifelse(age %in% c("", "unknown"), NA, age))

# check unusual entries in 'age' column
check_age <- combined_data %>%
  filter(!(age %in% c("adult", "juvenile", NA)))

combined_data %<>%
  # line with 'nbmp survey' in age has 'licenced bat worker' in event. this is
  # moved to experience to allow for nbmp survey to be moved into event
  mutate(experience = ifelse(age == "nbmp survey", 
                             "licenced bat worker", experience),
         # survey types included within the age column moved
         event = case_when(age == "consultant survey" ~ "consultant_survey",
                           age == "nbmp survey" ~ "nbmp survey",
                           .default = event),
         # all cases of 'licenced bat worker' in age column also have information in
         # remove all values except 'adult' and 'juvenile' from age column
         # NOTE: there are three values of 'general public' where the experience column
         # gives 'bat group member'. In these cases, the experience column is taken to
         # be authoritative
         age = ifelse(age %in% c("adult", "juvenile"), age, NA))

# fix spelling, grammar, categorisation issues in 'event', add NA
# add NA to 'experience'
combined_data %<>%
  mutate(event = case_when(event == "consultant_survey" ~ "consultant survey",
                           event == "nbmp" ~ "nbmp survey",
                           event %in% c("", "-") ~ NA,
                           .default = event),
         experience = ifelse(experience %in% c("", "unknown"), NA, experience))

# 'comment3' seems to give an old referencing system. It is removed as it does not
# provide any informative information and has no associated metadata
# blanks in 'comment1' and 'comment2' are made NA to allow for a true picture
# of the number of comments to be made
combined_data %<>%
  mutate(comment3 = NULL,
         comment1 = ifelse(comment1 == "", NA, comment1),
         comment2 = ifelse(comment2 == "", NA, comment2))

#### WHAT SHOULD I DO WITH COMMENT1 DATA??? str_detect does something, but there
# are 3028 entries just ones that contain 'survey'
comment1_no_na <- combined_data %>%
  filter(!is.na(comment1))

comment1_vector <- comment1_no_na$comment1
comment1_string <- str_c(comment1_vector, collapse = " ")

if(!require(tokenizers)){
  install.packages("tokenizers")
  library(tokenizers)
}

frequency <- tokenize_ngrams(comment1_string, n = 3L, n_min = 2L,
                             simplify = TRUE) %>%
  as_tibble() %>%
  count(value, sort = TRUE)

# working through from top of frequency list for items that appear to give
# information on survey type
filtered_hibernation_count <- combined_data %>%
  filter(str_detect(comment1, "hibernation count"))
# all confirm record is from hibernation count/survey. Many also give the series
unique(filtered_hibernation_count$comment1)
# number of which mine series in Westerham was being checked.
# check how many have 'hibernating bat' in record type column
sum(filtered_hibernation_count$record_type =="hibernating bat", na.rm = TRUE)
# all but two have this information, check these. both are records of no bat presence
check_set <- filtered_hibernation_count %>%
  filter(is.na(record_type))
# append information to event_from_comment column and add checked column 
combined_data %<>%
  mutate(event_from_comment = ifelse(str_detect(comment1, "hibernation count"),
                                     "hibernation count", NA),
         checked = case_when(str_detect(comment1, "hibernation count") ~ TRUE, 
                             comment1 == NA ~ TRUE,
                             .default = FALSE))

## 'emergence survey'
filtered_emergence_survey <- combined_data %>%
  filter(str_detect(comment1, "emergence survey"))

unique(filtered_emergence_survey$comment1)
# all confirm emergence survey. most reference consultant work
sum(!is.na(filtered_emergence_survey$count))
# one record with no count data does not give count data in comment
filtered_emergence_survey %>% filter(is.na(count))
# append 'emergence survey' to event_from_comment
combined_data %<>%
  mutate(event_from_comment = ifelse(str_detect(comment1, "emergence survey"),
                                     "emergence survey", event_from_comment),
         checked = ifelse(str_detect(comment1, "emergence survey"),
                          TRUE, checked))

# rerun frequency analysis on non-checked rows
comment1_check2 <- combined_data %>%
  filter(checked == FALSE)

comment1_vector2 <- comment1_check2$comment1
comment1_string2 <- str_c(comment1_vector2, collapse = " ")

frequency2 <- tokenize_ngrams(comment1_string2, n = 3L, n_min = 2L,
                              simplify = TRUE) %>%
  as_tibble() %>%
  count(value, sort = TRUE)

# 'nbmp'
filtered_nbmp <- combined_data %>%
  filter(checked == FALSE) %>%
  filter(str_detect(comment1, "nbmp"))

unique(filtered_nbmp$comment1)
sum(str_detect(filtered_nbmp$comment1, "nbmp temp"))
sum(str_detect(filtered_nbmp$comment1, "^nbmp$"))
sum(str_detect(filtered_nbmp$comment1, "nbmp daubenton's"))
sum(str_detect(filtered_nbmp$comment1, "nbmp serotine"))
sum(str_detect(filtered_nbmp$comment1, "nbmp sunrise"))
sum(str_detect(filtered_nbmp$comment1, "nbmp nsp"))
sum(str_detect(filtered_nbmp$comment1, "nbmp cloud"))
sum(str_detect(filtered_nbmp$comment1, "nbmp ext"))
sum(str_detect(filtered_nbmp$comment1, "nbmp field"))
sum(str_detect(filtered_nbmp$comment1, "nbmp waterway"))

# filter over each of the above, manually check, then add info to event_from_comment,
# change status of checked column

#### NBMP TEMP
filtered_nbmp_temp <- combined_data %>%
  filter(checked == FALSE) %>%
  filter(str_detect(comment1, "nbmp temp"))
# none of these have information more specific than nbmp - though they do contain
# environmental variable info which might be interesting in other studies.
combined_data %<>%
  mutate(event_from_comment = ifelse(str_detect(comment1, "nbmp temp") &
                                       checked == FALSE,
                                     "nbmp", event_from_comment),
         checked = ifelse(str_detect(comment1, "nbmp temp"),
                          TRUE, checked))
#### NBMP
filtered_nbmp_only <- combined_data %>%
  filter(checked == FALSE) %>%
  filter(str_detect(comment1, "^nbmp$"))
# none of these have information more specific than nbmp
combined_data %<>%
  mutate(event_from_comment = ifelse(str_detect(comment1, "^nbmp$") &
                                       checked == FALSE,
                                     "nbmp", event_from_comment),
         checked = ifelse(str_detect(comment1, "^nbmp$"),
                          TRUE, checked))
#### NBMP DAUBENTONS
filtered_nbmp_daubentons <- combined_data %>%
  filter(checked == FALSE) %>%
  filter(str_detect(comment1, "nbmp daubenton's"))
# all are nbmp daubenton's surveys (even when recording other species)
# these could be used to derive absence data for daubs
# some also contain environmental info
combined_data %<>%
  mutate(event_from_comment = ifelse(str_detect(comment1, "nbmp daubenton's") &
                                       checked == FALSE,
                                     "nbmp daubenton's", event_from_comment),
         checked = ifelse(str_detect(comment1, "nbmp daubenton's"),
                          TRUE, checked))

#### NBMP SEROTINE
filtered_nbmp_serotine <- combined_data %>%
  filter(checked == FALSE) %>%
  filter(str_detect(comment1, "nbmp serotine"))
# ID 20947 notes no bats, but does not specify nbmp serotine *survey* (only
# serotine roost). all others specify serotine survey
combined_data %<>%
  mutate(count = ifelse(id == 20947, 0, count),
         checked = ifelse(id == 20947, TRUE, checked)) %>%
  mutate(event_from_comment = ifelse(str_detect(comment1, "nbmp serotine") &
                                       checked == FALSE,
                                     "nbmp serotine", event_from_comment),
         checked = ifelse(str_detect(comment1, "nbmp serotine"),
                          TRUE, checked))

####  NBMP SUNRISE
filtered_nbmp_sunrise <- combined_data %>%
  filter(checked == FALSE) %>%
  filter(str_detect(comment1, "nbmp sunrise"))
# all sunrise
combined_data %<>%
  mutate(event_from_comment = ifelse(str_detect(comment1, "nbmp sunrise") &
                                       checked == FALSE,
                                     "nbmp sunrise", event_from_comment),
         checked = ifelse(str_detect(comment1, "nbmp sunrise"),
                          TRUE, checked))

#### NBMP NSP
filtered_nbmp_nsp <- combined_data %>%
  filter(checked == FALSE) %>%
  filter(str_detect(comment1, "nbmp nsp"))
# all are nsp (noctule/serotine/pip)/field surveys (these are the same survey)
# with two different names. a few records contain environmental information
combined_data %<>%
  mutate(event_from_comment = ifelse(str_detect(comment1, "nbmp nsp") &
                                       checked == FALSE,
                                     "nbmp nsp/field", event_from_comment),
         checked = ifelse(str_detect(comment1, "nbmp nsp"),
                          TRUE, checked))

#### NBMP CLOUD
filtered_nbmp_cloud <- combined_data %>%
  filter(checked == FALSE) %>%
  filter(str_detect(comment1, "nbmp cloud"))
# none contain survey information greater than 'nbmp'. most have environmental data.
# one long-eared brown bat record notes incorrect count of pipistrelles, but
# unclear where this record is
combined_data %<>%
  mutate(event_from_comment = ifelse(str_detect(comment1, "nbmp cloud") &
                                       checked == FALSE,
                                     "nbmp", event_from_comment),
         checked = ifelse(str_detect(comment1, "nbmp cloud"),
                          TRUE, checked))

#### NBMP EXT
filtered_nbmp_ext <- combined_data %>%
  filter(checked == FALSE) %>%
  filter(str_detect(comment1, "nbmp ext"))
# none have information greater than nbmp, though highly likely all are hibernation
# counts. all but three (one no info, two flying bat) specify they are observations
# of hibernating bats. two flying bat observations specify bats flying in shafts in
# comment. some entries have more specific location data inside comment
combined_data %<>%
  mutate(event_from_comment = ifelse(str_detect(comment1, "nbmp ext") &
                                       checked == FALSE,
                                     "nbmp probable hibernation count", 
                                     event_from_comment),
         checked = ifelse(str_detect(comment1, "nbmp ext"),
                          TRUE, checked))

#### NBMP FIELD
filtered_nbmp_field <- combined_data %>%
  filter(checked == FALSE) %>%
  filter(str_detect(comment1, "nbmp field"))
# all specify field survey, none give additional information. field survey is
# same as nsp
combined_data %<>%
  mutate(event_from_comment = ifelse(str_detect(comment1, "nbmp field") &
                                       checked == FALSE,
                                     "nbmp nsp/field", event_from_comment),
         checked = ifelse(str_detect(comment1, "nbmp field"),
                          TRUE, checked))

#### NBMP WATERWAY
filtered_nbmp_waterway <- combined_data %>%
  filter(checked == FALSE) %>%
  filter(str_detect(comment1, "nbmp waterway"))
# all specify waterway survey (daubs). many contain the passes information in
# the comment, though here count estimate is retained.
combined_data %<>%
  mutate(event_from_comment = ifelse(str_detect(comment1, "nbmp waterway") &
                                       checked == FALSE,
                                     "nbmp waterway", event_from_comment),
         checked = ifelse(str_detect(comment1, "nbmp waterway"),
                          TRUE, checked))

### RECHECK remaining nbmp entries
filtered_nbmp2 <- combined_data %>%
  filter(checked == FALSE) %>%
  filter(str_detect(comment1, "nbmp"))

unique(filtered_nbmp2$comment1)
sum(str_detect(filtered_nbmp2$comment1, "nbmp nathusius"))
sum(str_detect(filtered_nbmp2$comment1, "nbmp roost count"))
sum(str_detect(filtered_nbmp2$comment1, "nbmp wind"))

# filter over each of the above, manually check, then add info to event_from_comment,
# change status of checked column

#### NBMP NATHUSIUS
filtered_nbmp_nathusius <- combined_data %>%
  filter(checked == FALSE) %>%
  filter(str_detect(comment1, "nbmp nathusius"))
# these are all records from the 2009-2014 version of the Nauthusius' Pipistrelle
# Survey (distinct from the 2014-2023 National Nauthusius' Pipistrelle Project).
# most records contain more specific location data in comment
combined_data %<>%
  mutate(event_from_comment = ifelse(str_detect(comment1, "nbmp nathusius") &
                                       checked == FALSE,
                                     "nbmp nathusius 2009-2014", event_from_comment),
         checked = ifelse(str_detect(comment1, "nbmp nathusius"),
                          TRUE, checked))

#### NBMP ROOST COUNT
filtered_nbmp_roost_count <- combined_data %>%
  filter(checked == FALSE) %>%
  filter(str_detect(comment1, "nbmp roost count"))
# all from nbmp roost counts, many contain additional location information,
# no other additional information
combined_data %<>%
  mutate(event_from_comment = ifelse(str_detect(comment1, "nbmp roost count") &
                                       checked == FALSE,
                                     "nbmp roost count", event_from_comment),
         checked = ifelse(str_detect(comment1, "nbmp roost count"),
                          TRUE, checked))

#### NBMP WIND
filtered_nbmp_wind <- combined_data %>%
  filter(checked == FALSE) %>%
  filter(str_detect(comment1, "nbmp wind"))
# none of these have information more specific than nbmp - though they do contain
# environmental variable info which might be interesting in other studies.
combined_data %<>%
  mutate(event_from_comment = ifelse(str_detect(comment1, "nbmp wind") &
                                       checked == FALSE,
                                     "nbmp wind", event_from_comment),
         checked = ifelse(str_detect(comment1, "nbmp wind"),
                          TRUE, checked))

### RECHECK remaining nbmp entries
filtered_nbmp3 <- combined_data %>%
  filter(checked == FALSE) %>%
  filter(str_detect(comment1, "nbmp"))

unique(filtered_nbmp2$comment1)
sum(str_detect(filtered_nbmp2$comment1, "nsp"))
sum(str_detect(filtered_nbmp2$comment1, "noctule serotine pipistrelle"))
sum(str_detect(filtered_nbmp2$comment1, "daub"))

# filter over each of the above, manually check, then add info to event_from_comment,
# change status of checked column

#### NSP
filtered_nsp <- combined_data %>%
  filter(checked == FALSE) %>%
  filter(str_detect(comment1, "nbmp")) %>%
  filter(str_detect(comment1, "nsp"))
# all nbmp nsp/field surveys
combined_data %<>%
  mutate(event_from_comment = ifelse(str_detect(comment1, "nbmp") &
                                       str_detect(comment1, "nsp") &
                                       checked == FALSE,
                                     "nbmp nsp/field", event_from_comment),
         checked = ifelse(str_detect(comment1, "nbmp") &
                            str_detect(comment1, "nsp"),
                          TRUE, checked))

#### NOCTULE SEROTINE PIPISTRELLE
filtered_noctule_serotine_pipistrelle <- combined_data %>%
  filter(checked == FALSE) %>%
  filter(str_detect(comment1, "nbmp")) %>%
  filter(str_detect(comment1, "noctule serotine pipistrelle"))
# all contain only survey type data
combined_data %<>%
  mutate(event_from_comment = ifelse(str_detect(comment1, "nbmp") &
                                       str_detect(comment1,
                                                  "noctule serotine pipistrelle") &
                                       checked == FALSE,
                                     "nbmp nsp/field", event_from_comment),
         checked = ifelse(str_detect(comment1, "nbmp") &
                            str_detect(comment1, "noctule serotine pipistrelle"),
                          TRUE, checked))

#### NBMP WIND
filtered_daub <- combined_data %>%
  filter(checked == FALSE) %>%
  filter(str_detect(comment1, "nbmp")) %>%
  filter(str_detect(comment1, "daub"))
# ID 26083 includes comment on daubenton's numbers but does not specify survey
# type beyond nbmp. all others confirm daubenton's survey
combined_data %<>%
  mutate(event_from_comment = ifelse(id == 26083, "nbmp", event_from_comment),
         checked = ifelse(id == 26083, TRUE, checked)) %>%
  mutate(event_from_comment = ifelse(str_detect(comment1, "nbmp") &
                                       str_detect(comment1, "daub") &
                                       checked == FALSE,
                                     "nbmp daubenton's", event_from_comment),
         checked = ifelse(str_detect(comment1, "nbmp") &
                            str_detect(comment1, "daub"),
                          TRUE, checked))

### RECHECK remaining nbmp entries
filtered_nbmp4 <- combined_data %>%
  filter(checked == FALSE) %>%
  filter(str_detect(comment1, "nbmp"))

## of remaining 79 observations:
## IDs 13762, 17552 and 25645 are not nbmp surveys (and do not contain other
# survey information)
## ID 10717 is a sunrise survey.
## IDs 8872 and 8806 are brown long-eared bat surveys.
## ID 6005 is an nsp/field survey.
# all others are nbmp surveys with no further detail on survey type
combined_data %<>%
  mutate(event_from_comment = case_when(id == 10717 ~ "nbmp sunrise",
                                        id %in% c(8872, 8806) ~ "nbmp ble",
                                        id == 6005 ~ "nbmp nsp/field",
                                        .default = event_from_comment),
         checked = ifelse(id %in% c(13762, 17552, 25645, 10717, 8872,
                                    8806, 6005), TRUE, checked)) %>%
  mutate(event_from_comment = ifelse(str_detect(comment1, "nbmp") &
                                       checked == FALSE,
                                     "nbmp", event_from_comment),
         checked = ifelse(str_detect(comment1, "nbmp"), TRUE, checked))
# remove nbmp subsets to clear working directory
files <- ls()
nbmp_remove <- str_subset(files, "filtered")
rm(list = nbmp_remove)

# rerun frequency analysis on non-checked rows
comment1_check3 <- combined_data %>%
  filter(checked == FALSE)

comment1_vector3 <- comment1_check3$comment1
comment1_string3 <- str_c(comment1_vector3, collapse = " ")

frequency3 <- tokenize_ngrams(comment1_string3, n = 3L, n_min = 2L,
                              simplify = TRUE) %>%
  as_tibble() %>%
  count(value, sort = TRUE)

## 'ext temp'
filtered_ext_temp <- combined_data %>%
  filter(checked == FALSE) %>%
  filter(str_detect(comment1, "ext temp"))

# all are recordings of various environmental factors. These could be interesting
# to look at, and are reasonably well-formatted, but not here.
# none contain information about survey type or species id further than given
# some give more specific location data
combined_data %<>%
  mutate(checked = ifelse(str_detect(comment1, "ext temp"), TRUE, checked))

## 'car survey'
filtered_car_survey <- combined_data %>%
  filter(checked == FALSE) %>%
  filter(str_detect(comment1, "car survey"))

unique(filtered_car_survey$comment1)

# all contain brm car survey information and nothing else. none have correct
# information recorded in event
combined_data %<>%
  mutate(event_from_comment = ifelse(checked == FALSE & 
                                       str_detect(comment1, "car survey"),
                                     "brm car survey", event_from_comment),
         checked = ifelse(str_detect(comment1, "car survey"), TRUE, checked))

## 'activity survey'
filtered_activity_survey <- combined_data %>%
  filter(checked == FALSE) %>%
  filter(str_detect(comment1, "activity survey"))

# all confirm activity survey, many give consultancy survey was undertaken for,
# none give environmental variables
combined_data %<>%
  mutate(event_from_comment = ifelse(checked == FALSE & 
                                       str_detect(comment1, "activity survey"),
                                     "activity survey", event_from_comment),
         checked = ifelse(str_detect(comment1, "activity survey"), TRUE, checked))

## 'transect survey'
filtered_transect_survey <- combined_data %>%
  filter(checked == FALSE) %>%
  filter(str_detect(comment1, "transect survey"))

unique(filtered_transect_survey$comment1)

# most have just type of survey (/and consultant) - however large batch include
# passes number not otherwise included (all in same format). 
# appended and 'passes' added to count_type for these and another batch where
# count is recorded but count_type is not. other NAs here likely to also be
# passes, but left as NA as not explicit
combined_data %<>%
  mutate(count = ifelse(checked == FALSE & 
                          str_detect(comment1, "transect survey") &
                          str_detect(comment1, "recording"),
                        # this extracts 1 to 3 digits which are followed by ' recording'
                        as.numeric(str_extract(comment1, "\\d{1,3}(?= recording)")),
                        count),
         count_type = case_when(checked == FALSE &
                                  str_detect(comment1, "transect_survey") &
                                  str_detect(comment1, "recording") ~ "passes",
                                checked == FALSE &
                                  str_detect(comment1, "transect_survey") &
                                  str_detect(comment1, "passes") ~ "passes",
                                .default = count_type),
         event_from_comment = ifelse(checked == FALSE & 
                                       str_detect(comment1, "transect survey"),
                                     "transect survey", event_from_comment),
         checked = ifelse(str_detect(comment1, "transect survey"),
                          TRUE, checked))

## 'harp trapping'
filtered_harp_trapping <- combined_data %>%
  filter(checked == FALSE) %>%
  filter(str_detect(comment1, "harp trapping"))

unique(filtered_harp_trapping$comment1)
# large group that are harp trapping under nathusius project (2014-2023)
# rest are confirmed harp trapping. more information about number of traps,
# individual bat conditions, etc. is given in comments.
combined_data %<>%
  mutate(event_from_comment = ifelse(checked == FALSE &
                                       str_detect(comment1, 
                                                  "nathusius|nathuius survey. harp trapping"),
                                     "nathusius 2014-2023, harp trapping",
                                     event_from_comment),
         checked = ifelse(checked == FALSE &
                            str_detect(comment1, 
                                       "nathusius|nathuius survey. harp trapping"),
                          TRUE, checked)) %>%
  mutate(event_from_comment = ifelse(checked == FALSE &
                                       str_detect(comment1,
                                                  "harp trapping"),
                                     "harp trapping", event_from_comment),
         checked = ifelse(checked == FALSE &
                            str_detect(comment1,
                                       "harp trapping"),
                          TRUE, checked))

## 'bechstein's survey'
filtered_bechsteins_survey <- combined_data %>%
  filter(checked == FALSE) %>%
  filter(str_detect(comment1, "bechstein's survey"))

# all confirm bechsteins survey. some specify harp trapping but as this was part
# of the standard survey design it is not added. some give additional information
# on bats trapped in comment
combined_data %<>%
  mutate(event_from_comment = ifelse(checked == FALSE & 
                                       str_detect(comment1, "bechstein's survey"),
                                     "bechstein's survey", event_from_comment),
         checked = ifelse(str_detect(comment1, "bechstein's survey"),
                          TRUE, checked))

# rerun frequency analysis on non-checked rows
comment1_check4 <- combined_data %>%
  filter(checked == FALSE)

comment1_vector4 <- comment1_check4$comment1
comment1_string4 <- str_c(comment1_vector4, collapse = " ")

frequency4 <- tokenize_ngrams(comment1_string4, n = 3L, n_min = 2L,
                              simplify = TRUE) %>%
  as_tibble() %>%
  count(value, sort = TRUE)

## 'colony survey'
filtered_colony_survey <- combined_data %>%
  filter(checked == FALSE) %>%
  filter(str_detect(comment1, "colony survey"))

# all are 'ite colony survey' - NEED TO CHECK WITH JOHN WHAT THIS IS
combined_data %<>%
  mutate(event_from_comment = ifelse(checked == FALSE &
                                       str_detect(comment1, "colony survey"),
                                     "ite colony survey", event_from_comment),
         checked = ifelse(str_detect(comment1, "colony survey"),
                          TRUE, checked))
## 'nsp survey'
filtered_nsp_survey <- combined_data %>%
  filter(checked == FALSE) %>%
  filter(str_detect(comment1, "nsp survey"))

# all confirm nsp/field survey, no other information given
combined_data %<>%
  mutate(event_from_comment = ifelse(checked == FALSE &
                                       str_detect(comment1, "nsp survey"),
                                     "nbmp nsp/field", event_from_comment),
         checked = ifelse(str_detect(comment1, "nsp survey"), TRUE, checked))

## 'group survey'
filtered_group_survey <- combined_data %>%
  #filter(checked == FALSE) %>%
  filter(str_detect(comment1, "group survey"))

# all include information on the group doing the survey
combined_data %<>%
  mutate(event_from_comment = ifelse(checked == FALSE &
                                       str_detect(comment1, "group survey"),
                                     str_extract(comment1, "^.{1,} group survey"),
                                     event_from_comment),
         checked = ifelse(str_detect(comment1, "group survey"),
                          TRUE, checked))

# remove filtered subsets and previous vector lists to clear working directory
files <- ls()
filter_remove <- str_subset(files, "filtered")
rm(list = filter_remove)


# rerun frequency analysis on non-checked rows
comment1_check5 <- combined_data %>%
  filter(checked == FALSE)

comment1_vector5 <- comment1_check5$comment1
comment1_string5 <- str_c(comment1_vector5, collapse = " ")

frequency5 <- tokenize_ngrams(comment1_string5, n = 3L, n_min = 2L,
                              simplify = TRUE) %>%
  as_tibble() %>%
  count(value, sort = TRUE)

## 'nathuius'
filtered_nathuius_pip <- combined_data %>%
  filter(checked == FALSE) %>%
  filter(str_detect(comment1, "nathuius"))

# all are nathusius pip project 2014-2023
combined_data %<>%
  mutate(event_from_comment = ifelse(checked == FALSE &
                                       str_detect(comment1, "nathuius"),
                                     "nbmp nathusius 2014-2023",
                                     event_from_comment),
         checked = ifelse(str_detect(comment1, "nathuius"), TRUE, checked))

## 'maternity roost'
filtered_maternity_roost <- combined_data %>%
  filter(checked == FALSE) %>%
  filter(str_detect(comment1, "maternity roost"))

# most give records of probable maternity roosts. those confirmed have this
# information included already. no count or survey data not already included
combined_data %<>%
  mutate(checked = ifelse(str_detect(comment1, "maternity roost"), TRUE,
                          checked))

## 'rain =' - to exclude
filtered_rain_dry <- combined_data %>%
  filter(checked == FALSE) %>%
  filter(str_detect(comment1, "rain ="))

# ID 20082 is nsp/field survey. all others give no more survey/count information
# not already recorded
combined_data %<>%
  mutate(event_from_comment = ifelse(id == 20082, "nbmp nsp/field", 
                                     event_from_comment),
         checked = ifelse(id == 20082, TRUE, checked)) %>%
  mutate(checked = ifelse(str_detect(comment1, "rain ="), TRUE, checked))

# rerun frequency analysis on non-checked rows
comment1_check6 <- combined_data %>%
  filter(checked == FALSE)

comment1_vector6 <- comment1_check6$comment1
comment1_string6 <- str_c(comment1_vector6, collapse = " ")

frequency6 <- tokenize_ngrams(comment1_string6, n = 3L, n_min = 2L,
                              simplify = TRUE) %>%
  as_tibble() %>%
  count(value, sort = TRUE)

## 'entry survey'
filtered_entry_survey <- combined_data %>%
  filter(checked == FALSE) %>%
  filter(str_detect(comment1, "entry survey"))

# IDs 27798, 27799, and 27800 are dawn re-entry surveys
# all others are emergence surveys
combined_data %<>% 
  mutate(event_from_comment = ifelse(id %in% c(27798, 27799, 27800),
                                     "dawn re-entry", event_from_comment),
         checked = ifelse(id %in% c(27798, 27799, 27800), TRUE, checked)) %>%
  mutate(event_from_comment = ifelse(checked == FALSE &
                                       str_detect(comment1, "entry survey"),
                                     "emergence/re-entry", event_from_comment),
         checked = ifelse(str_detect(comment1, "entry survey"), TRUE, checked))

### STOPPED HERE, NOT COMPLETE

with_event_data <- combined_data %>%
  filter(!(is.na(event_from_comment)) | !(is.na(event)))

# vast majority of comment2 is either 'chiroptera', (which are removed) 
# or includes reference number for bms car survey (which are moved to seperate
# column
check_comment2 <- combined_data %>%
  filter(!is.na(comment2))

combined_data %<>%
  mutate(bms_survey_number = ifelse(str_detect(comment2, "\\d{2}/\\d{2}/3\\d{3}"),
                                    comment2, NA),
         comment2 = ifelse(str_detect(comment2, "\\d{2}/\\d{2}/3\\d{3}"),
                           NA, comment2),
         comment2 = ifelse(comment2 %in% c("", "chiroptera"), NA, comment2))
         
         # stopped here - comment 2 not complete