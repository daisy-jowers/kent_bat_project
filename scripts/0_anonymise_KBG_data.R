################################################
#########    KGB DATA ANONYMISATION    #########
################################################

# this script imports the non-anonymised datasets from a non-public folder,
# runs scripts to anonymise as much of the comments as possible, and outputs
# the anonymised dataset to the main raw data folder of the public repository.

IN_DIR_PATH #<- insert working directory here

setwd(IN_DIR_PATH)

dat <- read_csv(paste0(IN_DIR_PATH, 
                       "/non_anonymised_data/20240629_KBG_data_primary_non_anonymised.csv"))

# load packages
source("./directory/scripts/0_install_packages.R")

#### REMOVE PHONE NUMBERS ####
dat %<>%
  mutate(...16 = str_replace(...16, "(0|\\+\\d{2})\\d{4}(\\s|-)?\\d{6}",
                             "phone number"),
         # next three line removes specific unusually formatted phone numbers
         ...16 = str_replace(...16, "020\\d{1}\\s\\d{7}",
                             "phone number"),
         ...16 = str_replace(...16, "\\d{4}-\\d{6}",
                             "phone number"),
         ...16 = str_replace(...16, "\\d{6}\\s\\d{6}",
                             "phone number"))

#### REMOVE EMAIL ADDRESSES ####
# (?i) makes search case-insensitive
dat %<>%
  mutate(...16 = str_replace(...16, "(?i)\\b.{1,}\\.co(m|\\.uk)",
                             "email address"))


#### REMOVE HONORIFICS AND FIRST WORD FOLLOWING ####
# only one doctor in dataset, removed in direct names remove below
dat %<>%
  mutate(...16 = str_replace(...16, "(?i)\\bmr(s)?\\s\\w{1,}\\b",
                             "name"))

#### REMOVE COMMON NAMES ####
# data is taken from Office for National Statistics 'Top 100 baby names in
# England and Wales: historical data, 1904-1994 edition'

common_boys_names <- read_csv("./directory/data/common_names/ons_common_boys_names_1904-1994.csv") %>%
  select(2:11) %>%
  slice(2:101) %>%
  unlist(use.names = FALSE) %>%
  unique() 

common_girls_names <- read_csv("./directory/data/common_names/ons_common_girls_names_1904-1994.csv") %>%
  select(2:11) %>%
  slice(2:101) %>%
  unlist(use.names = FALSE) %>%
  unique()
  
# remove 'JUNE', 'ROSE', 'DAWN', 'MAY', 'IVY' - all are included in comments but not
# as names
common_girls_names <- common_girls_names[!common_girls_names %in% 
                                           c("JUNE", "ROSE", "DAWN", "MAY", "IVY")]

dat %<>%
  mutate(...16 = str_replace(...16, paste0("(?i)\\b(",
                                         paste(common_boys_names, collapse = "|"),
                                         ")\\b"),
                            "first_name"),
         ...16 = str_replace(...16, paste0("(?i)\\b(",
                                         paste(common_girls_names, collapse = "|"),
                                         ")\\b"),
                            "first_name"))

#### REMOVE COMMON SURNAMES ####
# data is taken from forebears.io (in absence of available census data)

common_surnames = c("Smith", "Jones", "Taylor", "Brown", "Williams",
                    "Wilson", "Johnson", "Davies", "Patel", "Robinson",
                    "Wright", "Thompson", "Evans", "Walker", "White",
                    "Roberts", "Green", "Hall", "Thomas", "Clarke", "Jackson",
                    "Wood", "Harris", "Edwards", "Turner", "Martin", "Cooper",
                    "Hill", "Ward", "Hughes", "Moore", "Clark", "King",
                    "Harrison", "Lewis", "Baker", "Lee", "Allen", "Morris",
                    "Khan", "Scott", "Watson", "Davis", "Parker", "James",
                    "Bennett", "Young", "Phillips", "Richardson", "Mitchell",
                    "Bailey", "Carter", "Cook", "Singh", "Shaw", "Bell",
                    "Collins", "Morgan", "Kelly", "Begum", "Miller", "Cox",
                    "Hussain", "Marshall", "Simpson", "Price", "Anderson",
                    "Adams", "Wilkinson", "Ali", "Ahmed", "Foster", "Ellis",
                    "Murphy", "Chapman", "Mason", "Gray", "Richards", "Webb",
                    "Griffiths", "Hunt", "Palmer", "Campbell", "Holmes",
                    "Mills", "Rogers", "Barnes", "Knight", "Matthews", "Barker",
                    "Powell", "Stevens", "Kaur", "Fisher", "Butler", "Dixon",
                    "Russell", "Harvey", "Pearson", "Graham")

# remove 'Young', 'Wood', 'Hall', 'Brown', 'White'
common_surnames <- common_surnames[!common_surnames %in% 
                                     c("Young", "Wood", "Hall", "Brown",
                                       "White")]

dat %<>%
  mutate(...16 = str_replace(...16, paste0("(?i)\\b(",
                                           paste(common_surnames, collapse = "|"),
                                           ")\\b"),
                             "surname"))

#### REMOVE VECTOR OF OTHER NAMES IDENTIFIED ####
# vector not supplied for self-evident reasons

dat %<>%
  mutate(...16 = str_replace(...16, paste(remove_names, collapse = ""),
                             "name"))

#### WRITE DATA TO NEW CSV ####
write_csv(dat, "./directory/data/raw/20240629_KBG_data_primary_anonymised.csv")
