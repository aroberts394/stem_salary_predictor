#### salary prediction shiny app project ####

### using IPUMS higher ed data to predict salary for STEM professionals ###
### Using NSCG, SDR and ISDR surveys from SESTAT NSCG 2013, SESTAT SDR 2013,and SDR 2013 and  ISDR 2013 samples ###
### load and transform data ###


# load required packages
library(vroom)
library(dplyr)
library(tidyr)
library(tigris)
library(leaflet)
library(rsample)
library(recipes)
library(parsnip)
library(workflows)
library(yardstick)

# loading the higher eduaction data set
highered_raw <- vroom("data/highered.csv", col_select = (-WTSURVY))
highered <- highered_raw

# remove fields that do not add any meaningful info, remove BIRYR variable cause of correlation with age, 
# remove NOCPR because same as NOCPRMG but lower level
highered <- highered %>% 
  select(-YEAR,-PERSONID, -WEIGHT, -BIRYR,-SURID, -SAMPLE, -NOCPR)

# check referential integrity of REFID
length(unique(highered$REFID))
length(highered$REFID) - length(unique(highered$REFID)) #30696 seem to be duplicated observations in some fashion

# remove duplicate rows and keep first row with non-null values
highered <- highered %>%
  distinct(REFID, .keep_all = TRUE)            

# inspect higher education dataset
#glimpse(highered)

# check for columns for missing values
apply(is.na(highered), 2, sum)

# remove missing values
highered <- na.omit(highered)

# check logical skip values or missing values in columns
#table_list <- mapply(table, highered)
#table_list

# remove rows that have 99 and 9999 logical skip
highered <- highered[rowSums(highered == 99) == 0, , drop = FALSE]
highered <- highered[rowSums(highered == 9999) == 0, , drop = FALSE]

# remove salaries == 0 and 999998
highered <- highered %>%
  filter(!SALARY %in% c(0, 9999998))

# replace 98s with 0 in all columns
# highered[highered == 98] <- 0

#glimpse(highered)

# factorizing categorical variables
cols_to_factor <- c("GENDER", "RACETH", "DGRDG", "OCEDRLP", "MRDGRUS",
                    "NOCPRMG", "BTHUS", "CTZUSIN", "CHTOT", "NDGMEMG",
                    "HDDGRUS", "MRDG", "NMRMEMG", "ACFPT", "CTZUS",
                    "HRSWKGR", "FTPRET", "EMSEC", "EMSIZE", "NEWBUS", "EMUS",
                    "GOVSUP", "NRREA", "JOBSATIS")

# highered[cols_to_factor] <- lapply(highered[cols_to_factor], factor)
highered <- highered %>% mutate(across(all_of(cols_to_factor), as.factor))


# create a variable to show years since graduated
highered <- highered %>%
  mutate(YEARS_SINCE_GRAD = 2013 - MR03Y5,
         YEARS_SINCE_GRAD = case_when(YEARS_SINCE_GRAD == 2 ~ "2 years or less",
                                      YEARS_SINCE_GRAD == 7 ~ "3 to 7 years",
                                      YEARS_SINCE_GRAD == 12 ~ "8 to 12 years",
                                      YEARS_SINCE_GRAD == 17 ~ "13 to 17 years",
                                      YEARS_SINCE_GRAD == 22 ~ "18 to 22 years",
                                      YEARS_SINCE_GRAD == 27 ~ "23 to 27 years",
                                      YEARS_SINCE_GRAD == 32 ~ "28 to 32 years",
                                      YEARS_SINCE_GRAD == 37 ~ "33 to 37 years",
                                      YEARS_SINCE_GRAD == 42 ~ "38 to 42 years",
                                      YEARS_SINCE_GRAD == 47 ~ "43 to 47 years",
                                      YEARS_SINCE_GRAD == 52 ~ "48 to 52 years"),
         YEARS_SINCE_GRAD = factor(YEARS_SINCE_GRAD))
  
# rename factor levels to make them understandable
highered$GENDER <- with(highered, plyr::revalue(GENDER, c("1" = "Female", "2" = "Male")))

highered$RACETH <- with(highered, plyr::revalue(RACETH, c("1" = "Asian", 
                                                          "2" = "White", 
                                                          "3" = "Under-represented Minorities")))

highered$DGRDG <-  with(highered, plyr::revalue(DGRDG, c("1" = "Bachelors", 
                                                         "2" = "Masters", 
                                                         "3" = "Doctorate", 
                                                         "4" = "Professional")))

highered$OCEDRLP <- with(highered, plyr::revalue(OCEDRLP, c("1" = "Closely related", 
                                                            "2" = "Somewhat related", 
                                                            "3" = "Not related")))

highered$NOCPRMG <- with(highered, plyr::revalue(NOCPRMG, c("1" = "Computer Scientists", 
                                                            "2" = "Life Scientists",
                                                            "3" = "Physical Scientists", 
                                                            "4" = "Social Scientists",
                                                            "5" = "Engineers", 
                                                            "6" = "Other Scientists", 
                                                            "7" = "Non-Science or Engineering occupation")))

highered$BTHUS <- with(highered, plyr::revalue(BTHUS, c("0" = "Not in the US",
                                                         "1"   = "United States")))

highered$CTZUSIN <- with(highered, plyr::revalue(CTZUSIN, c("0"  = "No",
                                                            "1"  = "Yes")))

highered$CTZUS <- with(highered, plyr::revalue(CTZUS, c("1" = "Born in US or US territories",
                                                            "2" = "Born abroad of American parents",
                                                            "3" = "By naturalization",
                                                            "98" = "Not a Citizen")))

# "0"  = "No children", "2"  = "One to three children", "4"  = "More than three children" removed
highered$CHTOT <- with(highered, plyr::revalue(CHTOT, c("1"  = "One child",
                                                        "3"  = "Two or more children",
                                                        "98"  = "No children")))

highered$NDGMEMG <- with(highered, plyr::revalue(NDGMEMG, c("1" = "Computer and mathematical sciences",
                                                            "2" = "Biological, agricultural and environmental life sciences",
                                                            "3" = "Physical and related sciences",
                                                            "4" = "Social and related sciences",
                                                            "5" = "Engineering",
                                                            "6" = "Science and engineering-related fields",
                                                            "7" = "Non-science and engineering fields")))


highered$HDDGRUS <- with(highered, plyr::revalue(HDDGRUS, c("0" = "Non-US",
                                                           "1" = "US")))


highered$MRDG <- with(highered, plyr::revalue(MRDG, c("1" = "Bachelors",
                                                        "2" = "Masters",
                                                        "3" = "Doctorate",
                                                        "4" = "Professional",
                                                       "5" = "Other")))

# "9" = "Other Categories" removed
highered$NMRMEMG <- with(highered, plyr::revalue(NMRMEMG, c("1" = "Computer and mathematical sciences",
                                                            "2" = "Biological, agricultural and environmental life sciences",
                                                            "3" = "Physical and related sciences",
                                                            "4" = "Social and related sciences",
                                                            "5" = "Engineering",
                                                            "6" = "Science and engineering-related fields",
                                                            "7" = "Non-science and engineering fields")))

highered$MRDGRUS <- with(highered, plyr::revalue(MRDGRUS, c("0" = "Non-US",
                                                            "1" = "US")))

# "97" = "Survey Exclusion/Confidentiality" removed
highered$ACFPT <- with(highered, plyr::revalue(ACFPT, c("1" =" Part-time Student",
                                                        "2" = "Full-time Student",
                                                        "3" =" Not enrolled in a degree program, but taking courses",
                                                        "98" = "Not a student")))

# "98" = "Logical Skip" removed
highered$HRSWKGR <- with(highered, plyr::revalue(HRSWKGR, c("1" = "20 or less",
                                                            "2" = "21-35",
                                                            "3" = "36-40",
                                                            "4" = "Greater than 40")))

highered$FTPRET <- with(highered, plyr::revalue(FTPRET, c("0"  = "No",
                                                            "1"  = "Yes")))

# "5" = "Non-US government" removed
highered$EMSEC <- with(highered, plyr::revalue(EMSEC, c("1" = "2 year college or other school system",
                                                        "2" = "4 year college or medical institution",
                                                        "3" = "Government",
                                                        "4" = "Business or industry")))

highered$EMSIZE <- with(highered, plyr::revalue(EMSIZE, c("1" = "10 or fewer employees",
                                                          "2" = "11-24 employees",
                                                          "3" = "25-99 employees",
                                                          "4" = "100-499 employees",
                                                          "5" = "500-999 employees",
                                                          "6" = "1000-4999 employees",
                                                          "7" = "5000-24999 employees",
                                                          "8" =  "25000+ employees")))

highered$NEWBUS <- with(highered, plyr::revalue(NEWBUS, c("0"  = "No",
                                                          "1"  = "Yes")))


highered$EMUS <- with(highered, plyr::revalue(EMUS, c("0" = "Non-US",
                                                      "1" = "US")))

highered$GOVSUP <- with(highered, plyr::revalue(GOVSUP, c("0"  = "No",
                                                          "1"  = "Yes",
                                                          "98" = "No")))

highered$NRREA <- with(highered, plyr::revalue(NRREA, c("1" = "Pay, promotion opportunities",
                                                        "2" = "Working conditions",
                                                        "3" = "Job location",
                                                        "4" = "Change in career or professional interests",
                                                        "5" = "Family-related reasons",
                                                        "6" = "Job in highest degree field not available",
                                                        "7" = " Other reason for not working",
                                                        "98"  =  "Not working outside of field")))

highered$JOBSATIS <- with(highered, plyr::revalue(JOBSATIS, c("1" = "Very satisfied",
                                                              "2" = "Somewhat satisfied",
                                                              "3" = "Somewhat dissatisfied",
                                                              "4" =  "Very dissatisfied")))

#check structure of data frame
#glimpse(highered)



# salary is based from 2013, adjust salary for 2020 purchasing power
# load Consumer Price Index data from U.S Bureau of Labor Statistics
cpi_data <- vroom("data/cpi_2013_to_2021.csv")

# load cost of living index data set
coli_index <- vroom("data/advisorsmith_cost_of_living_index.csv", 
                    col_select = list(cost_index = `Cost of Living Index`, everything()))

# get state level cost of living index
state_coli_index <- coli_index %>%
  group_by(State) %>%
  summarise(cost_index = mean(cost_index)) %>%
  ungroup()

# rename cost of living index column
# incorporate coordinates into coli data
# states_sf <- tigris::states() %>%
#   as("sf") %>%
#   rename(State = STUSPS) %>% # renaming abbreviated state name in tigris df
#   left_join(coli_index) %>%
#   na.omit()

# Downloading the shapefiles for states at the lowest resolution
states <- states(cb=T)

# Now we use the Tigris function geo_join to bring together
states_merge <- geo_join(states, state_coli_index, "STUSPS", "State")

# get cpi index difference of 2013 and 2020
cpi_index = as.numeric(cpi_data[9, "Mar"]/cpi_data[1,"Annual"])

# calculate adjusted salaries
highered <- highered %>%
  mutate(ADJ_SALARY = round(SALARY*cpi_index, 2))

# remove old salary variable
highered <- highered %>% select(-SALARY)

# remove raw file from memory
rm(highered_raw)


### SIMPLIFIED LINEAR REGRESSION ------------------------------------------------
highered_simple <- highered %>% select(AGE, GENDER, RACETH, CHTOT, HRSWKGR, EMSIZE, EMSEC, OCEDRLP,
                                      NOCPRMG, YEARS_SINCE_GRAD, ADJ_SALARY)

# split into train/test
set.seed(123)
highered_simple_split <- initial_split(highered_simple, prop = 0.75, strata = ADJ_SALARY)
highered_simple_train <- training(highered_simple_split)
highered_simple_test <- testing(highered_simple_split)

# create recipe
lm_simple_recipe <- recipe(ADJ_SALARY ~., data = highered_simple) %>%
  step_dummy(all_nominal()) %>%
  step_zv(all_predictors())

# specify the linear regression model
lm_model <- linear_reg() %>%
  set_engine("lm") %>%
  set_mode("regression")

# create workflow
lm_simple_wflow <- workflow() %>%
  add_model(lm_model) %>%
  add_recipe(lm_simple_recipe)

# simplified linear regression model
lm_simple_fit <- fit(lm_simple_wflow, data = highered_simple)


# test predictions
# test_df <- data.frame(AGE = 33,
#                       GENDER = "Female",
#                       RACETH = "White",
#                       CHTOT = "No children",
#                       HRSWKGR = "36-40",
#                       EMSIZE = "100-499 employees",
#                       EMSEC = "Business or industry",
#                       OCEDRLP = "Somewhat related",
#                       NOCPRMG = "Other Scientists",
#                       YEARS_SINCE_GRAD = "3 to 7 years")
# test_pred <- round(as.numeric(predict(lm_simple_fit, new_data = test_df, type = "conf_int")),2)
# 
# paste('Based on your inputs, the predicted salary is: $', test_pred[1],' - $',test_pred[2], sep = '')

