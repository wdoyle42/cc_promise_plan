################################################################################
##
## <PROJ> America's College Promise
## <FILE> ipeds.r
## <AUTH> Will Doyle and Benjamin Skinner
## <INIT> 2021-09-09
##
################################################################################

## PURPOSE

## The purpose of this file is automate the process of:
##
## (1) downloading appropriate IPEDS survey data files
## (2) subsetting full datasets to desired variables
## (3) combining across datasets and years
## (4) output in tidy dataset

## CODE

## Code modified from <ipeds_combine.r>:

## https://gist.github.com/btskinner/f42c87507169d0ba773c

##Libraries
library(tidyverse)
library(crosswalkr)
library(here)

## load functions
source('functions.r')

## directories
rddir <- file.path(here(), "data", "raw")
addir <- file.path(here(), "data", "cleaned")

## create directories if they don't exist
walk(c(rddir, addir), ~dir.create(.x, showWarnings = FALSE))

## =============================================================================
## BUILD DATASETS
## =============================================================================

## primary year
year <- 2019

## data sets
filenames <- c("HD2019.zip",
               "IC2019_AY.zip",
               "EFIA2019.zip",
               "F1718_F1A.zip",
               "C2018_C.zip")

## variables to use with each data set
vars <- list(
    ## HD2019.zip
    c("unitid","instnm","city","stabbr","control","sector","tribal",
      "carnegie", "c18ipug","c15basic","obereg","hloffer","latitude",
      "longitud","city","addr","zip"),
    ## IC2019_AY.zip
    c("unitid","tuition2","fee2"),
    ## EFIA2019.zip
    c("unitid","fteug"),
    ## F1718_F1A.zip
    c("unitid","f1b01","f1b02","f1b03","f1b04a","f1b11","f1b12"),
    ## C2018_C.zip
    c("unitid","awlevelc","cstotlt")
)

## walk through building each data set
inst <- map2(filenames,
             vars,
             ~ {
                 ## download and build the data set
                 tmp <- build.dataset.ipeds(filenames = .x,
                                          datadir = rddir,
                                          vars = .y,
                                          years = year) %>%
                     as_tibble()
                 if (.x == "C2018_C.zip") {
                     tmp <- tmp %>%
                         ## AWlevel codes
                         ## 3	Associate's degree
                         ## 5	Bachelor's degree
                         ## 7	Master's degree
                         ## 9	Doctor's degree
                         ## 10	Postbaccalaureate or Post-master's certificate
                         ## 1	Award of less than 1 academic year
                         ## 2	Award of at least 1 but less than 4 academic years
                         mutate(awlevelc = case_when(
                                    awlevelc == 1 ~ "Cert<1",
                                    awlevelc == 2 ~ "Cert>1",
                                    awlevelc == 3 ~ "Associates",
                                    awlevelc == 5 ~ "Bachelors",
                                    awlevelc == 7 ~ "Masters",
                                    awlevelc == 9 ~ "PhD",
                                    awlevelc == 10 ~ "Postbac"
                                )) %>%
                         pivot_wider(id_cols = c("unitid", "year"),
                                     names_from = awlevelc,
                                     values_from = cstotlt) %>%
                         mutate_all(replace_na,0)
                     tmp
                 } else {
                     tmp
                 }
             }) %>%
    ## join data sets
    reduce(left_join, by = c("unitid", "year")) %>%
    ## rename some variables
    rename(tuition_fee_revs = f1b01,
           fed_grants = f1b02,
           state_grants = f1b03,
           local_grants = f1b04a,
           state_approps = f1b11,
           local_approps = f1b12) %>%
    ## add state and region information
    left_join(crosswalkr::stcrosswalk, by = "stabbr") %>%
    ## rename state and region vars
    rename(name = stname,
           region = cenreg) %>%
    ## misc cleanup
    filter(fteug > 0,            # drop if no undergrads
           sector != 0,          # drop admin units
           obereg != 0,          # drop military academies
           unitid != 100636) %>% # drop cc of the airforce
    ## drop territories
    filter(!is.na(region)) %>%
    ## new vars
    mutate(tuition2 = as.numeric(tuition2),
           fee2 = as.numeric(fee2)) %>%
    ## drop tribal colleges
    filter(tribal == 2) %>%
    ## drop those with no degrees
    filter(Associates > 0 | Bachelors > 0)

## =============================================================================
## OUTPUT FINAL DATASET AS .CSV
## =============================================================================

write_csv(inst, file = file.path(addir, 'institutions.csv'))

## -----------------------------------------------------------------------------
## end script
################################################################################
