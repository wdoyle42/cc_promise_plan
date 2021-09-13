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
rddir<-"../data/raw/"
addir<-"../data/cleaned/"

## create directories if they don't exist
walk(c(rddir, addir), ~dir.create(.x, showWarnings = FALSE))

## =============================================================================
## BUILD DATASETS
## =============================================================================

year<-2019

## IPEDS institutional characteristics (using HD files)

filenames<-paste0('HD',year,'.zip')
var <- c('unitid','instnm','city','stabbr','control','sector','tribal' ,'carnegie', 'c18ipug','c15basic','obereg','hloffer','latitude','longitud','city','addr','zip')
hd_df <- build.dataset.ipeds(filenames=filenames, datadir = rddir, vars = var,years=year)

## Student Charges IC2019_AY
filenames<-paste0("IC",year,'_AY',".zip")
var<-c('unitid','tuition2')
ic_df<-build.dataset.ipeds(filenames=filenames, datadir = rddir, vars = var,years=year)


## IPEDS enrollments (using EFIA files)

filenames <-paste0('EFIA',year,'.zip')
var <- c('unitid','fteug')
efia_df <- build.dataset.ipeds(filenames=filenames, datadir = rddir, vars= var ,years=year)

## Finance

filenames<-"F1718_F1A.zip"

var<-c('unitid',"f1b01","f1b02","f1b03","f1b04a", "f1b11","f1b12")
var<-tolower(var)

finance_df<- build.dataset.ipeds(filenames=filenames, datadir = rddir, vars= var ,years=year)

## Degrees awarded
filenames<-'C2018_C.zip'
var<-c('unitid','awlevelc','cstotlt')
comp_df<-build.dataset.ipeds(filenames=filenames, datadir = rddir, vars= var ,years=year)
comp_df<-comp_df%>%
  pivot_wider(id_cols=c("unitid","year"),
              names_from = awlevelc,
              values_from =cstotlt )
names(comp_df)[3:9]<-c("Bachelors",
                       "Masters",
                       "PhD",
                       "Cert> 1",
                       "Postbac",
                       "Associates",
                       "Cert<1")
comp_df<-comp_df%>%
  mutate_all(replace_na,0)

# ## AWlevel codes
#   3	Associate's degree
# 5	Bachelor's degree
# 7	Master's degree
# 9	Doctor's degree
# 10	Postbaccalaureate or Post-master's certificate
# 1	Award of less than 1 academic year
# 2	Award of at least 1 but less than 4 academic years

## =============================================================================
## MERGE DATASETS
## =============================================================================

inst<-
  hd_df%>%
  left_join(ic_df)%>%
  left_join(efia_df)%>%
  left_join(finance_df)%>%
  left_join(comp_df)%>%
  rename(tuition_revs=f1b01,
         fed_grants=f1b02,
         state_grants=f1b03,
         local_grants=f1b04a,
         state_approps=f1b11,
         local_approps=f1b12)%>%
  as_tibble()

## =============================================================================
## Add full state names
## =============================================================================

inst<-inst%>%left_join(stcrosswalk,by="stabbr") %>% rename(name = stname)

## =============================================================================
## Some misc cleanup
## =============================================================================

inst<-inst%>%
  filter(fteug>0,   ##Drop if no undergrads
         sector!=0,  ## drop admin units
         obereg!=0, ## drop military academies
         unitid != 100636 ) ## drop cc of the airforce


## Drop territories

inst<-inst%>%
  filter(!(is.na(cenreg)))

inst<-inst%>%mutate(tuition2=as.numeric(tuition2))

## Drop tribal colleges

inst<-inst%>%filter(tribal==2)


## =============================================================================
## OUTPUT FINAL DATASET AS .CSV
## =============================================================================


write_csv(inst, file = file.path(addir, 'institutions.csv'))
