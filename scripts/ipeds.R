################################################################################
##
## <PROJ> National Afforability Report: 50 States
## <FILE> ipeds.r
## <AUTH> Will Doyle and Benjamin Skinner
## <INIT> 2015-05-27
## <REV> 2021-04-08
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
library(noncensus)
library(here)

## load functions
source('functions.r')

rddir<-"../data/raw/"
addir<-"../data/cleaned/"
## =============================================================================
## BUILD DATASETS 
## =============================================================================

years<-2019

## IPEDS institutional characteristics (using HD files)

filenames<-paste0('HD',2019,'.zip')
var <- c('unitid','instnm','city','stabbr','control','sector','carnegie', 'c18ipug','c15basic','obereg','latitude','longitud','city','addr','zip')
hd_df <- build.dataset.ipeds(filenames=filenames, datadir = rddir, vars = var,years=years)

## IPEDS enrollments (using EFIA files)

filenames <-paste0('EFIA',2019,'.zip')
var <- c('unitid','fteug')
efia_df <- build.dataset.ipeds(filenames=filenames, datadir = rddir, vars= var ,years=2019)


## Degrees awarded
filenames<-'C2019_C.zip'
var<-c('unitid','awlevelc','cstotlt')
comp_df<-build.dataset.ipeds(filenames=filenames, datadir = rddir, vars= var ,years=2019)
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
  left_join(efia_df)%>%
  left_join(comp_df)

## =============================================================================
## Add full state names
## =============================================================================

data(states)

states<-states%>%
  rename(stabbr=state)

inst<-inst%>%left_join(states,by="stabbr")

## =============================================================================
## Some misc cleanup
## =============================================================================

inst<-inst%>%
  filter(fteug>0,   ##Drop if no undergrads
         sector!=0,  ## drop admin units 
         obereg!=0, ## drop military academies
         unitid != 100636 ) ## drop cc of the airforce
  

##Drop rownames
rownames(inst) <- NULL

## Drop territories

inst<-inst%>%
  filter(!(is.na(region)))


## =============================================================================
## OUTPUT FINAL DATASET AS .CSV
## =============================================================================


write_csv(inst, file = paste0(addir, 'institutions.csv'))

library(haven)

inst_tn<-filter(inst,stabbr=="TN")%>%select(-`Cert> 1`,-`Cert<1`)
  

write_dta(inst_tn,"~/lpo_prac/lessons/s3-09-spatial/inst.dta")

## =============================================================================
## END
################################################################################

