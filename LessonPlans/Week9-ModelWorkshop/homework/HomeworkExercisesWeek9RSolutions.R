## R HOMEWORK WEEK 9, QUESTION 5
## CREATE RACE VARIABLE
## SOCIOLOGY 625: RESEARCH PRACTICUM
## AUTHOR: MIKE BADER

## SET UP ENVIRONMENT
rm(list=ls())
DIR <- "~/work/Data/dcas/dcas2016/Dataset/"  ## Change to the location of your DCAS data file
setwd(DIR)
dcas <- read.csv("DCAS_2016_weighted.csv")

## IDENTIFY HISPANICS
table(dcas$q53)
dcas$hispanic <- dcas$q53=='Yes'
dcas$hispanic[dcas$q53 %in% c('No Answer','Refused')] <- NA

## Check the variable
table(dcas$q53,dcas$hispanic,useNA='ifany')

dcas$hispanic <- (dcas$hispanic==TRUE |
                  dcas$q5401=='Hispanic or Hispanic-American' |
                  dcas$q5402=='Hispanic or Hispanic-American' |
                  dcas$q5403=='Hispanic or Hispanic-American' |
                  dcas$q5404=='Hispanic or Hispanic-American' )

## Check the variable
table(dcas$q5401,dcas$q53,dcas$hispanic)
table(dcas$q5402,dcas$q5401,dcas$hispanic) ## Commented to supress long output

## IDENTIFY BLACKS
dcas$black <- (dcas$q5401=='Black or African-American' |
               dcas$q5402=='Black or African-American' |
               dcas$q5403=='Black or African-American' |
               dcas$q5404=='Black or African-American' )

## Check the variable
table(dcas$q5401,dcas$black)
table(dcas[dcas$q5401!='White',c("q5402","black")])

## IDENTIFY API
dcas$api <- (dcas$q5401=='Asian or Asian-American' |
                 dcas$q5401=='Native Hawaiian/Other Pacific Islander' |
             dcas$q5402=='Asian or Asian-American' |
                 dcas$q5402=='Native Hawaiian/Other Pacific Islander' |
             dcas$q5403=='Asian or Asian-American' |
                 dcas$q5403=='Native Hawaiian/Other Pacific Islander' )

## Check variable
table(dcas$q5401,dcas$api)
table(dcas[
    !(dcas$q5401=='Asian or Asian-American'| dcas$q5401=='Native Hawaiian/Other Pacific Islander'),
    c("q5402","api")
])
table(dcas[
    !(dcas$q5401=='Asian or Asian-American'|
          dcas$q5401=='Native Hawaiian/Other Pacific Islander'|
          dcas$q5402=='Asian or Asian-American'|
          dcas$q5402=='Native Hawaiian/Other Pacific Islander'),
    c("q5403","api")
])

## IDENTIFY OTHER
dcas$other <- (dcas$q5401=='Native American/American Indian/Alaska Native' |
               dcas$q5401=='Some other race (specify):' |
               dcas$q5402=='Native American/American Indian/Alaska Native' |
               dcas$q5402=='Some other race (specify):' |
               dcas$q5403=='Native American/American Indian/Alaska Native' |
               dcas$q5403=='Some other race (specify):'
                )

## Check variable
table(dcas$q5401,dcas$other)
table(dcas[
    !(dcas$q5401=='Native American/American Indian/Alaska Native'|
      dcas$q5401=='Some other race (specify)'),
     c("q5402","other")
    ])
table(dcas[
    !(dcas$q5401=='Native American/American Indian/Alaska Native'|
      dcas$q5401=='Some other race (specify)' |
      dcas$q5402=='Native American/American Indian/Alaska Native'|
      dcas$q5402=='Some other race (specify)'),
     c("q5403","other")
    ])

## IDENTIFY WHITES
dcas$white <- (dcas$q5401=='White' |
               dcas$q5402=='White' |
               dcas$q5403=='White')

## Check variable
table(dcas$q5401,dcas$white)
table(dcas[dcas$q5401!='White',c("q5402","white")])

## CREATE RACE VARIABLE
dcas$race <- NA

# 1. Classify as Hispanics anyone who answered `q53` as 'Yes'
# **or** anyone who answers `q54` as "Hispanic or Hispanic-American"
dcas$race[dcas$hispanic] <- 'hispanic'

# 2. Classify as black anyone who answered `q54` as "Black or
# African-American" **and** was not already classified as Hispanic
dcas$race[dcas$black==TRUE & is.na(dcas$race)] <- 'black'

# 3. Classify as Asian/Pacific Islander anyone who answered
# `q54` as "Asian or Asian-American" **or** "Native Hawaiian/
# Other Pacific Islander" **and** was not already classified
# as Hispanic or black
dcas$race[dcas$api==TRUE & is.na(dcas$race)] <- 'api'

# 4. Classify as "other" anyone who did not answer `q53` as
# "Yes" **and** who did not answer `q54` as:
#     * Hispanic or Hispanic-American **or**
#     * Black or African-American **or**
#     * Asian or Asian American **or** Native Hawaiian/
#       Other Pacific Islander **or**
#     * White
dcas$race[dcas$other==TRUE & is.na(dcas$race)] <- 'other'

# 5. Classify as white anyone who answered `q54` as
# "White" and was not already classified
dcas$race[dcas$white==TRUE & is.na(dcas$race)] <- 'white'

## Check the variable against `dem.race`
table(dcas$race,dcas$dem.race) ## There will be minor inconsistences



#### SHORTCUTS

## SUBSTITUTING STRINGS
native <- 'Native American/American Indian/Alaska Native'
other_race <- 'Some other race (specify):'

dcas$other2 <- (dcas$q5401==native | dcas$q5401==other_race |
                dcas$q5402==native | dcas$q5402==other_race |
                dcas$q5403==native | dcas$q5403==other_race
               )
## Show that this is the same as the variable that we created above
table(dcas$other,dcas$other2)

## USING `paste0()`
race_vars <- paste0("q540",c(1:6))
race_vars

## USING APPLY
native <- 'Native American/American Indian/Alaska Native'
race_vars <- paste0("q540",c(1:6))

dcas$native <- apply(dcas[,race_vars],1,function(row){native %in% row})

## USE APPLY TO CLASSIFY OTHER RACE
native <- 'Native American/American Indian/Alaska Native'
other_race <- 'Some other race (specify):'
race_vars <- paste0("q540",c(1:6))

dcas$other3 <- (
            apply(dcas[,race_vars],1,function(row){native %in% row}) |
            apply(dcas[,race_vars],1,function(row){other_race %in% row})
)

table(dcas$other,dcas$other3)

table(dcas$race)


dcas$pol.fear.arrest <- ordered(dcas$pol.fear.arrest,levels=c("Not at all","A little","Somewhat","A lot"))
dcas$little.fear <- (dcas$pol.fear.arrest!="Not at all")*1
dcas$little.fear[is.na(dcas$pol.fear.arrest)] <- NA
round(prop.table(table(dcas$race,dcas$little.fear),1),2)


