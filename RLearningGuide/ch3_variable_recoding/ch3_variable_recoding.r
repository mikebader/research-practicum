rm(list=ls()) ## Like clear in Stata

setwd("~/work/Teaching/Courses/ResearchPracticum/LessonPlans/Week9-ModelWorkshop/homework")
dcas <- read.csv("DCAS_2016_weighted.csv")

## IDENTIFY HISPANICS
table(dcas$q53)
dcas$hispanic <- dcas$q53=='Yes'
dcas$hispanic[dcas$q53 %in% c('No Answer','Refused')] <- NA
table(dcas$q53,dcas$hispanic,useNA='ifany')

dcas$hispanic <- (dcas$hispanic==TRUE |
                  dcas$q5401=='Hispanic or Hispanic-American' |
                  dcas$q5402=='Hispanic or Hispanic-American' |
                  dcas$q5403=='Hispanic or Hispanic-American' |
                  dcas$q5404=='Hispanic or Hispanic-American' )

## IDENTIFY BLACKS
## Black and not Hispanic
table(dcas$q5401)
dcas$black <- (dcas$q5401=='Black or African-American' |
               dcas$q5402=='Black or African-American' |
               dcas$q5403=='Black or African-American' |
               dcas$q5404=='Black or African-American' )
table(dcas$q5401,dcas$black)

## IDENTIFY API
## Asian OR Pacific Islander AND NOT Hispanic AND NOT black
dcas$api <- (dcas$q5401=='Asian or Asian-American' | dcas$q5401=='Native Hawaiian/Other Pacific Islander' |
             dcas$q5402=='Asian or Asian-American' | dcas$q5402=='Native Hawaiian/Other Pacific Islander' |
             dcas$q5403=='Asian or Asian-American' | dcas$q5403=='Native Hawaiian/Other Pacific Islander' )
table(dcas$q5401,dcas$api)

## IDENTIFY WHITE
## White AND NOT Hispanic AND NOT Black AND NOT API
dcas$white <- (dcas$q5401=='White' |
               dcas$q5402=='White' |
               dcas$q5403=='White')

## IDENTIFY OTHER
## NOT 1-4 AND NOT NA on all race questions
dcas$other <- (dcas$q5401=='Native American/American Indian/Alaska Native' |
               dcas$q5402=='Native American/American Indian/Alaska Native' |
               dcas$q5403=='Native American/American Indian/Alaska Native'
                )

## CREATE RACE VARIABLE
dcas$race <- NA
dcas$race[dcas$hispanic] <- 'hispanic'
dcas$race[dcas$black==TRUE & is.na(dcas$race)] <- 'black'
dcas$race[dcas$api==TRUE & is.na(dcas$race)] <- 'api'
dcas$race[dcas$other==TRUE & is.na(dcas$race)] <- 'other'
dcas$race[dcas$white==TRUE & is.na(dcas$race)] <- 'white'


table(dcas$race,dcas$dem.race)



