
library(tidyverse)
library(mgcv)
library(nlme)

# Version 2.1

# Two data files:
#   1. tbi_data: data frame that contains only cases that have sustained
#       a TBI (n = 531)
#   2. full_data: data frame that contains cases that have sustained TBI and
#       comparison group (N = 1623)

#Measures:

# Outcomes
#   recid = any admissions
#   felony = felony admissions
#   misd = misdemeanor admissions

# Time measure
#   time = time centered at the midpoint of the year of first TBI

# Covariates
#   osu_1c1 = age at first reported TBI
#   knocked_out = loss of consciousness after first TBI
#   incar_days = proportion of each study interval incarcerated
#   sex = individual's sex
#   race_cat2 = individual's race

#   wtvec = weighted probability score of TBI (only for models with
#           comparison group)



#Baseline Models with time modeled as a parametric covariate
# Table 1 models

#   Any Admissions

m1.2 <- gamm(recid ~ time +
               osu_1c1 +
               factor(knocked_out) +
               offset(incar_days) +
               factor(sex) +  factor(race_cat2) ,
             niterPQL = 200,
             method = "REML",
             data=tbi_data,
             family = binomial(link=logit),
             random = list(id= ~1))

m1.2$gam %>% summary()

#   Felony Admissions

m2.2 <- gamm(felony ~ time +
               osu_1c1 +
               factor(knocked_out) +
               offset(incar_days) +
               factor(sex) +  factor(race_cat2) ,
             niterPQL = 200,
             method = "REML",
             data=tbi_data,
             family = binomial(link=logit),
             random = list(id= ~1))

m2.2$gam %>% summary()


#   Misdemeanor Admissions

m3.2 <- gamm(misd ~ time +
               osu_1c1 +
               factor(knocked_out) +
               offset(incar_days) +
               factor(sex) +  factor(race_cat2) ,
             niterPQL = 200,
             method = "REML",
             data=tbi_data,
             family = binomial(link=logit),
             random = list(id= ~1))

m3.2$gam %>% summary()


#Models with time as a nonparametric covariate
# Figure 2 models

#   Any admission
m1.0 <- gamm(recid ~ s(time) +
             osu_1c1 +
             factor(knocked_out) +
             offset(incar_days) +
             factor(sex) +  factor(race_cat2) ,
           niterPQL = 200,
           method = "REML",
           data=tbi_data,
           family = binomial(link=logit),
           random = list(id= ~1))

m1.0$gam %>% summary()


#   Felony Admissions

m2.0 <- gamm(felony ~ s(time) +
               osu_1c1 +
               factor(knocked_out) +
               offset(incar_days) +
               factor(sex) +  factor(race_cat2) ,
             niterPQL = 200,
             method = "REML",
             data=tbi_data,
             family = binomial(link=logit),
             random = list(id= ~1))

m2.0$gam %>% summary()


#   Misdemeanor Admissions

m3.0 <- gamm(misd ~ s(time) +
               osu_1c1 +
               factor(knocked_out) +
               offset(incar_days) +
               factor(sex) +  factor(race_cat2) ,
             niterPQL = 200,
             method = "REML",
             data=tbi_data,
             family = binomial(link=logit),
             random = list(id= ~1))

m3.0$gam %>% summary()


#Models with interaction (discontinuity)
# Figure 3 models

#   Any Admissions

m1 <- gamm(recid ~ s(time,by=factor(d)) +
                      s(time) +
                      factor(d) +
                      osu_1c1 +
                      factor(knocked_out) +
                      offset(incar_days) +
                      factor(sex) +
                      factor(race_cat2) ,
                   niterPQL = 200,
                   method = "REML",
                   data=tbi_data,
                   family = binomial(link=logit),
                   random = list(id= ~1))

m1$gam %>% summary()


#   Felony Admission


m2 <- gamm(felony ~ s(time,by=factor(d)) +
             s(time) +
             factor(d) +
             osu_1c1 +
             factor(knocked_out) +
             offset(incar_days) +
             factor(sex) +
             factor(race_cat2) ,
           niterPQL = 200,
           method = "REML",
           data=tbi_data,
           family = binomial(link=logit),
           random = list(id= ~1))

m2$gam %>% summary()


#   Misdemeanor admission

m3 <- gamm(misd ~ s(time, by=factor(d)) +
             s(time) +
             factor(d) +
             osu_1c1 +
             factor(knocked_out) +
             offset(incar_days) +
             factor(sex) +
             factor(race_cat2) ,
           niterPQL = 200,
           method = "REML",
           data=tbi_data,
           family = binomial(link=logit),
           random = list(id= ~1))

m3$gam %>% summary()



#Models with sex interaction
# Figure 4 models

#   Any Admission

m1_sex <- gamm(recid ~ s(time,by=factor(sex)) +
                 s(time) +
                 osu_1c1 +
                 factor(knocked_out) +
                 offset(incar_days) +
                 factor(sex) +
                 factor(race_cat2) ,
               niterPQL = 200,
               method = "REML",
               data=tbi_data,
               family = binomial(link=logit),
               random = list(id= ~1))

m1_sex$gam %>% summary()


#   Felony Admission

m2_sex <- gamm(felony ~ s(time,by=factor(sex)) +
                 s(time) +
                 osu_1c1 +
                 factor(knocked_out) +
                 offset(incar_days) +
                 factor(sex) +
                 factor(race_cat2) ,
              niterPQL = 200,
              method = "REML",
              data=tbi_data,
              family = binomial(link=logit),
              random = list(id= ~1))

m2_sex$gam %>% summary()


#   Misdemeanor Admission

m3_sex <- gamm(misd ~ s(time,by=factor(sex)) +
                 s(time) +
                 osu_1c1 +
                 factor(knocked_out) +
                 offset(incar_days) +
                 factor(sex) +
                 factor(race_cat2) ,
             niterPQL = 200,
             method = "REML",
             data=tbi_data,
             family = binomial(link=logit),
             random = list(id= ~1))

m3_sex$gam %>% summary()


#Models with LOC interaction
# Figure 5 Models

#   Any Admission

ad_loc <- gamm(recid ~ s(time,by=factor(knocked_out)) +
                 s(time) +
                 osu_1c1 +
                 factor(knocked_out) +
                 offset(incar_days) +
                 factor(sex) +
                 factor(race_cat2) ,
               niterPQL = 200,
               method = "REML",
               data=tbi_data,
               family = binomial(link=logit),
               random = list(id= ~1))

ad_loc$gam %>% summary()


#   Felony Admission

fel_loc <- gamm(felony ~ s(time,by=factor(knocked_out)) +
                  s(time) +
                  osu_1c1 +
                  factor(knocked_out) +
                  offset(incar_days) +
                  factor(sex) +
                  factor(race_cat2) ,
                niterPQL = 200,
                method = "REML",
                data=tbi_data,
                family = binomial(link=logit),
                random = list(id= ~1))

fel_loc$gam %>% summary()


#   Misdemeanor Admission

misd_loc <- gamm(misd ~ s(time,by=factor(knocked_out)) +
                   s(time) +
                   osu_1c1 +
                   factor(knocked_out) +
                   offset(incar_days) +
                   factor(sex) +
                   factor(race_cat2) ,
                 niterPQL = 200,
                 method = "REML",
                 data=tbi_data,
                 family = binomial(link=logit),
                 random = list(id= ~1))


misd_loc$gam %>% summary()



#Models with Comparison Group interaction
# Figure 6 Models

#   Any Admission

ad_comp <- gamm(recid ~ s(time,by=factor(tbi_di)) +
                  s(time) +
                  osu_1c1 +
                  factor(tbi_di) +
                  wtvec +
                  offset(incar_days) +
                  factor(sex) +
                  factor(race_cat2) ,
                niterPQL = 200,
                method = "REML",
                data=full_data,
                family = binomial(link=logit),
                random = list(id= ~1))

ad_comp$gam %>% summary()


#Felony Admission

fel_comp <- gamm(felony ~ s(time,by=factor(tbi_di)) +
                   s(time) +
                   osu_1c1 +
                   factor(tbi_di) +
                   wtvec +
                   offset(incar_days) +
                   factor(sex) +
                   factor(race_cat2) ,
                 niterPQL = 200,
                 method = "REML",
                 data=full_data,
                 family = binomial(link=logit),
                 random = list(id= ~1))


fel_comp$gam %>% summary()


#Misdemeanor Admission

misd_comp <- gamm(misd ~ s(time,by=factor(tbi_di)) +
                    s(time) +
                    osu_1c1 +
                    factor(tbi_di) +
                    wtvec +
                    offset(incar_days) +
                    factor(sex) +
                    factor(race_cat2) ,
                  niterPQL = 200,
                  method = "REML",
                  data=full_data,
                  family = binomial(link=logit),
                  random = list(id= ~1))

misd_comp$gam %>% summary()



# Models with Age Interaction
#   Age entered as a continuous covariate
#   Results presented in supplemental material (Table S3 and Figures S1-S3)

#   Any Admission

ad_age <- gamm(recid ~ s(time,by=osu_1c1) +
                 s(osu_1c1) +
                 factor(knocked_out) +
                 offset(incar_days) +
                 factor(sex) +
                 factor(race_cat2) ,
               niterPQL = 200,
               method = "REML",
               data=tbi_data,
               family = binomial(link=logit),
               random = list(id= ~1))

ad_age$gam %>% summary()


#Felony Admission

fel_age <- gamm(felony ~ s(time) +
                  s(osu_1c1) +
                  factor(knocked_out) +
                  offset(incar_days) +
                  factor(sex) +
                  factor(race_cat2) ,
                niterPQL = 200,
                method = "REML",
                data=tbi_data,
                family = binomial(link=logit),
                random = list(id= ~1))

fel_age$gam %>% summary()


#Misdemeanor Admission

misd_age <- gamm(misd ~ s(time) +
                   s(osu_1c1) +
                   factor(knocked_out) +
                   offset(incar_days) +
                   factor(sex) +
                   factor(race_cat2) ,
                 niterPQL = 200,
                 method = "REML",
                 data=tbi_data,
                 family = binomial(link=logit),
                 random = list(id= ~1))

misd_age$gam %>% summary()


#Data frame for predicted values
# Sets variables to values used to estimate predicted values
# Can be used for no interaction and time interaction models

pre <- data.frame(
  time = seq(1,8),
  d = 2 ,
  osu_1c1 = mean(tbi_data$osu_1c1),
  knocked_out = 0 ,
  incar_days = mean(tbi_data$incar_days),
  sex = 1 ,
  race_cat2 = 1 )

#Data frame for predicted values
# Sets variables to values used to estimate predicted values
# Can be used for sex interaction models

pre_sex <- data.frame(
  time = seq(1,8),
  osu_1c1 = mean(tbi_data$osu_1c1),
  knocked_out = 0 ,
  incar_days = mean(tbi_data$incar_days),
  sex = 1 ,
  race_cat2 = 1 )


#Data frame for predicted values
# Sets variables to values used to estimate predicted values
# Can be used for LOC interaction models

pre_loc <- data.frame(
  time = seq(1,8),
  osu_1c1 = mean(tbi_data$osu_1c1),
  knocked_out = 0 ,
  incar_days = mean(tbi_data$incar_days),
  sex = 1 ,
  race_cat2 = 1 )

#Data frame for predicted values
# Sets variables to values used to estimate predicted values
# Can be used for group comparison interaction models

pre_comp <- data.frame(
  time = seq(1,8),
  tbi_di = 0,
  wtvec = mean(full_data$wtvec),
  osu_1c1 = mean(full_data$osu_1c1),
  incar_days = mean(full_data$incar_days),
  sex = 1 ,
  race_cat2 = 1 )

#Data frame for predicted values
# Sets variables to values used to estimate predicted values
# Can be used for age interaction models

pre_age <- expand.grid(
  time = seq(1,8),
  time = mean(tbi_data$time),
  osu_1c1 = seq(min(tbi_data$osu_1c1),67),
  knocked_out = 0 ,
  incar_days = mean(tbi_data$incar_days),
  sex = 1 ,
  race_cat2 = 1 )


# No Interactions
p_recid1.0 <- predict.gam(m1.0$gam,newdata=pre,type="response", se.fit=TRUE)
p_fel1.0 <- predict.gam(m2.0$gam,newdata=pre,type="response", se.fit=TRUE)
p_misd1.0 <- predict.gam(m3.0$gam,newdata=pre,type="response", se.fit=TRUE)

# Interactions (time, discontinuity)
pre$d <- 1
p_recid1 <- predict.gam(m1$gam,newdata=pre,type="response", se.fit=TRUE)
pre$d <- 2
p_recid2 <- predict.gam(m1$gam, newdata=pre, type="response", se.fit=TRUE)
pre$d <- 3
p_recid3 <- predict.gam(m1$gam, newdata=pre, type="response", se.fit=TRUE)

pre$d <- 1
p_fel1 <- predict.gam(m2$gam,newdata=pre,type="response", se.fit=TRUE)
pre$d <- 2
p_fel2 <- predict.gam(m2$gam, newdata=pre, type="response", se.fit=TRUE)
pre$d <- 3
p_fel3 <- predict.gam(m2$gam, newdata=pre, type="response", se.fit=TRUE)

pre$d <- 1
p_misd1 <- predict.gam(m3$gam,newdata=pre,type="response", se.fit=TRUE)
pre$d <- 2
p_misd2 <- predict.gam(m3$gam, newdata=pre, type="response", se.fit=TRUE)
pre$d <- 3
p_misd3 <- predict.gam(m3$gam, newdata=pre, type="response", se.fit=TRUE)

# Interactions (sex)
pre_sex$sex <- 1
p_recidsex1 <- predict.gam(m1_sex$gam,newdata=pre_sex,type="response",
                se.fit=TRUE)
pre_sex$sex <- 2
p_recidsex2 <- predict.gam(m1_sex$gam, newdata=pre_sex, type="response",
                se.fit=TRUE)

pre_sex$sex <- 1
p_felsex1 <- predict.gam(m2_sex$gam,newdata=pre_sex,type="response",
              se.fit=TRUE)
pre_sex$sex <- 2
p_felsex2 <- predict.gam(m2_sex$gam, newdata=pre_sex, type="response",
              se.fit=TRUE)

pre_sex$sex <- 1
p_misdsex1 <- predict.gam(m3_sex$gam,newdata=pre_sex,type="response",
                se.fit=TRUE)
pre_sex$sex <- 2
p_misdsex2 <- predict.gam(m3_sex$gam, newdata=pre_sex, type="response",
                se.fit=TRUE)


# Interactions (LOC)
pre_loc$knocked_out <- 0
p_ad_loc1 <- predict.gam(ad_loc$gam,newdata=pre_loc,type="response",
              se.fit=TRUE)
pre_loc$knocked_out <- 1
p_ad_loc2 <- predict.gam(ad_loc$gam, newdata=pre_loc, type="response",
              se.fit=TRUE)

pre_loc$knocked_out <- 0
p_fel_loc1 <- predict.gam(fel_loc$gam,newdata=pre_loc,type="response",
              se.fit=TRUE)
pre_loc$knocked_out <- 1
p_fel_loc2 <- predict.gam(fel_loc$gam, newdata=pre_loc, type="response",
              se.fit=TRUE)

pre_loc$knocked_out <- 0
p_misd_loc1 <- predict.gam(misd_loc$gam,newdata=pre_loc,type="response",
                se.fit=TRUE)
pre_loc$knocked_out <- 1
p_misd_loc2 <- predict.gam(misd_loc$gam, newdata=pre_loc, type="response",
                se.fit=TRUE)


# Interactions (Comparison Group)
pre_comp$tbi_di <- 0
p_ad_comp1 <- predict.gam(ad_comp$gam,newdata=pre_comp,type="response",
              se.fit=TRUE)
pre_comp$tbi_di <- 1
p_ad_comp2 <- predict.gam(ad_comp$gam, newdata=pre_comp, type="response",
              se.fit=TRUE)

pre_comp$tbi_di <- 0
p_fel_comp1 <- predict.gam(fel_comp$gam,newdata=pre_comp,type="response",
                se.fit=TRUE)
pre_comp$tbi_di <- 1
p_fel_comp2 <- predict.gam(fel_comp$gam, newdata=pre_comp, type="response",
                se.fit=TRUE)

pre_comp$tbi_di <- 0
p_misd_comp1 <- predict.gam(misd_comp$gam,newdata=pre_comp,type="response",
                se.fit=TRUE)
pre_comp$tbi_di <- 1
p_misd_comp2 <- predict.gam(misd_comp$gam, newdata=pre_comp, type="response",
                se.fit=TRUE)

# Interactions (Age)
p_ad_age$fit <- predict.gam(ad_age$gam,newdata=pre_age,type="response",
                se.fit=FALSE)

p_fel_age$fit <- predict.gam(fel_age$gam,newdata=pre_age,type="response",
                  se.fit=FALSE)

p_misd_age$fit <- predict.gam(misd_age$gam,newdata=pre_age,type="response",
                  se.fit=FALSE)

# Combines data.frames for graphing
recid_pred <- bind_rows(list(p_recid1, p_recid2, p_recid3), .id="id")
fel_pred <- bind_rows(list(p_fel1, p_fel2, p_fel3), .id="id")
misd_pred <- bind_rows(list(p_misd1, p_misd2, p_misd3), .id="id")

no_int <- bind_rows(list(p_recid1.0, p_fel1.0, p_misd1.0), .id="id")

recid_sex_int <- bind_rows(list(p_recidsex1, p_recidsex2), .id="id")
fel_sex_int <- bind_rows(list(p_felsex1, p_felsex2), .id="id")
misd_sex_int <- bind_rows(list(p_misdsex1, p_misdsex2), .id="id")

recid_loc_int <- bind_rows(list(p_ad_loc1, p_ad_loc2), .id="id")
fel_loc_int <- bind_rows(list(p_fel_loc1, p_fel_loc2), .id="id")
misd_loc_int <- bind_rows(list(p_misd_loc1, p_misd_loc2), .id="id")

recid_comp_int <- bind_rows(list(p_ad_comp1, p_ad_comp2), .id="id")
fel_comp_int <- bind_rows(list(p_fel_comp1, p_fel_comp2), .id="id")
misd_comp_int <- bind_rows(list(p_misd_comp1, p_misd_comp2), .id="id")
