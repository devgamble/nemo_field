

library(tidyverse)
library(here)
library(stringr)


### ClimateNA import adapted from `cch2_getCNA.R` in nemo_herb project folder

nemo_pops <- tibble(pop = c("AC", "BO", "BB", "HA"), .rows = 4)


norms_1951_1980 <- read_csv(here::here("ClimateNA_data", "nemo_norms_1981-2010.csv"), na = c("-9999", "-9999.0")) %>% #Fix NAs
  rename(pop = ID1, lat = Latitude, long = Longitude) %>% 
  select(-ID2, -Elevation) 

TS_ann <- read_csv(here::here("ClimateNA_data", "nemo_out_1901-2018Y.csv"), col_types = cols(.default = "d", MAR = "d", ID1 = "c"), na = c("-9999", "-9999.0")) %>%  #Specify col types to avoid errors due to many NAs 
  select(-ID2, -Elevation) %>% 
  rename(pop = ID1, year = Year)






#Calculating anomolies and coefficients of variation
#Filter for certain period of years?

#Plus corresponding SD

AC_MAP <- TS_ann %>% 
  select(pop, year, MAP) %>% 
  filter(pop == "AC") %>% 
  mutate(MAP_anom = MAP-mean(MAP))
AC_cv <- sd(AC_MAP$MAP)/mean(AC_MAP$MAP)
AC_SD <- sd(AC_MAP$MAP)


BO_MAP <- TS_ann %>% 
  select(pop, year, MAP) %>% 
  filter(pop == "BO") %>% 
  mutate(MAP_anom = MAP-mean(MAP))
BO_cv <- sd(BO_MAP$MAP)/mean(BO_MAP$MAP)
BO_SD <- sd(BO_MAP$MAP)

BB_MAP <- TS_ann %>% 
  select(pop, year, MAP) %>% 
  filter(pop == "BB") %>% 
  mutate(MAP_anom = MAP-mean(MAP))
BB_cv <- sd(BB_MAP$MAP)/mean(BB_MAP$MAP)
BB_SD <- sd(BB_MAP$MAP)

HA_MAP <- TS_ann %>% 
  select(pop, year, MAP) %>% 
  filter(pop == "HA") %>% 
  mutate(MAP_anom = MAP-mean(MAP))
HA_cv <- sd(HA_MAP$MAP)/mean(HA_MAP$MAP)
HA_SD <- sd(HA_MAP$MAP)

list(AC_cv, BO_cv, BB_cv, HA_cv)
list(AC_SD, BO_SD, BB_SD, HA_SD)


##
#For past 60 years
##


AC_MAP2 <- TS_ann %>% 
  select(pop, year, MAP) %>% 
  filter(pop == "AC", year >= 1991) %>% 
  mutate(MAP_anom = MAP-mean(MAP))
AC_cv2 <- sd(AC_MAP2$MAP)/mean(AC_MAP2$MAP)
AC_SD2 <- sd(AC_MAP2$MAP)

BO_MAP2 <- TS_ann %>% 
  select(pop, year, MAP) %>% 
  filter(pop == "BO", year >= 1991) %>% 
  mutate(MAP_anom = MAP-mean(MAP))
BO_cv2 <- sd(BO_MAP2$MAP)/mean(BO_MAP2$MAP)
BO_SD2 <- sd(BO_MAP2$MAP)

BB_MAP2 <- TS_ann %>% 
  select(pop, year, MAP) %>% 
  filter(pop == "BB", year >= 1991) %>% 
  mutate(MAP_anom = MAP-mean(MAP))
BB_cv2 <- sd(BB_MAP2$MAP)/mean(BB_MAP2$MAP)
BB_SD2 <- sd(BB_MAP2$MAP)

HA_MAP2 <- TS_ann %>% 
  select(pop, year, MAP) %>% 
  filter(pop == "HA", year >= 1991) %>% 
  mutate(MAP_anom = MAP-mean(MAP))
HA_cv2 <- sd(HA_MAP2$MAP)/mean(HA_MAP2$MAP)
HA_SD2 <- sd(HA_MAP2$MAP)

list(AC_cv2, BO_cv2, BB_cv2, HA_cv2)
list(AC_SD2, BO_SD2, BB_SD2, HA_SD2)

#Playing around with different dates/spans of years - CV coefficients get more and more similar the more recent in time you go (up to 1991 and 2009 - 2018). Suggests that AC is getting more variabile in interannual precipitation through time, or other (dry) sites are getting less variable!!!! Almost like a homogenization of precipitation variability across these sites!!!


####
# Temperature variation
####

# MAT


AC_MAT <- TS_ann %>% 
  select(pop, year, MAT) %>% 
  filter(pop == "AC") %>% 
  mutate(MAT_anom = MAT-mean(MAT))
AC_cv_MAT <- sd(AC_MAT$MAT)/mean(AC_MAT$MAT)
BO_MAT <- TS_ann %>% 
  select(pop, year, MAT) %>% 
  filter(pop == "BO") %>% 
  mutate(MAT_anom = MAT-mean(MAT))
BO_cv_MAT <- sd(BO_MAT$MAT)/mean(BO_MAT$MAT)
BB_MAT <- TS_ann %>% 
  select(pop, year, MAT) %>% 
  filter(pop == "BB") %>% 
  mutate(MAT_anom = MAT-mean(MAT))
BB_cv_MAT <- sd(BB_MAT$MAT)/mean(BB_MAT$MAT)
HA_MAT <- TS_ann %>% 
  select(pop, year, MAT) %>% 
  filter(pop == "HA") %>% 
  mutate(MAT_anom = MAT-mean(MAT))
HA_cv_MAT <- sd(HA_MAT$MAT)/mean(HA_MAT$MAT)

list(c(AC_cv_MAT, BO_cv_MAT, BB_cv_MAT, HA_cv_MAT))



## MWMT

AC_MWMT <- TS_ann %>% 
  select(pop, year, MWMT) %>% 
  filter(pop == "AC") %>% 
  mutate(MWMT_anom = MWMT-mean(MWMT))
AC_cv_MWMT <- sd(AC_MWMT$MWMT)/mean(AC_MWMT$MWMT)
BO_MWMT <- TS_ann %>% 
  select(pop, year, MWMT) %>% 
  filter(pop == "BO") %>% 
  mutate(MWMT_anom = MWMT-mean(MWMT))
BO_cv_MWMT <- sd(BO_MWMT$MWMT)/mean(BO_MWMT$MWMT)
BB_MWMT <- TS_ann %>% 
  select(pop, year, MWMT) %>% 
  filter(pop == "BB") %>% 
  mutate(MWMT_anom = MWMT-mean(MWMT))
BB_cv_MWMT <- sd(BB_MWMT$MWMT)/mean(BB_MWMT$MWMT)
HA_MWMT <- TS_ann %>% 
  select(pop, year, MWMT) %>% 
  filter(pop == "HA") %>% 
  mutate(MWMT_anom = MWMT-mean(MWMT))
HA_cv_MWMT <- sd(HA_MWMT$MWMT)/mean(HA_MWMT$MWMT)

list(c(AC_cv_MWMT, BO_cv_MWMT, BB_cv_MWMT, HA_cv_MWMT))


######
# CMD (Cumulative Moisture Deficit)
######

AC_CMD <- TS_ann %>% 
  select(pop, year, CMD) %>% 
  filter(pop == "AC") %>% 
  mutate(CMD_anom = CMD-mean(CMD))
AC_cv_CMD <- sd(AC_CMD$CMD)/mean(AC_CMD$CMD)
BO_CMD <- TS_ann %>% 
  select(pop, year, CMD) %>% 
  filter(pop == "BO") %>% 
  mutate(CMD_anom = CMD-mean(CMD))
BO_cv_CMD <- sd(BO_CMD$CMD)/mean(BO_CMD$CMD)
BB_CMD <- TS_ann %>% 
  select(pop, year, CMD) %>% 
  filter(pop == "BB") %>% 
  mutate(CMD_anom = CMD-mean(CMD))
BB_cv_CMD <- sd(BB_CMD$CMD)/mean(BB_CMD$CMD)
HA_CMD <- TS_ann %>% 
  select(pop, year, CMD) %>% 
  filter(pop == "HA") %>% 
  mutate(CMD_anom = CMD-mean(CMD))
HA_cv_CMD <- sd(HA_CMD$CMD)/mean(HA_CMD$CMD)

list(c(AC_cv_CMD, BO_cv_CMD, BB_cv_CMD, HA_cv_CMD))






