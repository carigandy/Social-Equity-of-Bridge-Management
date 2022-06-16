# National Bridge Inventory (NBI) Panel Preparation for Regression
## 1992-2020 Records Used to Obtain 1990-2020 Inspections - Must be downloaded and unzipped first
## U.S. Census Bureau spatial and demographic data through API - Must input key on line 23
## Author: Cari Gandy <cgandy@andrew.cmu.edu>

# Load data.table for faster processing than data.frame functions
install.packages("data.table", dependencies = FALSE)
install.packages("tidyverse",dependencies = TRUE)
install.packages("sf")
install.packages("tigris")
install.packages("tidycensus")
install.packages("dismo")
install.packages("deldir")

library(data.table)
library(tidyverse)
library(sf)
library(dismo)
library(deldir)
library(tigris)
library(tidycensus)

census_api_key("INSERT_KEY_HERE") # https://api.census.gov/data/key_signup.html
options(tigris_use_cache = TRUE)
options(scipen=999)

# Part 1: Download Census tracts, race data, and income data:

## Download Census TIGER/LINE Shapefiles with tigris
us_state_terr_codes <- unique(fips_codes$state_code)
us_state_codes <- us_state_terr_codes[1:51] ## Territories are in separate files

for (yr in c(2000,2010, 2020)){ 
  name <- paste("tracts_",yr,sep="")
  
  assign(name, 
         rbind_tigris(lapply(us_state_codes,
                             function(x) {
                               tracts(state = x, year=yr)
                             }
         ))
  )
  
}


## These are the suffixes of the variables from the Decennial Census table: 
## "Hispanic or Latino, and not Hispanic or Latino by Race"
dec_vars<-c("01", # Population
            "02", # Hispanic or Latino
            "05", # White of a single race, not Hispanic or Latino
            "06", # Remainder are Black or African American, not Hispanic or 
            "13", #   Latino of any number and combination of races
            "18",
            "19",
            "20",
            "21",
            "29",
            "30",
            "31",
            "32",
            "39",
            "40",
            "41",
            "42",
            "43",
            "44",
            "50",
            "51",
            "52",
            "53",
            "54",
            "55",
            "60",
            "61",
            "62",
            "63",
            "66",
            "67",
            "68",
            "69",
            "71",
            "73")

## Each Decennial Census year has different coding so we adjust each separately 
## Table: "Hispanic or Latino, and not Hispanic or Latino by Race"
dec_vars_2020 <- paste("P2_0",dec_vars,"N",sep="")
dec_vars_2010 <- paste("P0020",dec_vars,sep="") 
dec_vars_2000 <- paste("PL0020",dec_vars,sep="")

## For tract race demographics, download Decennial Census tables with tidycensus
race_2020 <- get_decennial(geography = "tract",
                           sumfile = "pl",
                           state = us_state_codes, 
                           year = 2020,
                           variables = dec_vars_2020,
                           output = "wide")

race_2010 <- get_decennial(geography = "tract",
                           sumfile = "pl",
                           state = us_state_codes, 
                           year = 2010,
                           variables = dec_vars_2010,
                           output = "wide")

race_2000 <- get_decennial(geography = "tract",
                           sumfile = "pl",
                           state = us_state_codes, 
                           year = 2000,
                           variables = dec_vars_2000,
                           output = "wide")

## Assign race variables
race_2020$POPULATION <- race_2020[,dec_vars_2020[1]]
race_2020$HISP_LATINO <- race_2020[,dec_vars_2020[2]]
race_2020$WHITE_ALONE <- race_2020[,dec_vars_2020[3]] 
race_2020$BLACK_AFAM <- rowSums(race_2020[,dec_vars_2020[4:31]])

race_2010$POPULATION <- race_2010[,dec_vars_2010[1]]
race_2010$HISP_LATINO <- race_2010[,dec_vars_2010[2]]
race_2010$WHITE_ALONE <- race_2010[,dec_vars_2010[3]] 
race_2010$BLACK_AFAM <- rowSums(race_2010[,dec_vars_2010[4:31]])

race_2000$POPULATION <- race_2000[,dec_vars_2000[1]]
race_2000$HISP_LATINO <- race_2000[,dec_vars_2000[2]]
race_2000$WHITE_ALONE <- race_2000[,dec_vars_2000[3]] 
race_2000$BLACK_AFAM <- rowSums(race_2000[,dec_vars_2000[4:31]])

race_nat_avg <- as.data.frame(list(DECENNIAL = c(2000,2010,2020),
                                   TOTAL_POPULATION = c(sum(race_2000$POPULATION),
                                                        sum(race_2010$POPULATION),
                                                        sum(race_2020$POPULATION)),
                                   TOTAL_WHITE_ALONE = c(sum(race_2000$WHITE_ALONE),
                                                         sum(race_2010$WHITE_ALONE),
                                                         sum(race_2020$WHITE_ALONE)),
                                   TOTAL_HISP_LATINO = c(sum(race_2000$HISP_LATINO),
                                                         sum(race_2010$HISP_LATINO),
                                                         sum(race_2020$HISP_LATINO)),
                                   TOTAL_BLACK_AFAM = c(sum(race_2000$BLACK_AFAM),
                                                        sum(race_2010$BLACK_AFAM),
                                                        sum(race_2020$BLACK_AFAM))))

race_nat_avg <- as.data.table(race_nat_avg)
race_nat_avg[,AVG_WHITE_ALONE := TOTAL_WHITE_ALONE/TOTAL_POPULATION*100]
race_nat_avg[,AVG_HISP_LATINO := TOTAL_HISP_LATINO/TOTAL_POPULATION*100]
race_nat_avg[,AVG_BLACK_AFAM := TOTAL_BLACK_AFAM/TOTAL_POPULATION*100]

fwrite(race_nat_avg,file="decennial_nat_avg_race.csv")

## Download 5-year ACS aggregate household income by tract with tidycensus
## (1-year estimates are only available for populations over 65,000)
income_2020 <- get_acs(geography = "tract",
                       state = us_state_codes, 
                       year = 2020,
                       variables = c("B19025_001", "B19326_001"),
                       survey = "acs5",
                       output = "wide")
setnames(income_2020,"B19025_001E","AGG_INCOME")
setnames(income_2020,"B19326_001E","MED_INCOME")

income_2010 <- get_acs(geography = "tract",
                       state = us_state_codes, 
                       year = 2010,
                       variables = c("B19025_001", "B19326_001"),
                       survey = "acs5",
                       output = "wide")
setnames(income_2010,"B19025_001E","AGG_INCOME")
setnames(income_2010,"B19326_001E","MED_INCOME")

# For aggregate household income before 2005, use 2000 Decennial Census SF3 P054
income_2000 <- get_decennial(geography = "tract",
                             state = us_state_codes, 
                             year = 2000,
                             variables = c("P053001","P054001"),
                             output = "wide")
setnames(income_2000,"P054001","AGG_INCOME")
setnames(income_2000,"P053001","MED_INCOME")


## Join Census data to shapefiles by tract 

## Limit tables to relevant demographic variables
race_vars <- c("GEOID","POPULATION", "HISP_LATINO", "WHITE_ALONE", "BLACK_AFAM")
income_vars <- c("GEOID", "AGG_INCOME","MED_INCOME")

## Both income and race for 2000
tracts_2000$GEOID <- paste(tracts_2000$STATEFP00, 
                           tracts_2000$COUNTYFP00, 
                           tracts_2000$TRACTCE00,
                           sep="")
geo_race_2000 <- merge(tracts_2000,race_2000[,race_vars], by = "GEOID", 
                       all.x = TRUE)
geo_race_income_2000 <- merge(geo_race_2000,income_2000[,income_vars], 
                              by = "GEOID", all.x = TRUE)

## ACS income for 2006-2010
setnames(tracts_2010, "GEOID10","GEOID",skip_absent=TRUE) 
geo_income_2010 <- merge(tracts_2010,income_2010[,income_vars], by = "GEOID", 
                         all.x = TRUE)

## ACS income for 2016-2020
geo_income_2020 <- merge(tracts_2020,income_2020[,income_vars], by = "GEOID", 
                         all.x = TRUE)

## Decennial Race for 2001-2010
setnames(tracts_2010, "GEOID10","GEOID", skip_absent=TRUE)
geo_race_2010 <- merge(tracts_2010,race_2010[,race_vars], by = "GEOID", 
                       all.x = TRUE)

## Decennial Race for 2011-2020
geo_race_2020 <- merge(tracts_2020,race_2020[,race_vars], by = "GEOID", 
                       all.x = TRUE)



# Part 2: Load, append, and censor NBI records while tracking changes

## Load and append records that were quality checked according to technical note
#setwd("INSERT_FILE_WD")
panel<-NULL

### For using data quality checked according to Kang, X., Ritsch, N., Gandy, C., 
### & Armanios, D. (2022). Quality Control and Spatial Approaches for Gauging 
### Feasibility and Enhancing Accuracy of Existing Transportation Data for  
### Equity Purposes. Working paper #1.
# for (i in c(2020:1992)){
#   temp<-unique(fread(paste(i,"new.csv",sep=""),
#                      header=TRUE), 
#       by="STATE_CODE_001_STRUCTURE_NUMBER_008_RECORD_TYPE_005A_YEAR_BUILT_027")
#   
#   temp[,RECORD_YR:=i]
#   panel<-rbind(panel,temp,idcol=FALSE, fill=TRUE)
# }
#### Shorten key
# setnames(panel,
#          "STATE_CODE_001_STRUCTURE_NUMBER_008_RECORD_TYPE_005A_YEAR_BUILT_027", 
#          "BRIDGE_KEY")

### For using unzipped delimited NBI data from 
### https://www.fhwa.dot.gov/bridge/nbi/ascii.cfm
for (i in c(2020:1992)){
  temp<-fread(paste(i,"AllRecordsDelimitedAllStates.txt",sep=""),header=TRUE)
  temp[,BRIDGE_KEY := paste(STATE_CODE_001,STRUCTURE_NUMBER_008,
                            RECORD_TYPE_005A,YEAR_BUILT_027,sep="_")]
  temp <- unique(temp, by="BRIDGE_KEY")
  temp[,RECORD_YR:=i]
  panel<-rbind(panel,temp,idcol=FALSE, fill=TRUE)
}

#setwd("INSERT_WRITE_WD")

## Save interim result and begin tracking changes
fwrite(panel,"NBI_panel_raw_IY_1990-2020_age.csv")
#panel<-fread("NBI_panel_raw_IY_1990-2020_age.csv") #For starting here
panel_initial<-panel
reason<-"Initial"
record_ct<-nrow(panel)
bridge_ct<-nrow(unique(panel,by="BRIDGE_KEY"))

## Create inspection year from inspection date (MMYY)
panel[,INSPECT_YR:=as.integer(str_sub(panel$DATE_OF_INSPECT_090,-2,-1))]
panel[INSPECT_YR>20,INSPECT_YR:=1900+INSPECT_YR]
panel[INSPECT_YR<=20,INSPECT_YR:=2000+INSPECT_YR]
panel[is.na(INSPECT_YR)|INSPECT_YR<1990|INSPECT_YR<YEAR_BUILT_027, 
      INSPECT_YR:=RECORD_YR]

## Create a time index by bridge age = inspection year - start of lifecycle
## Assign the start of the bridges' lifecycle as the year built
panel[!is.na(YEAR_BUILT_027)
      & YEAR_BUILT_027<=2021
      & YEAR_BUILT_027<=INSPECT_YR
      & YEAR_BUILT_027>1776,
      START_LFC:=YEAR_BUILT_027]

## If the bridge was reconstructed before this inspection start lifecycle then
panel[!is.na(YEAR_RECONSTRUCTED_106) 
      & YEAR_RECONSTRUCTED_106<=2021
      & YEAR_RECONSTRUCTED_106>1776
      & YEAR_RECONSTRUCTED_106>YEAR_BUILT_027 # Must be after construction
      & YEAR_RECONSTRUCTED_106<=INSPECT_YR,   # Must be before inspection
      START_LFC:=YEAR_RECONSTRUCTED_106]


## Remove records with no construction date information 
panel<-panel[!is.na(START_LFC)]

reason<-c(reason,"Remove records without a construction date")
bridge_ct<-c(bridge_ct,nrow(unique(panel,by="BRIDGE_KEY")))
record_ct<-c(record_ct,nrow(panel))

## Calculate age for records retained in panel
panel[,AGE:=(INSPECT_YR-START_LFC)]
summary(panel$AGE)

## Remove records for bridges over 100 years old
panel <- panel[AGE <= 100]

reason<-c(reason,"Remove records for bridges over 100 years old")
bridge_ct<-c(bridge_ct,nrow(unique(panel,by="BRIDGE_KEY")))
record_ct<-c(record_ct,nrow(panel))

## Remove records that do not contain condition data 
panel<-panel[!is.na(as.numeric(DECK_COND_058))
             &!is.na(as.numeric(SUPERSTRUCTURE_COND_059))
             &!is.na(as.numeric(SUBSTRUCTURE_COND_060))]

### Optional: Check why records do not have condition data
# total_no_cond <- panel_initial[is.na(as.numeric(DECK_COND_058))
#                               |is.na(as.numeric(SUPERSTRUCTURE_COND_059))
#                               |is.na(as.numeric(SUBSTRUCTURE_COND_060)),.N]
# panel_initial[is.na(as.numeric(DECK_COND_058))
#               |is.na(as.numeric(SUPERSTRUCTURE_COND_059))
#               |is.na(as.numeric(SUBSTRUCTURE_COND_060)),
#               .N/total_no_cond,
#               by=RECORD_TYPE_005A]
# 
# panel_initial[(is.na(as.numeric(DECK_COND_058))
#               |is.na(as.numeric(SUPERSTRUCTURE_COND_059))
#               |is.na(as.numeric(SUBSTRUCTURE_COND_060)))
#               & RECORD_TYPE_005A==1,
#               .N/total_no_cond,
#               by=STRUCTURE_TYPE_043B]
# 
# panel_initial[(is.na(as.numeric(DECK_COND_058))
#               |is.na(as.numeric(SUPERSTRUCTURE_COND_059))
#               |is.na(as.numeric(SUBSTRUCTURE_COND_060)))
#               & (RECORD_TYPE_005A!=1        #Not carrying NHS Route
#                  |STRUCTURE_TYPE_043B==11   #Filled arch
#                  |STRUCTURE_TYPE_043B==19), #Culvert
#               .N/total_no_cond]
# 
# panel_initial[(is.na(as.numeric(DECK_COND_058))
#                |is.na(as.numeric(SUPERSTRUCTURE_COND_059))
#                |is.na(as.numeric(SUBSTRUCTURE_COND_060)))
#               & (RECORD_TYPE_005A==1        #Not carrying NHS Route
#                 &STRUCTURE_TYPE_043B!=11   #Filled arch
#                 &STRUCTURE_TYPE_043B!=19), #Culvert
#               .N]

reason<-c(reason,"Remove records that do not contain condition data")
bridge_ct<-c(bridge_ct,nrow(unique(panel,by="BRIDGE_KEY")))
record_ct<-c(record_ct,nrow(panel))

#Remove records that do not contain service related information                                                                    
panel<-panel[!is.na(FUNCTIONAL_CLASS_026)
             & !is.na(as.numeric(ADT_029))
             & !is.na(as.numeric(DETOUR_KILOS_019))
             & !is.na(SERVICE_UND_042B)
             & !is.na(STRUCTURE_TYPE_043B)
             & ADT_029!=999999 & ADT_029>0]

### Optional: Check distribution of records without percent ADT truck
# panel[is.na(PERCENT_ADT_TRUCK_109),.N,by=FUNCTIONAL_CLASS_026]
# panel[is.na(PERCENT_ADT_TRUCK_109),.N,by=STATE_CODE_001]
# panel[is.na(PERCENT_ADT_TRUCK_109),.N,by=RECORD_YR] #Improved data over time
# summary(panel[is.na(PERCENT_ADT_TRUCK_109),ADT_029])

panel[is.na(PERCENT_ADT_TRUCK_109),PERCENT_ADT_TRUCK_109:=0]

reason<-c(reason,"Remove records without service related regressors")
bridge_ct<-c(bridge_ct,nrow(unique(panel,by="BRIDGE_KEY")))
record_ct<-c(record_ct,nrow(panel))

## Remove records with missing or illogical coordinates
panel <- panel[!is.na(LAT_016)         # Missing
               & LAT_016>=0          # Invalid geometry
               & LAT_016<=90000000   # Invalid geometry
               & nchar(LAT_016)>6    # Insufficient precision
               &!is.na(LONG_017)     # Missing
               & LONG_017>=0         # Invalid geometry
               & LONG_017<=180000000 # Invalid geometry
               & nchar(LONG_017)>6]  # Insufficient precision

reason<-c(reason,"Remove records with missing or illogical coordinates")
bridge_ct<-c(bridge_ct,nrow(unique(panel,by="BRIDGE_KEY")))
record_ct<-c(record_ct,nrow(panel))


# Part 3: Spatially join panel to Census tracts within 400-meters

## Convert NBI latitude to decimal degrees
chars_y<-nchar(panel$LAT_016)
DD_y<-as.numeric(substr(panel$LAT_016,1,chars_y-6))
MM_y<-as.numeric(substr(panel$LAT_016,chars_y-5,chars_y-4))      
SS_y<-as.numeric(substr(panel$LAT_016,chars_y-3,chars_y-2))     
ss_y<-as.numeric(substr(panel$LAT_016,chars_y-1,chars_y))
panel$LAT_016<-DD_y+(MM_y/60)+((SS_y+ss_y/100)/3600)
s=panel[STATE_CODE_001==60,which=TRUE]  #No records for American Samoa (60).. 
panel[s,21]<-(-1)*panel[s,21]         #..in the Southern Hemisphere

## Convert NBI longitude to decimal degrees
chars_x<-nchar(panel$LONG_017)
DD_x<-as.numeric(substr(panel$LONG_017,1,chars_x-6))
MM_x<-as.numeric(substr(panel$LONG_017,chars_x-5,chars_x-4))      
SS_x<-as.numeric(substr(panel$LONG_017,chars_x-3,chars_x-2))     
ss_x<-as.numeric(substr(panel$LONG_017,chars_x-1,chars_x))
panel$LONG_017<-DD_x+(MM_x/60)+((SS_x+ss_x/100)/3600)
#Excluding Guam(66) & CNMI(69), assign negative longitude in Western Hemisphere
w=panel[STATE_CODE_001!=66 & STATE_CODE_001!=69,which=TRUE]
panel[w,22]<-(-1)*panel[w,22]

fwrite(panel,"NBI_Panel_Prejoin_age.csv") # Optional save
#panel<-fread("NBI_Panel_Prejoin_age.csv") # For starting here

## Create spatial dataframe of unique bridges with latest coordinates and
## join Decennial and ACS data so that you can conduct sensitivity analysis
geo_race_vars <- c("GEOID","STATEFP","COUNTYFP","POPULATION","HISP_LATINO",
                   "WHITE_ALONE","BLACK_AFAM","geometry")
geo_income_vars <- c("GEOID","STATEFP","COUNTYFP","AGG_INCOME","MED_INCOME",
                     "geometry")

bridges_sf <- panel[order(INSPECT_YR,decreasing=TRUE)] %>% # Sort most recent
  unique(by="BRIDGE_KEY") %>%             # Most recent record for each bridge
  st_as_sf(coords = c("LONG_017", "LAT_016"), crs = "NAD83") %>% 
  st_join(geo_race_income_2000[c(geo_race_vars,"AGG_INCOME","MED_INCOME")], 
          join = st_intersects,
          left = TRUE) %>%
  st_join(geo_race_2010[geo_race_vars],
          join = st_intersects,
          suffix = c(".2000.dec",".2010.dec"),
          left = TRUE) %>%  
  st_join(geo_race_2020[geo_race_vars],
          join = st_intersects,
          suffix = c("",".2020.dec"), # No overlap, suffix will not be assigned
          left = TRUE) %>%
  st_join(geo_income_2010[geo_income_vars],
          join = st_intersects,
          suffix = c(".2020.dec", ".2010.acs"), # 2020 suffix on 2000 income
          left = TRUE) %>%
  st_join(geo_income_2020[geo_income_vars],
          join = st_intersects,
          suffix = c(".2010.acs", ".2020.acs"),
          left = TRUE) %>% 
  rename(AGG_INCOME.2000.dec=AGG_INCOME.2020.dec, 
         MED_INCOME.2000.dec=MED_INCOME.2020.dec,
         POPULATION.2020.dec=POPULATION,
         WHITE_ALONE.2020.dec=WHITE_ALONE,
         HISP_LATINO.2020.dec=HISP_LATINO,
         BLACK_AFAM.2020.dec=BLACK_AFAM,
         AGG_INCOME.2020.acs=AGG_INCOME,
         MED_INCOME.2020.acs=MED_INCOME)

# summary(bridges_sf$POPULATION)
# summary(bridges_sf$POPULATION.2010.dec)
# summary(bridges_sf$POPULATION.2020.dec)
# summary(bridges_sf$MED_INCOME.2020.acs)


## Add interpolated climate data from Voronoi polygons between climate stations
## Climate station data from: "Liao, T., P. Kepley, I. Kumar, S. Labi, (2022) 
## Revisiting the Secondary Climate Attributes for Transportation Infrastructure 
## Management: A Redux and Update for 2020. working paper #1"

climate <- fread("climate normals 1991-2020.csv")

climate_sp <- SpatialPoints(climate[,5:4], 
                            proj4string=CRS("+proj=longlat +datum=NAD83"))
climate_sp <- SpatialPointsDataFrame(climate_sp, climate)

v <- voronoi(climate_sp)

bridges_sp <- as(bridges_sf,Class = "Spatial")

climate_key <-over(bridges_sp,v) 

bridges_climate <-as.data.table(cbind(bridges_sf,climate_key))

## Join disadvantaged community indicator from CEJST Version 1.0:
## https://screeningtool.geoplatform.gov/en/downloads
CEJST <- fread("communities-2022-05-31-1915.csv",header=TRUE) # May update
CEJST[,GEOID10_TRACT:=str_sub(GEOID10_TRACT,3,-3)] # Make 11-digit char
setnames(CEJST,"Identified as disadvantaged (v0.1)","DISADVANTAGED")
bridges_climate_J40 <- merge(bridges_climate[,GEOID.2010.dec:=
                                               str_pad(GEOID.2010.dec,11,pad="0")], 
                             CEJST[,c("GEOID10_TRACT","DISADVANTAGED")],
                             by.x="GEOID.2010.dec",
                             by.y="GEOID10_TRACT",
                             all.x=TRUE)

## Rejoin panel, taking time-invariant factors from the latest record which is
## assumed to be the most accurate inventory
panel_join <- merge(panel[,c("BRIDGE_KEY",
                             "INSPECT_YR",
                             "AGE",
                             "DECK_COND_058",
                             "SUPERSTRUCTURE_COND_059",
                             "SUBSTRUCTURE_COND_060")],
                    bridges_climate_J40,
                    suffixes = c("","_final"),
                    by = "BRIDGE_KEY",
                    all=FALSE)

fwrite(panel_join,"NBI_Census_J40_Panel_age.csv")
#panel_join_old<-fread("NBI_Census_J40_Panel_age.csv")

# Part 4: Check spatial joins and track changes

## Retain the most recent inventory for the same bridge and inspection year
## NBIS: bridges must be inspected every 24 months or 48 months by exception
## Some record years will repeat inspection years as a result
panel_clean<-panel_join[order(RECORD_YR,decreasing=TRUE)] %>% 
  unique(by=c("BRIDGE_KEY","INSPECT_YR"))
reason<-c(reason,"Retain the most recent record of each inspection")
bridge_ct<-c(bridge_ct,nrow(unique(panel_clean,by="BRIDGE_KEY")))
record_ct<-c(record_ct,nrow(panel_clean))

## Delete records that were not joined to demographic data
panel_clean <- panel_clean[!is.na(POPULATION.2020.dec) 
                           & !is.na(MED_INCOME.2020.acs)
                           & !is.na(POPULATION.2010.dec) 
                           & !is.na(MED_INCOME.2014.acs)
                           & !is.na(MED_INCOME.2010.acs)
                           & !is.na(POPULATION.2000.dec) 
                           & !is.na(MED_INCOME.2000.dec)]

reason<-c(reason,"Coordinates not within a tract")
bridge_ct<-c(bridge_ct,nrow(unique(panel_clean,by="BRIDGE_KEY")))
record_ct<-c(record_ct,nrow(panel_clean))

## Delete records with coordinates in the wrong state (Decennial boundaries)
panel_clean <- panel_clean[as.integer(STATE_CODE_001) ==
                             as.integer(STATEFP.2020.dec)
                           |as.integer(STATE_CODE_001) ==
                             as.integer(STATEFP.2010.dec)
                           |as.integer(STATE_CODE_001) ==
                             as.integer(STATEFP.2000.dec)]

reason<-c(reason,"Coordinates in the wrong state")
bridge_ct<-c(bridge_ct,nrow(unique(panel_clean,by="BRIDGE_KEY")))
record_ct<-c(record_ct,nrow(panel_clean))

## Delete records with coordinates in the wrong county (Decennial boundaries)
panel_clean <- panel_clean[(as.integer(STATE_CODE_001) ==
                              as.integer(STATEFP.2020.dec) &
                              as.integer(COUNTY_CODE_003) ==
                              as.integer(COUNTYFP.2020.dec))
                           |(as.integer(STATE_CODE_001) ==
                               as.integer(STATEFP.2010.dec) &
                               as.integer(COUNTY_CODE_003) ==
                               as.integer(COUNTYFP.2010.dec))
                           |(as.integer(STATE_CODE_001) ==
                               as.integer(STATEFP.2000.dec) &
                               as.integer(COUNTY_CODE_003) ==
                               as.integer(COUNTYFP.2000.dec))] 

reason<-c(reason,"Coordinates in the wrong county")
bridge_ct<-c(bridge_ct,nrow(unique(panel_clean,by="BRIDGE_KEY")))
record_ct<-c(record_ct,nrow(panel_clean))

## Delete records in an unpopulated tract
panel_clean <- panel_clean[POPULATION.2020.dec > 0
                           & MED_INCOME.2020.acs > 0
                           & POPULATION.2010.dec > 0
                           & MED_INCOME.2010.acs > 0
                           & POPULATION.2000.dec > 0
                           & MED_INCOME.2000.dec > 0]

reason<-c(reason,"Coordinates not within a populated tract")
bridge_ct<-c(bridge_ct,nrow(unique(panel_clean,by="BRIDGE_KEY")))
record_ct<-c(record_ct,nrow(panel_clean))

## Delete records missing climate data
panel_clean <- panel_clean[!is.na(ANNUAL.TAVG..DEG.F.)
                           & !is.na(ANNUAL.PRCP..INCH.)
                           & !is.na(FREEZE.THAW.CYCLE)]

reason<-c(reason,"Missing climate data")
bridge_ct<-c(bridge_ct,nrow(unique(panel_clean,by="BRIDGE_KEY")))
record_ct<-c(record_ct,nrow(panel_clean))

## Remove bridges with less than three observations in the panel
bridges_3_obs <- list(panel_clean[,.N,by="BRIDGE_KEY"][N>=3,BRIDGE_KEY])
panel_clean <- panel_clean[bridges_3_obs, on = "BRIDGE_KEY"]

reason<-c(reason,"Bridge had less than three inspections in the panel")
bridge_ct<-c(bridge_ct,nrow(unique(panel_clean,by="BRIDGE_KEY")))
record_ct<-c(record_ct,nrow(panel_clean))

track_changes <- data.frame(Reason = reason, 
                            Records = record_ct, 
                            Bridges = bridge_ct)


fwrite(panel_clean,"NBI_Census_Climate_Panel_min3.csv") # Optional save
fwrite(track_changes,"panel_track_changes_age.csv") # Recommended save

### For starting here:
#panel_clean <- fread("NBI_Census_Climate_Panel_min3.csv")
#track_changes <- fread("panel_track_changes_age.csv")


# Part 5: Create regression variables

## Create bridge condition (lowest component condition) for pre-2018 records
## Original 2018-2021 data in "BRIDGE_CONDITION"
panel_clean[,BRIDGE_COND := pmin(as.integer(DECK_COND_058),
                                 as.integer(SUPERSTRUCTURE_COND_059),
                                 as.integer(SUBSTRUCTURE_COND_060))]

## Create condition states based on Saeed, T. U., Qiao, Y., Chen, S., 
## Gkritza, K., & Labi, S. (2017). Methodology for Probabilistic Modeling of 
## Highway Bridge Infrastructure Condition: Accounting for Improvement 
## Effectiveness and Incorporating Random Effects. Journal of Infrastructure 
## Systems, 23(4), 04017030. https://doi.org/10.1061/(asce)is.1943-555x.0000389
panel_clean[DECK_COND_058==9,DECK_COND_STATE:=4]
panel_clean[DECK_COND_058==7 | DECK_COND_058==8,DECK_COND_STATE:=3]
panel_clean[DECK_COND_058==6,DECK_COND_STATE:=2]
panel_clean[DECK_COND_058<=5,DECK_COND_STATE:=1]

panel_clean[SUPERSTRUCTURE_COND_059==9,SUPERSTRUCTURE_COND_STATE:=4]
panel_clean[SUPERSTRUCTURE_COND_059==7 | SUPERSTRUCTURE_COND_059==8,
            SUPERSTRUCTURE_COND_STATE:=3]
panel_clean[SUPERSTRUCTURE_COND_059==6,SUPERSTRUCTURE_COND_STATE:=2]
panel_clean[SUPERSTRUCTURE_COND_059<=5,SUPERSTRUCTURE_COND_STATE:=1]

panel_clean[SUBSTRUCTURE_COND_060==9,SUBSTRUCTURE_COND_STATE:=4]
panel_clean[SUBSTRUCTURE_COND_060==7 | SUBSTRUCTURE_COND_060==8,
            SUBSTRUCTURE_COND_STATE:=3]
panel_clean[SUBSTRUCTURE_COND_060==6,SUBSTRUCTURE_COND_STATE:=2]
panel_clean[SUBSTRUCTURE_COND_060<=5,SUBSTRUCTURE_COND_STATE:=1]


## Create Dummy Variables
panel_clean[,URBAN := FUNCTIONAL_CLASS_026>10]
panel_clean[,INTERSTATE := FUNCTIONAL_CLASS_026 == "1"
            |FUNCTIONAL_CLASS_026 == "11"]
panel_clean[,DECK_PROTECTION := DECK_PROTECTION_108C != "0"]
panel_clean[,STEEL_STRUCT := STRUCTURE_TYPE_043B =="3"
            |STRUCTURE_TYPE_043B =="4"]
panel_clean[,WATERWAY := SERVICE_UND_042B =="5"
            |SERVICE_UND_042B =="6"
            |SERVICE_UND_042B =="7"
            |SERVICE_UND_042B =="8"
            |SERVICE_UND_042B =="9"]

## Express racial demographic variables in percent and above national average
panel_clean[,WHITE_ALONE_PERCENT_2000 := 
              WHITE_ALONE.2000.dec/POPULATION.2000.dec*100]
panel_clean[,BLACK_AFAM_PERCENT_2000 := 
              BLACK_AFAM.2000.dec/POPULATION.2000.dec*100]
panel_clean[,HISP_LATINO_PERCENT_2000 := 
              HISP_LATINO.2000.dec/POPULATION.2000.dec*100]

panel_clean[,WHITE_ALONE_PERCENT_2010 := 
              WHITE_ALONE.2010.dec/POPULATION.2010.dec*100]
panel_clean[,BLACK_AFAM_PERCENT_2010 := 
              BLACK_AFAM.2010.dec/POPULATION.2010.dec*100]
panel_clean[,HISP_LATINO_PERCENT_2010 := 
              HISP_LATINO.2010.dec/POPULATION.2010.dec*100]

panel_clean[,WHITE_ALONE_PERCENT_2020 := 
              WHITE_ALONE.2020.dec/POPULATION.2020.dec*100]
panel_clean[,BLACK_AFAM_PERCENT_2020 := 
              BLACK_AFAM.2020.dec/POPULATION.2020.dec*100]
panel_clean[,HISP_LATINO_PERCENT_2020 := 
              HISP_LATINO.2020.dec/POPULATION.2020.dec*100]

panel_clean[,MINORITY_ABOVE_NAT_AVG_2000 := 
              WHITE_ALONE_PERCENT_2000 < race_nat_avg[DECENNIAL==2000,
                                                      AVG_WHITE_ALONE]]
panel_clean[,MINORITY_ABOVE_NAT_AVG_2010 := 
              WHITE_ALONE_PERCENT_2010 < race_nat_avg[DECENNIAL==2010,
                                                      AVG_WHITE_ALONE]]
panel_clean[,MINORITY_ABOVE_NAT_AVG_2020 := 
              WHITE_ALONE_PERCENT_2020 < race_nat_avg[DECENNIAL==2020,
                                                      AVG_WHITE_ALONE]]
panel_clean[,BLACK_AFAM_ABOVE_NAT_AVG_2000 := 
              BLACK_AFAM_PERCENT_2000 > race_nat_avg[DECENNIAL==2000,
                                                     AVG_BLACK_AFAM]]
panel_clean[,BLACK_AFAM_ABOVE_NAT_AVG_2010 := 
              BLACK_AFAM_PERCENT_2010 > race_nat_avg[DECENNIAL==2010,
                                                     AVG_BLACK_AFAM]]
panel_clean[,BLACK_AFAM_ABOVE_NAT_AVG_2020 := 
              BLACK_AFAM_PERCENT_2020 > race_nat_avg[DECENNIAL==2020,
                                                     AVG_BLACK_AFAM]]
panel_clean[,HISP_LATINO_ABOVE_NAT_AVG_2000 := 
              HISP_LATINO_PERCENT_2000 > race_nat_avg[DECENNIAL==2000,
                                                      AVG_HISP_LATINO]]
panel_clean[,HISP_LATINO_ABOVE_NAT_AVG_2010 := 
              HISP_LATINO_PERCENT_2010 > race_nat_avg[DECENNIAL==2010,
                                                      AVG_HISP_LATINO]]
panel_clean[,HISP_LATINO_ABOVE_NAT_AVG_2020 := 
              HISP_LATINO_PERCENT_2020 > race_nat_avg[DECENNIAL==2020,
                                                      AVG_HISP_LATINO]]

# Over 50% in each category

panel_clean[,MINORITY_ABOVE_50_PERCENT_2000 :=
              WHITE_ALONE_PERCENT_2000 <= 50]
panel_clean[,MINORITY_ABOVE_50_PERCENT_2010 :=
              WHITE_ALONE_PERCENT_2010 <= 50]
panel_clean[,MINORITY_ABOVE_50_PERCENT_2020 :=
              WHITE_ALONE_PERCENT_2020 <= 50]

panel_clean[,BLACK_AFAM_ABOVE_50_PERCENT_2000 :=
              BLACK_AFAM_PERCENT_2000 >50]
panel_clean[,BLACK_AFAM_ABOVE_50_PERCENT_2010 :=
              BLACK_AFAM_PERCENT_2010 >50]
panel_clean[,BLACK_AFAM_ABOVE_50_PERCENT_2020 :=
              BLACK_AFAM_PERCENT_2020 >50]

panel_clean[,HISP_LATINO_ABOVE_50_PERCENT_2000 :=
              HISP_LATINO_PERCENT_2000 > 50]
panel_clean[,HISP_LATINO_ABOVE_50_PERCENT_2010 :=
              HISP_LATINO_PERCENT_2010 > 50]
panel_clean[,HISP_LATINO_ABOVE_50_PERCENT_2020 :=
              HISP_LATINO_PERCENT_2020 > 50]

# Over 60% in each category

panel_clean[,MINORITY_ABOVE_60_PERCENT_2000 :=
              WHITE_ALONE_PERCENT_2000 <= 40]
panel_clean[,MINORITY_ABOVE_60_PERCENT_2010 :=
              WHITE_ALONE_PERCENT_2010 <= 40]
panel_clean[,MINORITY_ABOVE_60_PERCENT_2020 :=
              WHITE_ALONE_PERCENT_2020 <= 40]

panel_clean[,BLACK_AFAM_ABOVE_60_PERCENT_2000 :=
              BLACK_AFAM_PERCENT_2000 >60]
panel_clean[,BLACK_AFAM_ABOVE_60_PERCENT_2010 :=
              BLACK_AFAM_PERCENT_2010 >60]
panel_clean[,BLACK_AFAM_ABOVE_60_PERCENT_2020 :=
              BLACK_AFAM_PERCENT_2020 >60]

panel_clean[,HISP_LATINO_ABOVE_60_PERCENT_2000 :=
              HISP_LATINO_PERCENT_2000 > 60]
panel_clean[,HISP_LATINO_ABOVE_60_PERCENT_2010 :=
              HISP_LATINO_PERCENT_2010 > 60]
panel_clean[,HISP_LATINO_ABOVE_60_PERCENT_2020 :=
              HISP_LATINO_PERCENT_2020 > 60]


## Climate categorical variables
### 64 deg F based on Köppen-Geiger Climate Subdivision break point
panel_clean[,WARM_REGION := ANNUAL.TAVG..DEG.F.>64] 
### Freeze-thaw cycles based on Saeed et. al. (2017)
panel_clean[,HIGH_FREEZE_THAW := FREEZE.THAW.CYCLE>60]
quantile(panel_clean$ANNUAL.PRCP..INCH.,0.75,na.rm=TRUE) # Q3 = 49.24 inches
panel_clean[,HIGH_PRECIP := ANNUAL.PRCP..INCH.> 50] # Round up to 50 inches

## Census Regions for subset regressions
panel_clean[STATE_CODE_001 %in% c("9","23","25","33","44","50","34","36","42"), 
            REGION:='Northeast']
panel_clean[STATE_CODE_001 %in% c("18","17","26","39","55","19","20","27","29",
                                  "31","38","46"), REGION:='Midwest']
panel_clean[STATE_CODE_001 %in% c("10","11","12","13","24","37","45","51","54",
                                  "1","21","28","47","5","22","40","48"), 
            REGION:='South']
panel_clean[STATE_CODE_001 %in% c("4","8","16","35","30","49","32","56","2",
                                  "6","15","41","53"), REGION:='West']


# Write final panel with all 244 variables and a version with only 54 variables

fwrite(panel_clean,
       "NBI_Census_Climate_J40_Panel_min3_100yrs_final_avg_50_60.csv")
#panel_clean <- fread("NBI_Census_Climate_J40_Panel_min3_100yrs_final_avg_50_60.csv")


fwrite(panel_clean[,c("BRIDGE_KEY",
                      "INSPECT_YR",
                      "BRIDGE_COND",
                      "DECK_COND_058",
                      "SUPERSTRUCTURE_COND_059",
                      "SUBSTRUCTURE_COND_060",
                      "DECK_COND_STATE",
                      "SUPERSTRUCTURE_COND_STATE",
                      "SUBSTRUCTURE_COND_STATE",
                      "STATE_CODE_001",
                      "REGION",
                      "AGE",
                      "URBAN",
                      "INTERSTATE",
                      "ADT_029",
                      "PERCENT_ADT_TRUCK_109",
                      "DETOUR_KILOS_019",
                      "DECK_PROTECTION",
                      "STEEL_STRUCT",
                      "WATERWAY",
                      "WARM_REGION",
                      "HIGH_FREEZE_THAW", 
                      "HIGH_PRECIP",
                      "MINORITY_ABOVE_NAT_AVG_2020",
                      "MINORITY_ABOVE_NAT_AVG_2010",
                      "MINORITY_ABOVE_NAT_AVG_2000",
                      "MINORITY_ABOVE_50_PERCENT_2020",
                      "MINORITY_ABOVE_50_PERCENT_2010",
                      "MINORITY_ABOVE_50_PERCENT_2000",
                      "MINORITY_ABOVE_60_PERCENT_2020",
                      "MINORITY_ABOVE_60_PERCENT_2010",
                      "MINORITY_ABOVE_60_PERCENT_2000",
                      "BLACK_AFAM_ABOVE_NAT_AVG_2020",
                      "BLACK_AFAM_ABOVE_NAT_AVG_2010",
                      "BLACK_AFAM_ABOVE_NAT_AVG_2000",
                      "BLACK_AFAM_ABOVE_50_PERCENT_2020",
                      "BLACK_AFAM_ABOVE_50_PERCENT_2010",
                      "BLACK_AFAM_ABOVE_50_PERCENT_2000",
                      "BLACK_AFAM_ABOVE_60_PERCENT_2020",
                      "BLACK_AFAM_ABOVE_60_PERCENT_2010",
                      "BLACK_AFAM_ABOVE_60_PERCENT_2000",
                      "HISP_LATINO_ABOVE_NAT_AVG_2020",
                      "HISP_LATINO_ABOVE_NAT_AVG_2010",
                      "HISP_LATINO_ABOVE_NAT_AVG_2000",
                      "HISP_LATINO_ABOVE_50_PERCENT_2020",
                      "HISP_LATINO_ABOVE_50_PERCENT_2010",
                      "HISP_LATINO_ABOVE_50_PERCENT_2000",
                      "HISP_LATINO_ABOVE_60_PERCENT_2020",
                      "HISP_LATINO_ABOVE_60_PERCENT_2010",
                      "HISP_LATINO_ABOVE_60_PERCENT_2000",
                      "MED_INCOME.2020.acs",
                      "MED_INCOME.2010.acs",
                      "MED_INCOME.2000.dec",
                      "DISADVANTAGED")],
       "NBI_Census_Climate_J40_Panel_min3_100yrs_small_avg_50_60.csv")

