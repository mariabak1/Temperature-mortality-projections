###################################################################
## This script loads and prepares mortality and temperature data ##
## before they are used in temperature-mortality projections ######
###################################################################

library(dplyr)
library(data.table)


# 1. read in mortality data #####
load("small_all cause p10p75p90 lag21.Rdata")

deathspercity = as.data.frame(do.call(rbind, DEATHDOY.AGE)) # average number of deaths per day for every city
deathspercity = deathspercity %>%
  mutate(`deathdoy.0-49` = `deathdoy.1-4`+deathdoy.Infant+`deathdoy.5-19`+`deathdoy.20-34`+`deathdoy.35-49`)%>%
  dplyr::select(nsalid1, doy, `deathdoy.0-49`, `deathdoy.50-64`, `deathdoy.65+`)

length(unique(deathspercity$nsalid1)) # 326 cities
length(unique(deathspercity$doy)) # 365 days

DEATHDOY.AGE2 = split(deathspercity, deathspercity$nsalid1)


# 2. load city names and country ids for convenience ####
bec=read.csv("BEC_Dataset_v24_20210824.csv")%>%
  filter(SALID1 %in% deathspercity$nsalid1)%>%
  mutate(nsalid1=SALID1)%>%
  dplyr::select(nsalid1, L1Name, Country)%>%
  arrange(nsalid1)%>%
  mutate(id=1:nrow(.)) # need this column for later
bec$Country[bec$Country=='Brasil']='Brazil' 
table(bec$Country)


# 3. load the UN-derived population weights ####
brw=read.csv("UN_weights.csv")
brw=merge(brw, bec, by="Country")
brw=split(brw, brw$nsalid1)


# 4. read in projected temperature data
rcp2p6 = read.csv("futuretemp_RCP26.csv") 
rcp2p6=rcp2p6[order(rcp2p6$nsalid1),] # it's important to order by city id

# limit to relevant columns
rcp2p6 = rcp2p6 %>%
  mutate(date=as.Date(date),
         md=format(date,"%m-%d"),
         tmean2p6=tmeanf)%>%
  dplyr::select(nsalid1, date, tmean2p6)

length(unique(rcp2p6$nsalid1)) # double-check # of cities: 326 

rcp2p6=split(rcp2p6, rcp2p6$nsalid1) # create a list


# rcp8p5
rcp8p5 = read.csv("futuretemp_RCP85.csv") 
rcp8p5=rcp8p5[order(rcp8p5$nsalid1),]

# limit to relevant columns
rcp8p5 = rcp8p5 %>%
  mutate(date=as.Date(date),
         md=format(date,"%m-%d"),
         tmean8p5=tmeanf)%>%
  dplyr::select(nsalid1, date, tmean8p5)

length(unique(rcp8p5$nsalid1)) # 326 cities

rcp8p5=split(rcp8p5, rcp8p5$nsalid1) # transform into a list

  
# 5. create projected mortality series for the future time period (equal to the length of period in MS85)
DEATHPROJ = list()
for (c in  1:length(unique(deathspercity$nsalid1))){
  #c=1
  w=brw[[c]]
  w=w$brw[w$a == '50-64']
    DEATHPROJ[[c]] = rep(DEATHDOY.AGE2[[c]]$`deathdoy.50-64`* w, length = length(rcp2p6[[c]]$date)) 
}


# 6. create projected mortality series equal to each country's time period of mortality data ('historical period')
# also need temperature for the 'historical' (study period in MS85), so handle temperature first

dailytempcity = as.data.frame(do.call(rbind, dailytemp))
dailytempcity = dailytempcity %>%
  group_by(nsalid1)%>%
  mutate(md=format(date,"%m-%d"))%>%
  filter(!md == '02-29')%>%
  mutate(range = paste(min(substr(date, 1, 4)), max(substr(date, 1, 4)), sep = "-" ),
         start.year = as.numeric(min(substr(date, 1, 4))),
         end.year = as.numeric(max(substr(date, 1, 4))),
         year = (substr(date, 1, 4)),
         ndates = length(unique(date)), 
         correctndates = 365*(end.year - start.year + 1 ))

# get the range of mortality years for every city (need it in this format to link to the temp data)
cityrange = dailytempcity %>%
  dplyr::select(nsalid1, range, start.year, end.year)%>%
  group_by(nsalid1) %>%
  filter(!duplicated(nsalid1))

# load a complete temperature dataset, remove leap dates
citytemp = fread("L1AD_UX_96_15.csv")
citytemp = citytemp %>%
  mutate(year = substr(date, 1,4),
         nsalid1 = SALID1,
         date=as.Date(date),
         md=format(date,"%m-%d"),
         tmean = ADtemp_pw)%>%
        filter(!md == '02-29')%>%
  dplyr::select(nsalid1, date, year, tmean)%>%
  left_join(., cityrange, by="nsalid1")

citytemp = citytemp[citytemp$nsalid1 %in% dailytempcity$nsalid1] # limit to cities with mortality data
length(unique(citytemp$nsalid1))

citytemp = subset(citytemp, year >= citytemp$start.year & year <= citytemp$end.year) # limit temperature data to study period in MS85
citytemp = citytemp[order(citytemp$nsalid1),]
dailytempcity = split(citytemp, citytemp$nsalid1) # put into a list


# finally, create a mortality series equal in length to the time period in MS85 for each city
DEATHPROJ_HIST = list()
for (c in  1:length(unique(deathspercity$nsalid1))){
  #c=1
  DEATHPROJ_HIST[[c]] = rep(DEATHDOY.AGE2[[c]]$`deathdoy.50-64`, length = 365*length(seq(dailytempcity[[c]]$start.year[1], dailytempcity[[c]]$end.year[1]))) 
}
