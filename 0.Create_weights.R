# This code creates weights for computing daily mortality based on the changing population dynamics

library(dplyr)

bec=read.csv("BEC_Dataset_v24_20210824.csv")%>%
  mutate(nsalid1=SALID1)%>%
  dplyr::select(nsalid1, L1Name, Country)%>%
  arrange(nsalid1)%>%
  mutate(id=1:nrow(.)) # need this column for later

citytemp = read.csv("city_temp_mort_periods.csv")
citytemp = citytemp%>%
  dplyr::left_join(. , bec[, c("nsalid1", "Country")], by = "nsalid1")
citytemp = citytemp[!duplicated(citytemp$Country),]
citytemp$Country[citytemp$Country=='Brasil']='Brazil' 

  
brest=read.csv("all_countries_baseline_lifetable.csv")%>% 
  left_join(., citytemp, by="Country")
brest = subset(brest, Year >= brest$start.year & Year <= brest$end.year) # limit temperature data to study period in MS85
check = brest[!duplicated(brest$Country),]

  
brprj=read.csv("all_countries_midcentury_lifetable.csv")
length(unique(brest$Country))
length(unique(brprj$Country))

brest$a[brest$Age..x.==0]='0-49'
brest$a[brest$Age..x.>=1 & brest$Age..x. <5]='0-49'
brest$a[brest$Age..x.>=5 & brest$Age..x. <=19]='0-49'
brest$a[brest$Age..x.>=20 & brest$Age..x. <=34]='0-49'
brest$a[brest$Age..x.>=35 & brest$Age..x. <=49]='0-49'
brest$a[brest$Age..x.>=50 & brest$Age..x. <=64]='50-64'
brest$a[brest$Age..x.>=65]='65+'


brprj$a[brprj$Age..x.==0]='0-49'
brprj$a[brprj$Age..x.>=1 & brprj$Age..x. <5]='0-49'
brprj$a[brprj$Age..x.>=5 & brprj$Age..x. <=19]='0-49'
brprj$a[brprj$Age..x.>=20 & brprj$Age..x. <=34]='0-49'
brprj$a[brprj$Age..x.>=35 & brprj$Age..x. <=49]='0-49'
brprj$a[brprj$Age..x.>=50 & brprj$Age..x. <=64]='50-64'
brprj$a[brprj$Age..x.>=65]='65+'

dxcurrent = brest %>%
  group_by(a, Country)%>%
  summarise(dxc = mean(Probability.of.dying.q.x.n.))


dxfuture = brprj %>%
  group_by(a, Country)%>%
  summarise(dxf = mean(Probability.of.dying.q.x.n.))


bresttot=read.csv("all_countries_total_baseline.csv")
bresttot=bresttot %>% left_join(., citytemp, by="Country")
bresttot = subset(bresttot, Year >= bresttot$start.year & Year <= bresttot$end.year) # limit temperature data to study period in MS85
checktot = bresttot[!duplicated(bresttot$Country),]


bresttot2 = reshape(data = bresttot,
        idvar = c("Year", "Country"),
        varying = 10:30, #specify the columns to be reshaped
        sep= "",
        timevar= "a2",
        direction = "long")%>%
  dplyr::select(Year, a2, X, Country)


# Infant, 1-4, 5-19, 20-34, 35-49; 50-64, 65+
bresttot2$a[bresttot2$a2==0.40]='0-49'
bresttot2$a[bresttot2$a2==5.90]='0-49'
bresttot2$a[bresttot2$a2==10.14]='0-49'
bresttot2$a[bresttot2$a2==15.19]='0-49'
bresttot2$a[bresttot2$a2==20.24]='0-49'
bresttot2$a[bresttot2$a2==25.29]='0-49'
bresttot2$a[bresttot2$a2==30.34]='0-49'
bresttot2$a[bresttot2$a2==35.39]='0-49'
bresttot2$a[bresttot2$a2==40.44]='0-49'
bresttot2$a[bresttot2$a2==45.49]='0-49'
bresttot2$a[bresttot2$a2==50.54]='50-64'
bresttot2$a[bresttot2$a2==55.59]='50-64'
bresttot2$a[bresttot2$a2==60.64]='50-64'
bresttot2$a[bresttot2$a2==65.69]='65+'
bresttot2$a[bresttot2$a2==70.74]='65+'
bresttot2$a[bresttot2$a2==75.79]='65+'
bresttot2$a[bresttot2$a2==80.84]='65+'
bresttot2$a[bresttot2$a2==85.89]='65+'
bresttot2$a[bresttot2$a2==90.94]='65+'
bresttot2$a[bresttot2$a2==95.99]='65+'
bresttot2$a[bresttot2$a2==100.00]='65+'


brtotpop=bresttot2 %>%
  mutate(X = 1000*as.numeric(gsub(" ", "", X)))

brtotpop=brtotpop %>%
  group_by(Country) %>%
  mutate(nyears = length(unique(Year)),
            sumpop=sum(X), 
         totpop = sumpop/nyears)
  
dat1=brtotpop%>%
  dplyr::group_by(a, Country)%>%
  dplyr::summarise(a_totpop=sum(X)/nyears,
            a_pop_percent = a_totpop/totpop)%>%
  distinct()

totalpop=brtotpop[!duplicated(brtotpop$Country),]

brprj=read.csv("all_countries_total_midcentury.csv")

brprj2 = reshape(data = brprj,
                    idvar= c("Year", "Country"),
                    varying = 10:30, #We need to specify here the columns to be reshaped
                    sep= "",
                    timevar= "a2",
                    direction = "long")%>%
  dplyr::select(Year, a2, X, Country)



# Infant, 1-4, 5-19, 20-34, 35-49; 50-64, 65+
brprj2$a[brprj2$a2==0.40]='0-49'
brprj2$a[brprj2$a2==5.90]='0-49'
brprj2$a[brprj2$a2==10.14]='0-49'
brprj2$a[brprj2$a2==15.19]='0-49'
brprj2$a[brprj2$a2==20.24]='0-49'
brprj2$a[brprj2$a2==25.29]='0-49'
brprj2$a[brprj2$a2==30.34]='0-49'
brprj2$a[brprj2$a2==35.39]='0-49'
brprj2$a[brprj2$a2==40.44]='0-49'
brprj2$a[brprj2$a2==45.49]='0-49'
brprj2$a[brprj2$a2==50.54]='50-64'
brprj2$a[brprj2$a2==55.59]='50-64'
brprj2$a[brprj2$a2==60.64]='50-64'
brprj2$a[brprj2$a2==65.69]='65+'
brprj2$a[brprj2$a2==70.74]='65+'
brprj2$a[brprj2$a2==75.79]='65+'
brprj2$a[brprj2$a2==80.84]='65+'
brprj2$a[brprj2$a2==85.89]='65+'
brprj2$a[brprj2$a2==90.94]='65+'
brprj2$a[brprj2$a2==95.99]='65+'
brprj2$a[brprj2$a2==100.00]='65+'


brtotpopf=brprj2 %>%
  mutate(X = 1000*as.numeric(gsub(" ", "", X)))

brtotpopf=brtotpopf %>%
  group_by(Country) %>%
  mutate(nyears = length(unique(Year)),
         sumpop=sum(X), 
         totpopf = sumpop/nyears)

dat2=brtotpopf%>%
  dplyr::group_by(a, Country)%>%
  dplyr::summarise(a_totpopf=sum(X)/nyears,
                   a_pop_percentf = a_totpopf/totpopf)%>%
  distinct()

totalpopf=brtotpopf[!duplicated(brtotpopf$Country),]

brw = left_join(dat1, dat2, by=c("a", "Country"))%>%
  left_join(., dxcurrent, by=c("a", "Country"))%>%
  left_join(., dxfuture, by=c("a", "Country"))%>%
  left_join(., totalpop[, c("Country", "totpop")], by="Country")%>%
  left_join(., totalpopf[, c("Country", "totpopf")], by="Country")%>%
  mutate(brw=(totpopf/totpop) * (a_pop_percentf/a_pop_percent) * (dxf/dxc) )

write.csv(brw, "UN_weights.csv", row.names = F)

rm(list=setdiff(ls(), "brw"))
