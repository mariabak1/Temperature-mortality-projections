# Add up results from each simulation and compute total AFs combined for all ages

library(dplyr)
library(data.table)


an049=fread("age_specific/ansim049.csv")%>%
  mutate(ag="049")
an5064=fread("age_specific/ansim5064.csv")%>%
  mutate(ag="5064")
an65=fread("age_specific/ansim65+.csv")%>%
  mutate(ag="65")

df049=read.csv("age_specific/df049.csv")
df5064=read.csv("age_specific/df5064.csv")
df65=read.csv("age_specific/df65.csv")
df=rbind(df049, df5064, df65)%>%
  dplyr::select(-id)

df=df%>%
  group_by(nsalid1)%>%
  summarise(nd=sum(ndf))


dh049=read.csv("age_specific/dh049.csv")
dh5064=read.csv("age_specific/dh5064.csv")
dh65=read.csv("age_specific/dh65.csv")
dh=rbind(dh049, dh5064, dh65)%>%
  dplyr::select(-id)

dh=dh%>%
  group_by(nsalid1)%>%
  summarise(nd=sum(ndh))


an=rbind(an049, an5064, an65)%>%
  dplyr::select(-Country, -id, -L1Name, -ag)

result <- an %>%
  group_by(nsalid1, period) %>%
  summarise(across(everything(), .fns = ~ sum(., na.rm = TRUE)))

vars = c("tot.abs.1.RCP2.6.sim", "cold.abs.1.RCP2.6.sim", "heat.abs.1.RCP2.6.sim", "excold.abs.1.RCP2.6.sim", "exheat.abs.1.RCP2.6.sim",
         "tot.rel.1.RCP2.6.sim", "cold.rel.1.RCP2.6.sim", "heat.rel.1.RCP2.6.sim", "excold.rel.1.RCP2.6.sim", "exheat.rel.1.RCP2.6.sim",
         "tot.abs.1.RCP8.5.sim", "cold.abs.1.RCP8.5.sim", "heat.abs.1.RCP8.5.sim", "excold.abs.1.RCP8.5.sim", "exheat.abs.1.RCP8.5.sim",
         "tot.rel.1.RCP8.5.sim", "cold.rel.1.RCP8.5.sim", "heat.rel.1.RCP8.5.sim", "excold.rel.1.RCP8.5.sim", "exheat.rel.1.RCP8.5.sim")


datr=list()
#i="tot.abs.1.RCP2.6.sim"
for (i in vars){
  
  datr[[i]] = result %>%
    dplyr::select(starts_with(i))
  nc1 = paste("ci.l.", substring(i, 1, nchar(i) - 4), sep="")
  nc2 = paste("ci.u.", substring(i, 1, nchar(i) - 4), sep="")
  datr[[i]][[nc1]] = apply(datr[[i]], 1, quantile, probs = 0.025, na.rm = TRUE)
  datr[[i]][[nc2]] = apply(datr[[i]], 1, quantile, probs = 0.975, na.rm = TRUE)
  # datr[[i]] = datr[[i]] %>%
  #   dplyr::select(nc1, nc2)
}


dat2 <- do.call(cbind, lapply(datr, as.data.frame))
datci = dat2 %>%
  dplyr::select(matches("ci.l|ci.u"))
datest = result %>%
  dplyr::select(nsalid1, period, contains("est"))
datf=cbind(datest, datci)

datf1=datf%>%filter(period == "2045-2054") # future period
datf1 = merge(datf1, df, by="nsalid1")

datf2=datf%>%filter(!period == "2045-2054") # baseline period
datf2 = merge(datf2, dh, by="nsalid1")

datf = rbind(datf1, datf2)


# grand totals an ####

# total edf, baseline
tot_an_p=sum(datf2$tot.abs.1.RCP2.6.est)

xp=(datf2$tot.abs.1.RCP2.6.sim.ci.u.tot.abs.1.RCP2.6-datf2$tot.abs.1.RCP2.6.sim.ci.l.tot.abs.1.RCP2.6)/2
totci = c(tot_an_p,
          (tot_an_p - (sqrt(sum(xp^2)))),
          (tot_an_p + (sqrt(sum(xp^2)))))
#  979442  937982 1,020,901

# rcp 2.6
tot_an_p=sum(datf1$tot.abs.1.RCP2.6.est)

xp=(datf1$tot.abs.1.RCP2.6.sim.ci.u.tot.abs.1.RCP2.6-datf1$tot.abs.1.RCP2.6.sim.ci.l.tot.abs.1.RCP2.6)/2

totci = c(tot_an_p,
          (tot_an_p - (sqrt(sum(xp^2)))),
          (tot_an_p + (sqrt(sum(xp^2)))))
# 2,123,355 2,008,132 2,238,577

# rcp 8.5
tot_an_p=sum(datf1$tot.abs.1.RCP8.5.est)

xp=(datf1$tot.abs.1.RCP8.5.sim.ci.u.tot.abs.1.RCP8.5-datf1$tot.abs.1.RCP8.5.sim.ci.l.tot.abs.1.RCP8.5)/2
totci = c(tot_an_p,
          (tot_an_p - (sqrt(sum(xp^2)))),
          (tot_an_p + (sqrt(sum(xp^2)))))
# 1,934,273 1,820,301 2,048,246


# grand totals edf ####
# total edf, baseline
tot_an_p=sum(datf2$tot.abs.1.RCP2.6.est)
tot_edf_p=sum(datf2$tot.abs.1.RCP2.6.est)/sum(datf2$nd)*100

xp=(datf2$tot.abs.1.RCP2.6.sim.ci.u.tot.abs.1.RCP2.6-datf2$tot.abs.1.RCP2.6.sim.ci.l.tot.abs.1.RCP2.6)/2
#ci1=tot_edf_p - (sqrt(sum(x^2)))/sum(datf2$nd)*100
#ci2=tot_edf_p + (sqrt(sum(x^2)))/sum(datf2$nd)*100

totci = c(round(tot_edf_p, 2),
          round(tot_edf_p - (sqrt(sum(xp^2)) / sum(datf2$nd)) * 100, 2),
          round(tot_edf_p + (sqrt(sum(xp^2)) / sum(datf2$nd)) * 100, 2))


# total edf, future rcp2.6
tot_edf_f=sum(datf1$tot.abs.1.RCP2.6.est)/sum(datf1$nd)*100
xf=(datf1$tot.abs.1.RCP2.6.sim.ci.u.tot.abs.1.RCP2.6-datf1$tot.abs.1.RCP2.6.sim.ci.l.tot.abs.1.RCP2.6)/2

totcif2.6 = c(round(tot_edf_f, 2),
              round(tot_edf_f - (sqrt(sum(xf^2)) / sum(datf1$nd)) * 100, 2),
              round(tot_edf_f + (sqrt(sum(xf^2)) / sum(datf1$nd)) * 100, 2))


# total edf, future rcp8.5
tot_edf_f=sum(datf1$tot.abs.1.RCP8.5.est)/sum(datf1$nd)*100
xf=(datf1$tot.abs.1.RCP8.5.sim.ci.u.tot.abs.1.RCP8.5-datf1$tot.abs.1.RCP8.5.sim.ci.l.tot.abs.1.RCP8.5)/2

totcif85 = c(round(tot_edf_f, 2),
             round(tot_edf_f - (sqrt(sum(xf^2)) / sum(datf1$nd)) * 100, 2),
             round(tot_edf_f + (sqrt(sum(xf^2)) / sum(datf1$nd)) * 100, 2))




# heat edf, future rcp2.6
heat_edf_f=sum(datf1$heat.abs.1.RCP2.6.est)/sum(datf1$nd)*100
xf=(datf1$heat.abs.1.RCP2.6.sim.ci.u.heat.abs.1.RCP2.6-datf1$heat.abs.1.RCP2.6.sim.ci.l.heat.abs.1.RCP2.6)/2

heatcif = c(round(heat_edf_f, 2),
           round(heat_edf_f - (sqrt(sum(xf^2)) / sum(datf1$nd)) * 100, 2),
           round(heat_edf_f + (sqrt(sum(xf^2)) / sum(datf1$nd)) * 100, 2))



# heat edf, future rcp8.5
heat_edf_f=sum(datf1$heat.abs.1.RCP8.5.est)/sum(datf1$nd)*100
xf=(datf1$heat.abs.1.RCP8.5.sim.ci.u.heat.abs.1.RCP8.5-datf1$heat.abs.1.RCP8.5.sim.ci.l.heat.abs.1.RCP8.5)/2

heatcif85 = c(round(heat_edf_f, 2),
            round(heat_edf_f - (sqrt(sum(xf^2)) / sum(datf1$nd)) * 100, 2),
            round(heat_edf_f + (sqrt(sum(xf^2)) / sum(datf1$nd)) * 100, 2))




# heat edf, baseline
heat_edf_p=sum(datf2$heat.abs.1.RCP2.6.est)/sum(datf2$nd)*100
xp=(datf2$heat.abs.1.RCP2.6.sim.ci.u.heat.abs.1.RCP2.6-datf2$heat.abs.1.RCP2.6.sim.ci.l.heat.abs.1.RCP2.6)/2

heatcip = c(round(heat_edf_p, 2),
            round(heat_edf_p - (sqrt(sum(xp^2)) / sum(datf2$nd)) * 100, 2),
            round(heat_edf_p + (sqrt(sum(xp^2)) / sum(datf2$nd)) * 100, 2))



# cold edf, baseline
cold_edf_p=sum(datf2$cold.abs.1.RCP2.6.est)/sum(datf2$nd)*100
xp=(datf2$cold.abs.1.RCP2.6.sim.ci.u.cold.abs.1.RCP2.6-datf2$cold.abs.1.RCP2.6.sim.ci.l.cold.abs.1.RCP2.6)/2

coldcip = c(round(cold_edf_p, 2),
            round(cold_edf_p - (sqrt(sum(xp^2)) / sum(datf2$nd)) * 100, 2),
            round(cold_edf_p + (sqrt(sum(xp^2)) / sum(datf2$nd)) * 100, 2))


# cold edf, future rcp2.6
cold_edf_f=sum(datf1$cold.abs.1.RCP2.6.est)/sum(datf1$nd)*100
xp=(datf1$cold.abs.1.RCP2.6.sim.ci.u.cold.abs.1.RCP2.6-datf1$cold.abs.1.RCP2.6.sim.ci.l.cold.abs.1.RCP2.6)/2

coldcif = c(round(cold_edf_f, 2),
            round(cold_edf_f - (sqrt(sum(xp^2)) / sum(datf1$nd)) * 100, 2),
            round(cold_edf_f + (sqrt(sum(xp^2)) / sum(datf1$nd)) * 100, 2))


# cold edf, future rcp8.5
cold_edf_f=sum(datf1$cold.abs.1.RCP8.5.est)/sum(datf1$nd)*100
xp=(datf1$cold.abs.1.RCP8.5.sim.ci.u.cold.abs.1.RCP8.5-datf1$cold.abs.1.RCP8.5.sim.ci.l.cold.abs.1.RCP8.5)/2

coldcif85 = c(round(cold_edf_f, 2),
            round(cold_edf_f - (sqrt(sum(xp^2)) / sum(datf1$nd)) * 100, 2),
            round(cold_edf_f + (sqrt(sum(xp^2)) / sum(datf1$nd)) * 100, 2))



# computing delta between baseline and future #####
# compute based on the differences in ANs
dif=sum(datf1$tot.abs.1.RCP2.6.est)/sum(datf1$nd)*100-sum(datf2$tot.abs.1.RCP2.6.est)/sum(datf2$nd)*100
xp=((datf2$tot.abs.1.RCP2.6.sim.ci.u.tot.abs.1.RCP2.6-datf2$tot.abs.1.RCP2.6.sim.ci.l.tot.abs.1.RCP2.6)/2)/sum(datf2$nd)*100
xf=((datf1$tot.abs.1.RCP2.6.sim.ci.u.tot.abs.1.RCP2.6-datf1$tot.abs.1.RCP2.6.sim.ci.l.tot.abs.1.RCP2.6)/2)/sum(datf1$nd)*100

xc = sqrt(sum(xp^2)+sum(xf^2))
deltatot = c(round(dif, 2),
             round(dif - xc, 2),
             round(dif + xc, 2))

# total for rcp85
dif=sum(datf1$tot.abs.1.RCP8.5.est)/sum(datf1$nd)*100-sum(datf2$tot.abs.1.RCP8.5.est)/sum(datf2$nd)*100
xp=((datf2$tot.abs.1.RCP8.5.sim.ci.u.tot.abs.1.RCP8.5-datf2$tot.abs.1.RCP8.5.sim.ci.l.tot.abs.1.RCP8.5)/2)/sum(datf2$nd)*100
xf=((datf1$tot.abs.1.RCP8.5.sim.ci.u.tot.abs.1.RCP8.5-datf1$tot.abs.1.RCP8.5.sim.ci.l.tot.abs.1.RCP8.5)/2)/sum(datf1$nd)*100

xc = sqrt(sum(xp^2)+sum(xf^2))
deltatot85 = c(round(dif, 2),
               round(dif - xc, 2),
               round(dif + xc, 2))


# compute based on the differences in ANs for heat
dif=sum(datf1$heat.abs.1.RCP2.6.est)/sum(datf1$nd)*100-sum(datf2$heat.abs.1.RCP2.6.est)/sum(datf2$nd)*100
xp=((datf2$heat.abs.1.RCP2.6.sim.ci.u.heat.abs.1.RCP2.6-datf2$heat.abs.1.RCP2.6.sim.ci.l.heat.abs.1.RCP2.6)/2)/sum(datf2$nd)*100
xf=((datf1$heat.abs.1.RCP2.6.sim.ci.u.heat.abs.1.RCP2.6-datf1$heat.abs.1.RCP2.6.sim.ci.l.heat.abs.1.RCP2.6)/2)/sum(datf1$nd)*100

xc = sqrt(sum(xp^2)+sum(xf^2))
deltaheat = c(round(dif, 2),
              round(dif - xc, 2),
              round(dif + xc, 2))

# total for rcp85
dif=sum(datf1$heat.abs.1.RCP8.5.est)/sum(datf1$nd)*100-sum(datf2$heat.abs.1.RCP8.5.est)/sum(datf2$nd)*100
xp=((datf2$heat.abs.1.RCP8.5.sim.ci.u.heat.abs.1.RCP8.5-datf2$heat.abs.1.RCP8.5.sim.ci.l.heat.abs.1.RCP8.5)/2)/sum(datf2$nd)*100
xf=((datf1$heat.abs.1.RCP8.5.sim.ci.u.heat.abs.1.RCP8.5-datf1$heat.abs.1.RCP8.5.sim.ci.l.heat.abs.1.RCP8.5)/2)/sum(datf1$nd)*100

xc = sqrt(sum(xp^2)+sum(xf^2))
deltaheat85 = c(round(dif, 2),
                round(dif - xc, 2),
                round(dif + xc, 2))




# compute based on the differences in ANs for cold
dif=sum(datf1$cold.abs.1.RCP2.6.est)/sum(datf1$nd)*100-sum(datf2$cold.abs.1.RCP2.6.est)/sum(datf2$nd)*100
xp=((datf2$cold.abs.1.RCP2.6.sim.ci.u.cold.abs.1.RCP2.6-datf2$cold.abs.1.RCP2.6.sim.ci.l.cold.abs.1.RCP2.6)/2)/sum(datf2$nd)*100 # Margin of error
xf=((datf1$cold.abs.1.RCP2.6.sim.ci.u.cold.abs.1.RCP2.6-datf1$cold.abs.1.RCP2.6.sim.ci.l.cold.abs.1.RCP2.6)/2)/sum(datf1$nd)*100

xc = sqrt(sum(xp^2)+sum(xf^2))
deltacold = c(round(dif, 2),
              round(dif - xc, 2),
              round(dif + xc, 2))

# total for rcp85
dif=sum(datf1$cold.abs.1.RCP8.5.est)/sum(datf1$nd)*100-sum(datf2$cold.abs.1.RCP8.5.est)/sum(datf2$nd)*100
xp=((datf2$cold.abs.1.RCP8.5.sim.ci.u.cold.abs.1.RCP8.5-datf2$cold.abs.1.RCP8.5.sim.ci.l.cold.abs.1.RCP8.5)/2)/sum(datf2$nd)*100
xf=((datf1$cold.abs.1.RCP8.5.sim.ci.u.cold.abs.1.RCP8.5-datf1$cold.abs.1.RCP8.5.sim.ci.l.cold.abs.1.RCP8.5)/2)/sum(datf1$nd)*100

xc = sqrt(sum(xp^2)+sum(xf^2))
deltacold85 = c(round(dif, 2),
                round(dif - xc, 2),
                round(dif + xc, 2))






library(ggplot2)


# create matrix with 4 columns and 4 rows
data= matrix(c(totci[1], totci[2],totci[3], "total", "Baseline",
               totcif2.6[1], totcif2.6[2],totcif2.6[3], "total", "RCP2.6",
               totcif85[1], totcif85[2], totcif85[3], "total", "RCP8.5",
               heatcip[1], heatcip[2],heatcip[3], "heat", "Baseline",
               coldcip[1],coldcip[2],coldcip[3], "cold",  "Baseline",
               heatcif[1],heatcif[2],heatcif[3], "heat", "RCP2.6",
               heatcif85[1], heatcif85[2], heatcif85[3], "heat", "RCP8.5",
               coldcif[1], coldcif[2], coldcif[3], "cold", "RCP2.6",
               coldcif85[1], coldcif85[2], coldcif85[3], "cold", "RCP8.5",
               deltatot[1], deltatot[2], deltatot[3], "dif", "totdifrcp26",
               deltatot85[1], deltatot85[2], deltatot85[3],"dif", "totdifrcp85",
               deltaheat[1], deltaheat[2], deltaheat[3], "dif", "heatdifrcp26",
               deltaheat85[1], deltaheat85[2], deltaheat85[3], "dif",  "heatdifrcp85",
               deltacold[1], deltacold[2], deltacold[3],"dif",  "colddifrcp26",
               deltacold85[1], deltacold85[2], deltacold85[3], "dif", "colddifrcp85"), ncol=5, byrow=TRUE)

# specify the column names and row names of matrix
colnames(data) = c('est','cil','ciu','type', "scenario")
#rownames(data) <- c('Baseline','Baseline','RCP2.6', "RCP8.5", "RCP2.6", "RCP8.5")

# assign to table
final=as.data.frame(data)%>%
  mutate(country="All countries")

ggplot(data=final[final$type=="heat",], aes(fill = type, x=scenario, y=est)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar( aes(x=scenario, ymin=cil, ymax=ciu), width=0.4, colour="orange", alpha=0.9, size=1.5)


write.csv(final, "age_specific/dataforplots.csv", row.names = F)









bec=read.csv("BEC_Dataset_v24_20210824.csv")%>%
  mutate(nsalid1=SALID1)%>%
  dplyr::select(nsalid1, L1Name, Country)%>%
  arrange(nsalid1)
bec$Country[bec$Country=='Brasil']='Brazil' 


datf=merge(datf, bec[,c("nsalid1", "L1Name", "Country")], by="nsalid1")
datf = datf %>%
  dplyr::select(1:2, 184, 185, 64:183 )
  
  
check = datf %>%
  filter(nsalid1 == "102190")


write.csv(datf, "age_specific/total_af_an_from_age_groups.csv", row.names = F)

write.csv(an, "age_specific/an_all.csv", row.names = F)



# compute anrel and afrel by city #####
anrelafrel = read.csv("age_specific/total_af_an_from_age_groups.csv")
anrelafrel=  anrelafrel[order(anrelafrel$nsalid1, anrelafrel$period), ]
df=df %>%
  mutate(period="2045-2054")%>%
  dplyr::select(nsalid1, period, nd)

datest2=datest %>%
  filter(period != "2045-2054")%>%
  dplyr::select(nsalid1, period)%>%
  left_join(., dh, by="nsalid1")

datest2$id=(1:length(unique(datest2$nsalid1)))

d=rbind(datest2, df)

anrelafrel=merge(anrelafrel, datest2, by=c("nsalid1", "period"), all.x=T)
anrelafrel=  anrelafrel[order(anrelafrel$nsalid1, anrelafrel$id), ]

afrel = anrelafrel%>% 
  group_by(nsalid1) %>% 
  mutate(afreltot85 =af.tot.RCP8.5  - af.tot.RCP8.5[1],
         moe85tot = sqrt(((ci.u.af.tot.RCP8.5-ci.l.af.tot.RCP8.5)/2)^2),
         cireltot85 = paste(round(afreltot85 -  moe85tot, 2), round(afreltot85 +  moe85tot, 2), sep = "; " ),
         
         afrelheat85 =af.heat.RCP8.5  - af.heat.RCP8.5[1],
         moe85heat = sqrt(((ci.u.af.heat.RCP8.5-ci.l.af.heat.RCP8.5)/2)^2),
         cirelheat85 = paste(round(afrelheat85 -  moe85heat, 2), round(afrelheat85 +  moe85heat, 2), sep = "; " ),
         
         
         afrelcold85 =af.cold.RCP8.5 - af.cold.RCP8.5[1],
         moe85cold = sqrt(((ci.u.af.cold.RCP8.5-ci.l.af.cold.RCP8.5)/2)^2),
         cirelcold85 = paste(round(afrelcold85 -  moe85cold, 2), round(afrelcold85 +  moe85cold, 2), sep = "; " ))
         

write.csv(afrel, "afrel_anrel_from_age_groups.csv", row.names = F)





