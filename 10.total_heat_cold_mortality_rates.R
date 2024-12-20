# Add up results from each simulation and compute total heat and cold mortality rates for all ages


an049=fread("age_specific/ansim049.csv")%>%
  mutate(ag="049")
an5064=fread("age_specific/ansim5064.csv")%>%
  mutate(ag="5064")
an65=fread("age_specific/ansim65+.csv")%>%
  mutate(ag="65")

df = read.csv("baseline_and_prj_city_pops.csv")

dh = df%>%
  mutate(nd = totpop)%>%
  select(nsalid1, nd)

df = df%>%
  mutate(nd = citytotpopf)%>%
  select(nsalid1, nd)


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

datf1=datf%>%filter(period == "2045-2054")
datf1 = merge(datf1, df, by="nsalid1")

datf2=datf%>%filter(!period == "2045-2054")
datf2 = merge(datf2, dh, by="nsalid1")

datf = rbind(datf1, datf2)

datf = datf %>%
  mutate( # AFs
    af.tot.RCP2.6 = tot.abs.1.RCP2.6.est/nd*1000,
         ci.l.af.tot.RCP2.6 = tot.abs.1.RCP2.6.sim.ci.l.tot.abs.1.RCP2.6/nd*1000,
         ci.u.af.tot.RCP2.6 = tot.abs.1.RCP2.6.sim.ci.u.tot.abs.1.RCP2.6/nd*1000,
         
         af.heat.RCP2.6 = heat.abs.1.RCP2.6.est/nd*1000,
         ci.l.af.heat.RCP2.6 = heat.abs.1.RCP2.6.sim.ci.l.heat.abs.1.RCP2.6/nd*1000,
         ci.u.af.heat.RCP2.6 = heat.abs.1.RCP2.6.sim.ci.u.heat.abs.1.RCP2.6/nd*1000,
         
         af.cold.RCP2.6 = cold.abs.1.RCP2.6.est/nd*1000,
         ci.l.af.cold.RCP2.6 = cold.abs.1.RCP2.6.sim.ci.l.cold.abs.1.RCP2.6/nd*1000,
         ci.u.af.cold.RCP2.6 = cold.abs.1.RCP2.6.sim.ci.u.cold.abs.1.RCP2.6/nd*1000,
         
         af.exheat.RCP2.6 = exheat.abs.1.RCP2.6.est/nd*1000,
         ci.l.af.exheat.RCP2.6 = exheat.abs.1.RCP2.6.sim.ci.l.exheat.abs.1.RCP2.6/nd*1000,
         ci.u.af.exheat.RCP2.6 = exheat.abs.1.RCP2.6.sim.ci.u.exheat.abs.1.RCP2.6/nd*1000,
         
         af.excold.RCP2.6 = excold.abs.1.RCP2.6.est/nd*1000,
         ci.l.af.excold.RCP2.6 = excold.abs.1.RCP2.6.sim.ci.l.excold.abs.1.RCP2.6/nd*1000,
         ci.u.af.excold.RCP2.6 = excold.abs.1.RCP2.6.sim.ci.u.excold.abs.1.RCP2.6/nd*1000,
         
         af.tot.RCP8.5 = tot.abs.1.RCP8.5.est/nd*1000,
         ci.l.af.tot.RCP8.5 = tot.abs.1.RCP8.5.sim.ci.l.tot.abs.1.RCP8.5/nd*1000,
         ci.u.af.tot.RCP8.5 = tot.abs.1.RCP8.5.sim.ci.u.tot.abs.1.RCP8.5/nd*1000,
         
         af.heat.RCP8.5 = heat.abs.1.RCP8.5.est/nd*1000,
         ci.l.af.heat.RCP8.5 = heat.abs.1.RCP8.5.sim.ci.l.heat.abs.1.RCP8.5/nd*1000,
         ci.u.af.heat.RCP8.5 = heat.abs.1.RCP8.5.sim.ci.u.heat.abs.1.RCP8.5/nd*1000,
         
         af.cold.RCP8.5 = cold.abs.1.RCP8.5.est/nd*1000,
         ci.l.af.cold.RCP8.5 = cold.abs.1.RCP8.5.sim.ci.l.cold.abs.1.RCP8.5/nd*1000,
         ci.u.af.cold.RCP8.5 = cold.abs.1.RCP8.5.sim.ci.u.cold.abs.1.RCP8.5/nd*1000,
         
         af.exheat.RCP8.5 = exheat.abs.1.RCP8.5.est/nd*1000,
         ci.l.af.exheat.RCP8.5 = exheat.abs.1.RCP8.5.sim.ci.l.exheat.abs.1.RCP8.5/nd*1000,
         ci.u.af.exheat.RCP8.5 = exheat.abs.1.RCP8.5.sim.ci.u.exheat.abs.1.RCP8.5/nd*1000,
         
         af.excold.RCP8.5 = excold.abs.1.RCP8.5.est/nd*1000,
         ci.l.af.excold.RCP8.5 = excold.abs.1.RCP8.5.sim.ci.l.excold.abs.1.RCP8.5/nd*1000,
         ci.u.af.excold.RCP8.5 = excold.abs.1.RCP8.5.sim.ci.u.excold.abs.1.RCP8.5/nd*1000,
         
         
         # AFrel
         rel.af.tot.RCP2.6 = tot.rel.1.RCP2.6.est/nd*1000,
         rel.ci.l.af.tot.RCP2.6 = tot.rel.1.RCP2.6.sim.ci.l.tot.rel.1.RCP2.6/nd*1000,
         rel.ci.u.af.tot.RCP2.6 = tot.rel.1.RCP2.6.sim.ci.u.tot.rel.1.RCP2.6/nd*1000,
         
         rel.af.heat.RCP2.6 = heat.rel.1.RCP2.6.est/nd*1000,
         rel.ci.l.af.heat.RCP2.6 = heat.rel.1.RCP2.6.sim.ci.l.heat.rel.1.RCP2.6/nd*1000,
         rel.ci.u.af.heat.RCP2.6 = heat.rel.1.RCP2.6.sim.ci.u.heat.rel.1.RCP2.6/nd*1000,
         
         rel.af.cold.RCP2.6 = cold.rel.1.RCP2.6.est/nd*1000,
         rel.ci.l.af.cold.RCP2.6 = cold.rel.1.RCP2.6.sim.ci.l.cold.rel.1.RCP2.6/nd*1000,
         rel.ci.u.af.cold.RCP2.6 = cold.rel.1.RCP2.6.sim.ci.u.cold.rel.1.RCP2.6/nd*1000,
         
         rel.af.exheat.RCP2.6 = exheat.rel.1.RCP2.6.est/nd*1000,
         rel.ci.l.af.exheat.RCP2.6 = exheat.rel.1.RCP2.6.sim.ci.l.exheat.rel.1.RCP2.6/nd*1000,
         rel.ci.u.af.exheat.RCP2.6 = exheat.rel.1.RCP2.6.sim.ci.u.exheat.rel.1.RCP2.6/nd*1000,
         
         rel.af.excold.RCP2.6 = excold.rel.1.RCP2.6.est/nd*1000,
         rel.ci.l.af.excold.RCP2.6 = excold.rel.1.RCP2.6.sim.ci.l.excold.rel.1.RCP2.6/nd*1000,
         rel.ci.u.af.excold.RCP2.6 = excold.rel.1.RCP2.6.sim.ci.u.excold.rel.1.RCP2.6/nd*1000,
         
         rel.af.tot.RCP8.5 = tot.rel.1.RCP8.5.est/nd*1000,
         rel.ci.l.af.tot.RCP8.5 = tot.rel.1.RCP8.5.sim.ci.l.tot.rel.1.RCP8.5/nd*1000,
         rel.ci.u.af.tot.RCP8.5 = tot.rel.1.RCP8.5.sim.ci.u.tot.rel.1.RCP8.5/nd*1000,
         
         rel.af.heat.RCP8.5 = heat.rel.1.RCP8.5.est/nd*1000,
         rel.ci.l.af.heat.RCP8.5 = heat.rel.1.RCP8.5.sim.ci.l.heat.rel.1.RCP8.5/nd*1000,
         rel.ci.u.af.heat.RCP8.5 = heat.rel.1.RCP8.5.sim.ci.u.heat.rel.1.RCP8.5/nd*1000,
         
         rel.af.cold.RCP8.5 = cold.rel.1.RCP8.5.est/nd*1000,
         rel.ci.l.af.cold.RCP8.5 = cold.rel.1.RCP8.5.sim.ci.l.cold.rel.1.RCP8.5/nd*1000,
         rel.ci.u.af.cold.RCP8.5 = cold.rel.1.RCP8.5.sim.ci.u.cold.rel.1.RCP8.5/nd*1000,
         
         rel.af.exheat.RCP8.5 = exheat.rel.1.RCP8.5.est/nd*1000,
         rel.ci.l.af.exheat.RCP8.5 = exheat.rel.1.RCP8.5.sim.ci.l.exheat.rel.1.RCP8.5/nd*1000,
         rel.ci.u.af.exheat.RCP8.5 = exheat.rel.1.RCP8.5.sim.ci.u.exheat.rel.1.RCP8.5/nd*1000,
         
         rel.af.excold.RCP8.5 = excold.rel.1.RCP8.5.est/nd*1000,
         rel.ci.l.af.excold.RCP8.5 = excold.rel.1.RCP8.5.sim.ci.l.excold.rel.1.RCP8.5/nd*1000,
         rel.ci.u.af.excold.RCP8.5 = excold.rel.1.RCP8.5.sim.ci.u.excold.rel.1.RCP8.5/nd*1000,
         
         # ANs
         an.tot.RCP2.6 = tot.abs.1.RCP2.6.est,
         ci.l.an.tot.RCP2.6 = tot.abs.1.RCP2.6.sim.ci.l.tot.abs.1.RCP2.6,
         ci.u.an.tot.RCP2.6 = tot.abs.1.RCP2.6.sim.ci.u.tot.abs.1.RCP2.6,
         
         an.heat.RCP2.6 = heat.abs.1.RCP2.6.est,
         ci.l.an.heat.RCP2.6 = heat.abs.1.RCP2.6.sim.ci.l.heat.abs.1.RCP2.6,
         ci.u.an.heat.RCP2.6 = heat.abs.1.RCP2.6.sim.ci.u.heat.abs.1.RCP2.6,
         
         an.cold.RCP2.6 = cold.abs.1.RCP2.6.est,
         ci.l.an.cold.RCP2.6 = cold.abs.1.RCP2.6.sim.ci.l.cold.abs.1.RCP2.6,
         ci.u.an.cold.RCP2.6 = cold.abs.1.RCP2.6.sim.ci.u.cold.abs.1.RCP2.6,
         
         an.exheat.RCP2.6 = exheat.abs.1.RCP2.6.est,
         ci.l.an.exheat.RCP2.6 = exheat.abs.1.RCP2.6.sim.ci.l.exheat.abs.1.RCP2.6,
         ci.u.an.exheat.RCP2.6 = exheat.abs.1.RCP2.6.sim.ci.u.exheat.abs.1.RCP2.6,
         
         an.excold.RCP2.6 = excold.abs.1.RCP2.6.est,
         ci.l.an.excold.RCP2.6 = excold.abs.1.RCP2.6.sim.ci.l.excold.abs.1.RCP2.6,
         ci.u.an.excold.RCP2.6 = excold.abs.1.RCP2.6.sim.ci.u.excold.abs.1.RCP2.6,
         
         an.tot.RCP8.5 = tot.abs.1.RCP8.5.est,
         ci.l.an.tot.RCP8.5 = tot.abs.1.RCP8.5.sim.ci.l.tot.abs.1.RCP8.5,
         ci.u.an.tot.RCP8.5 = tot.abs.1.RCP8.5.sim.ci.u.tot.abs.1.RCP8.5,
         
         an.heat.RCP8.5 = heat.abs.1.RCP8.5.est,
         ci.l.an.heat.RCP8.5 = heat.abs.1.RCP8.5.sim.ci.l.heat.abs.1.RCP8.5,
         ci.u.an.heat.RCP8.5 = heat.abs.1.RCP8.5.sim.ci.u.heat.abs.1.RCP8.5,
         
         an.cold.RCP8.5 = cold.abs.1.RCP8.5.est,
         ci.l.an.cold.RCP8.5 = cold.abs.1.RCP8.5.sim.ci.l.cold.abs.1.RCP8.5,
         ci.u.an.cold.RCP8.5 = cold.abs.1.RCP8.5.sim.ci.u.cold.abs.1.RCP8.5,
         
         an.exheat.RCP8.5 = exheat.abs.1.RCP8.5.est,
         ci.l.an.exheat.RCP8.5 = exheat.abs.1.RCP8.5.sim.ci.l.exheat.abs.1.RCP8.5,
         ci.u.an.exheat.RCP8.5 = exheat.abs.1.RCP8.5.sim.ci.u.exheat.abs.1.RCP8.5,
         
         an.excold.RCP8.5 = excold.abs.1.RCP8.5.est,
         ci.l.an.excold.RCP8.5 = excold.abs.1.RCP8.5.sim.ci.l.excold.abs.1.RCP8.5,
         ci.u.an.excold.RCP8.5 = excold.abs.1.RCP8.5.sim.ci.u.excold.abs.1.RCP8.5,
         
        # ANrel
        rel.an.tot.RCP2.6 = tot.rel.1.RCP2.6.est,
        rel.ci.l.an.tot.RCP2.6 = tot.rel.1.RCP2.6.sim.ci.l.tot.rel.1.RCP2.6,
        rel.ci.u.an.tot.RCP2.6 = tot.rel.1.RCP2.6.sim.ci.u.tot.rel.1.RCP2.6,
        
        rel.an.heat.RCP2.6 = heat.rel.1.RCP2.6.est,
        rel.ci.l.an.heat.RCP2.6 = heat.rel.1.RCP2.6.sim.ci.l.heat.rel.1.RCP2.6,
        rel.ci.u.an.heat.RCP2.6 = heat.rel.1.RCP2.6.sim.ci.u.heat.rel.1.RCP2.6,
        
        rel.an.cold.RCP2.6 = cold.rel.1.RCP2.6.est,
        rel.ci.l.an.cold.RCP2.6 = cold.rel.1.RCP2.6.sim.ci.l.cold.rel.1.RCP2.6,
        rel.ci.u.an.cold.RCP2.6 = cold.rel.1.RCP2.6.sim.ci.u.cold.rel.1.RCP2.6,
        
        rel.an.exheat.RCP2.6 = exheat.rel.1.RCP2.6.est,
        rel.ci.l.an.exheat.RCP2.6 = exheat.rel.1.RCP2.6.sim.ci.l.exheat.rel.1.RCP2.6,
        rel.ci.u.an.exheat.RCP2.6 = exheat.rel.1.RCP2.6.sim.ci.u.exheat.rel.1.RCP2.6,
        
        rel.an.excold.RCP2.6 = excold.rel.1.RCP2.6.est,
        rel.ci.l.an.excold.RCP2.6 = excold.rel.1.RCP2.6.sim.ci.l.excold.rel.1.RCP2.6,
        rel.ci.u.an.excold.RCP2.6 = excold.rel.1.RCP2.6.sim.ci.u.excold.rel.1.RCP2.6,
        
        rel.an.tot.RCP8.5 = tot.rel.1.RCP8.5.est,
        rel.ci.l.an.tot.RCP8.5 = tot.rel.1.RCP8.5.sim.ci.l.tot.rel.1.RCP8.5,
        rel.ci.u.an.tot.RCP8.5 = tot.rel.1.RCP8.5.sim.ci.u.tot.rel.1.RCP8.5,
        
        rel.an.heat.RCP8.5 = heat.rel.1.RCP8.5.est,
        rel.ci.l.an.heat.RCP8.5 = heat.rel.1.RCP8.5.sim.ci.l.heat.rel.1.RCP8.5,
        rel.ci.u.an.heat.RCP8.5 = heat.rel.1.RCP8.5.sim.ci.u.heat.rel.1.RCP8.5,
        
        rel.an.cold.RCP8.5 = cold.rel.1.RCP8.5.est,
        rel.ci.l.an.cold.RCP8.5 = cold.rel.1.RCP8.5.sim.ci.l.cold.rel.1.RCP8.5,
        rel.ci.u.an.cold.RCP8.5 = cold.rel.1.RCP8.5.sim.ci.u.cold.rel.1.RCP8.5,
        
        rel.an.exheat.RCP8.5 = exheat.rel.1.RCP8.5.est,
        rel.ci.l.an.exheat.RCP8.5 = exheat.rel.1.RCP8.5.sim.ci.l.exheat.rel.1.RCP8.5,
        rel.ci.u.an.exheat.RCP8.5 = exheat.rel.1.RCP8.5.sim.ci.u.exheat.rel.1.RCP8.5,
        
        rel.an.excold.RCP8.5 = excold.rel.1.RCP8.5.est,
        rel.ci.l.an.excold.RCP8.5 = excold.rel.1.RCP8.5.sim.ci.l.excold.rel.1.RCP8.5,
        rel.ci.u.an.excold.RCP8.5 = excold.rel.1.RCP8.5.sim.ci.u.excold.rel.1.RCP8.5,
        
         
         )


bec=read.csv("BEC_Dataset_v24_20210824.csv")%>%
  mutate(nsalid1=SALID1)%>%
  dplyr::select(nsalid1, L1Name, Country)%>%
  arrange(nsalid1)
bec$Country[bec$Country=='Brasil']='Brazil' 


datf=merge(datf, bec[,c("nsalid1", "L1Name", "Country")], by="nsalid1")
datf = datf %>%
  dplyr::select(1:2, 184, 185, 64:183 )
  
  

write.csv(datf, "total_HMRCMR_an_from_age_groups.csv", row.names = F)

write.csv(an, "age_specific/an_all.csv", row.names = F)
