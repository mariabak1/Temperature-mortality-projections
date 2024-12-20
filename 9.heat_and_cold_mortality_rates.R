# compute age specific HEAT AND COLD MORTALITY RATE
# HMR = HEAT AN / POP COUNT * 1,000 = an.exheat.RCP2.6/citytotpopf
# CMR = COLD AN / POP COUNT * 1,000


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


# AN AND AF BY CITY
ans = read.csv("afrel_anrel_from_age_groups.csv")

# NUMBER OF PROJECTED POP COUNTS BY CITY  TO USE AS DENOMINTAOR
dat = read.csv("prj_city_pop_agrdistr_asdr.csv")

byage2 = dat[!duplicated(dat$nsalid1),]%>%
  mutate(popdiff = citytotpopf   - totpop )%>%
  select(nsalid1, totpop, citytotpopf, popdiff)

datr=merge(ans, dat[!duplicated(dat$nsalid1),], by = "nsalid1")

  
# for the future period
datf = datr %>%
  filter(period == '2045-2054')%>%
  mutate(hmr.rcp2.6 = an.heat.RCP2.6/citytotpopf*1000, # heat rcp26, heat rcp8.5, cold for two rcps, tot for two rcps
         hmr.cil.rcp2.6 = ci.l.an.heat.RCP2.6/citytotpopf*1000,
         hmr.ciu.rcp2.6 = ci.u.an.heat.RCP2.6/citytotpopf*1000,
         
         hmr.rcp8.5 = an.heat.RCP8.5/citytotpopf*1000, 
         hmr.cil.rcp8.5 = ci.l.an.heat.RCP8.5/citytotpopf*1000,
         hmr.ciu.rcp8.5 = ci.u.an.heat.RCP8.5/citytotpopf*1000,
         
         
         cmr.rcp2.6 = an.cold.RCP2.6/citytotpopf*1000, # heat rcp26, heat rcp8.5, cold for two rcps, tot for two rcps
         cmr.cil.rcp2.6 = ci.l.an.cold.RCP2.6/citytotpopf*1000,
         cmr.ciu.rcp2.6 = ci.u.an.cold.RCP2.6/citytotpopf*1000,
         
         cmr.rcp8.5 = an.cold.RCP8.5/citytotpopf*1000, 
         cmr.cil.rcp8.5 = ci.l.an.cold.RCP8.5/citytotpopf*1000,
         cmr.ciu.rcp8.5 = ci.u.an.cold.RCP8.5/citytotpopf*1000,
         
         ttmr.rcp2.6 = an.tot.RCP2.6/citytotpopf*1000,
         ttmr.cil.rcp2.6 = ci.l.an.tot.RCP2.6/citytotpopf*1000,
         ttmr.ciu.rcp2.6 = ci.u.an.tot.RCP2.6/citytotpopf*1000,
         
         ttmr.rcp8.5 = an.tot.RCP8.5/citytotpopf*1000,
         ttmr.cil.rcp8.5 = ci.l.an.tot.RCP8.5/citytotpopf*1000,
         ttmr.ciu.rcp8.5 = ci.u.an.tot.RCP8.5/citytotpopf*1000
         
         
         )

summary(datf)





# HMR and CMR for the current period
datc = datr %>%
  filter(!period == '2045-2054')%>%
  mutate(hmr.rcp2.6 = an.heat.RCP2.6/totpop*1000, # heat, cold, tot 
         hmr.cil.rcp2.6 = ci.l.an.heat.RCP2.6/totpop*1000,
         hmr.ciu.rcp2.6 = ci.u.an.heat.RCP2.6/totpop*1000,
         
         hmr.rcp8.5 = an.heat.RCP8.5/totpop*1000, 
         hmr.cil.rcp8.5 = ci.l.an.heat.RCP8.5/totpop*1000,
         hmr.ciu.rcp8.5 = ci.u.an.heat.RCP8.5/totpop*1000,
         
         
         cmr.rcp2.6 = an.cold.RCP2.6/totpop*1000, # heat rcp26, heat rcp8.5, cold for two rcps, tot for two rcps
         cmr.cil.rcp2.6 = ci.l.an.cold.RCP2.6/totpop*1000,
         cmr.ciu.rcp2.6 = ci.u.an.cold.RCP2.6/totpop*1000,
         
         cmr.rcp8.5 = an.cold.RCP8.5/totpop*1000, 
         cmr.cil.rcp8.5 = ci.l.an.cold.RCP8.5/totpop*1000,
         cmr.ciu.rcp8.5 = ci.u.an.cold.RCP8.5/totpop*1000,
         
         ttmr.rcp2.6 = an.tot.RCP2.6/totpop*1000,
         ttmr.cil.rcp2.6 = ci.l.an.tot.RCP2.6/totpop*1000,
         ttmr.ciu.rcp2.6 = ci.u.an.tot.RCP2.6/totpop*1000,
         
         ttmr.rcp8.5 = an.tot.RCP8.5/totpop*1000,
         ttmr.cil.rcp8.5 = ci.l.an.tot.RCP8.5/totpop*1000,
         ttmr.ciu.rcp8.5 = ci.u.an.tot.RCP8.5/totpop*1000
         
         
  )

summary(datc)


dat_rates = rbind(datf, datc)

write.csv(dat_rates, "baseline_and_prj_hmr_cmr_ALL_CHANGES.csv", row.names = F) #






mutate(brw=(totpopf/totpop) * (a_pop_percentf/a_pop_percent) * (dxf/dxc) )
# no brw - CC only
mutate(brw=(a_pop_percentf/a_pop_percent)) # pop aging only
# brw=(totpopf/totpop) # pop size only
# brw =  (dxf/dxc) MR only
