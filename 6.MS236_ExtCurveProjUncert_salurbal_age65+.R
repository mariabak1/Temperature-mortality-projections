### Code to compute temperature-related mortality for ages 65+ ####
### while considering changes in population size, age group size, and mortality rates in mid-century ###
### Age-group specific weights are read in during MS236_Read_in_data_age65+.R load ###

################################################################################
# Code is adapted from: 
#
#  "A hands-on tutorial on a modelling framework for projections of climate 
#    change impacts on health"
#   Ana M. Vicedo-Cabrera, Francesco Sera, Antonio Gasparrini
#  http://www.ag-myresearch.com/2019_vicedo-cabrera_Epidem.html
#

################################################################################
# 04 EXTRAPOLATION OF THE EXPOSURE-RESPONSE CURVE
# 05 PROJECTION & QUANTIFICATION OF THE IMPACT
# 06 ENSEMBLE ESTIMATES & QUANTIFICATION OF THE UNCERTAINTY
################################################################################

# The three last steps of the analysis (extrapolation of the curve, 
#   impact projections and quantification of the uncertainty) can be performed 
#   sequentially using the following code.

# In brief, once we extrapolate the curve, we estimate the daily number of attributable 
#   deaths (AN) in each scenario, GCM and temperature range. 
# Then, we compute the sum of ANs per each 10-years period for the ensemble 
#   per each RCP and temperature range. We also estimate the difference in AN 
#   relative to the ANs estimated for the current days (2010-19). 
# By dividing between the total mortality, we estimate the corresponding 
#   attributable fractions (AFs).
# Uncertainty of the estimated impacts is expressed in terms of empirical 
#   confidence intervals, defined as the 2.5th and 97.5th percentiles of the 
#   empirical distribution of the impacts across coefficients samples and GCMs. 
#   The distribution is obtained through Monte Carlo simulation of the coefficients.


# EXTRACT COEFFICIENTS AND VCOV FROM THE MODEL IN STAGE 1
# With the crossreduce function we can reduce the fit of the bidimensional DLNM
#   (of the stage 1 using the observed temperature-mortality series)
#   to summaries defined in the exposure-response dimension. We can then 
#   extract the coefficients and the covariance matrix defining the overall 
#   exposure-response association.


options("scipen"=100, "digits"=6) # force R not to use scientific notation

# load the data
source("MS236_Read_in_data_age65+.R")

# load the packages
library(dlnm) ; library(splines) ; library(MASS)

# define lists to store results
ANABS = list()
AFABS = list()
ANREL = list()
AFREL = list()

ANSIM = list()

DEATHDENOM=list()
DEATHDENOMH=list()

start_time <- Sys.time() # 1,000 simulations per city take about 8 mins
# start city loop ####
for (c in 1:length(unique(cities$nsalid1))){
  red=PREDICTED.65PLUS$PredictedMV1[[c]] 
  cen=PREDICTED.65PLUS$MMT$MMT1[[c]]
  coef <- coef(red)
  vcov <- vcov(red)
  
  # STORE THE MORTALITY BY PERIOD -- this is later used as the denominator to compute AFs
  # a. for the projection period (now same length as the baseline period)
  deathdoy = DEATHDOY.AGE2[[c]]$`deathdoy65+`
  deathperiod <- sum(DEATHPROJ[[c]])                  # total mortality during the MS85 time period; for c=114 it is 1,281,369 deaths

  # b. for the country-specific baseline period (from MS85)
  deathperiod_hist <- sum(DEATHPROJ_HIST[[c]]) # total mortality during the time period in MS85 (historical time period)
  
  DEATHDENOM[[c]] = deathperiod
  DEATHDENOMH[[c]] = deathperiod_hist
  
#}

  # CREATE TEMPORARY OBJECT TO STORE THE ESTIMATED AN 
  # We create an array with 6 dimensions to store the AN estimated in each 
  #   simulation for each period, range of temperature, gcm and rcp. An additional
  #   dimension is also included to store the absolute AN (estimated directly from
  #   the formula below), and the difference in AN relative to current period.
  
  # - DEFINE THE NAMES AND NUMBER OF LEVELS PER DIMENSION (ALSO USED TO DEFINE 
  #     THE INDEX TO RUN THE LOOPS BELOW)
  
  
  # (1a) DIMENSION - COUNTRY-SPECIFIC HISTORICAL REFERENCE PERIOD (FROM MS85)
  #  *LABEL THE HISTORICAL PERIOD
  histperiod <- dailytempcity[[c]]$range ## use the same range as in MS85 ####

  # (1B) DIMENSION - MID-CENTURY PROJECTION PERIOD (this period used to be 10 years, 2045-2054, but now it's equal
  # to the number of years in the baseline period; I left the labels as mid-century anyway, can change them in you'd like)
  #  *LABELS THE PROJECTION PERIODS 
  projperiod <- paste(2045, "-", 2054 ,sep="")  
  
  #  *DEFINE SEQUENCE OF PERIODS FOR THE PREDICTIONS (HISTORICAL & PROJECTED)
  histseqperiod <- factor(rep(histperiod,length.out=365*length(seq(dailytempcity[[c]]$start.year[1], dailytempcity[[c]]$end.year[1])))) # length(histseqperiod) shows number of days in historical (MS85) period (unique to each city)
  projseqperiod <- factor(rep(projperiod, length.out=365*length(seq(dailytempcity[[c]]$start.year[1], dailytempcity[[c]]$end.year[1])))) #same length in days as histperiod now
  seqperiod <- factor(c(as.numeric(histseqperiod)-1, as.numeric(projseqperiod))) # n of days in the hist period + n of days in the proj period
  levels(seqperiod) <- c(histperiod,projperiod)
  
  # (2) DIMENSION - RANGE OF TEMPERATURES
  temprange <- c("tot","cold","heat", "excold", "exheat") # added extreme heat and cold

  # (3) DIMENSION - ABSOLUTE AN/DIFFERENCE IN AN
  absrel <- c("abs","rel")
  
  # (4) DIMENSION - GENERAL CIRCULATION MODELS
  # *LIST OF GCMs: WE ONLY HAVE ONE (1) GCM  
  gcm <- c("GCM") # we only have one
  
  # (5) DIMENSION - SCENARIO DIMENSION
  #  *LIST OF REPRESENTATIVE CONCENTRATION PATHWAYS SCENARIOS 
  rcp <- c(RCP2.6="rcp2p6", RCP8.5="rcp8p5")
  
  # (6) DIMENSION - NUMBER OF ITERATION IN THE MONTE-CARLO SIMULATION 
  nsim <- 1000


  # DEFINE THE ARRAY
  ansim <- array(NA,dim=c(length(levels(seqperiod)),length(temprange),
    length(absrel), length(gcm),length(rcp),nsim+1), 
    dimnames=list(levels(seqperiod),temprange,absrel,
      names(gcm),names(rcp), c("est",paste0("sim",seq(nsim)))))
  
  #i=1
  #j=1
  
  # RUN LOOP PER RCP
  for (i in seq(rcp)) {
    
    # PRINT
    cat("\n\n",names(rcp)[i],"\n")
    
    # SELECTION OF THE PROJECTED TEMPERATURE SERIES FOR A SPECIFIC RCP SCENARIO
    tmeanproj <- get(rcp[[i]])[[c]] # subset by RCP scenario and city 
    
    
    # RUN LOOP PER GCM
    for(j in seq(gcm)) {
      
      # PRINT
      cat(gcm[j],"")
      
      # (4) EXTRAPOLATION OF THE CURVE: 
      # - DERIVE THE CENTERED BASIS USING THE PROJECTED TEMPERATURE SERIES
      #   AND EXTRACT PARAMETERS
      
      # 4a. for the future time period
      argvar <- list(fun = "ns", knots=KNOTS1[c,], Boundary.knots=BOUNDS1[c,]) # from ms85
      bvar <- do.call(onebasis, c(list(x=tmeanproj[,j+2]), argvar)) # entire curve not centered at MMT
      cenvec <- do.call(onebasis,c(list(x=cen),argvar)) # recreates temp-mortality associations based on parameters above/at MMT
      bvarcen <- scale(bvar,center=cenvec,scale=F) # centered temp-mortality curve
      
      bvar2 <- do.call(onebasis, c(list(x=dailytempcity[[c]]$tmean), argvar)) # entire curve not centered at MMT 
      cenvec2 <- do.call(onebasis,c(list(x=cen),argvar)) # recreates temp-mortality associations based on parameters above/at MMT
      bvarcen2 <- scale(bvar2,center=cenvec2,scale=F) # centered temp-mortality curve
      
      
      # INDICATOR FOR COLD/HEAT DAYS (yes/no)
      # a. for the projected temperature data
      indheat <- tmeanproj[,j+2]>cen
      indexheat <- tmeanproj[,j+2]>=per.temp[c,]$p95
      indexcold <- tmeanproj[,j+2]<=per.temp[c,]$p5
      
      # b. for the baseline (historical) data
      indheat2 <- dailytempcity[[c]]$tmean>cen
      indexheat2 <- dailytempcity[[c]]$tmean>=per.temp[c,]$p95
      indexcold2 <- dailytempcity[[c]]$tmean<=per.temp[c,]$p5
      
      
      # (5) IMPACT PROJECTIONS:
      # - COMPUTE THE DAILY CONTRIBUTIONS OF ATTRIBUTABLE DEATHS
      
      # a. for the future time period
      deathproj = DEATHPROJ[[c]] # 'projected' mortality series for the number of years equal to the study period in MS85
      an1 <- (1-exp(-bvarcen%*%coef))*deathproj 
      
      # b. for the historical time period
      deathproj2 = DEATHPROJ_HIST[[c]] # 'projected' mortality series for the number of years equal to the study period in MS85
      an2 <- (1-exp(-bvarcen2%*%coef))*deathproj2 #  should be of size equal to # of days in the historical period
      
      

      # - SUM AN (ABS) BY TEMPERATURE RANGE AND PERIOD, STORE IN ARRAY BEFORE THE ITERATIONS
      # NB: ACCOUNT FOR NO TEMPERATURE BELOW/ABOVE CEN FOR GIVEN PERIODS
     
      #ansim[,"tot","abs",j,i,1] <- tapply(an,seqperiod,sum) # this is how it was done in Vicedo-Cabrera et al. code (I left -- and commented out -- those out; just so you can compare if needed)
      ansim["2045-2054","tot","abs",j,i,1] <- sum(an1)
      ansim[histperiod[1],"tot","abs",j,i,1] <- sum(an2)
      
      #ansim[,"cold","abs",j,i,1] <- tapply(an[!indheat],factor(seqperiod[!indheat]),sum) # same comment as above applies to all the commented out lines for this ansim
      ansim["2045-2054","cold","abs",j,i,1] <- sum(an1[!indheat])
      ansim[histperiod[1],"cold","abs",j,i,1] <- sum(an2[!indheat2])
      
      #ansim[,"heat","abs",j,i,1] <- tapply(an[indheat],factor(seqperiod[indheat]),sum)
      ansim["2045-2054","heat","abs",j,i,1] <- sum(an1[indheat])
      ansim[histperiod[1],"heat","abs",j,i,1] <- sum(an2[indheat2])
      
      #ansim[,"excold","abs",j,i,1] <- tapply(an[indexcold],factor(seqperiod[indexcold]),sum)
      ansim["2045-2054","excold","abs",j,i,1] <- sum(an1[indexcold])
      ansim[histperiod[1],"excold","abs",j,i,1] <- sum(an2[indexcold2])
      
      # ansim[,"exheat","abs",j,i,1] <- tapply(an[indexheat],factor(seqperiod[indexheat]),sum)
      ansim["2045-2054","exheat","abs",j,i,1] <- sum(an1[indexheat])
      ansim[histperiod[1],"exheat","abs",j,i,1] <- sum(an2[indexheat2])
      
            
      # (6) ESTIMATE UNCERTAINTY OF THE PROJECTED AN:
      # - SAMPLE COEF ASSUMING A MULTIVARIATE NORMAL DISTRIBUTION
      set.seed(13041975+j)
      
      coefsim <- mvrnorm(nsim,coef,vcov) 
      
      # - LOOP ACROSS ITERATIONS
      for(s in seq(nsim)) {
        #s=1
        # COMPUTE THE DAILY CONTRIBUTIONS OF ATTRIBUTABLE DEATHS (again, an1 for the future period, an2 for the 'historical' period)
        an1 <- (1-exp(-bvarcen%*%coefsim[s,]))*deathproj
        an2 <- (1-exp(-bvarcen2%*%coefsim[s,]))*deathproj2

        ansim["2045-2054","tot","abs",j,i,s+1] <- sum(an1)
        ansim[histperiod[1],"tot","abs",j,i,s+1] <- sum(an2)
        
        #ansim[,"cold","abs",j,i,1] <- tapply(an[!indheat],factor(seqperiod[!indheat]),sum)
        ansim["2045-2054","cold","abs",j,i,s+1] <- sum(an1[!indheat])
        ansim[histperiod[1],"cold","abs",j,i,s+1] <- sum(an2[!indheat2])
        
        #ansim[,"heat","abs",j,i,1] <- tapply(an[indheat],factor(seqperiod[indheat]),sum)
        ansim["2045-2054","heat","abs",j,i,s+1] <- sum(an1[indheat])
        ansim[histperiod[1],"heat","abs",j,i,s+1] <- sum(an2[indheat2])
        
        #ansim[,"excold","abs",j,i,1] <- tapply(an[indexcold],factor(seqperiod[indexcold]),sum)
        ansim["2045-2054","excold","abs",j,i,s+1] <- sum(an1[indexcold])
        ansim[histperiod[1],"excold","abs",j,i,s+1] <- sum(an2[indexcold2], na.rm = T)
        
        # ansim[,"exheat","abs",j,i,1] <- tapply(an[indexheat],factor(seqperiod[indexheat]),sum)
        ansim["2045-2054","exheat","abs",j,i,s+1] <- sum(an1[indexheat])
        ansim[histperiod[1],"exheat","abs",j,i,s+1] <- sum(an2[indexheat2])
        
        
    }
  }

 }

  ################################################################################
  # ESTIMATE AN IN EACH PERIOD RELATIVE TO MS85 BASELINE
  
  ansim[histperiod[1],,"rel",,,] <- ansim["2045-2054",,"abs",,,] - ansim[histperiod[1],,"abs",,,]


  ################################################################################
  # SUMMARIZE THE RESULTS 
  # COMPUTE AN/AF (95%CI) IN THE ENSEMBLE, BY RANGE & PERIOD & RCP
  
  # CREATE NEW OBJECTS TO STORE RESULTS
  # We now create 4 new arrays (2 arrays to store the abs and rel AN, and another 2
  #   to store the estimated rel and abs AF) with 4 dimensions each to store the 
  #   ensemble estimates (average impacts across GCMs) with the empirical 
  #   95% confidence intervals. 
  # In this case, the 4 dimensions correspond to the 10-year period, the point estimate 
  #   and the CI, the temperature range and the scenario. 

  estci <- c("est","ci.l","ci.u")
  
  anabs <- afabs <- anrel <- afrel <- array(NA,dim=c(length(levels(seqperiod)),
    length(estci),length(temprange),length(rcp)), 
    dimnames=list(levels(seqperiod),estci,temprange,names(rcp)))
  
  
  # ATTRIBUTABLE NUMBERS 
  # ABSOLUTE
  anabs[,"est",,"RCP2.6"] <- apply(ansim[,,"abs",,"RCP2.6",1],1:2,mean)
  anabs[,"ci.l",,"RCP2.6"] <- apply(ansim[,,"abs",,"RCP2.6",-1],1:2,quantile,0.025, na.rm=T)
  anabs[,"ci.u",,"RCP2.6"] <- apply(ansim[,,"abs",,"RCP2.6",-1],1:2,quantile,0.975, na.rm=T)
  
  anabs[,"est",,"RCP8.5"] <- apply(ansim[,,"abs",,"RCP8.5",1],1:2,mean)
  anabs[,"ci.l",,"RCP8.5"] <- apply(ansim[,,"abs",,"RCP8.5",-1],1:2,quantile,0.025, na.rm=T)
  anabs[,"ci.u",,"RCP8.5"] <- apply(ansim[,,"abs",,"RCP8.5",-1],1:2,quantile,0.975, na.rm=T)
  
  # RELATIVE
  anrel[,"est",,"RCP2.6"] <- apply(ansim[,,"rel",,"RCP2.6",1],1:2,mean)
  anrel[,"ci.l",,"RCP2.6"] <- apply(ansim[,,"rel",,"RCP2.6",-1],1:2,quantile,0.025, na.rm=T)
  anrel[,"ci.u",,"RCP2.6"] <- apply(ansim[,,"rel",,"RCP2.6",-1],1:2,quantile,0.975, na.rm=T)
  
  anrel[,"est",,"RCP8.5"] <- apply(ansim[,,"rel",,"RCP8.5",1],1:2,mean)
  anrel[,"ci.l",,"RCP8.5"] <- apply(ansim[,,"rel",,"RCP8.5",-1],1:2,quantile,0.025, na.rm=T)
  anrel[,"ci.u",,"RCP8.5"] <- apply(ansim[,,"rel",,"RCP8.5",-1],1:2,quantile,0.975, na.rm=T)
  
  # ATTRIBUTABLE FRACTION
  # old note: the denominator should be specific to the time period (historical vs future) - bc they differ in length
  # update: since implementing Sarav's temperature idea, the time periods are not longer diff in length, but I am leaving this as is
  
  afabs[histperiod[1],,,] <- anabs[histperiod[1],,,]/deathperiod_hist*100
  afabs["2045-2054",,,] <- anabs["2045-2054",,,]/deathperiod*100
  
  afrel[histperiod[1],,,] <- anrel[histperiod[1],,,]/deathperiod_hist*100
  afrel["2045-2054",,,] <- anrel["2045-2054",,,]/deathperiod*100
  
  ANABS[[c]] = anabs
  AFABS[[c]] = afabs
  ANREL[[c]] = anrel
  AFREL[[c]] = afrel

  ANSIM[[c]] = ansim
}
end_time <- Sys.time()
end_time - start_time


# Save ansim to estimate total (across the 3 age groups) AF
ansim <- do.call(rbind, lapply(ANSIM, as.data.frame))
ansim = ansim %>%
  mutate(period = substr(row.names(.), 1, 9),
         id=rep(seq(1, length(unique(deathspercity$nsalid1))), each = 2))%>%
  left_join(., bec, by="id")%>%
  dplyr::select(id, nsalid1, L1Name, Country, period, 1:20020)


df <- do.call(rbind, lapply(DEATHDENOM, as.data.frame))%>%
  mutate(id = 1:length(unique(deathspercity$nsalid1)))
df=df%>%
  mutate(ndf = df[,1])%>%
  left_join(., bec, by="id")%>%
  dplyr::select(id, nsalid1, ndf)

dh <- do.call(rbind, lapply(DEATHDENOMH, as.data.frame))%>%
  mutate(id = 1:length(unique(deathspercity$nsalid1)))
dh=dh%>%
  mutate(ndh = dh[,1])%>%
  left_join(., bec, by="id")%>%
  dplyr::select(id, nsalid1, ndh)



# REFORMAT THE AFS AND ANS AND STORE THEM
afs <- do.call(rbind, lapply(AFABS, as.data.frame))
afs = afs %>%
  mutate(period = substr(row.names(.), 1, 9),
         id=rep(seq(1, length(unique(deathspercity$nsalid1))), each = 2))%>%
  left_join(., bec, by="id")%>%
  dplyr::select(id, nsalid1, L1Name, Country, period, 1:30)

afrels <- do.call(rbind, lapply(AFREL, as.data.frame))
afrels = afrels %>%
  mutate(period = substr(row.names(.), 1, 9),
         id=rep(seq(1, length(unique(deathspercity$nsalid1))), each = 2))%>%
  left_join(., bec, by="id")%>%
  dplyr::select(id, nsalid1, L1Name, Country, period, 1:30)%>%
  filter(!period == '2045-2054')

ans <- do.call(rbind, lapply(ANABS, as.data.frame))
ans = ans %>%
  mutate(period = substr(row.names(.), 1, 9),
         id=rep(seq(1, length(unique(deathspercity$nsalid1))), each = 2))%>%
  left_join(., bec, by="id")%>%
  dplyr::select(id, nsalid1, L1Name, Country, period, 1:30)

anrels <- do.call(rbind, lapply(ANREL, as.data.frame))
anrels = anrels %>%
  mutate(period = substr(row.names(.), 1, 9),
         id=rep(seq(1, length(unique(deathspercity$nsalid1))), each = 2))%>%
  left_join(., bec, by="id")%>%
  dplyr::select(id, nsalid1, L1Name, Country, period, 1:30)%>%
  filter(!period == '2045-2054')



write.csv(ansim, "age_specific/ansim65+.csv", row.names = F)
write.csv(afs, "age_specific/afabs65+.csv", row.names = F)
write.csv(afrels, "age_specific/afrels65+.csv", row.names = F)
write.csv(ans, "age_specific/anabs65+.csv", row.names = F)
write.csv(anrels, "age_specific/anrels65+.csv", row.names = F)

write.csv(df, "age_specific/df65.csv", row.names = F)
write.csv(dh, "age_specific/dh65.csv", row.names = F)


rm(list = ls())

