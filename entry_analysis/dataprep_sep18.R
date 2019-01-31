#### dataprep for ivprobit analyses ####
## includes both conflict-year & episodes

#library(tidyverse)
library(dplyr)
library(tidyr)
library(readr)
library(haven)
library(readxl)
library(WDI)
library(countrycode)

load("~/Dropbox/Civil War Data/master_rebel_yearly_jan19.Rdata")

# revert to old column names
ucdp.dyad <- rename(ucdp.dyad, DyadId=dyad_id, ConflictId=conflict_id, SideBID=side_b_id, Year=year, Location=location, GWNoLoc=gwno_loc, SideA=side_a, GWNoA=gwno_a, SideA2nd=side_a_2nd, GWNoA2nd=gwno_a_2nd, SideB=side_b, SideB2nd=side_b_2nd, Incompatibility=incompatibility, TerritoryName=territory, IntensityLevel=intensity, TypeOfConflict=type)

ucdp.dyad$GWNoA <- as.numeric(as.character(ucdp.dyad$GWNoA))
ucdp.dyad$GWNoLoc <- as.numeric(as.character(ucdp.dyad$GWNoLoc))
ucdp.dyad$start_date <- as.character(ucdp.dyad$start_date)
ucdp.dyad$start_date2 <- as.character(ucdp.dyad$start_date2)


#remove cases with ambiguous actor data (these are generic 'Palestinian Insurgents', etc)
ucdp.dyad$imprecise <- 0
ucdp.dyad$imprecise[ucdp.dyad$SideB=="Palestinian insurgents"] <- 1
ucdp.dyad$imprecise[ucdp.dyad$SideB=="Taiwanese insurgents"] <- 1
ucdp.dyad$imprecise[ucdp.dyad$SideB=="Sikh insurgents"] <- 1
ucdp.dyad$imprecise[ucdp.dyad$SideB=="Kashmir insurgents"] <- 1
ucdp.dyad$imprecise[ucdp.dyad$SideB=="Patani insurgents"] <- 1
ucdp.dyad$imprecise[ucdp.dyad$SideB=="Syrian insurgents"] <- 1
ucdp.dyad$imprecise[ucdp.dyad$SideB=="al-Qaida" & ucdp.dyad$GWNoLoc==2] <- 1


# I. Create Conflict Episodes ----------
ep <- ucdp.dyad %>%
  group_by(ConflictId, Year) %>%
  summarize()

ep <- ep %>%
  group_by(ConflictId) %>%
  mutate(lag_Year=lag(Year), min_Year=min(Year))

ep$new_ep <- ifelse((ep$Year - ep$lag_Year) > 3 | ep$Year==ep$min_Year, 1, 0)

ep <- ep %>%
  group_by(ConflictId) %>%
  mutate(epnum=cumsum(new_ep))

ep$conflict_ep <- paste(ep$ConflictId, ep$epnum, sep="-")

ucdp.dyad <- left_join(ucdp.dyad, ep)

ucdp.dyad <- select(ucdp.dyad, -lag_Year, -min_Year, -delete)

rm(ep)


# 1. Code DVs ---------------

## Code ethnic status DV

ucdp.dyad$monoeth <- ifelse(ucdp.dyad$tot_eth==1, 1, 0)
ucdp.dyad$multieth <- ifelse(ucdp.dyad$tot_eth>1, 1, 0)
ucdp.dyad$noneth <- ifelse(ucdp.dyad$tot_eth==0, 1, 0)

## Code joiner / splinter / alliance DV

# Then code new groups
ucdp.dyad$new.joiner <- ifelse(ucdp.dyad$rebel_age==0 & ucdp.dyad$new_ep==0 & ucdp.dyad$origin!="splinter" & ucdp.dyad$origin!="alliance" , 1, 0)
ucdp.dyad$new.splinter <- ifelse(ucdp.dyad$rebel_age==0 & ucdp.dyad$origin=="splinter", 1, 0)
ucdp.dyad$new.alliance <- ifelse(ucdp.dyad$rebel_age==0 & ucdp.dyad$origin=="alliance", 1, 0)
ucdp.dyad$new.mono.alliance <- ifelse(ucdp.dyad$new.alliance==1 & ucdp.dyad$monoeth==1, 1, 0)
ucdp.dyad$new.multi.alliance <- ifelse(ucdp.dyad$new.alliance==1 & ucdp.dyad$multieth==1, 1, 0)

# Code general entry status
ucdp.dyad$splinter <- ifelse(ucdp.dyad$origin=="splinter", 1, 0)
ucdp.dyad$alliance <- ifelse(ucdp.dyad$origin=="alliance", 1, 0)
ucdp.dyad <- ucdp.dyad %>% group_by(DyadId) %>% mutate(joiner=ifelse(max(new.joiner)==1, 1, 0))
ucdp.dyad$originator <- ifelse(ucdp.dyad$splinter==0 & ucdp.dyad$alliance==0 & ucdp.dyad$joiner==0, 1, 0)


# 2. IV 1: Merge in Human Protection Scores ------
hrp <- read_csv("entry_analysis/HumanRightsProtectionScores_v2.04.csv")

hrp <- rename(hrp, Year = YEAR, GWNoA = COW)

hrp <- select(hrp, Year, GWNoA, latentmean)

hrp <- hrp %>% 
  group_by(GWNoA) %>% 
  arrange(Year) %>% 
  mutate(latentmean_lag=lag(latentmean), latentmean_lag2=lag(latentmean,2))

hrp$latentmean_diff <- hrp$latentmean_lag - hrp$latentmean_lag2

ucdp.dyad <- left_join(ucdp.dyad, hrp)

rm(hrp)


# 2b. NAVCO repression ----
navco <- read_excel("entry_analysis/NAVCO v2.0.xlsx")

navco <- select(navco, Year=year, GWNoA=lccode, navco_prim_method=prim_method, navco_repression=repression)

navco[navco < 0] <- NA

#summarize to country-years
navco <- navco %>% 
  group_by(GWNoA, Year) %>% 
  summarize(navco_campaigns=n(), navco_nonviolent_campaigns=sum(navco_prim_method==1), navco_violent_campaigns=sum(navco_prim_method==0), navco_repression=max(navco_repression))

navco$navco_repression[is.na(navco$navco_repression)] <- 0
navco$navco_campaigns[is.na(navco$navco_campaigns)] <- 0
navco$navco_violent_campaigns[is.na(navco$navco_violent_campaigns)] <- 0
navco$navco_nonviolent_campaigns[is.na(navco$navco_nonviolent_campaigns)] <- 0

navco <- navco %>% 
  group_by(GWNoA) %>% 
  arrange(Year) %>% 
  mutate(navco_repression_lag=lag(navco_repression))

ucdp.dyad <- left_join(ucdp.dyad, navco)
rm(navco)


# 3. IV 2: Ethnolinguistic fractionalization -------
fearon <- read_dta("entry_analysis/repdata.dta")

fearon <- select(fearon, GWNoA = ccode, Year = year, mtnest, Oil, ethfrac, relfrac)

ucdp.dyad <- left_join(ucdp.dyad, fearon)

rm(fearon)

ucdp.dyad <- ucdp.dyad %>%
  group_by(DyadId) %>%
  fill(ethfrac, mtnest)

ucdp.dyad <- ucdp.dyad %>%
  group_by(DyadId) %>%
  fill(ethfrac, mtnest, .direction = "up")


# 4. IV 2a: EPR groups --------
epr <- read_csv("entry_analysis/EPR-2018.csv")

epr <- epr %>%
  rowwise() %>%
  do(data.frame(GWNoA=.$gwid, Year=seq(.$from, .$to), gwgroupid=.$gwgroupid, size=.$size, status=.$status, reg_aut=.$reg_aut))

epr$status_recode <- recode(epr$status, "MONOPOLY" = "absolute", "DOMINANT" = "absolute", "SENIOR PARTNER" = "power-sharing", "JUNIOR PARTNER" = "power-sharing", "DISCRIMINATED" = "excluded", "POWERLESS" = "excluded", "IRRELEVANT" = "excluded", "SELF-EXCLUSION" = "excluded")

#aggregate to country years
epr <- epr %>%
  group_by(GWNoA, Year) %>%
  summarize(country_ethnic=n_distinct(gwgroupid), country_excluded=sum(status_recode=="excluded", na.rm=T))

ucdp.dyad <- left_join(ucdp.dyad, epr)

rm(epr)


# 5a. Instrument: Oil Revenue -------

##oil rents per capita (Ross measure)
rents <- WDI(country = "all", indicator = "NY.GDP.PETR.RT.ZS", start=1960, end=2015) #% of GDP
gdppc <- WDI(country="all", indicator="NY.GDP.PCAP.CD", start=1960, end=2015)

rents <- left_join(rents, gdppc)

#multiplyin oil as % of GDP by gdppc should produce oilpc
rents$oilpc <- rents$NY.GDP.PETR.RT.ZS * rents$NY.GDP.PCAP.CD

rents$oilpc <- log(rents$oilpc + 1)

rents$GWNoA <- countrycode(rents$iso2c, "iso2c", "cown")

rents <- select(rents, GWNoA, Year=year, oilpc)

ucdp.dyad <- left_join(ucdp.dyad, rents)

rm(rents, gdppc)

## Oil as % of merch exports
fuel <- WDI(country="all", indicator="TX.VAL.FUEL.ZS.UN", start=1960, end=2015)

fuel$GWNoA <- countrycode(fuel$iso2c, "iso2c", "cown")

fuel <- select(fuel, GWNoA, Year=year, oil.pc.exports=TX.VAL.FUEL.ZS.UN)

ucdp.dyad <- left_join(ucdp.dyad, fuel)

rm(fuel)


# 5b. Instrument: Youth Bulge ------
bulge <- read_csv("entry_analysis/youth_bulge_updated.csv")

bulge <- bulge %>% 
  group_by(GWNoA) %>% 
  mutate(youth_bulge_lag=lag(youth_bulge))

bulge <- bulge %>% 
  group_by(GWNoA, Year) %>% 
  summarize_all(.funs = "last")

ucdp.dyad <- left_join(ucdp.dyad, bulge)

rm(bulge)


# 6. Controls: Pop & GDP  -----
gdp <- read_delim("entry_analysis/expgdpv6.0/gdpv6.txt", delim="\t")

gdp <- select(gdp, GWNoA = statenum, Year = year, pop, realgdp, rgdppc)

ucdp.dyad <- left_join(ucdp.dyad, gdp)

rm(gdp)


# 7. Controls: Area ------
area <- WDI(country="all", indicator="AG.LND.TOTL.K2", start=1960, end=2015)

area$GWNoA <- countrycode(area$iso2c, "iso2c", "cown")

area <- select(area, GWNoA, Year=year, area=AG.LND.TOTL.K2)

ucdp.dyad <- left_join(ucdp.dyad, area)

#fill pre-1960
ucdp.dyad <- ucdp.dyad %>%
  group_by(DyadId) %>%
  fill(area, .direction=c("up"))

rm(area)


# 8. Controls: Polity -----
polity <- read_excel("entry_analysis/p4v2017.xls")

polity <- select(polity, GWNoA = ccode, Year = year, polity2, xconst)

polity[polity < -10] <- NA

ucdp.dyad <- left_join(ucdp.dyad, polity)

rm(polity)


# 9. Control: Mediation -----
# mediation <- read_excel("entry_analysis/CWM Data, August 2014.xlsx")
# 
# mediation <- filter(mediation, `Med_yes/no`==1)
# 
# mediation <- select(mediation, country, `med begins`, `med ends`, `episode end date`)
# 
# mediation$byear <- str_extract(mediation$med.begins, "\\d\\d\\d\\d")
# mediation$eyear <- str_extract(mediation$med.ends, "\\d\\d\\d\\d")
# mediation$eyear <- ifelse(is.na(mediation$eyear), str_extract(mediation$episode.end.date, "\\d\\d\\d\\d"), mediation$eyear)
# 
# mediation <- filter(mediation, !is.na(eyear) & !is.na(byear))
# 
# mediation$GWNoA <- countrycode(mediation$country, "country.name", "cown")
# 
# mediation <- mediation %>%
#   rowwise() %>%
#   do(data.frame(GWNoA=.$GWNoA, Year=seq(as.numeric(.$byear), as.numeric(.$eyear)), mediation=1)) %>%
#   arrange(GWNoA, Year)
# 
# mediation$dup <- ifelse(duplicated(mediation)==T, 1, 0)
# 
# mediation <- subset(mediation, dup==0)
# 
# mediation <- select(mediation, -dup)
# 
# ucdp.dyad <- left_join(ucdp.dyad, mediation)
# 
# ucdp.dyad$mediation[is.na(ucdp.dyad$mediation)] <- 0
# 
# rm(mediation)


# 11. Control: External Support -------
# support <- read_excel("entry_analysis/extsup_large.xls")
# 
# support <- select(support, Year = ywp_year, SideBID = actorID, external_exists, external_type_code, external_name)
# 
# support$external_exists[support$external_exists < 0] <- NA
# 
# # aggregated to group-years
# support <- support %>%
#   group_by(SideBID, Year) %>%
#   summarize(external_supporters=sum(external_exists, na.rm=T))
# 
# ucdp.dyad <- left_join(ucdp.dyad, support)
# 
# ucdp.dyad$external_supporters[ucdp.dyad$Year > 1974 & is.na(ucdp.dyad$external_supporters)] <- 0
# 
# rm(support)


# 12. Control: Lootable resources ------
loot <- read_csv("entry_analysis/all_resources_country.csv")

loot <- rename(loot, GWNoA = COWCODE)

loot$loot.dia <- ifelse(!is.na(loot$diamond.sites), 1, 0)
loot$loot.gem <- ifelse(!is.na(loot$gem.sites), 1, 0)
loot$loot.gold <- ifelse(!is.na(loot$gold.sites), 1, 0)
loot$loot.oil <- ifelse(!is.na(loot$oil.sites), 1, 0)
loot$loot.drugs <- ifelse(!is.na(loot$drug.sites), 1, 0)
loot$lootable <- ifelse(loot$loot.dia==1 | loot$loot.gem==1 | loot$loot.gold==1 | loot$loot.oil==1 | loot$loot.drugs==1, 1, 0)

loot <- select(loot, GWNoA, loot.dia, loot.gem, loot.gold, loot.oil, loot.drugs, lootable, tot.resource.sites)

ucdp.dyad <- left_join(ucdp.dyad, loot)

rm(loot)


# 13. Battle-related deaths -------
# brd <- read_csv("entry_analysis/ucdp-brd-dyadic-181.csv")
# 
# # switch to old numbering system
# trans <- read_csv("entry_analysis/translate_dyad.csv")
# trans <- select(trans, dyad_id = new_id, DyadId = old_id)
# brd <- left_join(brd, trans)
# 
# brd <- select(brd, DyadId, Year=year, BdBest=bd_best)
# 
# ucdp.dyad <- left_join(ucdp.dyad, brd)
# 
# rm(brd, trans)


# 14. Contiguous conflicts --------
cont <- read_csv("entry_analysis/contdird.csv")

#limit to land contiguity
cont <- filter(cont, conttype==1)

cont <- select(cont, GWNoA = state1no, state2 = state2no, Year = year, conttype)

#load ucdp data and aggregate to country years
#keep all conflict types
ucdp.prio.acd <- read_csv("entry_analysis/ucdp-prio-acd-181.csv")

#ucdp.prio.acd <- select(ucdp.prio.acd, ConflictId=conflict_id, Year=year, Location=location, GWNoLoc=gwno_loc, SideA=side_a, GWNoA=gwno_a, SideA2nd=side_a_2nd, GWNoA2nd=gwno_a_2nd, SideB=side_b, SideBID=side_b_id, SideB2nd=side_b_2nd, Incompatibility=incompatibility, TerritoryName=territory_name, IntensityLevel=intensity_level, TypeOfConflict=type_of_conflict)

ucdp.prio.acd <- separate_rows(ucdp.prio.acd, gwno_loc, sep=", ")

ucdp.prio.acd <- ucdp.prio.acd %>%
  group_by(gwno_loc, year) %>%
  summarize(cont_interstate=sum(type_of_conflict==2), cont_civil=sum(type_of_conflict!=2), cont_maxint=max(intensity_level))

ucdp.prio.acd <- rename(ucdp.prio.acd, state2 = gwno_loc, Year = year)

ucdp.prio.acd$state2 <- as.numeric(as.character(ucdp.prio.acd$state2))

cont <- left_join(cont, ucdp.prio.acd)

rm(ucdp.prio.acd)

#aggregate
cont <- cont %>%
  group_by(GWNoA, Year) %>%
  summarize(cont_interstate=sum(cont_interstate, na.rm=T), tot_cont_civil=sum(cont_civil, na.rm=T), conttype=first(conttype))

cont$cont_civil <- ifelse(cont$tot_cont_civil > 1, 1, 0)

ucdp.dyad <- left_join(ucdp.dyad, cont)

rm(cont)


##### II. Conflict-Year Output #######

# conflict_years <- ucdp.dyad %>% 
#   group_by(ConflictId, Year) %>% 
#   summarize(GWNoLoc=first(GWNoLoc), n_rebels=n_distinct(SideBID), new_ep=max(new_ep), epnum=max(epnum), new.joiner=ifelse(sum(new.joiner, na.rm=T)>0,1,0), new.splinter=sum(new.splinter, na.rm=T), new.alliance=sum(new.alliance, na.rm=T), splinter=sum(splinter, na.rm=T), alliance=sum(alliance, na.rm=T), alliance=sum(alliance, na.rm=T), joiner=sum(joiner, na.rm=T), originator=sum(originator, na.rm=T), territory=ifelse(min(Incompatibility)==1, 1, 0), maxint=max(IntensityLevel), TypeOfConflict=first(TypeOfConflict), prev_transnational=max(rebpresosts_bin[rebel_age > 0]), prev_largestreb=max(rebestimate[rebel_age > 0]), tot_eth=max(tot_eth), upgrade=max(upgrade), downgrade=max(downgrade), latentmean=min(latentmean), latentsd=min(latentsd), navco_campaigns=max(navco_campaigns), navco_repression=max(navco_repression), mtnest=max(mtnest), Oil=max(Oil), ethfrac=max(ethfrac), relfrac=max(relfrac), oilpc=max(oilpc), oil.pc.exports=max(oil.pc.exports), youth_bulge=max(youth_bulge), lpop=log(max(pop)), lgdp=log(max(rgdppc)), larea=log(max(area)), polity2=max(polity2), xconst=max(xconst), tot_cont_civil=max(tot_cont_civil), cont_civil=max(cont_civil))
# 
# # create some lagged variables
# conflict_years <- conflict_years %>%
#   group_by(ConflictId) %>%
#   mutate(latentmean_lag=lag(latentmean), latentmean_lag2=lag(latentmean,2), lagged_rebels=lag(n_rebels), youth_bulge_lag=lag(youth_bulge), youth_bulge_lag2=lag(youth_bulge,2))
# 
# 
# #recodes
# conflict_years$multireb <- ifelse(conflict_years$lagged_rebels>1, 1, 0)
# conflict_years$postcw <- ifelse(conflict_years$Year > 1989, 1, 0)
# conflict_years$repression <- - conflict_years$latentmean
# conflict_years$GWNoLoc <- as.numeric(conflict_years$GWNoLoc)
# 
# conflict_years <- subset(conflict_years, !is.na(GWNoLoc))
# 
# 
# #write csv
# write.csv(conflict_years, "entry_analysis/conflict_year.csv", row.names = F)
# 
# #write dta
# names(conflict_years) <- gsub("\\.", "_" , names(conflict_years))
# 
# haven::write_dta(conflict_years, "entry_analysis/conflict_year.dta", version=13)
# 
# rm(conflict_years)


#### III. Country-Year Output ####

country_years <- ucdp.dyad %>% 
  filter(imprecise == 0) %>% 
  group_by(GWNoA, Year) %>% 
  summarize(n_conflicts=n_distinct(ConflictId), n_rebels=n_distinct(SideBID), new_ep=max(new_ep), epnum=max(epnum), new.joiner=ifelse(sum(new.joiner, na.rm=T)>0,1,0), new.splinter=sum(new.splinter, na.rm=T), new.alliance=sum(new.alliance, na.rm=T), splinter=sum(splinter, na.rm=T), alliance=sum(alliance, na.rm=T), alliance=sum(alliance, na.rm=T), joiner=sum(joiner, na.rm=T), originator=sum(originator, na.rm=T), territory=ifelse(sum(Incompatibility==1)>0, 1, 0), govconflict=ifelse(sum(Incompatibility==2)>0,1,0), maxint=max(IntensityLevel), TypeOfConflict=first(TypeOfConflict), prev_transnational=max(rebpresosts_bin[rebel_age > 0]), prev_largestreb=max(rebestimate[rebel_age > 0]), tot_eth=max(tot_eth), upgrade=max(upgrade), downgrade=max(downgrade), latentmean=min(latentmean), navco_campaigns=max(navco_campaigns), navco_repression=max(navco_repression), mtnest=max(mtnest), Oil=max(Oil), ethfrac=max(ethfrac), relfrac=max(relfrac), oilpc=max(oilpc), oil.pc.exports=max(oil.pc.exports), youth_bulge=max(youth_bulge), lpop=log(max(pop)), lgdp=log(max(rgdppc)), larea=log(max(area)), polity2=max(polity2), xconst=max(xconst), tot_cont_civil=max(tot_cont_civil), cont_civil=max(cont_civil))

# create some lagged variables
country_years <- country_years %>%
  group_by(GWNoA) %>%
  arrange(Year) %>% 
  mutate(latentmean_lag=lag(latentmean), latentmean_lag2=lag(latentmean,2), lagged_rebels=lag(n_rebels), youth_bulge_lag=lag(youth_bulge), youth_bulge_lag2=lag(youth_bulge,2), n_conflicts_lag=lag(n_conflicts), downgrade_lag=lag(downgrade), navco_repression_lag=lag(navco_repression))


#recodes
country_years$multireb <- ifelse(country_years$lagged_rebels>1, 1, 0)
country_years$postcw <- ifelse(country_years$Year > 1989, 1, 0)
country_years$repression <- - country_years$latentmean
country_years$GWNoLoc <- as.numeric(country_years$GWNoLoc)
country_years$new_conflict <- ifelse(country_years$n_conflicts > country_years$n_conflicts_lag, 1, 0)
country_years$new.joiner <- ifelse(country_years$new.joiner==1 & country_years$new_conflict==0, 1, 0)
country_years$latentmean_diff <- country_years$latentmean_lag - country_years$latentmean_lag2

#country_years <- subset(country_years, !is.na(GWNoLoc))


#write csv
write.csv(country_years, "entry_analysis/country_year.csv", row.names = F)

#write dta
country_years_stata <- country_years
names(country_years_stata) <- gsub("\\.", "_" , names(country_years_stata))

haven::write_dta(country_years_stata, "entry_analysis/country_year.dta", version=13)

#rm(country_years)









# ucdp.dyad$latentmean_diff <- ucdp.dyad$latentmean_lag - ucdp.dyad$latentmean_lag2
# ucdp.dyad$GWNoA <- as.factor(ucdp.dyad$GWNoA)
# ucdp.dyad$yearf <- as.factor(ucdp.dyad$Year)
# ucdp.dyad$new_conflict <- ifelse(ucdp.dyad$n_conflicts > ucdp.dyad$lag_conflicts, 1, 0)
# 
# ucdp.dyad$latentmean_diff <- set_label(ucdp.dyad$latentmean_diff, "Change in Human Rights")
# ucdp.dyad$new.joiner <- set_label(ucdp.dyad$new.joiner, "New Rebel Group")
# 
# ucdp.dyad$conflictfe <- as.factor(ucdp.dyad$ConflictId)


#### IV. Episode-level output ----

# ucdp.dyad$epnum <- paste(ucdp.dyad$GWNoA, ucdp.dyad$epnum, sep="-")
# 
# episode <- ucdp.dyad %>% 
#   group_by(epnum) %>% 
#   summarize()