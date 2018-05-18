library(dplyr)
library(tidyr)
library(stringr)
library(foreign)
library(gdata)
library(readr)
library(readxl)
library(WDI)
library(countrycode)


load("~/Dropbox/Dissertation/Document/realignment/splinter_analysis/master_rebel_yearly.Rdata")

ucdp.dyad$GWNoA <- as.numeric(as.character(ucdp.dyad$GWNoA))

#remove cases with ambiguous actor data (these are generic 'Palestinian Insurgents', etc)
ucdp.dyad <- filter(ucdp.dyad, is.na(delete))

# o. Create Conflict Episodes ----------
ep <- ucdp.dyad %>%
  group_by(ConflictId, Year) %>%
  summarize()

ep <- ep %>%
  group_by(ConflictId) %>%
  mutate(lag_Year=lag(Year), min_Year=min(Year))

ep$new_ep <- ifelse((ep$Year - ep$lag_Year) > 3 | ep$Year==ep$min_Year, 1, 0)

ep <- ep %>%
  group_by(GWNoA) %>%
  mutate(epnum=cumsum(new_ep))

ep$conflict_ep <- paste(ep$GWNoA, ep$epnum, sep="-")

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
ucdp.dyad <- ucdp.dyad %>% group_by(DyadId) %>% mutate(joiner=ifelse(max(new.joiner, na.rm=T)==1, 1, 0))
ucdp.dyad$originator <- ifelse(ucdp.dyad$splinter==0 & ucdp.dyad$alliance==0 & ucdp.dyad$joiner==0, 1, 0)


# 2. IV 1: Merge in Human Protection Scores ------
hrp <- read.csv("HumanRightsProtectionScores_v2.04.csv")

hrp <- rename(hrp, Year = YEAR, GWNoA = COW)

hrp <- select(hrp, -CIRI)

ucdp.dyad <- left_join(ucdp.dyad, hrp)

rm(hrp)


# 3. IV 2: Ethnolinguistic fractionalization -------
fearon <- read.dta("repdata.dta")

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
epr <- read.csv("EPR-2014.csv")

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


# 5. Instrument: Oil Revenue -------

##oil rents per capita (Ross measure)
rents <- WDI(country = "all", indicator = "NY.GDP.PETR.RT.ZS", start=1960, end=2015) #% of GDP
gdppc <- WDI(country="all", indicator="NY.GDP.PCAP.CD", start=1960, end=2015)

rents <- left_join(rents, gdppc)

#multiplyin oil as % of GDP by gdppc should produce oilpc
rents$oilpc <- rents$NY.GDP.PETR.RT.ZS * rents$NY.GDP.PCAP.CD

rents$oilpc <- log(rents$oilpc)

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


# 6. Controls: Pop & GDP  -----
gdp <- read_delim("expgdpv6.0/gdpv6.txt", delim="\t")

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
polity <- read_excel("p4v2015.xls")

polity <- select(polity, GWNoA = ccode, Year = year, polity2, xconst)

ucdp.dyad <- left_join(ucdp.dyad, polity)

rm(polity)


# 9. Control: Mediation -----
mediation <- read.xls("CWM Data, August 2014.xlsx")

mediation <- filter(mediation, Med_yes.no==1)

mediation <- select(mediation, country, med.begins, med.ends, episode.end.date)

mediation$byear <- str_extract(mediation$med.begins, "\\d\\d\\d\\d")
mediation$eyear <- str_extract(mediation$med.ends, "\\d\\d\\d\\d")
mediation$eyear <- ifelse(is.na(mediation$eyear), str_extract(mediation$episode.end.date, "\\d\\d\\d\\d"), mediation$eyear)

mediation <- filter(mediation, !is.na(eyear) & !is.na(byear))

mediation$GWNoA <- countrycode(mediation$country, "country.name", "cown")

mediation <- mediation %>%
  rowwise() %>%
  do(data.frame(GWNoA=.$GWNoA, Year=seq(as.numeric(.$byear), as.numeric(.$eyear)), mediation=1)) %>%
  arrange(GWNoA, Year)

mediation$dup <- ifelse(duplicated(mediation)==T, 1, 0)

mediation <- subset(mediation, dup==0)

mediation <- select(mediation, -dup)

ucdp.dyad <- left_join(ucdp.dyad, mediation)

ucdp.dyad$mediation[is.na(ucdp.dyad$mediation)] <- 0

rm(mediation)


# 10. Control: Contiguous Civil Wars ------
cont <- read.csv("contdird.csv")

#limit to land contiguity
cont <- filter(cont, conttype==1)

cont <- select(cont, GWNoA = state1no, state2 = state2no, Year = year)

#load ucdp data and aggregate to country years
#keep all conflict types
load("ucdp-prio-acd-4-2016.RData")

ucdp.prio.acd <- separate_rows(ucdp.prio.acd, GWNoA, sep=", ")

ucdp.prio.acd <- ucdp.prio.acd %>%
  group_by(GWNoA, Year) %>%
  summarize(cont_interstate=sum(TypeOfConflict==2), cont_civil=sum(TypeOfConflict!=2), cont_maxint=max(IntensityLevel))

ucdp.prio.acd <- rename(ucdp.prio.acd, state2 = GWNoA)

ucdp.prio.acd$state2 <- as.numeric(as.character(ucdp.prio.acd$state2))

cont <- left_join(cont, ucdp.prio.acd)

rm(ucdp.prio.acd)

#aggregate
cont <- cont %>%
  group_by(GWNoA, Year) %>%
  summarize(cont_interstate=sum(cont_interstate, na.rm=T), cont_civil=sum(cont_civil, na.rm=T), cont_maxint=max(cont_maxint, na.rm=T))

ucdp.dyad <- left_join(ucdp.dyad, cont)

rm(cont)


# 11. Control: External Support -------
support <- read_excel("extsup_large.xls")

support <- select(support, Year = ywp_year, SideBID = actorID, external_exists, external_type_code, external_name)

support$external_exists[support$external_exists < 0] <- NA

# aggregated to group-years
support <- support %>%
  group_by(SideBID, Year) %>%
  summarize(external_supporters=sum(external_exists, na.rm=T))

ucdp.dyad <- left_join(ucdp.dyad, support)

ucdp.dyad$external_supporters[ucdp.dyad$Year > 1974 & is.na(ucdp.dyad$external_supporters)] <- 0

rm(support)


# 12. Control: Lootable resources ------

loot <- read.csv("all_resources_country.csv")

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
brd <- read.csv("ucdp-brd-dyadic-50-2016.csv")

brd <- select(brd, DyadId, Year, BdBest)

ucdp.dyad <- left_join(ucdp.dyad, brd)

rm(brd)


########## Aggregation ##############

#write dyad-years
write.csv(ucdp.dyad, "dyad_years.csv", row.names = F)

# Country - years --------

country.year <- ucdp.dyad %>%
  group_by(GWNoA, Year) %>%
  summarize(n_rebels=n_distinct(SideBID), n_conflicts=n_distinct(ConflictId), maxint=max(IntensityLevel), new.joiner=max(new.joiner), new.alliance=max(new.alliance), new.splinter=max(new.splinter), latentmean=min(latentmean), mtnest=max(mtnest), oil.fearon=max(Oil), oil.pc.exports=max(oil.pc.exports), oilpc=max(oilpc), ethfrac=max(ethfrac), relfrac=max(relfrac), country_ethnic=max(country_ethnic), country_excluded=max(country_excluded), pop=max(pop), area=max(area), rgdppc=max(rgdppc), polity2=max(polity2), mediation=max(mediation), external_supporters=sum(external_supporters>0), lootable=max(lootable), tot.resource.sites=max(tot.resource.sites), BdBest=sum(BdBest), existing.support=sum(external_supporters>0 & new.joiner==0), cont_civil=max(cont_civil), new.multi.alliance=max(new.multi.alliance), new.mono.alliance=max(new.mono.alliance))

# create some lagged variables
country.year <- country.year %>%
  group_by(GWNoA) %>%
  mutate(latentmean_lag=lag(latentmean), latentmean_lag2=lag(latentmean,2) ,lagged_rebels=lag(n_rebels), oilpc_lag=lag(oilpc), oil.pc.exports_lag=lag(oil.pc.exports), lag_conflicts=lag(n_conflicts))

country.year$latentmean_diff <- country.year$latentmean_lag - country.year$latentmean_lag2
country.year$GWNoA <- as.factor(country.year$GWNoA)
country.year$lgdp <- log(country.year$rgdppc)
country.year$lpop <- log(country.year$pop)
country.year$larea <- log(country.year$area)
country.year$yearf <- as.factor(country.year$Year)
country.year$new_conflict <- ifelse(country.year$n_conflicts > country.year$lag_conflicts, 1, 0)

write.csv(country.year, "~/Dropbox/Dissertation/Document/entry_chapter/entry_analysis/country_year.csv", row.names = F)

# Conflict - years --------

conflict.year <- ucdp.dyad %>%
  group_by(ConflictId, Year) %>%
  summarize(GWNoA=first(GWNoA), Incompatibility=first(Incompatibility), n_rebels=n_distinct(SideBID), n_conflicts=n_distinct(ConflictId), maxint=max(IntensityLevel), new.joiner=max(new.joiner), new.alliance=max(new.alliance), new.splinter=max(new.splinter), latentmean=min(latentmean), mtnest=max(mtnest), oil.fearon=max(Oil), oil.pc.exports=max(oil.pc.exports), oilpc=max(oilpc, na.rm=T), ethfrac=max(ethfrac), relfrac=max(relfrac), country_ethnic=max(country_ethnic), country_excluded=max(country_excluded), pop=max(pop), area=max(area), rgdppc=max(rgdppc), polity2=max(polity2), xconst=max(xconst), mediation=max(mediation), external_supporters=sum(external_supporters>0), lootable=max(lootable), tot.resource.sites=max(tot.resource.sites), BdBest=sum(BdBest), existing.support=sum(external_supporters>0 & new.joiner==0), cont_civil=max(cont_civil), new.multi.alliance=max(new.multi.alliance), new.mono.alliance=max(new.mono.alliance), tot.resource.sites=max(tot.resource.sites))

# create some lagged variables
conflict.year <- conflict.year %>%
  group_by(ConflictId) %>%
  mutate(latentmean_lag=lag(latentmean), latentmean_lag2=lag(latentmean,2), lagged_rebels=lag(n_rebels), oilpc_lag=lag(oilpc), oil.pc.exports_lag=lag(oil.pc.exports), lag_conflicts=lag(n_conflicts))

#recodes
conflict.year$latentmean_diff <- conflict.year$latentmean_lag - conflict.year$latentmean_lag2
conflict.year$GWNoA <- as.factor(conflict.year$GWNoA)
conflict.year$lgdp <- log(conflict.year$rgdppc)
conflict.year$lpop <- log(conflict.year$pop)
conflict.year$larea <- log(conflict.year$area)
conflict.year$yearf <- as.factor(conflict.year$Year)
conflict.year$new_conflict <- ifelse(conflict.year$n_conflicts > conflict.year$lag_conflicts, 1, 0)
conflict.year$multireb <- ifelse(conflict.year$lagged_rebels>1, 1, 0)
conflict.year$latentmean_diff <- set_label(conflict.year$latentmean_diff, "Change in Human Rights")
conflict.year$new.joiner <- set_label(conflict.year$new.joiner, "New Rebel Group")
conflict.year$postcw <- ifelse(conflict.year$Year > 1989, 1, 0)
conflict.year$conflictfe <- as.factor(conflict.year$ConflictId)

write.csv(conflict.year, "~/Dropbox/Dissertation/Document/entry_chapter/entry_analysis/conflict_year.csv", row.names = F)

# Group-level -------
ucdp.dyad <- ucdp.dyad %>%
  group_by(SideBID) %>%
  mutate(duration=(max(Year) - min(Year)))

group <- ucdp.dyad %>%
  group_by(SideBID) %>%
  summarise_all(first)

write.csv(group, "~/Dropbox/Dissertation/Document/entry_chapter/entry_analysis/group.csv", row.names = F)

conflict.year2 <- conflict.year

names(conflict.year2) <- gsub("\\.", "_" , names(conflict.year2))

haven::write_dta(conflict.year2, "conflict_year.dta", version=13)

rm(conflict.year2)