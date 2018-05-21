#### dataprep for ivprobit analyses ####
## includes both conflict-year & episodes

library(dplyr)
library(tidyr)
library(stringr)
library(foreign)
library(gdata)
library(readr)
library(readxl)
library(WDI)
library(countrycode)

load("master_rebel_yearly.Rdata")

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

ep$new_ep1 <- ifelse((ep$Year - ep$lag_Year) > 1 | ep$Year==ep$min_Year, 1, 0)
ep$new_ep3 <- ifelse((ep$Year - ep$lag_Year) > 3 | ep$Year==ep$min_Year, 1, 0)
ep$new_ep5 <- ifelse((ep$Year - ep$lag_Year) > 5 | ep$Year==ep$min_Year, 1, 0)

ep <- ep %>%
  group_by(ConflictId) %>%
  mutate(epnum1=cumsum(new_ep1), epnum3=cumsum(new_ep3), epnum5=cumsum(new_ep5))

ep$conflict_ep1 <- paste(ep$ConflictId, ep$epnum1, sep="-")
ep$conflict_ep3 <- paste(ep$ConflictId, ep$epnum3, sep="-")
ep$conflict_ep5 <- paste(ep$ConflictId, ep$epnum5, sep="-")

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
ucdp.dyad$new.joiner1 <- ifelse(ucdp.dyad$rebel_age==0 & ucdp.dyad$new_ep1==0 & ucdp.dyad$origin!="splinter" & ucdp.dyad$origin!="alliance" , 1, 0)
ucdp.dyad$new.joiner3 <- ifelse(ucdp.dyad$rebel_age==0 & ucdp.dyad$new_ep3==0 & ucdp.dyad$origin!="splinter" & ucdp.dyad$origin!="alliance" , 1, 0)
ucdp.dyad$new.joiner5 <- ifelse(ucdp.dyad$rebel_age==0 & ucdp.dyad$new_ep5==0 & ucdp.dyad$origin!="splinter" & ucdp.dyad$origin!="alliance" , 1, 0)

ucdp.dyad$new.splinter <- ifelse(ucdp.dyad$rebel_age==0 & ucdp.dyad$origin=="splinter", 1, 0)
ucdp.dyad$new.alliance <- ifelse(ucdp.dyad$rebel_age==0 & ucdp.dyad$origin=="alliance", 1, 0)
ucdp.dyad$new.mono.alliance <- ifelse(ucdp.dyad$new.alliance==1 & ucdp.dyad$monoeth==1, 1, 0)
ucdp.dyad$new.multi.alliance <- ifelse(ucdp.dyad$new.alliance==1 & ucdp.dyad$multieth==1, 1, 0)

# Code general entry status
ucdp.dyad$splinter <- ifelse(ucdp.dyad$origin=="splinter", 1, 0)
ucdp.dyad$alliance <- ifelse(ucdp.dyad$origin=="alliance", 1, 0)
ucdp.dyad <- ucdp.dyad %>% group_by(DyadId) %>% mutate(joiner1=ifelse(max(new.joiner1, na.rm=T)==1, 1, 0), joiner3=ifelse(max(new.joiner3, na.rm=T)==1, 1, 0), joiner5=ifelse(max(new.joiner5, na.rm=T)==1, 1, 0))
ucdp.dyad$originator1 <- ifelse(ucdp.dyad$splinter==0 & ucdp.dyad$alliance==0 & ucdp.dyad$joiner1==0, 1, 0)
ucdp.dyad$originator3 <- ifelse(ucdp.dyad$splinter==0 & ucdp.dyad$alliance==0 & ucdp.dyad$joiner3==0, 1, 0)
ucdp.dyad$originator5 <- ifelse(ucdp.dyad$splinter==0 & ucdp.dyad$alliance==0 & ucdp.dyad$joiner5==0, 1, 0)


# 2. IV 1: Merge in Human Protection Scores ------
hrp <- read.csv("entry_analysis/HumanRightsProtectionScores_v2.04.csv")

hrp <- rename(hrp, Year = YEAR, GWNoA = COW)

hrp <- select(hrp, Year, GWNoA, latentmean, latentsd)

ucdp.dyad <- left_join(ucdp.dyad, hrp)

rm(hrp)


# 3. IV 2: Ethnolinguistic fractionalization -------
fearon <- read.dta("entry_analysis/repdata.dta")

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
epr <- read.csv("entry_analysis/EPR-2014.csv")

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

# 5b. Instrument: Youth Bulge ------

bulge <- read.csv("entry_analysis/youth_bulge_updated.csv")

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
polity <- read_excel("entry_analysis/p4v2015.xls")

polity <- select(polity, GWNoA = ccode, Year = year, polity2, xconst)

polity[polity < -10] <- NA

ucdp.dyad <- left_join(ucdp.dyad, polity)

rm(polity)


# 9. Control: Mediation -----
mediation <- read.xls("entry_analysis/CWM Data, August 2014.xlsx")

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
cont <- read.csv("entry_analysis/contdird.csv")

#limit to land contiguity
cont <- filter(cont, conttype==1)

cont <- select(cont, GWNoA = state1no, state2 = state2no, Year = year)

#load ucdp data and aggregate to country years
#keep all conflict types
load("entry_analysis/ucdp-prio-acd-4-2016.RData")

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
support <- read_excel("entry_analysis/extsup_large.xls")

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

loot <- read.csv("entry_analysis/all_resources_country.csv")

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
brd <- read.csv("entry_analysis/ucdp-brd-dyadic-50-2016.csv")

brd <- select(brd, DyadId, Year, BdBest)

ucdp.dyad <- left_join(ucdp.dyad, brd)

rm(brd)


########## Aggregation ##############

# Conflict - episodes --------

episode3 <- ucdp.dyad %>%
  group_by(conflict_ep3) %>%
  summarize(GWNoA=first(GWNoA), start=min(Year), end=max(Year), Incompatibility=first(Incompatibility), n_rebels=n_distinct(SideBID), maxint=max(IntensityLevel), new.joiner=max(new.joiner3, na.rm=T), n_joiners=sum(new.joiner3, na.rm=T), new.alliance=max(new.alliance), new.splinter=max(new.splinter), latentmean=min(latentmean, na.rm=T), mtnest=max(mtnest), oil.fearon=max(Oil), oil.pc.exports=max(oil.pc.exports), oilpc=max(oilpc, na.rm=T), ethfrac=max(ethfrac), relfrac=max(relfrac), country_ethnic=max(country_ethnic), country_excluded=max(country_excluded), pop=max(pop, na.rm=T), area=max(area, na.rm=T), rgdppc=max(rgdppc, na.rm=T), polity2=max(polity2), xconst=max(xconst, na.rm=T), mediation=max(mediation), external_supporters=sum(external_supporters>0), lootable=max(lootable), tot.resource.sites=max(tot.resource.sites), BdBest=sum(BdBest), existing.support=sum(external_supporters>0 & new.joiner==0), cont_civil=max(cont_civil), new.multi.alliance=max(new.multi.alliance), new.mono.alliance=max(new.mono.alliance), tot.resource.sites=max(tot.resource.sites), youth_bulge=max(youth_bulge, na.rm=T))

#recodes
episode3$latentmean[is.infinite(episode3$latentmean)] <- NA
episode3$GWNoA <- as.factor(episode3$GWNoA)
episode3$lgdp <- log(episode3$rgdppc)
episode3$lpop <- log(episode3$pop)
episode3$larea <- log(episode3$area)
episode3$postcw <- ifelse(episode3$start > 1989, 1, 0)
episode3$duration <- episode3$end - episode3$start
episode3$repression <- -episode3$latentmean

write.csv(episode3, "entry_analysis/episode3.csv", row.names = F)



episode32 <- episode3

names(episode32) <- gsub("\\.", "_" , names(episode32))

haven::write_dta(episode32, "entry_analysis/episode3.dta", version=13)

rm(episode32)
