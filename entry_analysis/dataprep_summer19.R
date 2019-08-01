###############################################################
#### Script to create dataset for Bowden article on rebel group formation ####
###############################################################

library(tidyverse)
library(readxl)
library(haven)
library(lubridate)
library(countrycode)
library(zoo)

##########################################
##### I. Create base conflict dataset ####
##########################################

# 1. Import and clean CDP Dyadic Conflict data v 19.1 ----

dyad <- read_csv("entry_analysis/ucdp-dyadic-191.csv")

dyad <- select(dyad, -version)

# filter out extrasystemic and interstate conflicts
dyad <- filter(dyad, type_of_conflict > 2)

# convert to numeric for later merging
dyad$side_b_id <- as.numeric(dyad$side_b_id)

#remove cases with ambiguous actor data (these are generic 'Palestinian Insurgents', etc)
dyad$imprecise <- 0
dyad$imprecise[dyad$side_b=="Palestinian insurgents"] <- 1
dyad$imprecise[dyad$side_b=="Taiwanese insurgents"] <- 1
dyad$imprecise[dyad$side_b=="Sikh insurgents"] <- 1
dyad$imprecise[dyad$side_b=="Kashmir insurgents"] <- 1
dyad$imprecise[dyad$side_b=="Patani insurgents"] <- 1
dyad$imprecise[dyad$side_b=="Syrian insurgents"] <- 1
dyad$imprecise[dyad$side_b=="al-Qaida" & dyad$gwno_loc==2] <- 1

#create dummies
dyad$internationalized <- ifelse(dyad$type_of_conflict==4, 1, 0)
dyad$territory <- ifelse(dyad$incompatibility>1, 1, 0)


# 2. Create episodes, numbers, etc. ----

# this sets the number of consecutive peace years required to a define a new episode
INTERVAL <- 3

# aggregate to conflict year
conflict <- dyad %>% 
  group_by(conflict_id, year) %>% 
  summarize()

# get lagged year
conflict <- conflict %>% 
  group_by(conflict_id) %>% 
  mutate(lag_year = lag(year), min_year=min(year))

# create indicator for new episodes - new episode if there are 3 or more consecutive years below 25 fatalities
conflict$new_ep <- ifelse((conflict$year - conflict$lag_year) > INTERVAL | conflict$year==conflict$min_year, 1, 0)

# create episode numbers
conflict <- conflict %>% 
  group_by(conflict_id) %>% 
  mutate(epnum=cumsum(new_ep))

# id nums
conflict$ep_id <- paste(conflict$conflict_id, conflict$epnum, sep="-")

# merge into dyadic data
conflict <- select(conflict, conflict_id, year, new_ep, epnum, ep_id)
dyad <- left_join(dyad, conflict)
rm(conflict, INTERVAL)


#################################
#### II. Dependent Variables ####
#################################

## IIa. Joiners ----

# first get the age of the rebel group
dyad <- dyad %>% 
  group_by(side_b_id) %>% 
  mutate(group_start=min(start_date))

dyad$rebel_age <- dyad$year - year(dyad$group_start)

# 1. My data on rebel origins ----

load("~/Dropbox/Civil War Data/master_rebel_yearly_jan19.Rdata")

ucdp.dyad <- ucdp.dyad %>% 
  group_by(side_b_id) %>% 
  summarize(origin=first(origin))

dyad <- left_join(dyad, ucdp.dyad)
rm(ucdp.dyad)

# code variable
dyad$new_joiner <- ifelse(dyad$rebel_age==0 & dyad$new_ep==0 & dyad$origin!="splinter" & dyad$origin!="alliance", 1, 0)


# 2. FORGE data on rebel origins ----

forge <- read_excel("entry_analysis/forge_v1.0_public.xlsx")

forge <- select(forge, side_b_id=actorid, foundyear, fightyear, goalindep:preorgoth, merger:splinterUCDP)

# there are a few duplicates from FORGE
forge <- filter(forge, duplicated(side_b_id)==F)

dyad <- left_join(dyad, forge)
rm(forge)

# code joiner variable
dyad$splinter[is.na(dyad$splinter)] <- 0
dyad$splinterUCDP[is.na(dyad$splinterUCDP)] <- 0

dyad$new_joiner_forge <- ifelse(dyad$year==dyad$fightyear & dyad$new_ep==0 & dyad$preorgreb==0, 1, 0)

dyad$new_joiner_forge_splinter <- ifelse(dyad$year==dyad$fightyear & dyad$new_ep==0 & dyad$splinter==0, 1, 0)


## IIb. Joiner attributes ----

# 1. FORGE identity variables ----

dyad$identity_forge <- ifelse(dyad$ethnic==1 | dyad$religion==1, 1, 0)

# 2. EPR data ----

epr <- read_csv("entry_analysis/ACD2EPR-2018.1.1.csv")

#expand to yearly
epr <- epr %>% 
  rowwise() %>% 
  do(data.frame(side_b_id=.$sideb_id, year=seq(.$from, .$to), gwgroupid=.$gwgroupid, claim=.$claim, recruitment=.$recruitment, support=.$support))

#summarize
epr <- epr %>% 
  group_by(side_b_id, year) %>% 
  summarize(tot_eth=n_distinct(gwgroupid), eth_claim=sum(claim==1 | claim==2), eth_recruitment=sum(recruitment==1 | recruitment==2), eth_support=sum(support==1 | support==2))

dyad <- left_join(dyad, epr)
rm(epr)


#### III. Independent Vars ####

## IIIa. Repression Measures ----

# 1. Latent protection scores ----

hps <- read_csv("entry_analysis/HumanRightsProtectionScores_v3.01.csv")

hps <- select(hps, gwno_loc=COW, year=YEAR, DISAP:killing_present, theta_mean:theta_sd)

# create lags
hps <- hps %>% 
  group_by(gwno_loc) %>% 
  mutate(theta_mean_lag=lag(theta_mean), theta_mean_lag2=lag(theta_mean, 2))

dyad$gwno_loc <- as.numeric(dyad$gwno_loc)

dyad <- left_join(dyad, hps)
rm(hps)


# 2. EPR prop discriminated ----

epr <- read_csv("entry_analysis/EPR-2018.1.1.csv")

# expand to yearly
epr <- epr %>% 
  rowwise() %>% 
  do(data.frame(gwno_loc=.$gwid, year=seq(.$from, .$to), gwgroupid=.$gwgroupid, size=.$size, status=.$status, reg_aut=.$reg_aut))

# create country-year summaries
epr <- epr %>% 
  group_by(gwno_loc, year) %>% 
  summarize(epr_groups=n_distinct(gwgroupid), discrim_pct = sum(size[status=="DISCRIMINATED"]), discrim_groups=sum(status=="DISCRIMINATED"), discrim_lgst=max(size[status=="DISCRIMINATED"]), excluded_pct = sum(size[status=="DISCRIMINATED" | status=="POWERLESS" | status=="SELF-EXCLUSION"]))

# remove infinity
epr$discrim_lgst[is.infinite(epr$discrim_lgst)] <- 0

# create lags
epr <- epr %>% 
  group_by(gwno_loc) %>% 
  mutate(discrim_pct_lag=lag(discrim_pct), discrim_groups_lag=lag(discrim_groups), discrim_lgst_lag=lag(discrim_lgst), excluded_pct_lag=lag(excluded_pct))

dyad <- left_join(dyad, epr)
rm(epr)


# 3. VDEM torture and killing data ----

vdem <- read_csv("entry_analysis/V-Dem-CY-Core-v9.csv")

vdem <- select(vdem, gwno_loc=COWcode, year, v2clkill, v2cltort)

# create lags
vdem <- vdem %>% 
  group_by(gwno_loc) %>% 
  mutate(v2clkill_lag=lag(v2clkill), v2cltort_lag=lag(v2cltort))

dyad <- left_join(dyad, vdem)
rm(vdem)


## IIIb. Instruments ----

# 1. Youth bulge ----

bulge <- read_csv("entry_analysis/youth_bulge_updated.csv")

bulge <- rename(bulge, gwno_loc=GWNoA, year=Year)

bulge <- bulge %>% 
  group_by(gwno_loc) %>% 
  mutate(youth_bulge_lag=lag(youth_bulge))

bulge <- bulge %>% 
  group_by(gwno_loc, year) %>% 
  summarize_all(.funs = "last")

dyad <- left_join(dyad, bulge)
rm(bulge)


#### IV. Control Vars ####

# 1. Polity ---

polity <- read_excel("entry_analysis/p4v2017.xls")

polity <- select(polity, gwno_loc=ccode, year, polity2, xconst)

# create lags
polity <- polity %>% 
  group_by(gwno_loc) %>% 
  mutate(polity2_lag=lag(polity2), polity2_lag2=lag(polity2, 2), xconst_lag=lag(xconst))

dyad <- left_join(dyad, polity)
rm(polity)


# 2. GDPpc and pop from PWT ----

library(pwt9)

pwt <- pwt9.1

pwt$gwno_loc <- countrycode(pwt$isocode, "iso3c", "cown")
pwt$gwno_loc[pwt$isocode=="SRB"] <- 345

pwt$gdppc <- pwt$rgdpe / pwt$pop

pwt <- select(pwt, gwno_loc, year, gdppc, pop)

# lag all
pwt <- pwt %>% 
  group_by(gwno_loc) %>% 
  mutate(gdppc_lag=lag(gdppc), pop_lag=lag(pop))

dyad <- left_join(dyad, pwt)
rm(pwt)


# 3. Gini ----

wiid <- read_excel("entry_analysis/WIID_19Dec2018.xlsx")

#convert country codes
wiid$gwno_loc <- countrycode(wiid$c3, "iso3c", "cown")
wiid$gwno_loc[wiid$c3=="YUG"] <- 345
wiid$gwno_loc[wiid$c3=="SRB"] <- 345
wiid$gwno_loc[wiid$c3=="SUN"] <- 365

# select relevant columns
wiid <- select(wiid, gwno_loc, year, gini=gini_reported)

# some duplicates
wiid <- wiid %>% 
  group_by(gwno_loc, year) %>% 
  summarise_all(max, na.rm=T)

is.na(wiid) <- sapply(wiid, is.infinite)

# fill in some missing values
wiid <- wiid %>% 
  group_by(gwno_loc) %>% 
  arrange(year) %>% 
  complete(year=seq(1946, 2017))

wiid <- wiid %>% 
  group_by(gwno_loc) %>% 
  do(na.locf(., maxgap = 10, na.rm=F))

wiid <- wiid %>% 
  group_by(gwno_loc) %>% 
  do(na.locf(., maxgap = 10, na.rm=F, fromLast = T))

# lag
wiid <- wiid %>% 
  group_by(gwno_loc) %>% 
  mutate(gini_lag=lag(gini))

dyad <- left_join(dyad, wiid)
rm(wiid)

# add World Development indicators version
library(WDI)

gini <- WDI(country="all", indicator="SI.POV.GINI", start=1960, end=2018)

gini$gwno_loc <- countrycode(gini$iso2c, "iso2c", "cown")

gini <- select(gini, gwno_loc, year, gini_wb=SI.POV.GINI)

# fill in some missing values
gini <- gini %>% 
  group_by(gwno_loc) %>% 
  do(na.locf(., maxgap=10, na.rm=F))

gini <- gini %>% 
  group_by(gwno_loc) %>% 
  do(na.locf(., maxgap=10, na.rm=F, fromLast = T))

# lag
gini$year <- gini$year - 1
gini <- gini %>%
  group_by(gwno_loc) %>% 
  mutate(gini_wb_lag=lag(gini_wb))

dyad <- left_join(dyad, gini)
rm(gini)

dyad$gini_lag <- ifelse(is.na(dyad$gini_lag), dyad$gini_wb_lag, dyad$gini_lag)


# 4. Fearon and Laitin controls ----

fl03 <- read_dta("entry_analysis/repdata.dta")

fl03 <- select(fl03, gwno_loc=ccode, year, mtnest, Oil, ethfrac, relfrac)

#expand to full time period 
flyr <- fl03 %>% 
  group_by(gwno_loc) %>% 
  do(data.frame(year=as.numeric(seq(1946, 2018))))

fl03 <- left_join(flyr, fl03)
rm(flyr)

fl03 <- fl03 %>% 
  group_by(gwno_loc) %>% 
  fill(ethfrac, relfrac, mtnest)

fl03 <- fl03 %>% 
  group_by(gwno_loc) %>% 
  fill(ethfrac, relfrac, mtnest, .direction = "up")

dyad <- left_join(dyad, fl03)
rm(fl03)


# 5. Resources ----

loot <- read_csv("entry_analysis/all_resources_country.csv")

loot <- rename(loot, gwno_loc= COWCODE)

loot$loot.dia <- ifelse(!is.na(loot$diamond.sites), 1, 0)
loot$loot.gem <- ifelse(!is.na(loot$gem.sites), 1, 0)
loot$loot.gold <- ifelse(!is.na(loot$gold.sites), 1, 0)
loot$loot.oil <- ifelse(!is.na(loot$oil.sites), 1, 0)
loot$loot.drugs <- ifelse(!is.na(loot$drug.sites), 1, 0)
loot$lootable <- ifelse(loot$loot.dia==1 | loot$loot.gem==1 | loot$loot.gold==1 | loot$loot.oil==1 | loot$loot.drugs==1, 1, 0)

loot <- select(loot, gwno_loc, loot.dia, loot.gem, loot.gold, loot.oil, loot.drugs, lootable, tot.resource.sites)

dyad <- left_join(dyad, loot)
rm(loot)


# 6. Contiguous conflicts ----

cont <- read_csv("entry_analysis/contdird.csv")

#limit to land contiguity
cont <- filter(cont, conttype==1)

cont <- select(cont, gwno_loc = state1no, state2 = state2no,  year, conttype)

#load ucdp data and aggregate to country years
#keep all conflict types
ucdp.prio.acd <- read_csv("entry_analysis/ucdp-prio-acd-191.csv")

#ucdp.prio.acd <- select(ucdp.prio.acd, ConflictId=conflict_id, Year=year, Location=location, GWNoLoc=gwno_loc, SideA=side_a, GWNoA=gwno_a, SideA2nd=side_a_2nd, GWNoA2nd=gwno_a_2nd, SideB=side_b, SideBID=side_b_id, SideB2nd=side_b_2nd, Incompatibility=incompatibility, TerritoryName=territory_name, IntensityLevel=intensity_level, TypeOfConflict=type_of_conflict)

ucdp.prio.acd <- separate_rows(ucdp.prio.acd, gwno_loc, sep=", ")

ucdp.prio.acd <- ucdp.prio.acd %>%
  group_by(gwno_loc, year) %>%
  summarize(cont_interstate=sum(type_of_conflict==2), cont_civil=sum(type_of_conflict!=2), cont_maxint=max(intensity_level))

ucdp.prio.acd <- rename(ucdp.prio.acd, state2 = gwno_loc)

ucdp.prio.acd$state2 <- as.numeric(as.character(ucdp.prio.acd$state2))

cont <- left_join(cont, ucdp.prio.acd)
rm(ucdp.prio.acd)

#aggregate
cont <- cont %>%
  group_by(gwno_loc, year) %>%
  summarize(cont_interstate=sum(cont_interstate, na.rm=T), tot_cont_civil=sum(cont_civil, na.rm=T), conttype=first(conttype))

cont$cont_civil <- ifelse(cont$tot_cont_civil > 1, 1, 0)

#get lagged count
cont <- cont %>% 
  group_by(gwno_loc) %>% 
  mutate(cont_civil_lag=lag(cont_civil))

dyad <- left_join(dyad, cont)
rm(cont)


# 7. Area ----

library(WDI)

area <- WDI(country="all", indicator="AG.LND.TOTL.K2", start=1960, end=2017)

area$gwno_loc <- countrycode(area$iso2c, "iso2c", "cown")

area <- select(area, gwno_loc, year, area=AG.LND.TOTL.K2)

dyad <- left_join(dyad, area)

#fill pre-1960
dyad <- dyad %>%
  group_by(dyad_id) %>%
  fill(area, .direction=c("up"))
rm(area)


#### V. Aggregate ####

## Va. Conflict-Year ----

conflict_year <- dyad %>%
  filter(imprecise==0) %>% 
  group_by(conflict_id, year) %>%
  summarize(ep_id=first(ep_id), new_joiner=max(new_joiner), new_joiner_forge=max(new_joiner_forge), territory=max(territory), internationalized=max(internationalized), intensity=max(intensity_level), theta_mean_lag=min(theta_mean_lag), theta_mean_lag2=min(theta_mean_lag2), v2clkill_lag=min(v2clkill_lag), v2cltort_lag=min(v2cltort_lag), discrim_pct_lag=max(discrim_pct_lag), polity2_lag=max(polity2_lag), polity2_lag2=max(polity2_lag2), ethfrac=max(ethfrac), mtnest=max(mtnest), cont_civil=max(cont_civil), area=max(area), pop_lag=max(pop_lag), gdppc_lag=max(gdppc_lag), n_rebels=n_distinct(dyad_id), youth_bulge_lag=max(youth_bulge_lag), xconst_lag=max(xconst_lag), epnum=max(epnum))

conflict_year <- conflict_year %>% 
  group_by(conflict_id) %>% 
  mutate(n_rebels_lag=lag(n_rebels))

# Fjelde and Nilsson variables
conflict_year$repressive_move <- ifelse(conflict_year$theta_mean_lag < conflict_year$theta_mean_lag2, 1, 0)
conflict_year$democ_move <- ifelse((conflict_year$polity2_lag - conflict_year$polity2_lag2)>1, 1, 0)

# year within episode
conflict_year <- conflict_year %>% 
  group_by(ep_id) %>% 
  mutate(ep_start=min(year))

conflict_year$epyear <- conflict_year$year - conflict_year$ep_start 

# logs
conflict_year$lgdppc_lag <- log(conflict_year$gdppc_lag)
conflict_year$larea <- log(conflict_year$area)
conflict_year$lpop_lag <- log(conflict_year$pop_lag)

write_csv(conflict_year, "entry_analysis/conflict_year_summer2019.csv")

## Vb. Country-Year ----

## Vc. Episode ----

# ep_year <- dyad %>% 
#   filter(imprecise==0) %>% 
#   group_by(ep_id) %>% 
#   summarize()

## Vd. Group ----