#### Script to aggregate dyadic rebel group data into conflict episode summaries ###

# Use this var to adjust peace years between episodes
#epwindow <- 1

# load packages
library(tidyverse)

load("~/Dropbox/Civil War Data/master_rebel_yearly_sep18.Rdata")

ucdp.dyad$GWNoA <- as.numeric(ucdp.dyad$GWNoA)

library(tsibble)

panel <- as_tsibble(ucdp.dyad, key=id(DyadId), index=Year)

# 0. Document results of window sizes ----

ep <- ucdp.dyad %>%
  ungroup() %>%
  select(GWNoA, Year) %>%
  group_by(GWNoA) %>%
  mutate(lag_Year=lag(Year), min_Year=min(Year))
  
ep$newep1 <- ifelse((ep$Year - ep$lag_Year) > 1 | ep$Year==ep$min_Year, 1, 0)
ep$newep3 <- ifelse((ep$Year - ep$lag_Year) > 3 | ep$Year==ep$min_Year, 1, 0)
ep$newep5 <- ifelse((ep$Year - ep$lag_Year) > 10 | ep$Year==ep$min_Year, 1, 0)
ep$newep10 <- ifelse((ep$Year - ep$lag_Year) > 10 | ep$Year==ep$min_Year, 1, 0)

ep <- ep %>% 
  group_by(GWNoA) %>% 
  mutate(epnum1=cumsum(newep1), epnum3=cumsum(newep3), epnum5=cumsum(newep5), epnum10=cumsum(newep10))

ep$cep1 <- paste(ep$GWNoA, ep$epnum1, sep="-")
ep$cep3 <- paste(ep$GWNoA, ep$epnum3, sep="-")
ep$cep5 <- paste(ep$GWNoA, ep$epnum5, sep="-")
ep$cep10 <- paste(ep$GWNoA, ep$epnum10, sep="-")

ep %>% 
  ungroup() %>% 
  summarize(window1=n_distinct(cep1), window3=n_distinct(cep3), window5=n_distinct(cep5), window10=n_distinct(cep10)) %>% 
  gather(windowsize, count) %>% 
  ggplot(aes(windowsize, count)) + geom_bar(stat="identity")
  

# 1. Create episode numbers ----

ep <- ucdp.dyad %>%
  group_by(ConflictId, Year) %>%
  summarize()

ep <- ep %>%
  group_by(ConflictId) %>%
  mutate(lag_Year=lag(Year), min_Year=min(Year))

ep$new_ep <- ifelse((ep$Year - ep$lag_Year) > epwindow | ep$Year==ep$min_Year, 1, 0)

ep <- ep %>%
  group_by(ConflictId) %>%
  mutate(epnum=cumsum(new_ep))

ep$conflict_ep <- paste(ep$ConflictId, ep$epnum, sep="-")


n_distinct(ep$conflict_ep)
