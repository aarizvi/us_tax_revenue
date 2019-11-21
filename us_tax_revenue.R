## mts

## mts downloaded from  https://www.fiscal.treasury.gov/reports-statements/mts/current.html


mts <- readr::read_csv("~/Downloads/mts.csv", col_types = cols(.default="c"))
library(tidyverse)
library(lubridate)
mts %>%
  mutate(Period=as.Date(Period, format="%m-%y"))

mts <- mts %>%
  select(-X8:-X10) %>%
  slice(-470:-475) %>%
  separate(Period, c("month", "year"), sep="-") %>%
  mutate(year=as.double(year),
         year=case_when(
           year < 10 ~ paste0("200", year),
           year >= 10 & year <= 19 ~ paste0("20", year),
           year > 19 ~ paste0("19", year)
         )) %>%
  unite(period, c("year", "month"), sep="-") %>%
  mutate(period=paste0(period, "-01"),
         period=as.Date(period, format="%Y-%b-%d"),
         year=year(period))

mts_p <- mts %>%
  mutate(Receipts=str_replace_all(Receipts, "r", ""),
         Receipts=as.double(str_replace_all(Receipts, ",", ""))) %>%
  group_by(year) %>%
  summarize(revenue=sum(Receipts)) %>%
  mutate(tax_cut=ifelse(year %in% c(1981, 1993, 2001, 2003, 2008, 2009, 2010, 2012, 2017), "major tax cut", "no major tax cut")) %>%
  filter(year > 1980) %>%
  mutate(dollars=revenue/1e06)
  
mts_p %>%
  ggplot(aes(year, revenue)) +
    geom_line() +
    geom_point() +
    geom_point(data={filter(mts_p, tax_cut=="major tax cut")}, aes(year, revenue), color="red") + 
    geom_point(data=gdp_p, aes(DATE, GDP), color="blue") +
    ylab("Dollars trillions") +
    ggtitle("US Tax Revenue 1981-2019 (2019 thru Oct 2019)", subtitle="Source: https://www.fiscal.treasury.gov/reports-statements/mts/current.html\nRed points are major tax cut") +
    theme_bw()





