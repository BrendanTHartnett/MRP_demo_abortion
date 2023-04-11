library(dplyr)
library(tidyverse)
library(tidycensus)
library(tidyr)
library(tibble)
library(usmap)
library(purrr)

head(tidycensus::fips_codes)
us <- unique(fips_codes$state)[1:51]

#Create state targets (all)
my_states <- fips_codes %>%
  filter(state %in% us)


#Get AgeXGender data 
age_vars <- c(
  over45 = "B29001_004",
  over65 = "B29001_005"
)

age_dat <- map_dfr(
  my_states,
  ~ get_acs(
    geography = "state",
    variables = age_vars,
    state = us,
    summary_var = "B29001_001" #total citizen voting age pop
  )
)

age_dat = unique(age_dat)

agedat <- age_dat %>%
  group_by(NAME) %>%
  summarize(over45N = sum(estimate, na.rm=TRUE),
            total = summary_est)

age.dat = unique(agedat)
age.dat$over45 = age.dat$over45N/age.dat$total

age_data = age.dat[, c("NAME", "over45")]


#Get HHI data
hhi_vars <- c(
  less100k = "B19001_013", #100k
  less125k = "B19001_014", #125k
  less150k = "B19001_015", # 150k
  less200k = "B19001_016", #200k
  over200k = "B19001_017" # +200k+
)

hhi_dat <- map_dfr(
  my_states,
  ~ get_acs(
    geography = "state",
    variables = hhi_vars,
    state = us,
    summary_var = "B19001_001" 
  )
)

hhi_dat = unique(hhi_dat)
hhidat <- hhi_dat %>%
  group_by(NAME) %>%
  summarize(over100kN = sum(estimate, na.rm=TRUE),
            total = summary_est)
hhi.dat = unique(hhidat)
hhi.dat$over100k = hhi.dat$over100kN/hhi.dat$total
hhi.data = hhi.dat[, c("NAME", "over100k")]


#Get Labor Force data 
lab_dat <- map_dfr(
  my_states,
  ~ get_acs(
    geography = "state",
    variables = "B23025_007", #not in the labor force
    state = us,
    summary_var = "B23025_001"
  )
)

lab_dat$not_in_labor_force = lab_dat$estimate/lab_dat$summary_est
labor.data = lab_dat[c(1:51), c("GEOID", "NAME", "not_in_labor_force")]
head(labor.data)


#Get white no college data
edu_vars_gen <- c(
  Mnone = "B15002_003",
  M4 = "B15002_004",
  M6 = "B15002_005",
  M8 = "B15002_006",
  M9 = "B15002_007",
  M10 = "B15002_008",
  M11 = "B15002_009",
  M12 = "B15002_010",
  MHS = "B15002_011",
  MC1 = "B15002_012",
  MC2 = "B15002_013",
  MAss = "B15002_014",
  MBachelors = "B15002_015",
  MMasters = "B15002_016",
  MProf = "B15002_017",
  MDr = "B15002_018",
  Fnone = "B15002_020",
  F4 = "B15002_021",
  F6 = "B15002_022",
  F8 = "B15002_023",
  F9 = "B15002_024",
  F10 = "B15002_025",
  F11 = "B15002_026",
  F12 = "B15002_027",
  FHS = "B15002_028",
  FC1 = "B15002_029",
  FC2 = "B15002_030",
  FAss = "B15002_031",
  FBachelors = "B15002_032",
  FMasters = "B15002_033",
  FProf = "B15002_034",
  FDr = "B15002_035"
)

edu_dat <- map_dfr(
  my_states,
  ~ get_acs(
    geography = "state",
    variables = edu_vars_gen, 
    state = us,
  )
)

edudat = edu_dat[, c(2:4)]
edudat = unique(edudat)
edu.dat <- edudat %>%
  pivot_wider(names_from = variable, values_from = estimate)

edu_dat <- edu.dat

edu_dat$hsmax = 0
edu_dat$hsmax = edu_dat$Fnone +  edu_dat$F4 + edu_dat$F6 + edu_dat$F8 + edu_dat$F9 + edu_dat$F10 + edu_dat$F11 + edu_dat$F12 + edu_dat$FHS + edu_dat$Mnone +  edu_dat$M4 + edu_dat$M6 + edu_dat$M8 + edu_dat$M9 + edu_dat$M10 + edu_dat$M11 + edu_dat$M12 + edu_dat$MHS

edu_dat$base = edu_dat$FC1 + edu_dat$FC2 + edu_dat$FAss + edu_dat$FBachelors + edu_dat$FProf + edu_dat$FDr + edu_dat$FMasters + edu_dat$MC1 + edu_dat$MC2 + edu_dat$MAss + edu_dat$MBachelors + edu_dat$MProf + edu_dat$MDr + edu_dat$MMasters + edu_dat$hsmax

edu_dat$highschool_only = edu_dat$hsmax/edu_dat$base

edu_data = edu_dat[, c("NAME", "highschool_only")]


#Get black and Hispanic data
race_vars <- c(
  blackman18 = "B05003B_008",
  blackman18_noncit = "B05003B_012",
  blackwoman18 = "B05003B_019",
  blackwoman18_noncit = "B05003B_023",
  hispman18 = "B05003I_008",
  hispman18_noncit = "B05003I_012",
  hispwoman18 = "B05003I_019",
  hispwoman18_noncit = "B05003I_023",
  M18 = "B05003_008", #Male 18 + 
  M18noncit = "B05003_012", #Male non-citizen 18+
  F18 = "B05003_019", #Female 18 + 
  F18noncit = "B05003_023"# Female non-citizen 18+
)

race_dat <- map_dfr(
  my_states,
  ~ get_acs(
    geography = "state",
    variables = race_vars, 
    state = us
  )
)
head(race_dat)
race.dat = race_dat[, c(2:4)]
head(race.dat)
race.dat = unique(race.dat)
racedat <- race.dat %>%
  pivot_wider(names_from = variable, values_from = estimate)
head(racedat)

racedat$blackN = racedat$blackman18 + racedat$blackwoman18 - racedat$blackman18_noncit - racedat$blackwoman18_noncit

racedat$hispanicN = racedat$hispman18 + racedat$hispwoman18 - racedat$hispman18_noncit - racedat$hispwoman18_noncit

racedat$totalVAPcitizens = racedat$M18 + racedat$F18 - racedat$M18noncit - racedat$F18noncit

racedat$black = racedat$blackN/racedat$totalVAPcitizens
racedat$hispanic = racedat$hispanicN/racedat$totalVAPcitizens
race_data = racedat[, c("NAME", "black", "hispanic")]
totalvap = racedat [, c("NAME", "totalVAPcitizens")]


#dataframes to merge 
#race_data, white_no_college_data, hhi.data, age_data, labor.data
merge1 = merge(labor.data, age_data, by="NAME")
merge2 = merge(merge1, hhi.data, by="NAME")
merge3 = merge(merge2, edu_data, by="NAME")
merge4 = merge(merge3, race_data, by="NAME")
merge5 = merge(merge4, totalvap, by="NAME")

Census <-  merge5

#Merge with statetofips.csv to convert FIPS to a 1-50 alphabetical index. 
statesfips <- read_csv("https://raw.githubusercontent.com/BrendanTHartnett/MRP_demo_abortion/main/fipstostates.csv")

Census$GEOID <- as.numeric(Census$GEOID)
Census <- merge(Census, statesfips, by.x="GEOID", by.y="fips")


write.csv(Census, "state_census_data.csv")

