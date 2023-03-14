##replication
#first remove the 133 rows with no district/state identifier from covid death dataset

covid_death <- covid_infected_deaths[order(covid_infected_deaths$lgd_state_id, covid_infected_deaths$lgd_district_id),]

#convert all blank cells to NA
dat <- covid_death %>% mutate_if(is.character, list(~na_if(.,""))) 

#remove all NAs
sapply(dat, function(x) sum(is.na(x)))
dat2<- dat %>% drop_na()

#aggregate data district wise
agg <- aggregate(dat2$total_cases,
                by = list(dat2$lgd_district_name, dat2$lgd_state_name, dat2$lgd_district_id, dat2$lgd_state_id),
                FUN = sum)

agg

aggdeath <- aggregate(dat2$total_deaths,
                       by= list(dat2$lgd_district_id, dat2$lgd_state_id),
                       FUN = sum)


#renaming the joining variables so that I can join agg and aggdeath
names(agg)[3] <- "lgd_district_id"
names(agg)[4] <- "lgd_state_id"

names(agg)[1] <- "lgd_district_name"
names(agg)[2] <- "lgd_state_name"

names(aggdeath)[1] <- "lgd_district_id"
names(aggdeath)[2] <- "lgd_state_id"
                       
#create the final aggregated data-set for covid cases and death
covid_cases_death <- right_join(agg, aggdeath, by=c("lgd_district_id", "lgd_state_id"))

#rename cases and death
names(covid_cases_death)[5] <- "cases"
names(covid_cases_death)[6] <- "death"
                       
#merge covid death with migration data
covid_merged <- full_join(covid_cases_death, district_migration, by=c("lgd_district_id", "lgd_state_id"))

#merge ddl_health_infra data
ddl_health_infra_lgd$lgd_district_id <- as.numeric(ddl_health_infra_lgd$lgd_district_id)
ddl_health_infra_lgd$lgd_state_id <- as.numeric(ddl_health_infra_lgd$lgd_state_id)
class(covid_merged$lgd_district_id) <- "numeric"
class(covid_merged$lgd_state_id) <- "numeric"
covid_merged <- full_join(covid_merged, ddl_health_infra_lgd, by=c("lgd_district_id", "lgd_state_id"))

#merge ddl_nfhs
covid_merged <- full_join(covid_merged, ddl_nfhs_lgd, by=c("lgd_district_id", "lgd_state_id"))

#merge district_age_dist
class(district_age_dist_cfr$lgd_district_id) <- "numeric"
class(district_age_dist_cfr$lgd_state_id) <- "numeric"
covid_merged <- full_join(covid_merged, district_age_dist_cfr, by=c("lgd_district_id", "lgd_state_id"))

#merge ec_hospitals_dist
class(ec_hospitals_dist$lgd_district_id) <- "numeric"
class(ec_hospitals_dist$lgd_state_id) <- "numeric"
covid_merged <- full_join(covid_merged, ec_hospitals_dist, by=c("lgd_district_id", "lgd_state_id"))

#merge hospital_dist
#covid_merged <- full_join(covid_merged, hospitals_dist, by=c("lgd_district_id", "lgd_state_id"))

#merge pc11_demographics 
#first rename the joining variables to match
#names(pc11_demographics_district)[1] <- "lgd_state_id"
names(pc11_demographics_district)[2] <- "lgd_district_id"
