florida <- read.csv("raw/03172020Election.txt", sep="\t", as.is = T)

# get rid of double vote issue temporarily
florida <- florida[is.na(florida$Juris1num),]

office <- "US PRESIDENT"

party_detailed <- toupper(florida$PartyName)

party_simplified <- party_detailed

mode <- "TOTAL"

county_name <- toupper(florida$CountyName)

state <- "Florida"

candidate <- paste(florida$CanNameFirst, florida$CanNameMiddle, florida$CanNameLast)

district <- "statewide"

dataverse <- "president"

year <- 2020

stage <- "pri"

special <- FALSE

writein <- FALSE

readme_check <- FALSE

votes <- florida$CanVotes

florida_primary <- 
  data.frame("office" = office, 
             "party_detailed" = party_detailed, 
             "party_simplified" = party_simplified, "mode"=mode, 
             "votes" = votes, "jurisdiction_name" = county_name,
             "county_name" = county_name,
             "candidate" =  candidate, 
             "district" = district, "dataverse" = dataverse, "year" = year, 
             "stage" = stage, "state" = state, "special" = special, 
             "writein" = writein, "readme_check" = readme_check, 
             stringsAsFactors = F)


county_codes <- read.csv("raw/county-fips-codes.csv", as.is = T)

florida_primary <- plyr::join(florida_primary, county_codes)

state_codes <- read.csv("raw/merge_on_statecodes.csv", as.is = T)
florida_primary <- plyr::join(florida_primary, state_codes)

jurisdiction_codes <- read.csv("raw/jurisdiction-fips-codes.csv", as.is = T)
florida_primary <- plyr::join(florida_primary, jurisdiction_codes)

florida_primary <- florida_primary[,  c("office",
                                        "party_detailed",
                                        "party_simplified", "mode",
                                        "votes", "county_name",
                                        "county_fips",
                                        "jurisdiction_name",
                                        "jurisdiction_fips", "candidate",
                                        "district", "dataverse", "year",
                                        "stage", "state", "special",
                                        "writein", "state_po",
                                        "state_fips", "state_cen",
                                        "state_ic", "readme_check")]




write.csv(florida_primary, "2020-fl-county-primary.csv", row.names = F)
