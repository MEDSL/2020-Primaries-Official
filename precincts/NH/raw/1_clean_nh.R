
dir()

democratic_prim <- dir()[grepl("President Democratic", dir())]
repub_prim <- dir()[grepl("President Republican", dir())]

democratic_wi <- dir()[grepl("Democratic 2020", dir())]
repub_wi <- dir()[grepl("Republican 2020", dir())]
for (i in democratic_prim) {
  x <- readxl::read_xls(i)
  if (sum(is.na(x[1,]))/length(x[1,]) > .2 | 
      sum(grepl("COUNTY", x[2,])) > 1) {
    x[1, as.logical(is.na(x[1,]))] <- 
      x[2, as.logical(!is.na(x[2,]))]
    x <- x[-2, ]
  }
  
  if (sum(grepl("COUNTY", x[1,])) > 1) {
    x <- x[,c(T, !grepl("COUNTY", x[1,-1]))]
  }
  
  
  precincts <- t(x[1,])

  precinct <- precincts[is.na(precincts) == F & precincts != "TOTALS"]
  
  precinct <- gsub("\\*", "", gsub("  ", " ", gsub("  ", " ", gsub("  ", " ", precinct))))
  
  precinct <- precinct[!grepl("COUNTY", precinct)]
  
  office <- "US PRESIDENT"
  
  party_detailed <- "DEMOCRAT"
  
  party_simplified <- party_detailed
  
  mode <- "TOTAL"
  
  county_name <- gsub(" COUNTY", "", as.character(x[grepl("county",tolower(unlist(x[,1]))),1]))
  
  
  state <- "New Hampshire"
  
  candidate <- c("MICHAEL BENNET", "JOSEPH R BIDEN", "CORY BOOKER", "MOSIE BOYD", 
                 "STEVE BULLOCK", "STEVE BURKE", "PETE BUTTIGIEG", 
                 "JULIAN CASTRO", 'ROQUE "ROCKY" DE LA FUENTE', "JOHN K DELANEY",
                 "JASON EVRITTE DUNLAP", "MICHAEL A ELLINGER", "TULSI GABBARD",
                 "BEN GLEIB GLEIBERMAN", "MARK STEWART GREENSTEIN", 
                 "KAMALA HARRIS", "HENRY HEWES", "AMY KLOBUCHAR", "TOM KOOS",
                 "LORENZ KRAUS", "RITA KRICHEVSKY", "RAYMOND MICHAEL MOROZ",
                 "DEVAL PATRICK", "BERNIE SANDERS", "JOE SESTAK", "SAM SLOAN", 
                 "TOM STEYER", "DAVID JOHN THISTLE", "THOMAS JAMES TORGENSEN", 
                 "ELIZABETH WARREN", "ROBBY WELLS", "MARIANNE WILLIAMSON", 
                 "ANDREW YANG")
  
  district <- "statewide"
  
  dataverse <- "president"
  
  year <- 2020
  
  stage <- "pri"
  
  state <- "New Hampshire"
  
  special <- FALSE
  
  writein <- FALSE
  
  state_po <- "NH"
  
  state_fips <- 33
  
  state_cen <- 12
  
  state_ic <- 4
  
  readme_check <- FALSE
  
  county_fips <- read.csv("county-fips-codes.csv", as.is = T)
  county_fips <- county_fips[county_fips$state == "New Hampshire",]
  
  county_fips <- county_fips[county_fips$county_name == county_name,]$county_fips
  
  towns <- read.csv("2018-nh-precinct.csv", as.is = T)
  
  townconvert <- tapply(towns$jurisdiction, towns$precinct, function(x) return(x[1]))
  
  townconvert <- data.frame("precinct" = names(townconvert), 
                            "jurisdiction" = townconvert, stringsAsFactors = F)
  
  jurisdiction <- townconvert
  
  juris_fips <- read.csv("jurisdiction-fips-codes.csv", as.is = T)
  juris_fips <- juris_fips[juris_fips$state == "New Hampshire",]
  
  
  new_hampshire <- expand.grid("precinct"= precinct, "office" = office, 
              "party_detailed" = party_detailed, 
              "party_simplified" = party_simplified, "mode"=mode, 
              "votes" = -99,
              "county_name" = county_name, "county_fips" = county_fips, 
              "jurisdiction_fips" = "1", "candidate" =  candidate, 
              "district" = district, "dataverse" = dataverse, "year" = year, 
              "stage" = stage, "state" = state, "special" = special, 
              "writein" = writein, "state_po" = state_po, 
              "state_fips" = state_fips, "state_cen" = state_cen, 
              "state_ic" = state_ic, "readme_check" = readme_check, 
              stringsAsFactors = F)
  
  
  new_hampshire <- plyr::join(new_hampshire, jurisdiction, by="precinct")
  
  
  if (nrow(new_hampshire[is.na(new_hampshire$jurisdiction),]) > 0) {
    
    precinct[precinct %in% new_hampshire[is.na(new_hampshire$jurisdiction),]$precinct] <-
      gsub("Loc.", "Location", gsub("'", "", gsub("-", "", 
                 precinct[precinct %in% 
                            new_hampshire[is.na(new_hampshire$jurisdiction),]$precinct])))
    
    new_hampshire[is.na(new_hampshire$jurisdiction),]$precinct <- 
      gsub("Loc.", "Location", gsub("'", "", gsub("-", "", 
                    new_hampshire[is.na(new_hampshire$jurisdiction),]$precinct)))
  }
  
  new_hampshire <- new_hampshire[, names(new_hampshire) %in% "jurisdiction" == F]
  
  new_hampshire <- plyr::join(new_hampshire, jurisdiction, by="precinct")
  names(x) <- c("candidates", precinct, "totals")
  
  targ_rows <- !grepl("county",tolower(unlist(x[,1]))) & 
    !grepl("scatter",tolower(unlist(x[,1]))) & 
    !grepl("corrections",tolower(unlist(x[,1]))) & 
             !is.na(x[,1])
  
  
  x <- x[targ_rows, names(x) %in% "totals" == F]
  
  
  x$candidates <- candidate
  
  
  for (ii in x$candidates) {
    for (iii in names(x)[2:length(x)]) {
      new_hampshire[new_hampshire$candidate == ii & new_hampshire$precinct == iii &
                      new_hampshire$party_detailed == party_detailed & 
                      new_hampshire$county_name == county_name, ]$votes <-
        as.numeric(x[x$candidates == ii, names(x) %in% iii])
    }
  }
  if (i == democratic_prim[1]) {
    new_hampshire_final <- new_hampshire
  } else {
    new_hampshire_final <- rbind(new_hampshire_final, new_hampshire)
  }
  
}

for (i in repub_prim) {
  x <- readxl::read_xls(i)
  if (sum(is.na(x[1,]))/length(x[1,]) > .2 |
      sum(grepl("COUNTY", x[2,])) > 0) {
    x[1, as.logical(is.na(x[1,]))] <- 
      x[2, as.logical(!is.na(x[2,]))]
    x <- x[-2, ]
  }
  
  if (sum(grepl("COUNTY", x[1,])) > 1) {
    x <- x[,c(T, !grepl("COUNTY", x[1,-1]))]
  }
  
  
  precincts <- t(x[1,])
  
  precinct <- precincts[is.na(precincts) == F & precincts != "TOTALS"]
  
  precinct <- gsub("\\*", "", gsub("  ", " ", gsub("  ", " ", gsub("  ", " ", precinct))))
  
  precinct <- precinct[!grepl("COUNTY", precinct)]
  
  office <- "US PRESIDENT"
  
  party_detailed <- "REPUBLICAN"
  
  party_simplified <- party_detailed
  
  mode <- "TOTAL"
  
  county_name <- gsub(" COUNTY", "", as.character(x[grepl("county",tolower(unlist(x[,1]))),1]))
  
  
  state <- "New Hampshire"
  
  candidate <- c("ROBERT ARDINI", "PRESIDENT R BODDIE", "STEPHEN B COMLEY SR",
                 'ROQUE "ROCKY" DE LA FUENTE', "BOB ELY", "ZOLTAN ISTVAN GYURKO",
                 "LARRY HORN", "RICK KRAFT", "STAR LOCKE", "MATTHEW JOHN MATERN",
                 "MARY MAXWELL", "ERIC MERRILL", "WILLIAM N MURPHY", "JUAN PAYNE",
                 "DONALD J TRUMP", "JOE WALSH", "BILL WELD")
  
  district <- "statewide"
  
  dataverse <- "president"
  
  year <- 2020
  
  stage <- "pri"
  
  state <- "New Hampshire"
  
  special <- FALSE
  
  writein <- FALSE
  
  state_po <- "NH"
  
  state_fips <- 33
  
  state_cen <- 12
  
  state_ic <- 4
  
  readme_check <- FALSE
  
  county_fips <- read.csv("county-fips-codes.csv", as.is = T)
  county_fips <- county_fips[county_fips$state == "New Hampshire",]
  
  county_fips <- county_fips[county_fips$county_name == county_name,]$county_fips
  
  towns <- read.csv("2018-nh-precinct.csv", as.is = T)
  
  townconvert <- tapply(towns$jurisdiction, towns$precinct, function(x) return(x[1]))
  
  townconvert <- data.frame("precinct" = names(townconvert), 
                            "jurisdiction" = townconvert, stringsAsFactors = F)
  
  jurisdiction <- townconvert
  
  juris_fips <- read.csv("jurisdiction-fips-codes.csv", as.is = T)
  juris_fips <- juris_fips[juris_fips$state == "New Hampshire",]
  
  
  new_hampshire <- expand.grid("precinct"= precinct, "office" = office, 
                               "party_detailed" = party_detailed, 
                               "party_simplified" = party_simplified, "mode"=mode, 
                               "votes" = -99,
                               "county_name" = county_name, "county_fips" = county_fips, 
                               "jurisdiction_fips" = "1", "candidate" =  candidate, 
                               "district" = district, "dataverse" = dataverse, "year" = year, 
                               "stage" = stage, "state" = state, "special" = special, 
                               "writein" = writein, "state_po" = state_po, 
                               "state_fips" = state_fips, "state_cen" = state_cen, 
                               "state_ic" = state_ic, "readme_check" = readme_check, 
                               stringsAsFactors = F)
  
  
  new_hampshire <- plyr::join(new_hampshire, jurisdiction, by="precinct")
  
  
  if (nrow(new_hampshire[is.na(new_hampshire$jurisdiction),]) > 0) {
    
    precinct[precinct %in% new_hampshire[is.na(new_hampshire$jurisdiction),]$precinct] <-
      gsub("Loc.", "Location", gsub("'", "", gsub("-", "", 
                                                  precinct[precinct %in% 
                                                             new_hampshire[is.na(new_hampshire$jurisdiction),]$precinct])))
    
    new_hampshire[is.na(new_hampshire$jurisdiction),]$precinct <- 
      gsub("Loc.", "Location", gsub("'", "", gsub("-", "", 
                                                  new_hampshire[is.na(new_hampshire$jurisdiction),]$precinct)))
  }
  
  new_hampshire <- new_hampshire[, names(new_hampshire) %in% "jurisdiction" == F]
  
  new_hampshire <- plyr::join(new_hampshire, jurisdiction, by="precinct")
  names(x) <- c("candidates", precinct, "totals")
  
  targ_rows <- !grepl("county",tolower(unlist(x[,1]))) & 
    !grepl("scatter",tolower(unlist(x[,1]))) & 
    !grepl("corrections",tolower(unlist(x[,1]))) & 
    !is.na(x[,1])
  
  
  x <- x[targ_rows, names(x) %in% "totals" == F]
  
  
  x$candidates <- candidate
  
  
  for (ii in x$candidates) {
    for (iii in names(x)[2:length(x)]) {
      new_hampshire[new_hampshire$candidate == ii & new_hampshire$precinct == iii &
                      new_hampshire$party_detailed == party_detailed & 
                      new_hampshire$county_name == county_name, ]$votes <-
        as.numeric(x[x$candidates == ii, names(x) %in% iii])
    }
  }
  if (i == democratic_prim[1]) {
    new_hampshire_final <- new_hampshire
  } else {
    new_hampshire_final <- rbind(new_hampshire_final, new_hampshire)
  }
  
}

## write in
for (i in democratic_wi) {
  x <- readxl::read_xls(i)
  
  # messed up COOS county
  if (sum(x[2,1] == "COOS COUNTY")) {
    x <- x[1:21,]
  }
  if (sum(is.na(x[1,]))/length(x[1,]) > .2 | 
      sum(grepl("COUNTY", x[2,])) > 0) {
    x[1, as.logical(is.na(x[1,]))] <- 
      x[2, as.logical(!is.na(x[2,]))]
    x <- x[-2, ]
  }
  
  if (sum(grepl("COUNTY", x[1,])) > 1) {
    x <- x[,c(T, !grepl("COUNTY", x[1,-1]))]
  }
  
  
  precincts <- t(x[1,])
  
  precinct <- precincts[is.na(precincts) == F & precincts != "TOTALS"]
  
  precinct <- gsub("\\*", "", gsub("  ", " ", gsub("  ", " ", gsub("  ", " ", precinct))))
  
  precinct <- precinct[!grepl("COUNTY", precinct)]
  
  office <- "US PRESIDENT"
  
  party_detailed <- "DEMOCRAT"
  
  party_simplified <- party_detailed
  
  mode <- "TOTAL"
  
  county_name <- gsub(" COUNTY", "", as.character(x[grepl("county",tolower(unlist(x[,1]))),1]))
  
  
  state <- "New Hampshire"
  
  candidate <- toupper(gsub("\\.", "", gsub(",", "", 
  gsub(", r", "", x[grepl("county", tolower(x[[1]]))== F & 
                      !is.na(x[[1]]),][[1]]))))
  
  district <- "statewide"
  
  dataverse <- "president"
  
  year <- 2020
  
  stage <- "pri"
  
  state <- "New Hampshire"
  
  special <- FALSE
  
  writein <- TRUE
  
  state_po <- "NH"
  
  state_fips <- 33
  
  state_cen <- 12
  
  state_ic <- 4
  
  readme_check <- FALSE
  
  county_fips <- read.csv("county-fips-codes.csv", as.is = T)
  county_fips <- county_fips[county_fips$state == "New Hampshire",]
  
  county_fips <- county_fips[county_fips$county_name == county_name,]$county_fips
  
  towns <- read.csv("2018-nh-precinct.csv", as.is = T)
  
  townconvert <- tapply(towns$jurisdiction, towns$precinct, function(x) return(x[1]))
  
  townconvert <- data.frame("precinct" = names(townconvert), 
                            "jurisdiction" = townconvert, stringsAsFactors = F)
  
  jurisdiction <- townconvert
  
  juris_fips <- read.csv("jurisdiction-fips-codes.csv", as.is = T)
  juris_fips <- juris_fips[juris_fips$state == "New Hampshire",]
  
  
  new_hampshire <- expand.grid("precinct"= precinct, "office" = office, 
                               "party_detailed" = party_detailed, 
                               "party_simplified" = party_simplified, "mode"=mode, 
                               "votes" = -99,
                               "county_name" = county_name, "county_fips" = county_fips, 
                               "jurisdiction_fips" = "1", "candidate" =  candidate, 
                               "district" = district, "dataverse" = dataverse, "year" = year, 
                               "stage" = stage, "state" = state, "special" = special, 
                               "writein" = writein, "state_po" = state_po, 
                               "state_fips" = state_fips, "state_cen" = state_cen, 
                               "state_ic" = state_ic, "readme_check" = readme_check, 
                               stringsAsFactors = F)
  
  
  new_hampshire <- plyr::join(new_hampshire, jurisdiction, by="precinct")
  
  
  if (nrow(new_hampshire[is.na(new_hampshire$jurisdiction),]) > 0) {
    
    precinct[precinct %in% new_hampshire[is.na(new_hampshire$jurisdiction),]$precinct] <-
      gsub("Loc.", "Location", gsub("'", "", gsub("-", "", 
                                                  precinct[precinct %in% 
                                                             new_hampshire[is.na(new_hampshire$jurisdiction),]$precinct])))
    
    new_hampshire[is.na(new_hampshire$jurisdiction),]$precinct <- 
      gsub("Loc.", "Location", gsub("'", "", gsub("-", "", 
                                                  new_hampshire[is.na(new_hampshire$jurisdiction),]$precinct)))
  }
  
  new_hampshire <- new_hampshire[, names(new_hampshire) %in% "jurisdiction" == F]
  
  new_hampshire <- plyr::join(new_hampshire, jurisdiction, by="precinct")
  names(x) <- c("candidates", precinct, "totals")
  
  targ_rows <- !grepl("county",tolower(unlist(x[,1]))) & 
    !grepl("corrections",tolower(unlist(x[,1]))) & 
    !is.na(x[,1])
  
  
  x <- x[targ_rows, names(x) %in% "totals" == F]
  
  
  x$candidates <- candidate
  
  
  for (ii in x$candidates) {
    for (iii in names(x)[2:length(x)]) {
      new_hampshire[new_hampshire$candidate == ii & new_hampshire$precinct == iii &
                      new_hampshire$party_detailed == party_detailed & 
                      new_hampshire$county_name == county_name, ]$votes <-
        as.numeric(x[x$candidates == ii, names(x) %in% iii])
    }
  }
  if (i == democratic_prim[1]) {
    new_hampshire_final <- new_hampshire
  } else {
    new_hampshire_final <- rbind(new_hampshire_final, new_hampshire)
  }
  
}



for (i in repub_wi) {
  x <- readxl::read_xls(i)
  if (sum(is.na(x[1,]))/length(x[1,]) > .2 | 
      sum(grepl("COUNTY", x[2,])) > 0) {
    x[1, as.logical(is.na(x[1,]))] <- 
      x[2, as.logical(!is.na(x[2,]))]
    x <- x[-2, ]
  }
  
  if (sum(grepl("COUNTY", x[1,])) > 1) {
    x <- x[,c(T, !grepl("COUNTY", x[1,-1]))]
  }
  
  
  precincts <- t(x[1,])
  
  precinct <- precincts[is.na(precincts) == F & precincts != "TOTALS" & precincts != "Totals"]
  
  precinct <- gsub("\\*", "", gsub("  ", " ", gsub("  ", " ", gsub("  ", " ", precinct))))
  
  precinct <- precinct[!grepl("COUNTY", precinct)]
  
  office <- "US PRESIDENT"
  
  party_detailed <- "REPUBLICAN"
  
  party_simplified <- party_detailed
  
  mode <- "TOTAL"
  
  county_name <- gsub(" COUNTY", "", as.character(x[grepl("county",tolower(unlist(x[,1]))),1]))
  
  
  state <- "New Hampshire"
  
  candidate <- toupper(gsub("\\.", "", gsub(",", "", 
                                            gsub(", d", "", x[grepl("county", tolower(x[[1]]))== F & 
                                                                !is.na(x[[1]]),][[1]]))))
  
  district <- "statewide"
  
  dataverse <- "president"
  
  year <- 2020
  
  stage <- "pri"
  
  state <- "New Hampshire"
  
  special <- FALSE
  
  writein <- TRUE
  
  state_po <- "NH"
  
  state_fips <- 33
  
  state_cen <- 12
  
  state_ic <- 4
  
  readme_check <- FALSE
  
  county_fips <- read.csv("county-fips-codes.csv", as.is = T)
  county_fips <- county_fips[county_fips$state == "New Hampshire",]
  
  county_fips <- county_fips[county_fips$county_name == county_name,]$county_fips
  
  towns <- read.csv("2018-nh-precinct.csv", as.is = T)
  
  townconvert <- tapply(towns$jurisdiction, towns$precinct, function(x) return(x[1]))
  
  townconvert <- data.frame("precinct" = names(townconvert), 
                            "jurisdiction" = townconvert, stringsAsFactors = F)
  
  jurisdiction <- townconvert
  
  juris_fips <- read.csv("jurisdiction-fips-codes.csv", as.is = T)
  juris_fips <- juris_fips[juris_fips$state == "New Hampshire",]
  
  
  new_hampshire <- expand.grid("precinct"= precinct, "office" = office, 
                               "party_detailed" = party_detailed, 
                               "party_simplified" = party_simplified, "mode"=mode, 
                               "votes" = -99,
                               "county_name" = county_name, "county_fips" = county_fips, 
                               "jurisdiction_fips" = "1", "candidate" =  candidate, 
                               "district" = district, "dataverse" = dataverse, "year" = year, 
                               "stage" = stage, "state" = state, "special" = special, 
                               "writein" = writein, "state_po" = state_po, 
                               "state_fips" = state_fips, "state_cen" = state_cen, 
                               "state_ic" = state_ic, "readme_check" = readme_check, 
                               stringsAsFactors = F)
  
  
  new_hampshire <- plyr::join(new_hampshire, jurisdiction, by="precinct")
  
  
  if (nrow(new_hampshire[is.na(new_hampshire$jurisdiction),]) > 0) {
    
    precinct[precinct %in% new_hampshire[is.na(new_hampshire$jurisdiction),]$precinct] <-
      gsub("Loc.", "Location", gsub("'", "", gsub("-", "", 
                                                  precinct[precinct %in% 
                                                             new_hampshire[is.na(new_hampshire$jurisdiction),]$precinct])))
    
    new_hampshire[is.na(new_hampshire$jurisdiction),]$precinct <- 
      gsub("Loc.", "Location", gsub("'", "", gsub("-", "", 
                                                  new_hampshire[is.na(new_hampshire$jurisdiction),]$precinct)))
  }
  
  new_hampshire <- new_hampshire[, names(new_hampshire) %in% "jurisdiction" == F]
  
  new_hampshire <- plyr::join(new_hampshire, jurisdiction, by="precinct")
  names(x) <- c("candidates", precinct, "totals")
  
  targ_rows <- !grepl("county",tolower(unlist(x[,1]))) & 
    !grepl("corrections",tolower(unlist(x[,1]))) & 
    !is.na(x[,1])
  
  
  x <- x[targ_rows, names(x) %in% "totals" == F]
  
  
  x$candidates <- candidate
  
  
  for (ii in x$candidates) {
    for (iii in names(x)[2:length(x)]) {
      new_hampshire[new_hampshire$candidate == ii & new_hampshire$precinct == iii &
                      new_hampshire$party_detailed == party_detailed & 
                      new_hampshire$county_name == county_name, ]$votes <-
        as.numeric(x[x$candidates == ii, names(x) %in% iii])
    }
  }
  if (i == democratic_prim[1]) {
    new_hampshire_final <- new_hampshire
  } else {
    new_hampshire_final <- rbind(new_hampshire_final, new_hampshire)
  }
  
}


new_hampshire_final <- new_hampshire_final[!is.na(new_hampshire_final$votes),]


table(new_hampshire_final[is.na(new_hampshire_final$jurisdiction),]$precinct)

new_hampshire_final[new_hampshire_final$precinct == "Fitzilliam",]$precinct <- 
  "Fitzwilliam"

new_hampshire_final[new_hampshire_final$precinct == "Sargents Pur.",]$precinct <- "Sargents Purchase"
new_hampshire_final[new_hampshire_final$precinct == "Crawfords Pur.",]$precinct <- "Crawfords Purchase"
new_hampshire_final[new_hampshire_final$precinct == "Beans Pur",]$precinct <- "Beans Purchase"
new_hampshire_final[new_hampshire_final$precinct == "Beans Gt",]$precinct <- "Beans Grant"
new_hampshire_final[new_hampshire_final$precinct == "Hadleys Pur.",]$precinct <- "Hadleys Purchase"
new_hampshire_final[new_hampshire_final$precinct == "Second Coll. Gt.",]$precinct <- "Second College Grant"
new_hampshire_final[new_hampshire_final$precinct == "Pinkhams Gt.",]$precinct <- "Pinkhams Grant"
new_hampshire_final[new_hampshire_final$precinct == "Second College Gt.",]$precinct <- "Second College Grant"
new_hampshire_final[new_hampshire_final$precinct == "Greens Gt.",]$precinct <- "Greens Grant"
new_hampshire_final[new_hampshire_final$precinct == "Cutts Gt",]$precinct <- "Cutts Grant"
new_hampshire_final[new_hampshire_final$precinct == "Thompson & Mess Pur.",]$precinct <- "Thompson and Meserves Purchase"
new_hampshire_final[new_hampshire_final$precinct == "Wentworths Loc",]$precinct <- "Wentworths Location"
new_hampshire_final[new_hampshire_final$precinct == "Low & Burbanks Gt",]$precinct <- "Low Burbanks Grant"
new_hampshire_final[new_hampshire_final$precinct == "Dixs Gt.",]$precinct <- "Dixs Grant"

# new jurisdiction column merge
new_hampshire_final <- new_hampshire_final[, names(new_hampshire_final) %in% "jurisdiction" == F]
new_hampshire_final <- plyr::join(new_hampshire_final, jurisdiction, by="precinct")

# fix extra precincts
new_hampshire_final[new_hampshire_final$precinct == "At. & Gil. Ac. Gt",]$jurisdiction <- "Atkinson"
new_hampshire_final[new_hampshire_final$precinct == "At. & Gil. Ac. Gt",]$precinct <- "At Gil Ac Grant"
new_hampshire_final[new_hampshire_final$precinct == "Thompson and Meserves Purchase",]$jurisdiction <- 
  "Thompson and Meserves Purchase"

new_hampshire_final$jurisdiction_name <- toupper(new_hampshire_final$jurisdiction)
new_hampshire_final <- new_hampshire_final[, names(new_hampshire_final) %in% "jurisdiction_fips" == F]
juris_fips$jurisdiction_name <- gsub("'", "", juris_fips$jurisdiction_name)

# join jurisdiction fips
new_hampshire_final <- plyr::join(new_hampshire_final, juris_fips, by="jurisdiction_name")


# fix jurisdiction fips
new_hampshire_final[new_hampshire_final$jurisdiction == "Cambridge", ]$jurisdiction_fips <- 3300708420
new_hampshire_final[new_hampshire_final$jurisdiction == "Dixville", ]$jurisdiction_fips <- 3300718420
new_hampshire_final[new_hampshire_final$jurisdiction == "Kilkenny", ]$jurisdiction_fips <- 	3300739940
new_hampshire_final[new_hampshire_final$jurisdiction == "Low Burbanks Grant", ]$jurisdiction_fips <- 3300743620
new_hampshire_final[new_hampshire_final$jurisdiction == "Millsfield", ]$jurisdiction_fips <- 	3300748260
new_hampshire_final[new_hampshire_final$jurisdiction == "Odell", ]$jurisdiction_fips <- 3300757860
new_hampshire_final[new_hampshire_final$jurisdiction == "Success", ]$jurisdiction_fips <- 3300774500
new_hampshire_final[new_hampshire_final$jurisdiction == "Wentworths Location", ]$jurisdiction_fips <- 3300780740

for (i in names(new_hampshire_final)) {
  print(paste(i, sum(is.na(new_hampshire_final[, i]))))
}

new_hampshire_final <- new_hampshire_final[, c("precinct", "office",
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


write.csv(new_hampshire_final, "../2020-nh-precinct-primary.csv", row.names = F)
