Official results for the 2020 Presidential Primaries 

# 2020-primaries-official
Repository for the 2020 official primary results. The data are broken into precinct returns.

The source of the data is typically each state's Secretary of State website or comparable elections division page on an official state website.

## Variables
The variables are listed as they appear in the data file. Not all variables appear in each data file.

### year
- **Description**: election year	

------------------

### state
- **Description**: state name 

-----------------

### state_po
- **Description**: U.S. postal code state abbreviation

----------------

### state_fips
 - **Description**: State FIPS code

----------------

### state_cen
 - **Description**: U.S. Census state code

 ---------------
 
### state_ic
 - **Description**: ICPSR state code

-----------------

### county_name
 - **Description**: county name
 
 -----------------

### county_fips
 - **Description**: county fips code
 
 -----------------

### jurisdiction_name
 - **Description**: name of the jurisdiction, county/town name, (except in Alaska, where results are reported by state legislative district). Towns for New England states (i.e. NH, MA, VT, CT, etc.)
 
-----------------

### jurisdiction_fips
 - **Description**: fips code of the jurisdiction, county/town name, (except in Alaska, where results are reported by state legislative district). Towns for New England states (i.e. NH, MA, VT, CT, etc.)

-----------------

### office
- **Description**: office name ; i.e. president

-----------------

### district
 - **Description**: district identifier for the office; if state legislative or US House, string padded to be length of three on left side. Coded as "statewide" for statewide offices, and left as "" for offices without consistent district information.

-----------------

### stage
- **Description**: electoral stage; primary ("pri") for most states; caucus states will report stage (first, second, third...) and final delegate count

-----------------

### caucus
- **Description**: TRUE/FALSE indicator for whether nominating election a caucus

-----------------

### special
- **Description**: TRUE/FALSE indicator for whether the election was a special election

-----------------

### candidate
- **Description**: name of the candidate; write-in candidates/totals represented as NA's
 
-----------------

### party_detailed
- **Description**: party of the candidate (always entirely lowercase); write-in candidates/totals represented as ""

-----------------

### party_simplified
- **Description**: simplified party of the candidate, taking form of "democrat," "republican," "libertarian," "other," and "nonpartisan"

-----------------

### writein
- **Description**: TRUE/FALSE indicator for write-in candidates/totals

-----------------

### mode
- **Description**: mode of voting; states with data that doesn't break down returns by mode are marked as "total"; other states can have modes of "absentee," "machine," "absentee mail," "absentee walk-in," "election day," "polling," "early," "one stop," and "provisional" 

-----------------

### votes 
- **Description**: votes received by this candidate for this particular party

----------------

### source  
- **Description**: the source where the data were acquired. 

----------------

### dataverse  
- **Description**: in precinct file, whether this election corresponds to elections for President ("president"), Senate ("senate"), US House ("house"), or state ("state") files, or none of these ("local")

----------------

### readme_check:   
- **Description**: TRUE/FALSE indicator as to whether a there are notes in the readme file relevant to the state. 

--------------------------------------------------------------------------------------------------------------------

## Notes


South Carolina has detailed data on type of vote, including "Absentee By Mail", "Election Day", "Failsafe", "Failsafe Provisional" "InPerson Absentee", "Provisional", and "Total Votes" 


## 2020 New Hampshire Primaries
added 2020-03-11
