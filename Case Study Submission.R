##############################################################################################
# THE PACKAGES-"stringr", "tidyr" and "dplyr" ARE TO BE INSTALLED BEFORE LOADING THE PACKAGES#
# LOADING THE PACKAGES                                                                       #
##############################################################################################


# LOAD THE PACKAGES, "stringr", "tidyr" and "dplyr" TO THE R SESSION.
library(stringr)
library(tidyr)
library(dplyr)

###########################
# READING THE INPUT FILES #
###########################


# READING THE "companies.txt" FILE INTO "companies" DATAFRAME
companies <- read.table("companies.txt", 
                        header = TRUE, 
                        sep = "\t", 
                        quote="", 
                        col.names = c("permalink", "name", "homepage_url", "category_list", "status", "country_code", "state_code", "region", "city", "founded_at"), 
                        fill = TRUE, 
                        na.strings = "",  
                        colClasses = "character", 
                        comment.char = "")
View(companies)


# READING THE "rounds2.csv" FILE INTO "rounds2" DATAFRAME
rounds2 <- read.csv("rounds2.csv", 
                    header = TRUE,
                    fill = TRUE,
                    quote = "",
                    na.strings = "",
                    colClasses = "character")

# CONVERTING DATA TYPE OF "raised_amount_usd" COLUMN IN "rounds2" DATAFRAME AS "NUMERIC".
rounds2$raised_amount_usd <- as.numeric(rounds2$raised_amount_usd)

View(rounds2)

###############################################
##  NOTE  : PRIMARY KEYS IDENTIFIED          ##          
##  rounds2 DATA FRAME    - company_permalink## 
##  companies DATA FRAME  - permalink        ##
###############################################


##################################
##  CHECKPOINT 1: DATA CLEANING ## 
##  TABLE 1.1                   ##
##################################

###########################################################################################################################
# IMPORTANT NOTE ::                                                                                                       #
#                                                                                                                         #
# AS PART OF DATA CLEANING, BELOW DECISSIONS ARE TAKEN                                                                    #
#                                                                                                                         #
# DECISSION - 1  ::  CONVERT "company_permalink" COLUMN VALUES OF "rounds2" DATAFRAME TO LOWER CASE                       #
# DECISSION - 2  ::  CONVERT "permalink" COLUMN VALUES OF "companies" DATAFRAME TO LOWER                                  #
#                                                                                                                         #
# EXPLAINATION  ::  IT HAS BEEN OBSERVED THAT THERE ARE MULTIPLE ENTRIES WHICH ARE OF SAME COMPANY BUT DIFFERS BECAUSE OF # 
#                   CASE SENSITIVITY SO FOR EFFECTIVE ANALYSIS WE FIRST CONVERT THE COMPANY PERMA LINKS TO LOWER CASE AND #
#                   THEN DO OUR ANALYSIS                                                                                  #
###########################################################################################################################

rounds2$company_permalink <- tolower(rounds2$company_permalink)
companies$permalink <- tolower(companies$permalink)

# FINDING UNIQUE COMPANIES IN "rounds2" DATAFRAME.

uniqueCompanies_In_Rounds2 <- data.frame(unique(rounds2$company_permalink))
#View(uniqueCompanies_In_Rounds2)

# FINDING UNIQUE COMPANIES IN "companies" DATAFRAME.

uniqueCompanies_In_Companies <- data.frame(unique(companies$permalink))
#View(uniqueCompanies_In_Companies)

# TO FIND OUT WHETER ANY DIFFERENCE FROM uniqueCompaniesInRounds2 & uniqueCompaniesInCompanies

setdiff(uniqueCompanies_In_Rounds2$unique.tolower.rounds2.company_permalink..,uniqueCompanies_In_Companies$unique.tolower.companies.permalink..)

# MERGING ALL COLUMNS OF "companies" DATAFRAME TO "rounds2" DATAFRAME
# PERFORMING LEFT OUTER JOIN, HERE LEFT TABLE IS "rounds2" DATAFRAME & RIGHT TABLE IS "companies" DATAFRAME

master_frame <- merge(x=rounds2, y=companies, by.x="company_permalink", by.y="permalink", all.x = TRUE)


###########################################################################################################################
# IMPORTANT NOTE ::                                                                                                       #
#                                                                                                                         #
# AS PART OF DATA CLEANING, BELOW DECISSIONS ARE TAKEN                                                                    #
#                                                                                                                         #
# DECISSION - 3   ::  REPLACE "NA" VALUES IN "raised_amount_usd" COLUMN WITH VALUE, 0 (NUMERIC)                           #
# EXPLAINATION    ::  INSTEAD OF REMOVING THE ENTRIES WHERE INVESTMENT VALUE IS NOT DECLARED, WE CONSIDER IT              #
#                     AS NO INVESTMENT HAS BEEN DONE                                                                      #
#                                                                                                                         #
# DECISSION - 4   ::  REPLACE "NA" VALUES IN "country_code" COLUMN WITH VALUE, "UNKNOWN" (STRING)                         #
# EXPLAINATION    ::  INSTEAD OF REMOVING THE ENTRIES WHERE COUNTRY NAMES ARE NOT PROVIDED, WE CONSIDER IT AS             #
#                     "UNKNOWN" COUNTRY THIS MAY BE USEFUL FOR US TO KNOW HOW MUCH INVESTMENTS ARE DONE IN A              #
#                     COUNTRY WHICH WE DONT HAVE INFORMATION.                                                             #
# DECISSION - 5   ::  REPLACE "NA" VALUES IN "category_list" COLUMN WITH VALUE, "" (EMPTY STRING)                         #
# EXPLAINATION    ::  WHEN A "category_list" VALUE IS EMPTY ITS "main_category" HAVE TO BE TAKEN AS "Blanks"              #
#                     THIS MAPPIN WILL BE DONE USING "mapping.csv" FILE PROVIDED. SO WE HAVE TAKEN THIS DECISSION         #
###########################################################################################################################

master_frame[c("raised_amount_usd")][is.na(master_frame[c("raised_amount_usd")])] <- 0
master_frame[c("country_code")][is.na(master_frame[c("country_code")])] <- "UNKNOWN"
master_frame[c("category_list")][is.na(master_frame[c("category_list")])] <- ""

View(master_frame)

#######################################
##  CHECKPOINT 2: FUND TYPE ANALYSIS ## 
##  TABLE 2.1                        ##
#######################################

# WE DECLARE FOUR DATA FRAMES AS BELOW
# ventureDataFrame        -   CONTAINS DATA BELONGING TO FUNDING TYPE, "venture"
# angelDataFrame          -   CONTAINS DATA BELONGING TO FUNDING TYPE, "angel"
# seedDataFrame           -   CONTAINS DATA BELONGING TO FUNDING TYPE, "seed"
# privateVentureDataFrame -   CONTAINS DATA BELONGING TO FUNDING TYPE,s "private_equity"

ventureDataFrame <- subset(master_frame, master_frame$funding_round_type == "venture")
angelDataFrame <- subset(master_frame, master_frame$funding_round_type == "angel")
seedDataFrame <- subset(master_frame, master_frame$funding_round_type == "seed")
privateEquityDataFrame <- subset(master_frame, master_frame$funding_round_type == "private_equity")


# MEAN (AVERAGE) OF TOTAL FUND RAISED (Col Name :: raised_amount_usd) IS CALCULATED FOR DIFFERENT FUNDING TYPES
# averageFunding_In_Venture       -   AVERAGE FUND RAISED IN THE FUNDING TYPE, "VENTURE"
# averageFunding_In_Angel         -   AVERAGE FUND RAISED IN THE FUNDING TYPE, "ANGEL"
# averageFunding_In_Seed          -   AVERAGE FUND RAISED IN THE FUNDING TYPE, "SEED"
# averageFunding_In_PrivateEquity -   AVERAGE FUND RAISED IN THE FUNDING TYPE, "PRIVATE EQUITY" 

averageFunding_In_Venture <- mean(ventureDataFrame$raised_amount_usd)
averageFunding_In_Venture
averageFunding_In_Angel <- mean(angelDataFrame$raised_amount_usd)
averageFunding_In_Angel
averageFunding_In_Seed <- mean(seedDataFrame$raised_amount_usd)
averageFunding_In_Seed
averageFunding_In_PrivateEquity <- mean(privateEquityDataFrame$raised_amount_usd)
averageFunding_In_PrivateEquity

#######################################
##  CHECKPOINT 3: COUNTRY ANALYSIS   ## 
##  TABLE 3.1                        ##
#######################################

#######################################################################################################################
# IMPORTANT NOTE ::                                                                                                   #
#                                                                                                                     #
# FROM CHECKPOINT 3 - COUNTRY ANALYSIS, WE HAVE CONCLUDED THAT "venture" IS THE INVESTMENT TYPE THAT IS MORE SUITABLE # 
# FOR SPARK FUNDS TO INVESTMENT, WE SHALL WORK ON "ventureDataFrame" FROM HERE ON FOR FURTHER ANALYSIS.               #
#######################################################################################################################

# CONVERTING THE "country_code" COLUMN AS "FACTOR" FROM "CHARACTER"                               

ventureDataFrame$country_code <- as.factor(ventureDataFrame$country_code)

# CALCULATING THE TOTAL FUNDS INVESTED IN VARIOUS COUNTRIES AND SORTING THE SAME IN DECREASING ORDER TO KNOW 
# THE TOP 9 COUNTRIES WHERE MAX INVESTMENTS ARE DONE

totalFundingAcrossCountries <- aggregate(list(Total_Investment=ventureDataFrame$raised_amount_usd), by=list(Country=ventureDataFrame$country_code), FUN = sum)
totalFundingAcrossCountries_In_SortedOrder <- totalFundingAcrossCountries[with(totalFundingAcrossCountries, order(-Total_Investment)),]

# CREATING DATAFRAME, "top9" CONTAINING TOP 9 COUNTRIES WHERE MAXIMUM INVESTMENTS ARE DONE.

top9 <- data.frame(head(totalFundingAcrossCountries_In_SortedOrder[,], 9))

#######################################################################################################################
# IMPORTANT NOTE ::                                                                                                   #
#                                                                                                                     #
# "top9" DATA FRAME WILL EVEN CONTAIN NON ENGLISH SPEAKING COUNTRES WHICH IS NOT ACCEPTABLE                           #
#                                                                                                                     #
# ENGLISH SPEAKING COUNTRIES ARE THE ONLY ONES TO BE CONSIDERED FOR THE INVESTMENT IS A CONDITION.                    #
# ------------------------------------------------------------------------------------------------                    #
# SO WE HAVE CREATED A DATA FRAME, "englishSpeakingCountries" CONTAINING ONLY THE ENGLISH SPEAKING COUNTRIES          #
# (USED THE PDF PROVIDED IN THE DOWNLOAD SECTION) ALONG WITH THEIR COUNTRY CODES (DOWNLOADED FROM THE ONLINE AND      #
# MAPPED IT).                                                                                                         #
#                                                                                                                     #
# WE FURTHER FILTER THE TOP 9 COUNTRIES FROM "top9" DATA FRAME BASED ON THIS NEWLY CREATED DATA FRAME NAMED,          #
# "englishSpeakingCountries" TO GET FINAL DATA FRAME, "countriesConsideredForInvestments" WHICH WE USE FOR FURTHER    #
# ANALYSIS                                                                                                            #
#######################################################################################################################


# READING COUNTRY NAMES XLSX FILE AND MAPPING TO THE TOP9 DATAFRAME

# GENERATE DATA FRAME, "englishSpeakingCountries"
englishSpeakingCountries <- data.frame(matrix(ncol = 2, nrow = 60))

# GENERATE DATA TO BE FILT INTO THE DATA FRAME, "englishSpeakingCountries" USING A MATRIX
countryNamesAndCodesMatrix <- matrix(c("Botswana",  "Cameroon", "Ethiopia",  "Eritrea", 
                               "The Gambia",  "Ghana",  "Kenya", "Lesotho", 
                               "Liberia", "Malawi",  "Mauritius",  "Namibia", 
                               "Nigeria", "Rwanda", "Seychelles",  "Sierra Leone",                 
                               "South Africa", "South Sudan",  "Sudan", "Swaziland",                                
                               "Tanzania", "Uganda", "Zambia",  "Zimbabwe",                               
                               "Antigua and Barbuda", "The Bahamas", "Barbados", "Belize",                                
                               "Canada", "Dominica", "Grenada", "Guyana",                               
                               "Jamaica",  "Saint Kitts and Nevis",  "Saint Lucia", "Saint Vincent and the Grenadines",                               
                               "Trinidad and Tobago", "United States", "India", "Pakistan",                             
                               "Philippines",  "Singapore", "Australia", "Fiji",                                
                               "Kiribati", "Marshall Islands",  "Federated States of Micronesia",  "Nauru",                               
                               "New Zealand", "Palau", "Papua New Guinea", "Samoa",                              
                               "Solomon Islands", "Tonga", "Tuvalu", "Vanuatu",                               
                               "Ireland", "Malta", "United Kingdom", 
                               "BWA", "CMR", "ETH", "ERI", "GMB", "GHA", "KEN", "LSO", "LBR", "MWI", "MUS", "NAM", "NGA", "RWA",  "SYC", "SLE", "ZAF", "SSD", "SDN", "SWZ",
                               "TZA", "UGA", "ZMB",  "ZWE", "ATG", "BHS", "BRB", "BLZ", "CAN", "DMA", "GRD", "GUY", "JAM", "KNA", "LCA", "VCT", "TTO", "USA", "IND", "PAK", "PHL", "SGP", "AUS", 
                               "FJI",  "KIR", "MHL", "FSM", "NRU", "NZL", "PLW", "PNG", "WSM", "SLB", "TON", "TUV", "VUT", "IRL", "MLT", "GBR"), 
                             ncol = 2, 
                             nrow = 59)

# ASSIGN DATA TO THE DATA FRAME, "englishSpeakingCountries"
englishSpeakingCountries <- countryNamesAndCodesMatrix

# GENERATE DATA FRAME CONTAINING COLUMN NAMES TO BE ASSIGNED TO DATA FRAME, "englishSpeakingCountries"
countryNamesDataFrameColumnNames <- c("Country Name", "ALPHA-3-CODE")

# ASSIGN COLUMN NAMES TO DATA FRAME, "englishSpeakingCountries"
colnames(englishSpeakingCountries) <- countryNamesDataFrameColumnNames

#View(englishSpeakingCountries)

# MERGE "top9" AND "englishSpeakingCountries" DATA FRAMES TO GET THE NEW DATA FRAME, WHICH CONTAINS COUNTRY NAMES
# APPEND THE RESUT TO "top9" DATA FRAME
top9 <- merge(x=top9, y=englishSpeakingCountries, by.x="Country", by.y="ALPHA-3-CODE", all.x = TRUE)


# SORT THE "top9" DATA FRAME, BASED ON TOTAL INVESTMENT
top9 <- top9[with(top9, order(-Total_Investment)),]

View(top9)

# WE REMOVE THE COUNTRIES BASED ON BELOW TWO CONDITIONS
# 1.  THE COUNTRY IS NOT ENGLISH SPEAKING COUNTRY ("Country Name" WILL HAVE THE VALUE AS "NA")
# 2.  WE DONT HAVE THE COUNTRY NAME INFORMATION   ("Country Name" WILL HAVE THE VALUE AS "NA")

countriesConsideredForInvestments <- top9[complete.cases(top9[,3]),]
View(countriesConsideredForInvestments)

#######################################
##  CHECKPOINT 4: SECTOR ANALYSIS 1  ## 
##  TABLE :  NONE                     ##
#######################################

# READING THE "mapping.csv" FILE INTO DATAFRAME, "mappingDataFrame"

mappingDataFrame <- read.csv("mapping.csv", 
                             header = TRUE,
                             check.names = FALSE
                             )

###########################################################################################################################
# IMPORTANT NOTE ::                                                                                                       #
#                                                                                                                         #
# AS PART OF DATA CLEANING, BELOW DECISSIONS ARE TAKEN                                                                    #
#                                                                                                                         #
# DECISSION - 6   ::  REPLACING STRING PATTERN "0" WITH "na" STING IN THE ENTIRE "category_list" COLUMN  OF               #
#                     "mappingDataFrame"                                                                                  #
# EXPLAINATION    ::  DISCREPANCY FOUND - "A0lytics" and "Alter0tive Medicine" ARE TWO ENTITIES WHERE "0" IS GIVEN        #
#                     IN PLACE OF "na" STRING. SO WE HAVE TAKEN A DECISSION TO REPLACE "0" with "na"                      #
#                     AS NO INVESTMENT HAS BEEN DONE                                                                      #
#                                                                                                                         #
# DECISSION - 7   ::  REPLACE "NA" VALUES IN "country_code" COLUMN WITH VALUE, "UNKNOWN"                                  #
# EXPLAINATION    ::  INSTEAD OF REMOVING THE ENTRIES WHERE COUNTRY NAMES ARE NOT PROVIDED, WE CONSIDER IT AS             #
#                     "UNKNOWN" COUNTRY THIS MAY BE USEFUL FOR US TO KNOW HOW MUCH INVESTMENTS ARE DONE IN A              #
#                     COUNTRY WHICH WE DONT HAVE INFORMATION.                                                             #
###########################################################################################################################

# REPLACE "0" STRING PATTERN WITH "na" IN THE "category_list" COLUMN OF DATA FRAME, "mappingDataFrame"

mappingDataFrame$category_list <- gsub("0", "na", mappingDataFrame$category_list)

# APPLY "gather()" ON "mappingDataFrame"

mappingDataFrame <- gather(mappingDataFrame, main_sector, Val, "Automotive & Sports":"Social, Finance, Analytics, Advertising")

# MAKE THE DATA FRAME, "mappingDataFrame" TO CONTAIN ONLY ASSYMETRIC VARIABLES

mappingDataFrame <- mappingDataFrame[!(mappingDataFrame$Val == 0),]

View(mappingDataFrame)

#SPLITTING THE COLUMN VALUES OF "category_list" BASED ON "|" SYMBOL AND DECIDING THE FIRST CATEGORY AS THE CATEGORY TYPE

ventureDataFrame$category_list <- sapply(strsplit(ventureDataFrame$category_list, "|", fixed = TRUE), head, 1)
ventureDataFrame$category_list <- as.character(ventureDataFrame$category_list)

#REPLACE "character(0)" WITH EMPTY STRING
ventureDataFrame$category_list <- gsub("character(0)", "", ventureDataFrame$category_list, fixed = TRUE)
#View(ventureDataFrame)
#str(ventureDataFrame)

#INNERJOIN BETWEEN "ventureDataFrame" AND "mappingDataFrame"

result <- merge(x=ventureDataFrame, y=mappingDataFrame[,c("category_list", "main_sector")], by.x = "category_list", by.y="category_list", all.x = TRUE)
View(result)


#######################################
##  CHECKPOINT 5: SECTOR ANALYSIS 2  ## 
##  TABKE 5.1                        ##
#######################################

#######################################################################################################################
# IMPORTANT NOTE ::                                                                                                   #
#                                                                                                                     #
# WE HAVE IDENTIFIED THE ORDEROF COUNTRIES TO BE INVESTED AS MENTIONED BELOW                                          # 
# 1.  USA (United States)                                                                                             #
# 2.  GBR (United Kingdom)                                                                                            #
# 3.  IND (INDIA)                                                                                                     #
#                                                                                                                     #
# NOW WE CREATE THREE DATA FRAMES D1, D2, D3 WITH                                                                     #
# D1 CONTAINING INFORMATION RELATED TO "USA"                                                                          #  
# D2 CONTAINING INFORMATION RELATED TO "GBR"                                                                          #
# D3 CONTAINING INFORMATION RELATED TO "IND"                                                                          #
#######################################################################################################################

# D1 DATAFRAME CONTAINS ENTRIES FROM "result" DATAFRAME BELONGING TO COUNTRY "USA"
#---------------------------------------------------------------------------------

# GENERATE DATA FRAME, "D1"

D1 <- subset(result, result$country_code == "USA")
View(D1)

# TOTAL NUMBER OF INVESTMENTS IN USA

numberofInvestments_In_D1 <- nrow(D1)
numberofInvestments_In_D1

# TOTAL INVESTMENTS IN USA

totalInvestment_In_D1 <- sum(D1$raised_amount_usd)
totalInvestment_In_D1

# SECTOR WISE INVESTMENT AMOUNTS FOR COUNTRY USA IN SORTED ORDER

sectorWiseInfo_In_D1 <- aggregate(list(Number_of_investments=D1$company_permalink), by=list(MainSector=D1$main_sector), FUN = NROW)
sectorWiseInfo_In_D1_DecreasingOrder <- sectorWiseInfo_In_D1[with(sectorWiseInfo_In_D1, order(-Number_of_investments)),]


## FIRST BEST SECTOR IN USA ##
#----------------------------#

# DATA RELATED TO TOP INVESTMENT SECTOR IN USA

topFirstSector_In_D1 <- subset(D1, D1$main_sector == "Others")

# COMPANY WISE INVESTMENT IN TOP SECTOR IN USA IN SORTED ORDER

companywiseInvestement_In_TopFirstSector_In_D1 <- aggregate(list(Total_Investment=topFirstSector_In_D1$raised_amount_usd), by=list(Company_Name=topFirstSector_In_D1$name), FUN = sum)
companywiseInvestement_In_TopFirstSector_In_D1_DecreasingOrder <- companywiseInvestement_In_TopFirstSector_In_D1[with(companywiseInvestement_In_TopFirstSector_In_D1, order(-Total_Investment)),]

## SECOND BEST SECTOR IN USA ##
#-----------------------------#

# DATA RELATED TO SECOND TOP INVESTMENT SECTOR IN USA

topSecondSector_In_D1 <- subset(D1, D1$main_sector == "Cleantech / Semiconductors")

# COMPANY WISE INVESTMENT IN SECOND TOP SECTOR IN USA IN SORTED ORDER

companywiseInvestement_In_TopSecondSector_In_D1 <- aggregate(list(Total_Investment=topSecondSector_In_D1$raised_amount_usd), by=list(Company_Name=topSecondSector_In_D1$name), FUN = sum)
companywiseInvestement_In_TopSecondSector_In_D1_DecreasingOrder <- companywiseInvestement_In_TopSecondSector_In_D1[with(companywiseInvestement_In_TopSecondSector_In_D1, order(-Total_Investment)),]



# D2 DATAFRAME CONTAINS ENTRIES FROM "result" DATAFRAME BELONGING TO COUNTRY "GBR"
#---------------------------------------------------------------------------------

# GENERATE DATA FRAME, "D2"

D2 <- subset(result, result$country_code == "GBR")
View(D2)

# TOTAL NUMBER OF INVESTMENTS IN GBR

numberofInvestments_In_D2 <- nrow(D2)
numberofInvestments_In_D2

# TOTAL INVESTMENTS IN GBR

totalInvestment_In_D2 <- sum(D2$raised_amount_usd)
totalInvestment_In_D2

# SECTOR WISE INVESTMENT AMOUNTS FOR COUNTRY GBR IN SORTED ORDER

sectorWiseInfo_In_D2 <- aggregate(list(Number_of_investments=D2$company_permalink), by=list(MainSector=D2$main_sector), FUN = NROW)
sectorWiseInfo_In_D2_DecreasingOrder <- sectorWiseInfo_In_D2[with(sectorWiseInfo_In_D2, order(-Number_of_investments)),]

## FIRST BEST SECTOR IN GBR ##
#----------------------------#

# DATA RELATED TO TOP INVESTMENT SECTOR IN GBR

topFirstSector_In_D2 <- subset(D2, D2$main_sector == "Others")

# COMPANY WISE INVESTMENT IN TOP SECTOR IN GBR IN SORTED ORDER

companywiseInvestement_In_TopFirstSector_In_D2 <- aggregate(list(Total_Investment=topFirstSector_In_D2$raised_amount_usd), by=list(Company_Name=topFirstSector_In_D2$name), FUN = sum)
companywiseInvestementinTopFirstSector_In_D2_DecreasingOrder <- companywiseInvestement_In_TopFirstSector_In_D2[with(companywiseInvestement_In_TopFirstSector_In_D2, order(-Total_Investment)),]

## SECOND BEST SECTOR IN GBR ##
#-----------------------------#

# DATA RELATED TO SECOND TOP INVESTMENT SECTOR IN GBR

topSecondSector_In_D2 <- subset(D2, D2$main_sector == "Social, Finance, Analytics, Advertising")

# COMPANY WISE INVESTMENT IN SECOND TOP SECTOR IN GBR IN SORTED ORDER

companywiseInvestement_In_TopSecondSector_In_D2 <- aggregate(list(Total_Investment=topSecondSector_In_D2$raised_amount_usd), by=list(Company_Name=topSecondSector_In_D2$name), FUN = sum)
companywiseInvestementinTopSecondSector_In_D2_DecreasingOrder <- companywiseInvestement_In_TopSecondSector_In_D2[with(companywiseInvestement_In_TopSecondSector_In_D2, order(-Total_Investment)),]

# D3 DATAFRAME CONTAINS ENTRIES FROM "result" DATAFRAME BELONGING TO COUNTRY "IND"
#---------------------------------------------------------------------------------

# GENERATE DATA FRAME, "D3"

D3 <-subset(result, result$country_code == "IND")
View(D3)

# TOTAL NUMBER OF INVESTMENTS IN IND

numberofInvestments_In_D3 <- nrow(D3)
numberofInvestments_In_D3

# TOTAL INVESTMENTS IN IND

totalInvestment_In_D3 <- sum(D3$raised_amount_usd)
totalInvestment_In_D3

# SECTOR WISE INVESTMENT AMOUNTS IN COUNTRY IND IN SORTED ORDER

sectorWiseInfo_In_D3 <- aggregate(list(Number_of_investments=D3$company_permalink), by=list(MainSector=D3$main_sector), FUN = NROW)
sectorWiseInfo_In_D3_DecreasingOrder <- sectorWiseInfo_In_D3[with(sectorWiseInfo_In_D3, order(-Number_of_investments)),]

## FIRST BEST SECTOR IN IND ##
#----------------------------#

# DATA RELATED TO TOP INVESTMENT SECTOR IN IND

topFirstSector_In_D3 <- subset(D3, D3$main_sector == "Others")

# COMPANY WISE INVESTMENT IN TOP SECTOR IN IND IN SORTED ORDER

companywiseInvestement_In_TopFirstSector_In_D3 <- aggregate(list(Total_Investment=topFirstSector_In_D3$raised_amount_usd), by=list(Company_Name=topFirstSector_In_D3$name), FUN = sum)
companywiseInvestement_In_TopFirstSector_In_D3_DecreasingOrder <- companywiseInvestement_In_TopFirstSector_In_D3[with(companywiseInvestement_In_TopFirstSector_In_D3, order(-Total_Investment)),]


## SECOND BEST SECTOR IN IND ##
#-----------------------------#

# DATA RELATED TO SECOND TOP INVESTMENT SECTOR IN IND

topSecondSector_In_D3 <- subset(D3, D3$main_sector == "Social, Finance, Analytics, Advertising")

# COMPANY WISE INVESTMENT IN TOP SECOND SECTOR IN IND IN SORTED ORDER

companywiseInvestement_In_TopSecondSector_In_D3 <- aggregate(list(Total_Investment=topSecondSector_In_D3$raised_amount_usd), by=list(Company_Name=topSecondSector_In_D3$name), FUN = sum)
companywiseInvestement_In_TopSecondSector_In_D3_DecreasingOrder <- companywiseInvestement_In_TopSecondSector_In_D3[with(companywiseInvestement_In_TopSecondSector_In_D3, order(-Total_Investment)),]
