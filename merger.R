library(readxl)
library(tidyverse)

#import Sahel+BYA Cadre Harmonise CH data
cadre_harmonise_sahel <- read_excel("cadre_harmonise_sahel.xlsx")
#keep only Sep_Dec Current Estimates
cadre_harmonise_sahel <- cadre_harmonise_sahel %>% filter(exercise_code == 1, chtype == "current")
#keep only Admawa, Borno, Yobe in Nigeria and other Sahel Countries
cadre_harmonise_sahel <- cadre_harmonise_sahel %>% filter(adm0_name != "Nigeria" | adm1_name =="Adamawa" | adm1_name =="Borno"| adm1_name =="Yobe")
#create a reference period, reference year concatenation
cadre_harmonise_sahel$referenceconcat <- paste(cadre_harmonise_sahel$reference_code, cadre_harmonise_sahel$reference_year, sep = "", collapse = NULL)
#create a spoof Sep 2018 data set by filtering, copying, and adding
cadre_harmonise_sahelSep2018 <- cadre_harmonise_sahel %>% filter(referenceconcat==12017)
cadre_harmonise_sahelSep2018$reference_year <- 2018
cadre_harmonise_sahelSep2018$exercise_year <- 2018
cadre_harmonise_sahelSep2018$referenceconcat <- 12018
cadre_harmonise_sahel <- rbind(cadre_harmonise_sahel, cadre_harmonise_sahelSep2018)
#recode NA text into NA 
cadre_harmonise_sahel$adm1_pcod3[cadre_harmonise_sahel$adm1_pcod3=="NA"] <- NA
cadre_harmonise_sahel$adm2_pcod3[cadre_harmonise_sahel$adm2_pcod3=="NA"] <- NA
cadre_harmonise_sahel$adm1_gaulcode[cadre_harmonise_sahel$adm1_gaulcode=="NA"] <- NA
cadre_harmonise_sahel$adm2_gaulcode[cadre_harmonise_sahel$adm2_gaulcode=="NA"] <- NA
#creates adm1/adm2merge OCHA code to link with other datasets
cadre_harmonise_sahel$adm_merge <- ifelse(is.na(cadre_harmonise_sahel$adm2_pcod3), cadre_harmonise_sahel$adm1_pcod3, cadre_harmonise_sahel$adm2_pcod3)
cadre_harmonise_sahel$finalmerge <- paste(cadre_harmonise_sahel$adm_merge, cadre_harmonise_sahel$reference_year, sep = "", collapse = NULL)
#creates adm1/adm2merge GAUL code to link with other datasets
cadre_harmonise_sahel$adm_merge_gaul <- ifelse(is.na(cadre_harmonise_sahel$adm2_gaulcode), cadre_harmonise_sahel$adm1_gaulcode, cadre_harmonise_sahel$adm2_gaulcode)
cadre_harmonise_sahel$finalmerge_gaul <- paste(cadre_harmonise_sahel$adm_merge_gaul, cadre_harmonise_sahel$reference_year, sep = "", collapse = NULL)

##ACF biomass data - take the average vulnerability index for each year / adm1 in some areas and adm2 in the reast
#import ACF bio_mass data 
bio_adm2 <- read_csv("bio_adm2.csv")
# Rename admin0, admin1 and admin2 names in column in R
colnames(bio_adm2)[colnames(bio_adm2)=="admin0Name"] <- "adm0_name"
colnames(bio_adm2)[colnames(bio_adm2)=="admin1Name"] <- "adm1_name"
colnames(bio_adm2)[colnames(bio_adm2)=="admin2Name"] <- "adm2_name"
colnames(bio_adm2)[colnames(bio_adm2)=="adm1Pcod3"] <- "adm1_pcod3"
colnames(bio_adm2)[colnames(bio_adm2)=="adm2Pcod3"] <- "adm2_pcod3"
#keep only Sahel Countries + Nigeria 
bio_adm2 <- bio_adm2  %>% filter(adm0_name == "Burkina Faso" | adm0_name == "Chad" |  adm0_name == "Mali" | adm0_name == "Mauritania" | adm0_name == "Niger" | adm0_name == "Nigeria"| adm0_name == "Senegal")
#keep only Admawa, Borno, Yobe in Nigeria and other Sahel Countries
bio_adm2 <- bio_adm2 %>% filter(adm0_name != "Nigeria" | adm1_name =="Adamawa" | adm1_name =="Borno"| adm1_name =="Yobe")
#keep only the geographic, time variables and the vulnerability index
bio_adm2 <- bio_adm2 %>% select(adm0_name, admin0Pcod,	adm1_name, adm2_name,	admin1Pcod,	admin2Pcod,	admi0Pcod3,	adm1_pcod3, adm2_pcod3,	IDBIOHYDRO,	NAME,	AREA,	VI_2013, VI_2014,	VI_2015,	VI_2016,	VI_2017,	VI_2018)
#flip the date columns into rows
bio_adm2long <- gather(data = bio_adm2, 
                       key = year, 
                       value = vi,
                       VI_2013:VI_2018)
#creates date value 
bio_adm2long$reference_year[bio_adm2long$year=="VI_2013"] <- 2013
bio_adm2long$reference_year[bio_adm2long$year=="VI_2014"] <- 2014
bio_adm2long$reference_year[bio_adm2long$year=="VI_2015"] <- 2015
bio_adm2long$reference_year[bio_adm2long$year=="VI_2016"] <- 2016
bio_adm2long$reference_year[bio_adm2long$year=="VI_2017"] <- 2017
bio_adm2long$reference_year[bio_adm2long$year=="VI_2018"] <- 2018
#creates concatenation
bio_adm2long$referenceconcat <- paste(bio_adm2long$adm0_name, bio_adm2long$reference_year, sep = "", collapse = NULL)
#splits the files - Mauritania, Chad 2014 and rest for purpose of aggregation
MRTbio <- bio_adm2long %>% filter(adm0_name == "Mauritania")
NGAbio <- bio_adm2long %>% filter(bio_adm2long$referenceconcat  == "Nigeria2015" | bio_adm2long$referenceconcat  == "Nigeria2016")
Restbio <- bio_adm2long %>% filter(adm0_name != "Mauritania" & (bio_adm2long$referenceconcat != "Nigeria2015" & bio_adm2long$referenceconcat  != "Nigeria2016")) 
#aggregates to appropriate levels
MRTbio <- MRTbio %>% group_by(adm0_name, adm1_name, adm1_pcod3, reference_year) %>% summarize(avgvi = mean(vi))
NGAbio <- NGAbio %>% group_by(adm0_name, adm1_name, adm1_pcod3, reference_year) %>% summarize(avgvi = mean(vi))
Restbio <- Restbio %>% group_by(adm0_name, adm1_name, adm1_pcod3, adm2_name, adm2_pcod3, reference_year) %>% summarize(avgvi = mean(vi))
#puts all back togther 
totalbio <- rbind(MRTbio, NGAbio, Restbio)
#creates adm1/adm2merge code to link with CHSahel data
totalbio$adm_merge <- ifelse(is.na(totalbio$adm2_pcod3), totalbio$adm1_pcod3, totalbio$adm2_pcod3)
totalbio$finalmerge <- paste(totalbio$adm_merge, totalbio$reference_year, sep = "", collapse = NULL)
#just merge variable and vulnerability index
totalbio <- totalbio %>% select(finalmerge, avgvi)
#merges CH and ACF
CHACF <- left_join(cadre_harmonise_sahel, totalbio, by = "finalmerge")

###WFP price data - take the average price of key commodity / adm1 in some areas and adm2 in the rest
#import WFP price data 
wfpvam_foodprices <- read_csv("wfpvam_foodprices.csv")
#keep only July prices for every year
wfpvam_foodprices <- wfpvam_foodprices %>% filter(mp_month==7)
#renames year variable
colnames(wfpvam_foodprices)[colnames(wfpvam_foodprices)=="mp_year"] <- "reference_year"
#keep only Sahel Countries + Nigeria 
wfpvam_foodprices <- wfpvam_foodprices  %>% filter(adm0_name == "Burkina Faso" | adm0_name == "Chad" |  adm0_name == "Mali" | adm0_name == "Mauritania" | adm0_name == "Niger" | adm0_name == "Nigeria"| adm0_name == "Senegal")
#keep only Admawa, Borno, Yobe in Nigeria and other Sahel Countries
wfpvam_foodprices  <- wfpvam_foodprices  %>% filter(adm0_name != "Nigeria" | adm1_name =="Adamawa" | adm1_name =="Borno"| adm1_name =="Yobe")
#create a concatenation to identify key commodities for each country
wfpvam_foodprices$concat <-  paste(wfpvam_foodprices$adm0_name, wfpvam_foodprices$cm_name, sep = "", collapse = NULL)
#keep only key commodities for each country - maybe for prediction it would be good to keep all the prices
wfpvam_foodprices  <- wfpvam_foodprices  %>% filter(concat == "Burkina FasoSorghum - Retail" | concat == "ChadMillet - Retail" | concat == "MaliMillet - Retail" | 
                                                    concat == "MauritaniaRice (local) - Retail" | concat == "NigeriaYam - Retail" | concat == "NigerMillet - Retail"| concat == "SenegalMillet - Retail")
#import market names
RBD_Market_Names <- read_csv("RBD_Market_Names.csv")
#join with market names
wfpvam_foodprices <- left_join(wfpvam_foodprices, RBD_Market_Names, by = "mkt_id" )
#splits the files - Mauritania, Chad 2014 and rest for purpose of aggregation
MRTprice <- wfpvam_foodprices %>% filter(adm0_name == "Mauritania")
NGAprice <- wfpvam_foodprices %>% filter((adm0_name == "Nigeria" & reference_year == 2015) | (adm0_name == "Nigeria" & reference_year == 2016))
Restprice <- wfpvam_foodprices %>% filter((adm0_name != "Mauritania") & (adm0_name != "Nigeria" & reference_year != 2015) | ((adm0_name != "Nigeria" & reference_year != 2016)))
#aggregates to appropriate levels
MRTprice <- MRTprice %>% group_by(adm0_name, adm1_name.x, adm1_gaulcode, reference_year) %>% summarize(avgprice = mean(mp_price))
NGAprice <- NGAprice %>% group_by(adm0_name, adm1_name.x, adm1_gaulcode, reference_year) %>% summarize(avgprice = mean(mp_price))
Restprice <- Restprice %>% group_by(adm0_name, adm1_name.x, adm1_gaulcode, adm2_name, adm2_gaulcode, reference_year) %>% summarize(avgprice = mean(mp_price))
#puts all back togther 
totalprice <- rbind(MRTprice, NGAprice, Restprice)
#creates adm1/adm2merge code to link with CHSahel data
totalprice$adm_merge_gaul <- ifelse(is.na(totalprice$adm2_gaulcode), totalprice$adm1_gaulcode, totalprice$adm2_gaulcode)
totalprice$finalmerge_gaul <- paste(totalprice$adm_merge_gaul, totalprice$reference_year, sep = "", collapse = NULL)
#just merge variable and vulnerability index
totalpricenew <- totalprice %>% select(finalmerge_gaul, avgprice)
#merges CH and ACF
CHACFprice <- left_join(CHACF, totalpricenew, by = "finalmerge_gaul")

###ACLED conflict data - take the number of incidents in the June - September of each year (this could definetly be improved) - by adm1 in some areas and adm2 in the rest - I didnt finish geo-coding some areas - this still needs to be done
#import ACLED data - using the version changed in excel so we could have the month
Africa_2016_2018_Sep29 <- read_excel("Africa_2016-2018_Sep29r.xlsx")
# Rename admin0, admin1 and admin2 names in column in R
colnames(Africa_2016_2018_Sep29)[colnames(Africa_2016_2018_Sep29)=="COUNTRY"] <- "adm0_name"
colnames(Africa_2016_2018_Sep29)[colnames(Africa_2016_2018_Sep29)=="ADMIN1"] <- "adm1_name"
colnames(Africa_2016_2018_Sep29)[colnames(Africa_2016_2018_Sep29)=="ADMIN2"] <- "adm2_name"
#keep only Sahel Countries + Nigeria 
Africa_2016_2018_Sep29 <- Africa_2016_2018_Sep29  %>% filter(adm0_name == "Burkina Faso" | adm0_name == "Chad" |  adm0_name == "Mali" | adm0_name == "Mauritania" | adm0_name == "Niger" | adm0_name == "Nigeria"| adm0_name == "Senegal")
#keep only Admawa, Borno, Yobe in Nigeria and other Sahel Countries
Africa_2016_2018_Sep29 <- Africa_2016_2018_Sep29 %>% filter(adm0_name != "Nigeria" | adm1_name =="Adamawa" | adm1_name =="Borno"| adm1_name =="Yobe")
#filters after 2014
Africa_2016_2018_Sep29 <- Africa_2016_2018_Sep29 %>% filter(YEAR >= 2014)
#filters only July - September
Africa_2016_2018_Sep29 <- Africa_2016_2018_Sep29 %>% filter(MONTH >= 6 & MONTH <= 9)
#filters out "Riots/Protests"
Africa_2016_2018_Sep29 <- Africa_2016_2018_Sep29 %>% filter(EVENT_TYPE != "Riots/Protests")
#splits Mauritania and the rest
conflictaggMRT <- Africa_2016_2018_Sep29 %>% filter(adm0_name == "Mauritania") %>% group_by(adm0_name, adm1_name, YEAR) %>% summarize(count = n())
conflictaggRest <- Africa_2016_2018_Sep29 %>% filter(adm0_name != "Mauritania") %>% group_by(adm0_name, adm1_name, adm2_name, YEAR) %>% summarize(count = n())
#renames Mauritania adm1s to match codes and adds adm1 codes
conflictaggMRT$adm1_name[conflictaggMRT$adm1_name=="Hodh Ech Chargui"] <- "Hodh Ech Chargi"
conflictaggMRT$adm1_name[conflictaggMRT$adm1_name=="Tiris Zemmour"] <- "Tiris-Zemmour"
MRTadmin1codes <- read_csv("MRTadmin1codes.csv")
conflictaggMRT <- left_join(conflictaggMRT, MRTadmin1codes, by = "adm1_name")
#renames rest of countries admin2s and adds adm1 codes -ARRRGH I dont have time to do this - can clean this up later
REStadmin2codes <- read_csv("REStadmin2codes.csv")
conflictaggRest <- left_join(conflictaggRest, REStadmin2codes, by = "adm2_name")
#puts Mauritania and Rest together
conflictagg <- rbind(conflictaggMRT, conflictaggRest)
#recode NA text into NA 
conflictagg$adm1_pcod3[conflictagg$adm1_pcod3=="NA"] <- NA
conflictagg$adm2_pcod3[conflictagg$adm2_pcod3=="NA"] <- NA
#creates adm1/adm2merge code to link with CHSahel data
conflictagg$adm_merge <- ifelse(is.na(conflictagg$adm2_pcod3), conflictagg$adm1_pcod3, conflictagg$adm2_pcod3)
conflictagg$finalmerge <- paste(conflictagg$adm_merge, conflictagg$YEAR, sep = "", collapse = NULL)
#just merge variable and vulnerability index
conflictagg <- conflictagg %>% select(finalmerge, count)
#merges CH and ACF
CHACFpriceACLED <- left_join(CHACFprice, conflictagg, by = "finalmerge")
#recode missing conflict to 0 
CHACFpriceACLED$count[is.na(CHACFpriceACLED$count)] <- 0

write_csv(CHACFpriceACLED, "CHACFpriceACLED.csv")





