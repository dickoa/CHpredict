library(tidyverse)

#imports yearly averages 
RBD_Food_Prices_and_Indicators_Jul_2014 <- read_excel("price map changes/RBD Food Prices and Indicators - Jul 2014.xlsx")
RBD_Food_Prices_and_Indicators_Jul_2015 <- read_excel("price map changes/RBD Food Prices and Indicators - Jul 2015.xlsx")
RBD_Food_Prices_and_Indicators_Jul_2016 <- read_excel("price map changes/RBD Food Prices and Indicators - Jul 2016.xlsx")
RBD_Food_Prices_and_Indicators_Jul_2017 <- read_excel("price map changes/RBD Food Prices and Indicators - Jul 2017.xlsx")
RBD_Food_Prices_and_Indicators_Jul_2018 <- read_excel("price map changes/RBD Food Prices and Indicators - Jul 2018.xlsx")
#puts them together

#puts all back togther 
RBD_Food_Prices_and_Indicators <- rbind(RBD_Food_Prices_and_Indicators_Jul_2014, RBD_Food_Prices_and_Indicators_Jul_2015, RBD_Food_Prices_and_Indicators_Jul_2016, RBD_Food_Prices_and_Indicators_Jul_2017, RBD_Food_Prices_and_Indicators_Jul_2018)
#removes duplicate market id;
RBD_Food_Prices_and_Indicators <- RBD_Food_Prices_and_Indicators[!duplicated(RBD_Food_Prices_and_Indicators$mkt_id), ]
#keep only Sahel Countries + Nigeria 
RBD_Food_Prices_and_Indicators <- RBD_Food_Prices_and_Indicators  %>% filter(adm0_name == "Burkina Faso" | adm0_name == "Chad" |  adm0_name == "Mali" | adm0_name == "Mauritania" | adm0_name == "Niger" | adm0_name == "Nigeria"| adm0_name == "Senegal")
#keep only Admawa, Borno, Yobe in Nigeria and other Sahel Countries
RBD_Food_Prices_and_Indicators  <- RBD_Food_Prices_and_Indicators  %>% filter(adm0_name != "Nigeria" | adm1_name =="Adamawa" | adm1_name =="Borno"| adm1_name =="Yobe")
#keep only variables for 
RBD_Food_Prices_and_Indicators <- RBD_Food_Prices_and_Indicators %>% select(adm1_id, adm1_name, adm2_id, adm2_name,	mkt_id,	mkt_name)
colnames(RBD_Food_Prices_and_Indicators)[colnames(RBD_Food_Prices_and_Indicators)=="adm1_id"] <- "adm1_gaulcode"
colnames(RBD_Food_Prices_and_Indicators)[colnames(RBD_Food_Prices_and_Indicators)=="adm2_id"] <- "adm2_gaulcode"
#some markets do not have adm2 codes or names - but I dont have time to do this now
write_csv(RBD_Food_Prices_and_Indicators, "RBD_Market_Names.csv")