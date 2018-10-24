
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
write_csv(wfpvam_foodprices, "WFPpricesSahelBYA")