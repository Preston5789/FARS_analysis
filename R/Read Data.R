getwd()
setwd("/Users/Preston/Desktop/R/FARS_Project/data-raw/yearly_person_data")
library(foreign)
library(knitr)
library(tidyverse)
library(dplyr)
library(ggthemes)


#States
state<-c(6,15,17,33,44,54)

#Defining DRUGS
None<-1
Narcotic<- 100:295 
Depressant<-300:395 
Stimulant<-400:495 
Cannabinoid<-600:695 
Other<-c(500:595,700:795,800:895,900:996) 

Drugs_Cond<-c(None,Narcotic,Depressant,Stimulant,Cannabinoid,Other)
alc_cond<-0:94


data_generator <- function(i){    
  
##Filtering
   #Read to Data Frame
    final<- read.dbf(paste0(i,".dbf"), as.is = FALSE)
  #Select Relevant Columns
    final<-select(final,STATE, VEH_NO, PER_NO, AGE, SEX, PER_TYP, ALC_RES, DRUGRES1, DRUGRES2, DRUGRES3, INJ_SEV, LAG_HRS, LAG_MINS, ST_CASE)
  #Only Lethal Deaths
    final<- filter(final, INJ_SEV==4)
  #Only Drivers
    final<-filter(final,PER_TYP==1)
  #Only Select States
    final<-filter(final,STATE %in% state)
  #Died Less than 60 minutes
    final<-filter(final, 60*LAG_HRS+LAG_MINS <61)
    final<-select(final,-INJ_SEV,-PER_TYP,-LAG_MINS,-LAG_HRS)
    final <- mutate(final,unique_id = paste0(ST_CASE,"_",STATE,"_",PER_NO,"_",i))
  #Cleaning
    
  final <-mutate(final, DRUGRES1 = ifelse(DRUGRES1 %in% Drugs_Cond, DRUGRES1, NA))%>%
       mutate(DRUGRES2 = ifelse(DRUGRES2 %in% Drugs_Cond, DRUGRES2, NA))%>%
       mutate(DRUGRES3 = ifelse(DRUGRES3 %in% Drugs_Cond, DRUGRES3, NA))
     final<-mutate(final,Alcohol = ifelse(ALC_RES %in% 01:94, "True", ifelse(ALC_RES == 00,"False",NA))) 
    
    
    final<- mutate(final, YEAR=i)
    #Right Order
    final <- select(final, unique_id, SEX, YEAR, AGE,Alcohol, ALC_RES, DRUGRES1,DRUGRES2,DRUGRES3)
   ## final$AGE <- cut(final$AGE, breaks = c(0,24,44,64,120), labels=(c("< 25","25--44","45--64","> 65")))
 
    final <- mutate(final, AGE = ifelse(i < 2009 & AGE > 97, NA, AGE)) %>%
    mutate(agecat = cut(AGE, breaks = c(0, 24, 44, 64, 120), labels = (c("< 25","25--44","45--64","> 65")))) 
    final$SEX<-ifelse(final$SEX %in% 8:9, NA, final$SEX)
  final$SEX<-factor(final$SEX,levels=c(1,2), labels = c("Male","Female"))
  
  ##MAGIC
  gathered_df <- final %>%
  tidyr::gather(drug_number, drug_type_raw, contains("drugres")) %>% 
    dplyr::mutate(drug_type = ifelse(drug_type_raw %in% 100:295,
                      "Narcotic", NA),
  drug_type = ifelse(drug_type_raw %in% 300:395,
                    "Depressant", drug_type), 
  drug_type = ifelse(drug_type_raw %in% 400:495,
                     "Stimulant", drug_type), 
  drug_type = ifelse(drug_type_raw %in% 600:695,
                     "Cannabinoid", drug_type),
  drug_type = ifelse(drug_type_raw %in% c(500:595, 700:996),
                     "Other", drug_type), 
  drug_type = ifelse(drug_type_raw == 1, "None", drug_type),
  drug_type = factor(drug_type)) %>% dplyr::select(-drug_type_raw, -drug_number) %>%
 
  # Filter out any observations where both alcohol and drug data is missing
dplyr::filter(!(is.na(Alcohol) & is.na(drug_type)))


# Create a subset with only individuals with at least one non-missing # listing for drugs
library(dplyr)
# listing for drugs
non_missing_drugs <- gathered_df %>%
filter(!is.na(drug_type)) %>% 
group_by(unique_id, drug_type) %>%
mutate(has_drug = TRUE) %>%
ungroup() %>%
dplyr::mutate(row_num = 1:n()) %>%
spread(drug_type, has_drug, fill = FALSE)%>% 
select(-row_num,-ALC_RES)

# Join this back into the full dataset
final <- final %>%
    dplyr::select(-DRUGRES1, -DRUGRES2, -DRUGRES3,-ALC_RES,-AGE) 
    final<-full_join(final,non_missing_drugs, by = c("unique_id","SEX","Alcohol","YEAR","agecat")) 
 final<- select(final,-None) 

final <- gather(final,drug_type, positive_for_drug, Alcohol, 
                  Cannabinoid,Depressant, Narcotic, Other, Stimulant) 
  final<-mutate(final,drug_type = factor(drug_type))
 final<- mutate(final, positive_for_drug = as.logical(positive_for_drug))
##names(df)[,2]<-"sex"
##names(df)[,3]<-"year"
 final<-select(final, -AGE)
 return(final)
 }


for(i in 1999:2010){
  if(i == 1999){
clean_fars <- data_generator(i)
}
  else {
    temp<-data_generator(i)
clean_fars <- rbind(clean_fars,temp)
}
}
setwd("/Users/Preston/Desktop/R/FARS_Project")
save(clean_fars, file = "data/clean_fars.RData")

