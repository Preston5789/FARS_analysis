getwd()
setwd("/Users/Preston/Desktop/RProgramming/FARS_Project/data-raw/yearly_person_data")
library(foreign)
yearone <- read.dbf("2000.dbf", as.is = FALSE)
head(yearone)
colnames(yearone)
for(i in 1999:2010){
  
  }



for(study_year in 1999:2010){
df <- clean_yearly_person_file(study_year)
if(study_year == 1999){
clean_fars <- df
} else {
clean_fars <- rbind(clean_fars, df)
}
}
save(clean_fars, file = "data/clean_fars.RData")