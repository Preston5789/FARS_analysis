########################################
library(stats)
perc_cis <- function(x, n) {
  p <- x / n
  se <- sqrt((p * (1 - p)) / n)
  up_conf <- round((p + 1.96*se)*100, digits = 1)
  low_conf <- round((p - 1.96*se)*100, digits = 1)
  p <- round(p*100, digits = 1)
  complete <- paste0(p, "% (", low_conf, "%, ", up_conf, "%)")
return(complete)
}


##################


test_trend_ca <- function(drug, data=clean_fars ) {
 ##Establish Non-Alcohol
   if(drug == "Nonalcohol"){
      to_test <- filter(data, drug_type != "Alcohol")
    }else{
      to_test <- filter(data, drug_type == drug)
    }
  #Going off of hw-template
  orgo_test<-to_test %>%
    group_by(YEAR) %>%
    summarize(positive = sum(positive_for_drug, na.rm = TRUE),
              trials = sum(!is.na(positive_for_drug)))
  
  ca_alcohol <- prop.trend.test(x = orgo_test$positive, n = orgo_test$trials)
  z<-sqrt(ca_alcohol$statistic)
  return(tibble(Z=round(z, digits=2), p.value= round(ca_alcohol$p.value, digits=2)))
}


###################


test_trend_log_reg <- function(drug, data = clean_fars) {
 ##Establish Non-Alcohol
   if(drug == "Nonalcohol"){
      to_test <- filter(data, drug_type != "Alcohol")
    }else{
      to_test <- filter(data, drug_type == drug)
    }
 
log_reg <- glm(positive_for_drug ~ YEAR, data = to_test, family = binomial(link = "logit"))
    
coeff<- summary(log_reg)$coefficients
  return(tibble(Z = round(coeff[2,3], digits = 1), p.value = round(coeff[2,4], digits = 2)))

}



