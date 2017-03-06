summary(survey_clean)
#Clean care options variable--there is a space between Not sure--change to "unsure"
survey_clean$care_options[survey_clean$care_options== "Not sure"]<- "Unsure"
survey_clean[is.na(survey_clean$care_options),]
#Clean benefits--space between don't know
survey_clean$benefits[survey_clean$benefits == "Don't know"] <- "DK"
#Clean wellness program variable-- space between "don't know"
survey_clean$wellness_program[survey_clean$wellness_program == "Don't know"]<- "DK"
#Clean anonymity variable-- space between "don't know"
survey_clean$anonymity[survey_clean$anonymity == "Don't know"] <- "DK"
# Clean coworkers and supervisors columns-- some of them --spaces
survey_clean$coworkers[survey_clean$coworkers == "Some of them"] <- "Some"
survey_clean$supervisor[survey_clean$supervisor == "Some of them"] <- "Some"
#Clean mentalvs phys column--space between Don't know
survey_clean$mental_vs_physical[survey_clean$mental_vs_physical == "Don't know"] <- "DK"
#Clean seek help column-space in Don't know
survey_clean$seek_help[survey_clean$seek_help == "Don't know"]<- "DK"
#Clean leave column--many spaces 
survey_clean$leave[survey_clean$leave == "Don't know"]<- "DK"
survey_clean$leave[survey_clean$leave == "Very easy"] <- "Veryeasy"
survey_clean$leave[survey_clean$leave == "Somewhat difficult"]<- "Somediff"
survey_clean$leave[survey_clean$leave == "Somewhat easy"] <- "Someasy"
survey_clean$leave[survey_clean$leave == "Very difficult"]<-"Verydiff"
#save cleaned file
write_csv(survey_clean, "survey_clean1.csv")

