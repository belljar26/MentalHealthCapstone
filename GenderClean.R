#Cleaning Survey
# Clean Gender Columns
# Determine levels of variable " Gender"
b<- as.factor(survey$Gender)
levels(b)
# 45 levels of Gender-- will have them match to Male, Female, Other
survey$Gender[survey$Gender=="M"]<-"Male"
survey$Gender[survey$Gender=="m"] <-"Male"
survey$Gender[survey$Gender== "Agender"] <- "Other"
survey$Gender[survey$Gender=="All"] <- "Other"
survey$Gender[survey$Gender == "Androgyne"] <- "Other"
survey$Gender[survey$Gender == "Cis Female"] <- 'Other'
survey$Gender[survey$Gender== "cis male"] <- "Other"
survey$Gender[survey$Gender== "Cis Male"] <- "Other"
survey$Gender[survey$Gender== "Cis Man"] <- "Other"
survey$Gender[survey$Gender == "Enby"] <- "Other"
survey$Gender[survey$Gender == "f"] <-"Female"
survey$Gender[survey$Gender == "F"] <- "Female"
survey$Gender[survey$Gender== "femail"] <- "Other"
survey$Gender[survey$Gender== "Femake"] <- "Other"
survey$Gender[survey$Gender== "female"] <-"Female"
survey$Gender[survey$Gender == "Female (cis)"] <- "Other"
survey$Gender[survey$Gender== "Female (trans)"] <- "Other"
survey$Gender[survey$Gender== "fluid"] <- "Other"
survey$Gender[survey$Gender== "Genderqueer"] <- "Other"
survey$Gender[survey$Gender== "Guy (-ish) ^_^"] <- "Other"
survey$Gender[survey$Gender== "Mail"] <- "Other"
survey$Gender[survey$Gender == "maile"] <- "Other"
survey$Gender[survey$Gender == "Make"] <- "Male"
survey$Gender[survey$Gender == "male"] <- "Male"
survey$Gender[survey$Gender == "Male (CIS)"] <- "Other"
survey$Gender[survey$Gender == "male learning androgynous"]<- "Other"
survey$Gender[survey$Gender == "Male-ish"] <- "Other"
survey$Gender[survey$Gender =="Malr"] <- "Other"
survey$Gender[survey$Gender == "Man"] <- "Male"
survey$Gender[survey$Gender== "msle"] <- "Male"
survey$Gender[survey$Gender== "Nah"] <- "Other"
survey$Gender[survey$Gender== "Neuter"] <- "Other"
survey$Gender[survey$Gender == "non-binary"] <- "Other"
survey$Gender[survey$Gender== "ostensibly male, unsure what that
              really means" ] <- "Other"
survey$Gender[survey$Gender == "p"] <- "Other"
survey$Gender[survey$Gender == "queer"] <- "Other"
survey$Gender[survey$Gender =="queer/she/they"] <- "Other"
survey$Gender[survey$Gender == "something kinda male?"] <- "Other"
survey$Gender[survey$Gender == "Trans woman"] <- "Other"
survey$Gender [survey$Gender == "Trans-female"] <- "Other"
survey$Gender[survey$Gender == "woman"] <- "Female"
survey$Gender[survey$Gender == "Woman"] <- "Female"
survey$Gender[survey$Gender == "A little about you"] <- "Other"
survey$Gender[survey$Gender == "cis-female/femme"] <- "Other"
# save clean file
write.csv(survey, "survey_clean.csv")

