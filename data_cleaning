#Introduction - Mapping Masculinity Project - Data Cleaning
#Project Title: MCA and men's medical decision-making
#Code Revised: January 25, 2021

#Paper Authors: Daniel Nystrom*, Cristina del Campo^, Jose Andres Fernandez-Cornejo^
#Code: Daniel Nystrom*
#*University of California Irvine
#^Universidad Complutense de Madrid

#Data sourced from FiveThirtyEight.com's "masculinity_survey"
#Creative Commons Attribution 4.0 International Public License
#File directory included below


#Step 1 - Load required packages and import raw data####

#Install packages:
if(!require(pacman)) install.packages("pacman")
pacman::p_load(pacman, tidyverse, curl, naniar)
#pacman: Primarily for p_load here, keeps package installation tidy
#tidyverse: For its vast supply of powerful data wrangling libraries
#curl: For importing data from GitHub URL
#naniar: For analyzing and visualizing missing values

#Import dataset from FiveThirtyEight.com's GitHub page with directory
raw_data <- read.csv(curl("https://raw.githubusercontent.com/fivethirtyeight/data/master/masculinity-survey/raw-responses.csv"))
ls(raw_data) #variable names



#Step 2 - Data Cleaning and Feature Engineering######

#Variable 1 - How masculine/manly do you feel? - MascFeel####
MascFeel <- raw_data[,4] #pull relevant variable
levels(MascFeel) <- c(NA, "m.feel_no", "m.feel_low", "m.feel_med", "m.feel_high") #recode

#Variable 2 - How important is it that you appear masculine - MascApp ####
MascLook <- raw_data[,5] #pull relevant variable
levels(MascLook) <- c(NA, "m.app_no", "m.app_low", "m.app_med", "m.app_high")

#Variable 3 - Origin of respondents' sense of normative manhood - MascOrigin ####
raw_data_sub1 <- within(raw_data[,6:11], { #pull relevant variables from raw data
  #recode factors as binary 0/1 responses
  levels(q0004_0001) <- c("1","0")
  levels(q0004_0002) <- c("1","0")
  levels(q0004_0003) <- c("0","1")
  levels(q0004_0004) <- c("0","1")
  levels(q0004_0005) <- c("1","0")
  levels(q0004_0006) <- c("0","1")
  #correct order of factors for three variables
  q0004_0001 <- factor(q0004_0001, levels = c("0", "1"))
  q0004_0002 <- factor(q0004_0002, levels = c("0", "1"))
  q0004_0005 <- factor(q0004_0005, levels = c("0", "1"))
  #change binary factors to numeric integers (1:2) and correct to 0:1
  father <- as.numeric(q0004_0001) - 1
  mother <- as.numeric(q0004_0002) - 1
  ofam <- as.numeric(q0004_0003) - 1
  culture <- as.numeric(q0004_0004) - 1
  peers <- as.numeric(q0004_0005) - 1
  osoc <- as.numeric(q0004_0006) - 1
  #sum the binary variables to account for family vs. societal origins
  fam <- father + mother + ofam
  soc <- culture + peers + osoc
  #and use the intermediary variables as basis for MascOrigin
  MascOrigin <- ifelse(fam >= 1, ifelse(soc >= 1, 'o_both', 'o_family'),
                       ifelse(soc >= 1, "o_society", "o_none"))
})
MascOrigin <- factor(raw_data_sub1$MascOrigin)
rm(raw_data_sub1)

#Variable 4 - Daily worry about appearance - AppWorry ####
raw_data_sub2 <- within(raw_data[,c(24:27,29)], { #pull relevant variables from raw data
  #recode factors as binary 0/1 responses
  levels(q0008_0001) <- c("0","1")
  levels(q0008_0002) <- c("0","1")
  levels(q0008_0003) <- c("0","1")
  levels(q0008_0004) <- c("0","1")
  levels(q0008_0006) <- c("0","1")
  #change binary factors to numeric integers (1:2) and correct to 0:1
  height <- as.numeric(q0008_0001) - 1
  weight <- as.numeric(q0008_0002) - 1
  hair <- as.numeric(q0008_0003) - 1
  physique <- as.numeric(q0008_0004) - 1
  clothing <- as.numeric(q0008_0006) - 1
  #sum the binary variables to account for physique- vs. style-related worry
  phys <- height + weight + physique
  style <- hair + clothing
  #and use the intermediary variables as basis for AppWorry
  AppWorry <- ifelse(phys >= 1, ifelse(style >= 1, 'a.worry_both', 'a.worry_phys'),
                       ifelse(style >= 1, "a.worry_style", "a.worry_nr"))
})
AppWorry <- factor(raw_data_sub2$AppWorry)
rm(raw_data_sub2)

#Variable 5 - Daily worry about sex - SexWorry ####
raw_data_sub3 <- within(raw_data[,c(28,30)], { #pull relevant variables from raw data
  #recode factors as binary 0/1 responses
  levels(q0008_0005) <- c("1","0")
  levels(q0008_0007) <- c("0","1")
  #correct order of factors for one variables
  q0008_0005 <- factor(q0008_0005, levels = c("0", "1"))
  #change binary factors to numeric integers (1:2) and correct to 0:1
  gen <- as.numeric(q0008_0005) - 1
  sex <- as.numeric(q0008_0007) - 1
  #and use the intermediary variables as basis for SexWorry
  SexWorry <- ifelse(sex >= 1, ifelse(gen >= 1, 's.worry_both', "s.worry_perf"),
                     ifelse(gen >= 1, "s.worry_gen", "s.worry_nr"))
})
SexWorry <- factor(raw_data_sub3$SexWorry)
rm(raw_data_sub3)

#Variable 6 - Daily worry about health - HealthWorry ####
raw_data_sub4 <- within(raw_data[,31:32], { #pull relevant variables from raw data
  #recode factors as binary 0/1 responses
  levels(q0008_0008) <- c("0","1")
  levels(q0008_0009) <- c("0","1")
  #change binary factors to numeric integers (1:2) and correct to 0:1
  h.ment <- as.numeric(q0008_0008) - 1
  h.phys <- as.numeric(q0008_0009) - 1
  #and use the intermediary variables as basis for HealthWorry
  HealthWorry <- ifelse(h.ment >= 1, ifelse(h.phys >= 1, 'h.worry_both', 'h.worry_ment'),
                     ifelse(h.phys >= 1, "h.worry_phys", "h.worry_nr"))
})
HealthWorry <- factor(raw_data_sub4$HealthWorry)
rm(raw_data_sub4)

#Variable 7 - Daily worry about finance - FinWorry ####
raw_data_sub5 <- within(raw_data[,33:34], { #pull relevant variables from raw data
  #recode factors as binary 0/1 responses
  levels(q0008_0010) <- c("0","1")
  levels(q0008_0011) <- c("0","1")
  #change binary factors to numeric integers (1:2) and correct to 0:1
  finances <- as.numeric(q0008_0010) - 1
  provide <- as.numeric(q0008_0011) - 1
  #and use the intermediary variables as basis for FinWorry
  FinWorry <- ifelse(finances >= 1, ifelse(provide >= 1, 'f.worry_both', 'f.worry_personal'),
                     ifelse(provide >= 1, "f.worry_providing", "f.worry_nr"))
})
FinWorry <- factor(raw_data_sub5$FinWorry)
rm(raw_data_sub5)

#Variable 8 - Does the respondent have children? - Children####
raw_data_sub6 <- within(raw_data[,81:83], { #pull relevant variables from raw data
  #recode factors as binary 0/1 responses
  levels(q0025_0001) <- c("0","1")
  levels(q0025_0002) <- c("0","1")
  levels(q0025_0003) <- c("1","0")
  #correct order of factors for one variables
  q0025_0003 <- factor(q0025_0003, levels = c("0", "1"))
  #change binary factors to numeric integers (1:2) and correct to 0:1
  young <- as.numeric(q0025_0001) - 1
  old <- as.numeric(q0025_0002) - 1
  none <- as.numeric(q0025_0003) - 1
  #sum the binary variables to account for presence of any children
  yes <- young + old
  #and use the intermediary variables as basis for FinWorry
  Children <- ifelse(yes >= 1, "child_yes", 
                     ifelse(none >= 1, "child_no", NA))
})
Children <- factor(raw_data_sub6$Children)
rm(raw_data_sub6)

#Variable 9 - Respondents' income level - Income ####
Income <- raw_data[,88]
levels(Income) <- c("inc_low","inc_low","inc_high","inc_high","inc_high",
                    "inc_high","inc_high","inc_low","inc_mid","inc_mid",
                    "inc_nr")

#Variable 10 - Respondents' Sexual Orientation - Orientation ####
Orientation <- raw_data[,84]
levels(Orientation) <- c("so_bi","so_gay",NA,"so_other","so_straight")

#Variable 11 - Respondents' self-identified race - Race ####
Race <- raw_data[,85]
levels(Race) <- c("race_asian","race_black","race_hisp","race_other","race_White")

#Variable 12 - Respondents' educational attainment - Education ####
Education <- raw_data[,86]
levels(Education) <- c("ed_assoc", "ed_ugrad","ed_no.hs","ed_hs","ed_pgrad","ed_p.ugrad")

#Variable 13 - Age ####
Age <- raw_data[,95]
levels(Age) <- c("a.18-34","a.35-64","a.65+")

#Variable 14 - Respondents' martial status - Marital ####
Marital <- raw_data[,80]
levels(Marital) <- c("m_div","m_mar","m_nev",NA,"m_sep","m_wid")

#Variable 15 - Respondents' employment status - Employ ####
Employ <- raw_data[,36]
levels(Employ) <- c("emp_ft","emp_pt",NA,"emp_u.ret","emp_u.look","emp_u.nolook","emp_u.stud")

#Variable 16 - Do you feel society places unhealthy pressure on men - Pressure ####
Pressure <- raw_data[,12]
levels(Pressure) <- c("press_no",NA,"press_yes")

#Variable 17 - Do you feel you are expected to take the romantic lead - Lead ####
Rom.Lead <- raw_data[,60]
levels(Rom.Lead) <- c("r.lead_no",NA,"r.lead_yes")



#Step 3 - Joining Variables to Create Data Frame ####
df <- data.frame(MascFeel, MascLook, MascOrigin, Pressure, Rom.Lead, HealthWorry,
            FinWorry, AppWorry, SexWorry, Employ, Education, Income,
            Children, Marital, Orientation, Race, Age) %>%
  as_tibble() %>%
  print()



#Step 4 - Analysis of missing cases with the naniar package####
miss_var_summary(df)
miss_case_table(df)
gg_miss_case_cumsum(df)
vis_miss(df)
gg_miss_upset(df, nsets = n_var_miss(df))



#Step 5 - Listwise deletion of missing cases ####
masculinity_clean <- df %>%
  na.omit() %>%
  as_tibble() %>%
  print()
summary(masculinity_clean)



#Step 6 - Export clean data set ####
write.csv(masculinity_clean, 'C:/Users/Daniel Nystrom/Desktop/Masculinity and Medical Decision Making/2021_00_00_Social_Science_Research_Version/Data/masculinity_clean.csv')



#end script
