#Mapping Masculinity Project ########################################
#Project Name: Mapping Masculinity
#Paper authors: Daniel Nystrom*, Cristina del Campo^, Jose Andres Fernandez-Cornejo^
#Code design: Daniel Nystrom*
#*University of California Irvine - Paul Merage School of Business
#^Universidad Complutense de Madrid
#Revised code: August 25, 2020

#Data sourced from FiveThirtyEight.com's "masculinity_survey"
#Creative Commons Attribution 4.0 International Public License
#File directory included below

#Step 1 - Load required packages and import dataset ################

#Install packages:
if(!require(pacman)) install.packages("pacman")
pacman::p_load(pacman, tidyverse, FactoMineR, factoextra,
               RCurl, plyr, gmodels) #Install and load packages if not present
#pacman: Mostly just for p_load here, keeps package installation tidy
#tidyverse: For its vast supply of powerful data wrangling libraries
#FactoMineR: For performing the Multiple Correspondence Analysis
#factoextra: For conducting scree plot and other graphs
#RCurl: For importing dataset from GitHub url address

#Import dataset from FiveThirtyEight.com's GitHub page with directory
URL <- getURL("https://raw.githubusercontent.com/fivethirtyeight/data/master/masculinity-survey/raw-responses.csv")
df <- read.csv(text = URL) %>%
  as_tibble() %>%
  print() #Should be 1,615 observations, 98 variables
  #All variables should be factors except the weight score

#Step 2 - Clean and manipulate the dataset#####################

#Select only the 33 raw variables needed for analysis
df <- df %>% #here I don't use select( -c(x,...)) because of the large number of variables
  select(q0001, q0002, q0004_0001, q0004_0002, q0004_0003,
         q0004_0004, q0004_0005, q0004_0006, q0005,
         q0008_0001, q0008_0002, q0008_0003, q0008_0004,
         q0008_0005, q0008_0006, q0008_0007, q0008_0008,
         q0008_0009, q0008_0010, q0008_0011, q0008_0012,
         q0009, q0017, q0024, q0025_0001, q0025_0002,
         q0025_0003, q0026, age3, q0028, q0029, q0030,
         q0034
  ) %>%
  print()

#Next we need to combine questions 4, 8, and 25. In the raw
#dataset, these variables are comprised of a total of 21
#columns, and need to be condensed into just 6 variables

#First, we need recode all the existing columns as boolean expressions.
df1 <- df %>%
  mutate(
    Father = recode(
      q0004_0001, 
      'Father or father figure(s)' = 1, 
      "Not selected" = 0), #MascOrigins
    Mother = recode(
      q0004_0002, 
      'Mother or mother figure(s)' = 1, 
      "Not selected" = 0), #MascOrigins
    OthFam = recode(
      q0004_0003, 
      'Other family members' = 1, 
      "Not selected" = 0), #MascOrigins
    Culture = recode(
      q0004_0004, 
      "Pop culture" = 1, 
      "Not selected" = 0), #MascOrigins
    Friends = recode(
      q0004_0005, 
      "Friends" = 1, 
      "Not selected" = 0), #MascOrigins
    Other = recode(
      q0004_0006, 
      "Other (please specify)" = 1, 
      "Not selected" = 0), #MascOrigins
    Height = recode(
      q0008_0001, 
      'Your height' = 1, 
      'Not selected' = 0), #PhysWorry
    Weight = recode(
      q0008_0002, 
      'Your weight' = 1, 
      'Not selected' = 0), #PhysWorry
    Hair = recode(
      q0008_0003, 
      'Your hair or hairline' = 1, 
      'Not selected' = 0),#PhysWorry
    Physique = recode(
      q0008_0004, 
      'Your physique' = 1, 
      'Not selected' = 0), #Physworry
    Clothing = recode(
      q0008_0006, 
      'Your clothing or style' = 1, 
      'Not selected' = 0), #PhysWorry
    Genitalia = recode(
      q0008_0005, 
      'Appearance of your genitalia' = 1, 
      'Not selected' = 0), #SexWorry
    Sexuality = recode(
      q0008_0007, 
      'Sexual performance or amount of sex' = 1, 
      'Not selected' = 0), #SexWorry
    MentHealth = recode(
      q0008_0008, 
      'Your mental health' = 1, 
      'Not selected' = 0), #HealthWorry
    PhysHealth = recode(
      q0008_0009, 
      'Your physical health' = 1, 
      'Not selected' = 0), #HealthWorry
    Finances = recode(
      q0008_0010, 
      'Your finances, including your current or future income, assets, or debt' = 1, 
      'Not selected' = 0), #FinWorry
    Provide = recode(
      q0008_0011, 
      'Your ability to provide for your family, current or anticipated' = 1, 
      'Not selected' = 0), #FinWorry
    Young = recode(
      q0025_0001, 
      "Yes, one or more children under 18" = 1, 
      "Not selected" = 0), #Children
    Old = recode(
      q0025_0002, 
      "Yes, one or more children 18 or older" = 1, 
      "Not selected" = 0), #Children
    None = recode(
      q0025_0003, 
      "No children" = 1, 
      "Not selected" = 0 #Children
  )) %>%
  print() #Always, always, always double-check!
#Was this step totally required? No. But in this case, it makes the
#subsequent portions of the script significantly less tedious.

#Next, we combine these columns to create the six new variables.
df1$MascOrigin <- ifelse( #Origin of masculine ideals
  (df1$Father > 0 | df1$Mother > 0 | df1$OthFam > 0), #LOGICAL TEST
    ifelse( #IF LOGICAL TEST TRUE, PERFORM NESTED IFELSE()
      (df1$Culture > 0 | df1$Friends > 0 | df1$Other > 0),
      1, #If both
      2 #If only family
      ),
    ifelse( #IF LOGICAL TEST FALSE, PERFORM SECOND NESTED IFELSE()
      (df1$Culture > 0 | df1$Friends > 0 | df1$Other > 0),
      3, #If only peers/friends
      4 #If neither
   )
  )
df1$HealthWorry <- ifelse( #Health-related worry
  df1$MentHealth > 0,
  ifelse(
    df1$PhysHealth > 0,
    1, #Both
    2 #Mental Health only
    ),
  ifelse(
    df1$PhysHealth > 0,
    3, #Physical Only
    4 #Neither
   )
  )
df1$AppWorry <- ifelse( #Appearance-related worry
  (df1$Height > 0 | df1$Weight > 0 | df1$Physique > 0), 
  ifelse(
    (df1$Hair > 0 | df1$Clothing > 0), 
    1, #Both
    2 #Physique only
    ),
  ifelse(
    (df1$Hair > 0 | df1$Clothing > 0),
    3, #Appearance only
    4 #Neither
    )
  )
df1$FinWorry <- ifelse( #Finance-related worry
  df1$Finances > 0,
  ifelse(
    df1$Provide > 0,
    1, #Both
    2 #Finance only
    ),
  ifelse(
    df1$Provide > 0,
    3, #Providance only
    4 #Neither
    )
  )
df1$SexWorry <- ifelse( #Sexual-related worry
  df1$Sexuality > 0, 
  ifelse(
    df1$Genitalia > 0,
    1, #Both
    2 #Sex performance only
    ),
  ifelse(
    df1$Genitalia > 0,
    3, #Genitals only
    4 #Neither
    )
  )
df1$Children <- ifelse( #Does respondent have children?
  (df1$Young > 0 | df1$Old > 0),
  1, #Children
  ifelse(
    df1$None > 0,
    0, #No Children
    NA
    )
  )
#Alternatively, could have skipped recoding and used df1$var != "Not selected" but
#the confusion would come in keeping the variable names straight (e.g. q0004_0001).

#And, finally, remove the unneeded old variables, and intermediaries.
df1 <- df1 %>%
  select( -c(q0004_0001, q0004_0002, q0004_0003, q0004_0004,
             q0004_0005, q0004_0006, q0008_0001, q0008_0002,
             q0008_0005, q0008_0006, q0008_0007, q0008_0008,
             q0008_0009, q0008_0010, q0008_0011, q0008_0012,
             q0008_0003, q0008_0004, q0025_0001, q0025_0002,
             q0025_0003, Father, Mother, OthFam, Culture, Friends, 
             Other, Height, Weight, Hair, Physique, Genitalia, 
             Clothing, Sexuality, MentHealth, PhysHealth, Finances, 
             Provide, Young, Old, None, q0030
     )
  ) %>% #remove the raw versions
  print()

#Now that we have all the variables we're going to use
#we can go ahead with the next steps in the data-cleaning

#First, we will rename the 12 variables that weren't named
#by default in the previous steps.
df1 <- df1 %>%
  rename(c("q0001" = "MascFeel")) %>%
  rename(c("q0002" = "MascLook")) %>%
  rename(c("q0005" = "Pressure")) %>%
  rename(c("q0009" = "Employ")) %>%
  rename(c("q0017" = "RomLead")) %>%
  rename(c("q0024" = "Marital")) %>%
  rename(c("q0026" = "Orientation")) %>%
  rename(c("age3" = "Age")) %>%
  rename(c("q0028" = "Race")) %>%
  rename(c("q0029" = "Education")) %>%
  rename(c("q0034" = "Income")) %>%
  print()

summary(df1)

#And filter out the non-responses (discussed elsewhere)
df2 <- df1 %>%
  filter(
    !(MascFeel == "No answer" |
        MascLook == "No answer" |
        Pressure == "No answer" |
        Employ == "No answer" |
        RomLead == "No answer" |
        Marital == "No answer" |
        Orientation == "No answer" |
        Income == "Prefer not to answer" |
        is.na(Income) |
        is.na(Children)
   )
  ) %>%
  na.omit() %>%
  print()

#Recode income into 4 categories
df2$Income <- ifelse(
  (df2$Income == "$0-$9,999" | df2$Income == "$10,000-$24,999"),
  "<$25,000",
  ifelse(
    (df2$Income == "$25,000-$49,999" | df2$Income == "$50,000-$74,999"),
    "$25,000-$74,999",
    ifelse(
      (df2$Income == "$75,000-$99,999" | df2$Income == "$100,000-$124,999" |
        df2$Income == "$125,000-$149,999"),
      "$75,000-$149,999",
      ifelse(
        (df2$Income == "$150,000-$174,999" | df2$Income == "$175,000-$199,999" |
        df2$Income == "$200,000+"),
        "$150,000+",
        "NA"
        )
    )
  )
)

#Then make sure all variables are factors
df2 <- df2 %>%
  mutate(
    MascOrigin = factor(MascOrigin),
    HealthWorry = factor(HealthWorry),
    AppWorry = factor(AppWorry),
    FinWorry = factor(FinWorry),
    SexWorry = factor(SexWorry),
    Children = factor(Children),
    Income = factor(Income)
  ) %>%
  print()

#Now the data is set for further analysis
summary(df2) #Summary data

#Step 3 - Descriptive statistics ###############################

#Frequency distributions for demographic variables
CrossTable(df2$Age)
CrossTable(df2$Children)
CrossTable(df2$Education)
CrossTable(df2$Employ)
CrossTable(df2$Income)
CrossTable(df2$Marital)
CrossTable(df2$Race)
CrossTable(df2$Orientation)

#Pearson's Chi Square Tests comparing four variables to the
#general U.S. Population
local({
  .Table <- with(df2, table(Race))
  cat("\ncounts:\n")
  print(.Table)
  cat("\npercentages:\n")
  print(round(100 * .Table/sum(.Table), 2))
  .Probs <- c(0.059, 0.134, 0.183, 0.02, 0.604) #national stats
  chisq.test(.Table, p = .Probs)
})
local({
  .Table <- with(df2, table(Income))
  cat("\ncounts:\n")
  print(.Table)
  cat("\npercentages:\n")
  print(round(100 * .Table/sum(.Table), 2))
  .Probs <- c(0.191,0.380,0.274,0.155)
  chisq.test(.Table, p = .Probs)
})
local({
  .Table <- with(df2, table(Education))
  cat("\ncounts:\n")
  print(.Table)
  cat("\npercentages:\n")
  print(round(100 * .Table/sum(.Table), 2))
  .Probs <- c(0.098, 0.205, 0.295, 0.116, 
              0.12, 0.166)
  chisq.test(.Table, p = .Probs)
})
local({
  .Table <- with(df2, table(Orientation))
  cat("\ncounts:\n")
  print(.Table)
  cat("\npercentages:\n")
  print(round(100 * .Table/sum(.Table), 2))
  .Probs <- c(0.007, 0.016, 0.011, 0.966)
  chisq.test(.Table, p = .Probs)
})

#Step 4 - Multiple Correspondence Analysis #####################

#Here, it will be convenient to recode the variables
df3 <- df2 %>%
  mutate(
    MascFeel.c = recode(
      MascFeel,
      'Not at all masculine' = 'Masc0' ,
      'Not very masculine' = 'Masc1',
      'Somewhat masculine' = 'Masc2',
      'Very masculine' = 'Masc3'
    ),
    MascLook.c = recode(
      MascLook,
      'Not at all important' = 'MascApp0',
      'Not too important' = 'MascApp1',
      'Somewhat important' = 'MascApp2',
      'Very important' = 'MascApp3'
    ),
    MascOrigin.c = recode(
      MascOrigin,
      '1' = 'Origin_Both',
      '2' = 'Origin_Family',
      '3' = 'Origin_Peers',
      '4' = 'Origin_Neither'
    ),
    HealthWorry.c = recode(
      HealthWorry,
      '1' = 'Health_Both',
      '2' = 'Health_Mental',
      '3' = 'Health_Physical',
      '4' = 'Health_Neither'
    ),
    SexWorry.c = recode(
      SexWorry,
      '1' = 'Sex_Both',
      '2' = 'Sex_Perform',
      '3' = 'Sex_Genitals',
      '4' = 'Sex_Neither'
    ),
    FinWorry.c = recode(
      FinWorry,
      '1' = 'Fin_Both',
      '2' = 'Fin_Wealth',
      '3' = 'Fin_Provide',
      '4' = 'Fin_Neither'
    ),
    AppWorry.c = recode(
      AppWorry,
      '1' = 'App_Both',
      '2' = 'App_Phys',
      '3' = 'App_Material',
      '4' = 'App_Neither'
    ),
    Orientation.c = recode(
      Orientation,
      'Bisexual' = 'SO_Bi',
      'Gay' = 'SO_Gay',
      'Other' = 'SO_Other',
      'Straight' = 'SO_Straight'
    ),
    Race.c = recode(
      Race,
      'Asian' = 'Race_Asian',
      'Black' = 'Race_Black',
      'Hispanic' = 'Race_Hisp',
      'Other' = 'Race_Other',
      'White' = 'Race_White'
    ),
    Income.c = recode(
      Income,
      '$150,000+' = 'Inc_High',
      '$75,000-$149,999' = 'Inc_HighMid',
      '$25,000-$74,999' = 'Inc_LowMid',
      '<$25,000' = 'Inc_Low'
    ),
    Education.c = recode(
      Education,
      'Did not complete high school' = 'E0',
      'High school or G.E.D.' = 'E1',
      'Some college' = 'E2',
      "Associate's degree" = 'E3',
      'College graduate' = 'E4',
      'Post graduate degree' = 'E5'
    ),
    Age.c = recode(
      Age,
      '18 - 34' = 'Age_18_34',
      '35 - 64' = 'Age_35_64',
      '65 and up' = 'Age_65+'
    ),
    Marital.c = recode(
      Marital,
      'Never married' = 'M0',
      'Divorced' = 'M1',
      'Separated' = 'M2',
      'Widowed' = 'M3',
      'Married' = 'M4'
    ),
    Children.c = recode(
      Children,
      '0' = 'Child_No',
      '1' = 'Child_Yes'
    ),
    Employ.c = recode(
      Employ,
      'Not employed, NOT looking for work' = 'Emp0',
      'Not employed, looking for work' = 'Emp1',
      'Not employed, student' = 'Emp2',
      'Not employed-retired' = 'Emp3',
      'Employed, working part-time' = 'Emp4',
      'Employed, working full-time' = 'Emp5'
    )
  ) %>%
  print()

#Conduct the analysis itself
Masculinity1.MCA <- df3[, c("MascFeel.c", "MascLook.c", "MascOrigin.c", 
                                     "HealthWorry.c", "Pressure", "RomLead", 
                                     "SexWorry.c", "FinWorry.c", "AppWorry.c", 
                                     "Orientation.c", "Race.c", "Education.c", "Income.c",
                                     "Age.c", "Marital.c", "Children.c", "Employ.c")]
res <- MCA(Masculinity1.MCA, ncp = 5, quali.sup = 10:17, graph = TRUE)

#Run a scree plot
fviz_screeplot(
  res, 
  addlabels = TRUE, 
  geom=c("bar"), 
  ncp=23, 
  barcolor="black", 
  barfill="gray",
  xlim = c(0.5,23.5), 
  ylim = c(0, 10), 
  main="", 
  ggtheme=theme_bw())

#Plot the MCA first and second dimensions
plot.MCA(
  res, 
  axes = c(1, 2), 
  new.plot = TRUE, 
  choix = "var", 
  col.quali.sup = "gray50", 
  col.var = "black", 
  label = c("var"), 
  title="")

#Plot Supplimentary Variables - Figure 3
plot(
  res, 
  axes = c(1,2), 
  invisible = c("ind","var"), 
  col.quali.sup = "black", 
  autoLab = "y", 
  xlim = c(-0.5,1), 
  ylim= c(-0.5,1.0), 
  title="")

###Plot Complete Biplot - Figure 4 
plot.MCA(
  res, 
  axes = c(1, 2), 
  new.plot = TRUE,
  invisible=c("ind","quali.sup"), 
  col.var = "black", 
  col.quali.sup = "gray75", 
  autoLab="y", 
  xlim=c(-2.25,2.25), ylim=c(-1.5,3.0),
  label = c("var"), 
  title="")

###Analysis Readout and Close Model
summary(
  res, 
  nb.dec = 3, 
  nbelements = 50, 
  nbind = 10, 
  ncp = 3, 
  file = "")
dimdesc(res)

