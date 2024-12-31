pacman::p_load(
  rio,          # File import
  here,         # File locator
  skimr,        # get overview of data
  tidyverse,    # data management + ggplot2 graphics, 
  gtsummary,    # summary statistics and tests
  forcats,
  rstatix,      # statistics
  corrr,        # correlation analayis for numeric variables
  janitor,      # adding totals and percents to tables
  flextable,     # converting tables to HTML
  apyramid,     # a package dedicated to creating age pyramids
  stringr,
  dplyr
)

### importing our dataset with the here package

felix <- import(here("Dataset.sav"))

### I am subsetting section A out of the dataset
sectionA <- felix %>%
  select(c(age:ageoflastbabyforpospartummother))
### I want to perform the descriptive statz of section A while I exclude the address, its not important
### SECTION A: SOCIODEMOGRAPHIC FACTOR ####
secA<-sectionA %>% 
  select(-c(placeofresidence)) %>%  # keep only the columns of interest
  tbl_summary()

secA <- as.data.frame(secA) #### we must convert the sdf object into data frame for us to use flextable

secAtable  <- flextable(secA) %>%
  save_as_docx(secA, path = "socio demograhic table.docx")

### I want to subset sectionB out of the dataset
sectionB <- felix %>%
  select(c(haveyouheardofmentalhealthconditionsthatcanaffectpostpartumwomen:
             Haveyoueverdiscussedmentalhealthwithyourhealthcareprovider))

### I want to perform a descriptive statz on section B
secB <- sectionB %>%
  tbl_summary()

### we need to convert section B to dataframe
secB <- as.data.frame(secB)
### we are doing our flextable
secBtable <- flextable(secB) %>%
  save_as_docx(secB, path = "Mental health awareness.docx")

### I want to subset sectionC out of the dataset
sectionC <- felix %>%
  select(c(Wouldyouseekhelpifyouexperiencementalhealthconcernsduringpregnan
           :Haveyouevercontactedamentalhealthhotline))

### I want to perform a descriptive statz on section B
secC <- sectionC %>%
  tbl_summary()

### we need to convert section B to dataframe
secC <- as.data.frame(secC)
### we are doing our flextable
secCtable <- flextable(secC) %>%
  save_as_docx(secC, path = "Attitude towards seeking help.docx")


### I want to subset sectionD out of the dataset
sectionD <- felix %>%
  select(c(Isthereanymentalhealthfacilityclosetoyou:
             Ifyouranswertoquestion6aboveisyeswastherelongwaitingtimebeforeyo))

### I want to perform a descriptive statz on section B
secD <- sectionD %>%
  tbl_summary()

### we need to convert section B to dataframe
secD <- as.data.frame(secD)
### we are doing our flextable
secDtable <- flextable(secD) %>%
  save_as_docx(secD, path = "Barriers to seeking mental healthcare.docx")


select(age,education,maritalstatus,no.ofpreviouspregnancies,
       durationofpregnancies,ageoflastbabyforpospartummother,ATTLEVEL)

felix$ATTLEVEL <- as.factor(felix$ATTLEVEL)
felix$AWARELEVEL <- as.factor(felix$AWARELEVEL)
### To perform CHI-SQUARE table for the attitude
ATDchi <- felix %>% 
  select(age,education,maritalstatus,no.ofpreviouspregnancies,
         durationofpregnancies,ageoflastbabyforpospartummother,ATTLEVEL) %>% # keep only columns of interest
  tbl_summary(by = ATTLEVEL,                                               # stratify entire table by attitude level
              statistic = list( all_continuous() ~ "{mean} ({sd})",        # stats and format for continuous columns
                                all_categorical() ~ "{n} ({p}%)"),   # stats and format for categorical columns
              digits = all_continuous() ~ 1,                              # rounding for continuous columns
              type   = all_categorical() ~ "categorical",                 # force all categorical levels to display
              label  = list(                                              # display labels for column names
                age ~ "Age group",
                education    ~ "Educational level",
                maritalstatus    ~ "Marital Status",
                no.ofpreviouspregnancies  ~ "Number of previous pregnancies",
                durationofpregnancies ~ "Duration of pregnancies",
                ageoflastbabyforpospartummother ~ "Age of lastbaby for pospartum mother"),
              missing_text = "Missing"                                    # how missing values should display
  ) %>%
  add_p(
    test = list(
      all_categorical() ~ "chisq.test"  # Use chi-square test for all categorical variables
    )
  )
### Time to convert our tibble into dataframe so as to use flextable to generate a word document type table
ATDchi <- as.data.frame(ATDchi)
ATDchi  <- flextable(ATDchi) %>%
  save_as_docx(ATDchi, path = "Chi square ATTITUDE.docx")




AWAREchi <- felix %>% 
  select(age,education,maritalstatus,no.ofpreviouspregnancies,
         durationofpregnancies,ageoflastbabyforpospartummother,AWARELEVEL) %>% # keep only columns of interest
  tbl_summary(by = AWARELEVEL,                                               # stratify entire table by attitude level
              statistic = list( all_continuous() ~ "{mean} ({sd})",        # stats and format for continuous columns
                                all_categorical() ~ "{n} ({p}%)"),   # stats and format for categorical columns
              digits = all_continuous() ~ 1,                              # rounding for continuous columns
              type   = all_categorical() ~ "categorical",                 # force all categorical levels to display
              label  = list(                                              # display labels for column names
                age ~ "Age group",
                education    ~ "Educational level",
                maritalstatus    ~ "Marital Status",
                no.ofpreviouspregnancies  ~ "Number of previous pregnancies",
                durationofpregnancies ~ "Duration of pregnancies",
                ageoflastbabyforpospartummother ~ "Age of lastbaby for pospartum mother"),
              missing_text = "Missing"                                    # how missing values should display
  ) %>%
  add_p(
    test = list(
      all_categorical() ~ "chisq.test"  # Use chi-square test for all categorical variables
    ) %>%
####I want to be able to add a fisher exact p-value
  
      add_p(test = list(variable = "fisher.test"))
    
  )
### Time to convert our tibble into dataframe so as to use flextable to generate a word document type table
AWAREchi <- as.data.frame(AWAREchi)
AWAREchi  <- flextable(AWAREchi) %>%
  save_as_docx(AWAREchi, path = "Chi square AWARENESS.docx")

