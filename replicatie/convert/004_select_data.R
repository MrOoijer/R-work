# setwd("D:/R/replicatie/convert")
# setwd("D:/Jan/Documents/R/replicatie/convert")
# make dataframe of source data by joining 
# relevant section to the DIS section of exam.
load("../data/adult_df.Rdata")
load("../data/youth_df.Rdata")
load("../data/exam_df.Rdata")

library(data.table)
setDF(exam_df)
repweights <- paste0("WTPXRP", 1:52)
get_from_person <- c("SEQN",            ### ID 
                     "SDPPSU6",        ### design
                     "SDPSTRA6",
                     repweights,
                    'DMARETHN'  ,      ### Race (3 times)
                    'DMARACER'  ,
                    'DMAETHNR' ,
                    'HSSEX'     ,      ### Sex
                    'HSAGEIR'         ### Age (Yrs)
                    )


subset_fields <- function(full_set, start_string){
 N= nchar(start_string)
 sub_set <- full_set[substring(full_set, 1, N)==start_string]
 return(sub_set)
} 

WT <- c("WTPFEX6")

BM <- subset_fields(names(exam_df), "BM")
MQ <- subset_fields(names(exam_df), "MQ")
DIS <- exam_df[ !is.na(exam_df$MQPDLANG), c(get_from_person, BM, MQ, WT)]

## This gives us 8773 observations. They are used in Table 2, so we 
#   need to prepare that first, then remove the ones with
#   missing dat. Only then caluculate BMI
# ---- age class
a<- cut(DIS$HSAGEIR, c(15, 20, 25, 30, 35, 50), right=FALSE)
DIS$age_class <- as.integer(a)

# ----- race is a combination of two fields
# split group other from DMARETHN with DMAETHNR 2
DIS$race <- DIS$DMARETHN
DIS$race[DIS$DMAETHNR== 2] <- 3

# ----- depression data
DIS$depressed <- ifelse(DIS$MQPDEP == 3 & DIS$MQPLDDP %in% 51:52, 1, 0)

# education, maritial status, area of residence
# education table(youth_df$HFA8R)
#  maritial status table(adult_df$HFA12)
# rural_urban = DMPMETRO in  adult/youth

needed <- c("SEQN", "HFA8R", "HFA12", "DMPMETRO")
needed <- rbind(adult_df[, needed], youth_df[, needed])
needed$edu_class <- ifelse(needed$HFA8 <= 8, 1, 
                    ifelse(needed$HFA8 <= 11,2,
                    ifelse(needed$HFA8 == 12, 3,  4  )
)  ) 
DIS<- merge(DIS, needed, by="SEQN", all.x= TRUE)

keep_fields <- c("SEQN",          
                  'race'  ,    
                  'HSSEX'     ,   
                  'HSAGEIR'   ,
                  "WTPFEX6",
                  "HFA8R", "HFA12", "DMPMETRO",
                 "SDPPSU6",        ### design
                 "SDPSTRA6",
                 repweights,
                  "age_class", 
                  "edu_class",
                  'depressed'
)

new_names    <- c("id", 
                  "race", 
                  "sex", 
                  "age", 
                  'weight', 
                  "educ", "marit", "rural", 
                  "SDPPSU6",        ### design
                  "SDPSTRA6",
                  repweights,
                  "age_class", 
                  "edu_class", 
                  'depressed'
                  )

dis_df_2 <- DIS[, keep_fields]
names(dis_df_2) <- new_names





#####  - remove 28 records with missing data
# $BMPWT is weight in kilogram, $BMPHT length in cm
DIS <- DIS[DIS$BMPWT != 888888, ]
##### Depression data
#####  - removing 335 records with missing data 
DIS <- DIS[DIS$MQPDPFLG <= 3,]

DIS$bmi <- round(DIS$BMPWT / ((DIS$BMPHT/100)^2),1)
DIS$bmi_class <- ifelse(DIS$bmi >= 30, 3, 
                          ifelse(DIS$bmi >= 25, 2, 
                                 ifelse(DIS$bmi >= 18.5,1,0)))
DIS$bmi_class_2 <- DIS$bmi_class
DIS$bmi_class_2[DIS$bmi >= 35 ] <- 4
DIS$bmi_class_2[DIS$bmi >= 40 ] <- 5

## Those are all the observations that are needed. 
## Remove unneeded column; better names later. 

DIS <- DIS[, c(keep_fields, "bmi", "bmi_class", "bmi_class_2")]
names(DIS)<- c(new_names,  "bmi", "bmi_class", "bmi_class_2")
dis_df <- DIS

save(dis_df, dis_df_2, file="../data/dis_df.Rdata")
