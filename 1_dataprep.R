
#---------------- Setup ----------------#

# Empty environment.
rm(list=ls())

# Libraries.
library(dplyr)
library(janitor)


# Folder path.
fpath <- "..."


# Data.
data0 <- haven::read_spss(file.path(fpath, "uscombined_8.sav"))

# Counts and value labels.
for (i in c('C_Q07','YEARLYINCPR',
            'AGEG10LFS_T','RACETHN_4CAT','EDCAT6','GENDER_R',
            'J_Q04a','REGION_US','J_Q03a','ISCOSKIL4')){
  data0 %>% count(.[i]) %>% print()
}




#---------------- Data Pre-processing ----------------#


# Data cleaning tracker
report <- data.frame('task' = 'start',
                     'nrow_changed' = NA,
                     'ncol_changed'= NA,
                     'nrow_after'= dim(data0)[1],
                     'ncol_after'= dim(data0)[2])



# //Filter age groups.
df1 <- data0 %>% as.data.frame() %>% 
  filter(AGEG10LFS_T %in% c(2,3,4))
# update tracker
report <- rbind(report,
                data.frame('task' = 'filter age groups',
                           'nrow_changed' = dim(df1)[1] - report$nrow_after[dim(report)[1]],
                           'ncol_changed'= dim(df1)[2] - report$ncol_after[dim(report)[1]],
                           'nrow_after'= dim(df1)[1],
                           'ncol_after'= dim(df1)[2]))

# //Remove NAs from the outcome
df1 <- df1 %>% as.data.frame() %>% 
  filter(!is.na(YEARLYINCPR))
# update tracker
report <- rbind(report,
                data.frame('task' = 'remove NAs in outcome',
                           'nrow_changed' = dim(df1)[1] - report$nrow_after[dim(report)[1]],
                           'ncol_changed'= dim(df1)[2] - report$ncol_after[dim(report)[1]],
                           'nrow_after'= dim(df1)[1],
                           'ncol_after'= dim(df1)[2]))


# //Remove NAs from the predictors
df1 <- df1 %>% as.data.frame() %>% 
  filter(
    !is.na(AGEG10LFS_T),
    !is.na(RACETHN_4CAT),
    !is.na(EDCAT6),
    !is.na(GENDER_R),
    !is.na(J_Q04a), #US_born
    !is.na(REGION_US),
    !is.na(J_Q03a), #withkids
    !is.na(ISCOSKIL4) #OccupCat
  )
# update tracker
report <- rbind(report,
                data.frame('task' = 'remove NAs in predictors',
                           'nrow_changed' = dim(df1)[1] - report$nrow_after[dim(report)[1]],
                           'ncol_changed'= dim(df1)[2] - report$ncol_after[dim(report)[1]],
                           'nrow_after'= dim(df1)[1],
                           'ncol_after'= dim(df1)[2]))



# //Create new variables
df1 <- df1 %>% as.data.frame() %>% 
  # INCOME
  ##Low and High income
  mutate(LowIncome = ifelse(YEARLYINCPR %in% c(1,2), 1, 0)) %>% 
  mutate(HighIncome = ifelse(YEARLYINCPR %in% c(5,6), 1, 0)) %>% 
  mutate(YEARLYINCPR_R = case_when(YEARLYINCPR %in% c(1,2) ~ "Q1", 
                                   YEARLYINCPR==3 ~ "Q2",
                                   YEARLYINCPR==4 ~ "Q3", 
                                   YEARLYINCPR %in% c(5,6) ~ "Q4")) %>% 
  
  # AGE
  mutate(AGEG10LFS_T = case_when(AGEG10LFS_T==2 ~ "2534",
                                 AGEG10LFS_T==3 ~ "3544",
                                 AGEG10LFS_T==4 ~ "4554")) %>% 
  mutate(AGEG10LFS_T = factor(AGEG10LFS_T, levels = c("2534", "3544", "4554"))) %>% 
  
  # RACETHN
  mutate(RACETHN_4CAT = case_when(RACETHN_4CAT==1 ~ "Hispanic",
                                  RACETHN_4CAT==2 ~ "White",
                                  RACETHN_4CAT==3 ~ "Black",
                                  RACETHN_4CAT==6 ~ "Other")) %>% 
  mutate(RACETHN_4CAT = factor(RACETHN_4CAT, levels = c("White", "Black", "Hispanic", "Other"))) %>% 
  
  # EDCAT
  mutate(EDCAT3 = case_when(EDCAT6 %in% c(1,2) ~ "Secondary",
                            EDCAT6==3 ~ "Non-tertiary",
                            EDCAT6 %in% c(4,5,6) ~ "Tertiary")) %>%
  mutate(EDCAT3 = factor(EDCAT3, levels = c("Secondary","Non-tertiary","Tertiary"))) %>% 
  
  ##Gender and FEMALE
  mutate(FEMALE = case_when(GENDER_R==2 ~ 1, 
                            GENDER_R==1 ~ 0)) %>% 
  mutate(GENDER_R = case_when(GENDER_R==1 ~ "Male",
                              GENDER_R==2 ~ "Female")) %>% 
  mutate(GENDER_R = factor(GENDER_R, levels = c("Male","Female"))) %>% 
  
  # born in the country
  mutate(US_born = case_when(J_Q04a==1 ~ 1, 
                             J_Q04a==2 ~ 0)) %>% 
  
  # region US
  mutate(REGION_US = case_when(REGION_US==1 ~ "Northeast", 
                               REGION_US==2 ~ "Midwest",
                               REGION_US==3 ~ "South", 
                               REGION_US==4 ~ "West")) %>% 
  mutate(REGION_US = factor(REGION_US, levels = c("Northeast","Midwest","South","West"))) %>% 
  
  # having kids
  mutate(withkids = case_when(J_Q03a==1 ~ 1,
                              J_Q03a==2 ~ 0)) %>% 
  
  # occupational category
  mutate(OccupCat = case_when(ISCOSKIL4==1 ~ "Skilled",
                              ISCOSKIL4==2 ~ "Semi-skilled, white-collar",
                              ISCOSKIL4==3 ~ "Semi-skilled, blue-collar",
                              ISCOSKIL4==4 ~ "Elementary"
  )) %>% 
  mutate(OccupCat = factor(OccupCat, levels = c("Skilled","Semi-skilled, white-collar","Semi-skilled, blue-collar","Elementary"))) %>% 
  
  # employment status
  mutate(EmplStat = case_when(C_Q07==1 ~ "FullTime",
                              C_Q07==2 ~ "PartTime",
                              C_Q07==3 ~ "Unemployed",
                              C_Q07 %in% c(4,5,6,7,8,9,10) ~ "Other"
  )) %>% 
  mutate(EmplStat = factor(EmplStat, levels = c("FullTime","PartTime","Other","Unemployed")))
# update tracker
report <- rbind(report,
                data.frame('task' = 'create new vars',
                           'nrow_changed' = dim(df1)[1] - report$nrow_after[dim(report)[1]],
                           'ncol_changed'= dim(df1)[2] - report$ncol_after[dim(report)[1]],
                           'nrow_after'= dim(df1)[1],
                           'ncol_after'= dim(df1)[2]))




# //Remove outliers
df1 <- df1 %>% as.data.frame() %>% 
  group_by(GENDER_R,RACETHN_4CAT,EDCAT3) %>%
  mutate(q1 = quantile(EARNMTHBONUSUS_C, probs = 0.25, na.rm = T),
         q3 = quantile(EARNMTHBONUSUS_C, probs = 0.75, na.rm = T),
         iqr = q3 - q1,
         lower = q1 - 4 * iqr,
         upper = q3 + 4 * iqr,
         EARNMTHBONUSUS_C_outlier = case_when(
           EARNMTHBONUSUS_C > upper ~ "high",
           EARNMTHBONUSUS_C < lower ~ "low",
           is.na(EARNMTHBONUSUS_C) ~ "missing",
           TRUE ~ "normal")
  ) %>%
  ungroup() %>%
  filter(EARNMTHBONUSUS_C_outlier != "high") %>%
  select(-q1, -q3, -iqr, -lower, -upper, -EARNMTHBONUSUS_C_outlier)
# update tracker
report <- rbind(report,
                data.frame('task' = 'remove outliers',
                           'nrow_changed' = dim(df1)[1] - report$nrow_after[dim(report)[1]],
                           'ncol_changed'= dim(df1)[2] - report$ncol_after[dim(report)[1]],
                           'nrow_after'= dim(df1)[1],
                           'ncol_after'= dim(df1)[2]))


# //Create composites
df1 <- df1 %>% as.data.frame() %>% 
  mutate(STLITNUM1 = as.numeric(scale(PVLIT1 + PVNUM1)),
         STLITNUM2 = as.numeric(scale(PVLIT2 + PVNUM2)),
         STLITNUM3 = as.numeric(scale(PVLIT3 + PVNUM3)),
         STLITNUM4 = as.numeric(scale(PVLIT4 + PVNUM4)),
         STLITNUM5 = as.numeric(scale(PVLIT5 + PVNUM5)),
         STLITNUM6 = as.numeric(scale(PVLIT6 + PVNUM6)),
         STLITNUM7 = as.numeric(scale(PVLIT7 + PVNUM7)),
         STLITNUM8 = as.numeric(scale(PVLIT8 + PVNUM8)),
         STLITNUM9 = as.numeric(scale(PVLIT9 + PVNUM9)),
         STLITNUM10 = as.numeric(scale(PVLIT10 + PVNUM10)))
# update tracker
report <- rbind(report,
                data.frame('task' = 'create composites',
                           'nrow_changed' = dim(df1)[1] - report$nrow_after[dim(report)[1]],
                           'ncol_changed'= dim(df1)[2] - report$ncol_after[dim(report)[1]],
                           'nrow_after'= dim(df1)[1],
                           'ncol_after'= dim(df1)[2]))




# //Merge groups
nrow_changed <- df1 %>% filter(RACETHN_4CAT=="Other" & EDCAT3=="Non-tertiary") %>% NROW()
df1 <- df1 %>% as.data.frame() %>% 
  mutate(EDCAT3 = ifelse(RACETHN_4CAT=="Other" & EDCAT3=="Non-tertiary", "Tertiary", as.character(EDCAT3)))
# update tracker
report <- rbind(report,
                data.frame('task' = 'merge groups',
                           'nrow_changed' = paste0("(",nrow_changed,")"),
                           'ncol_changed'= dim(df1)[2] - report$ncol_after[dim(report)[1]],
                           'nrow_after'= dim(df1)[1],
                           'ncol_after'= dim(df1)[2]))



# //Replace NAs in income
nrow_changed <- df1 %>% filter(is.na(EARNMTHBONUS)==T) %>% NROW()
df1 <- df1 %>% as.data.frame() %>% 
  group_by(YEARLYINCPR) %>%
  mutate(EARNMTHBONUS_mean = mean(EARNMTHBONUS, na.rm=T),
         EARNMTHBONUSUS_C_mean = mean(EARNMTHBONUSUS_C, na.rm=T)) %>% 
  ungroup() %>%
  mutate(EARNMTHBONUS = ifelse(is.na(EARNMTHBONUS)==T, EARNMTHBONUS_mean, EARNMTHBONUS),
         EARNMTHBONUSUS_C = ifelse(is.na(EARNMTHBONUSUS_C)==T, EARNMTHBONUSUS_C_mean, EARNMTHBONUSUS_C))
# update tracker
report <- rbind(report,
                data.frame('task' = 'replace NAs',
                           'nrow_changed' = paste0("(",nrow_changed,")"),
                           'ncol_changed'= dim(df1)[2] - report$ncol_after[dim(report)[1]],
                           'nrow_after'= dim(df1)[1],
                           'ncol_after'= dim(df1)[2]))



# //Select variables
df1 <- df1 %>% as.data.frame() %>% 
  select(SEQID,EARNFLAG,
         LowIncome,HighIncome,
         FEMALE,
         C_Q07,YEARLYINCPR,GENDER_R,AGEG10LFS_T,RACETHN_4CAT,EDCAT6,EDCAT3,
         C_Q09,C_Q09_C,ISCOSKIL4,EARNMTHBONUS,EARNMTHBONUSUS_C,EARNMTHBONUSPPPUS_C,D_Q18a_T,EMPSTAT,
         J_Q04a,US_born,REGION_US,J_Q03a,withkids, OccupCat,EmplStat,
         LITSTATUS:PVNUM10,STLITNUM1:STLITNUM10,
         VEMETHOD,VENREPS, SPFWT0:SPFWT80)
# update tracker
report <- rbind(report,
                data.frame('task' = 'select vars',
                           'nrow_changed' = dim(df1)[1] - report$nrow_after[dim(report)[1]],
                           'ncol_changed'= dim(df1)[2] - report$ncol_after[dim(report)[1]],
                           'nrow_after'= dim(df1)[1],
                           'ncol_after'= dim(df1)[2]))



#---------------- Export ----------------#

readr::write_csv(df1 %>% as.data.frame(), file = file.path(fpath, "Data_final.csv"))
report


