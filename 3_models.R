
# Empty environment.
rm(list=ls())

# Libraries.
library(dplyr)
library(survey)
library(ggplot2)
library(broom)
library(openxlsx)
options(openxlsx.numFmt = "#0.00")
options(scipen=999)

# Folder path.
fpath <- "..."


# Data.
df0 <- readr::read_csv(file = file.path(fpath, "Data_final.csv"), show_col_types = F) %>% as.data.frame() %>% 
  
  # relevel the factors.
  mutate(AGEG10LFS_T = relevel(as.factor(AGEG10LFS_T), ref = "2534")) %>% 
  mutate(RACETHN_4CAT = relevel(as.factor(RACETHN_4CAT), ref = "White")) %>% 
  mutate(EDCAT3 = relevel(as.factor(EDCAT3), ref = "Secondary")) %>% 
  mutate(REGION_US = relevel(as.factor(REGION_US), ref = "Northeast")) %>% 
  mutate(OccupCat = relevel(as.factor(as.character(OccupCat)), ref = "Elementary")) %>% 
  mutate(EmplStat = relevel(as.factor(as.character(EmplStat)), ref = "Unemployed")) %>% 
  mutate(across(c(FEMALE, withkids, US_born), as.factor))




# Create a workbook to export.
wb <- createWorkbook()
# Create an empty table for Tjur's stat for fitted models.
tjur_all <- data.frame()
# Run models
for (dd in c("Q1","Q4")) {
  for (mm in c("M0","M1","M2","M3","M4","M5")) {
    
    #filter sample and select criterion.
    if(dd == "Q1"){
      df <- df0
      criterion <- "LowIncome ~ EmplStat + "
    }else{
      df <- df0 %>% filter(C_Q07==1)
      criterion <- "HighIncome ~ "
    }

        
    #model
    if(mm == "M0"){
      mod <- as.formula(paste0(criterion, "AGEG10LFS_T + RACETHN_4CAT + EDCAT3"))
    }else if(mm == "M1"){
      mod <- as.formula(paste0(criterion, "AGEG10LFS_T + RACETHN_4CAT + EDCAT3 + STLITNUM1"))
    }else if(mm == "M2"){
      mod <- as.formula(paste0(criterion, "AGEG10LFS_T + RACETHN_4CAT + EDCAT3 + STLITNUM1 + FEMALE"))
    }else if(mm == "M3"){
      mod <- as.formula(paste0(criterion, "AGEG10LFS_T + RACETHN_4CAT + EDCAT3 + STLITNUM1 + FEMALE + US_born + REGION_US"))
    }else if(mm == "M4"){
      mod <- as.formula(paste0(criterion, "AGEG10LFS_T + RACETHN_4CAT + EDCAT3 + STLITNUM1 + FEMALE + US_born + REGION_US + withkids + FEMALE*withkids"))
    }else if(mm == "M5"){
      mod <- as.formula(paste0(criterion, "AGEG10LFS_T + RACETHN_4CAT + EDCAT3 + STLITNUM1 + FEMALE + US_born + REGION_US + withkids + FEMALE*withkids + OccupCat"))
    }
    
    
    #--Model run.
    dsgn <- svydesign(ids = ~1, strata = NULL, weights = df[,which(colnames(df)=="SPFWT0")], data = df)
    
    qm <- svyglm(mod, dsgn, family = binomial("logit"))
    
    M_out <- tidy(qm) %>% as.data.frame()
    M_out <- M_out %>% 
      mutate(sig = gtools::stars.pval(p.value)) %>% 
      mutate(expB = exp(estimate))
    
    
    #--Save Tjur's D
    tjur_m <- data.frame("model"=paste0(dd,"_",mm), 
                         "tjur_D"=as.numeric(performance::r2_tjur(qm)),
                         "n"=qm$df.null + 1)
    tjur_all <- rbind(tjur_all, tjur_m)
    
    
    #--Export to workbook.
    addWorksheet(wb, sheetName = paste0(dd,"_",mm))
    writeData(wb, paste0(dd,"_",mm), M_out)
    setColWidths(wb, sheet = paste0(dd,"_",mm), cols = 1, widths = 30)
    #message:
    cat("COMPLETED:", paste0(dd,"_",mm), "\n\n")
    
  }
  
}


# export Tjur's D for the fitted models.
addWorksheet(wb, sheetName = "tjur_D")
writeData(wb, "tjur_D", tjur_all)




# Export workbook.
saveWorkbook(wb, file = file.path(fpath, paste0("wdp_output_",Sys.Date(),".xlsx")), overwrite = TRUE)



