# bureau 

path <- "E:/competitions/home-credit-default-risk"


library(data.table)
library(dplyr)


dir(path)


app_merged <- fread(paste(path, "/app_merged.txt", sep = ""), sep = "|")

paste("Wymiar zestawu danych")
dim(app_merged)

bureau <- fread(paste(path, "/bureau.csv", sep = ""))
bureau_bal <- fread(paste(path, "/bureau_balance.csv", sep = ""))
credit_bal <- fread(paste(path, "/credit_card_balance.csv", sep = ""))
insal_pay <- fread(paste(path, "/installments_payments.csv", sep = ""))
prev_aps  <- fread(paste(path, "/previous_application.csv", sep = ""))
pos_cash   <- fread(paste(path, "/POS_cash_balance.csv", sep = ""))


vars_text <- paste0("vars(DAYS_CREDIT, CREDIT_DAY_OVERDUE, 
                            DAYS_CREDIT, DAYS_CREDIT_ENDDATE, 
                    AMT_CREDIT_MAX_OVERDUE, 
                    AMT_CREDIT_SUM, 
                    AMT_CREDIT_SUM_DEBT, 
                    AMT_CREDIT_SUM_LIMIT, 
                    AMT_CREDIT_SUM_OVERDUE, 
                    DAYS_CREDIT_UPDATE)")

# BUREAU ACTIVE Description 
bureau_active_group <- bureau %>% 
  group_by(SK_ID_CURR, CREDIT_ACTIVE)  %>% 
  summarize_at(.vars = eval(parse(text = vars_text)), .funs = c(mean = "mean", 
                                                           median = "median", 
                                                           max = "max", 
                                                           min = "min"), na.rm = TRUE)


i
gsub(' ', '', i)

write.table(bureau_active_group, file = paste0(path, "/bureau_active_group.txt", sep = ""), 
            sep = "|", row.names = FALSE)

for ( i in c("Bad debt") ){
  
  ptm <- proc.time()
  cat( paste( "Procesowanie dla poziomu: \n", i , "\n", sep = "" ) )
  
  eval(parse(text = paste0(
    "bureau_group_", gsub(' ', '', i) , " <- bureau %>%
    filter(CREDIT_ACTIVE == '", 'Bad debt', "') %>%
    group_by(SK_ID_CURR) %>% 
    summarize_at(.vars = eval(parse(text = vars_text)), 
    .funs = c(mean_bureau_act_", gsub(' ', '', i), " = 'mean', 
              median_bureau_act_", gsub(' ', '', i), " = 'median',
              sum_bureau_act_", gsub(' ', '', i), " = 'sum'), na.rm = TRUE)")
    
    
    
    
    
  ))  
    
    app_merged <- app_merged %>% 
      left_join(., eval(parse(text = paste0('bureau_group_', gsub(' ', '', i) , sep = ""))), 
                              by = c("SK_ID_CURR" = "SK_ID_CURR" ))
    
  
  
 cat(paste("Procesowanie zajelo: \n", proc.time() - ptm, "\n")) 
  
}


# Descriptive statistics for all  levels of credit active variable 





# Bureau balance

dim(bureau_bal)




# Count and group by status, max value 
# Grupowanie po polach
# SK_ID_BUREAU - STATUS 
# C to potencjalnie CLOSED 


# Unikalne poziomy statusow klienta 
unique(bureau_bal$STATUS)




bureau_bal_aggr <- bureau_bal %>%
  select(SK_ID_BUREAU, MONTHS_BALANCE) %>%
  group_by(SK_ID_BUREAU) %>%
  summarize(count = n(), 
            max = max(MONTHS_BALANCE), 
            min = min(MONTHS_BALANCE), 
            mean = mean(MONTHS_BALANCE))
  


# Agregacja po statusac



bureau_bal_aggr_status <- bureau_bal %>% 
  select(SK_ID_BUREAU, MONTHS_BALANCE, STATUS) %>%
  group_by(SK_ID_BUREAU, STATUS) %>% 
  summarize(count = n()) %>%
  mutate(perc = count/sum(count))

# Podpiêcie SK_ID_CURR do bureau_bal oraz bureau_bal_aggr 


bureau_bal_aggr <- bureau_bal_aggr %>% 
  left_join(., bureau[, c("SK_ID_BUREAU", "SK_ID_CURR")], by = c("SK_ID_BUREAU"  = "SK_ID_BUREAU"))
bureau_bal_aggr


bureau_bal_aggr_status <- bureau_bal_aggr_status %>%
  left_join(., bureau[, c("SK_ID_BUREAU", "SK_ID_CURR")], by = c("SK_ID_BUREAU" = 'SK_ID_BUREAU'))




write.table(bureau_bal_aggr, file = paste(path, "/bureau_bal_aggr.txt", sep = ""), sep = "|", row.names = FALSE)
write.table(bureau_bal_aggr_status, file = paste(path, "/bureau_bal_aggr_status.txt", sep = ""), sep = "|", row.names = FALSE)


# Credit card balance


credit_bal <- credit_bal %>% 
  arrange(SK_ID_PREV)


credit_bal$MON_BAL_min <- credit_bal$MONTHS_BALANCE * -1


# AMT BALANCE ORAZ LIMIT OD 1 do max miesiecy 


cred_vars_agg <- paste0("vars(AMT_BALANCE, 
                    AMT_DRAWINGS_ATM_CURRENT, 
                    AMT_DRAWINGS_CURRENT, 
                    AMT_DRAWINGS_POS_CURRENT, 
                    AMT_INST_MIN_REGULARITY, AMT_PAYMENT_CURRENT ,
                    AMT_PAYMENT_TOTAL_CURRENT, AMT_TOTAL_RECEIVABLE)
                    ")

       
               
               
options(scipen = 100 )
options(digits = 3 )
               
for (i in seq(1:48) ){
  
  ptm <- proc.time()
  cat( paste("Procesowanie dla miesiaca: \n", i, "\n") )
  
  
  eval(parse(text = paste(
    
    "credit_bal_group_", i, "<- credit_bal %>%
    filter(MON_BAL_min <", i, ") %>%
    group_by(SK_ID_CURR) %>%
    summarize_at(.vars = eval(parse(text = cred_vars_agg)), 
    .funs = c(mean_", i , " = 'mean', 
            max_"   , i , " = 'max', 
            min_"   , i , " = 'min'), na.rm = TRUE)" , sep = "" )))
  
  
  app_merged <- app_merged %>%
    left_join(., eval(parse(text = paste( "credit_bal_group_", i, sep = "") ) ), 
                            by = c("SK_ID_CURR"  = "SK_ID_CURR" ))
  
  cat(paste("Czas trwania iteracji: \n", proc.time() - ptm , "\n"  ))
  cat(paste("Wymiar tablicy app_merged :\n", dim(app_merged) , "\n" ))
  
}               
  
# Credit balance in i-th month between date of research 

credit_bal_group
colnames(credit_bal_group)

ptm <- proc.time()
write.table(app_merged, file = paste0(path, "/app_merged_credit_bal_vars.txt", sep = ""), sep = "|", 
            row.names = FALSE)
# App merge with 
write.table(app_merged, file = paste0(path, "/app_merged_extend_bureau.txt", sep = ""), sep = "|", row.names = FALSE)
proc.time() - ptm

head(app_merged)