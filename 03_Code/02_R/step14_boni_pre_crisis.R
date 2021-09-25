# ESTIMATION
setwd("Q:\\Meine Bibliotheken\\Research\\SME_Corona")
source(file.path(getwd(), '03_Code', '02_R', 'step0_setup.R'))

# -------------------------------------------------------------------------
# #########################################################################
# -------------------------------------------------------------------------

# Load Appended & Cleaned Panel -------------------------------------------

df_panel <- read_delim(file = file.path(getwd(), '02_Data', '01_PanelData', 'df_panel_clean_final.txt'),
                       delim = '\t', 
                       col_types = cols(exitdat = col_date()))

load(file.path(getwd(), '05_Model', 'df_match_nocaliper.RData'))


# Some minor cleaning
df_panel <- df_panel %>% 
  filter(!is.na(csize_fct))

df_panel <- df_panel %>% 
  mutate(age = ifelse((jahr_rd + 1 - gryear) <= 0, 1, jahr_rd + 1 - gryear))

df_panel <- df_panel %>% 
  mutate(umsatz = ifelse(umsatz > 200000000000, NA, umsatz))

df_panel <- df_panel %>% 
  mutate(anzma = ifelse(anzma == 0, NA, anzma))

df_panel <- df_panel %>% 
  mutate(refo_dummy = case_when(refo_fct == 'limited liability' ~ 1,
                                refo_fct == 'full liability' ~ 0)) 





df_boni_pre_crisis <- df_panel %>% 
  filter(jahr_rd == 2019) %>% 
  select(crefo, bonitaet, insolv, wz12_fct, csize_fct)

quantile(df_boni_pre_crisis$bonitaet, probs = c(0.8, 0.9, 0.95, 0.99))

df_boni_pre_crisis <- df_boni_pre_crisis %>% 
  mutate(top_decile = ifelse(bonitaet > 325, 1, 0)) %>% 
  group_nest(wz12_fct, csize_fct)


for (i in 1:nrow(df_boni_pre_crisis)){
  
  df_temp <- df_boni_pre_crisis %>% 
    filter(row_number() == i) %>% 
    select(data) %>% 
    unnest(cols = data) %>% 
    select(top_decile, bonitaet) 

  
  
  
  
  df_boni_pre_crisis[i, 'fraction_top_decile'] <- mean(df_temp$top_decile)
  df_boni_pre_crisis[i, 'mean_boni'] <- mean(df_temp$bonitaet)
  
  
  
  
  print(i)
  
}


colnames(df_match)

df_res <- df_match %>% 
  select(wz12_fct, csize_fct, cont_mean_w, treat_mean, mean_w_diff, p_value_ch2_w) %>% 
  left_join(df_boni_pre_crisis %>% select(-c(data)), by=c('wz12_fct', 'csize_fct'))


write_delim(df_res, path = file.path(getwd(), '02_Data', '08_Matched', 'df_res.txt'), delim = '\t')
# to Simona
write_delim(df_res, path = 'K:\\jdo\\Corona_Insolvenzen\\Data\\4_MUP_bankruptcies\\df_res.txt', delim = '\t')
write_delim(df_res, path = 'K:\\jdo\\Corona_Insolvenzen\\Data\\4_MUP_bankruptcies\\df_res_nocaliper.txt', delim = '\t')
# K:\jdo\Corona_Insolvenzen\Data\4_MUP_bankruptcies

df_res %>% mutate_if(is.numeric,~round(.,digits=4)) %>% arrange(csize_fct) %>%  print(n=52)
