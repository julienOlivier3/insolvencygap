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


# DROP
df_drop <- read_delim(file = file.path(getwd(), '02_Data', '03_SanityChecks', 'df_drop.txt'), delim = '\t')
df_drop <- df_drop[1:13,]


# Matching ----------------------------------------------------------------
table(df_panel$treat_final, df_panel$insolv)

## Nearest Neighboor ======================================================


### Imputation ############################################################
df_temp <- df_panel %>% 
  distinct(crefo, .keep_all = TRUE)

# Age
df_crefo <- df_panel %>% filter(is.na(gryear)) %>% distinct(crefo) %>% mutate(gryear = as.numeric(NA))

for (i in 1:nrow(df_crefo)){
  
  crefo <- df_crefo[i,]$crefo
  
  refo_d <- unique(df_temp[df_temp$crefo == crefo,]$refo_dummy)
  wz12_f <- unique(df_temp[df_temp$crefo == crefo,]$wz12_fct)
  anzm <- unique(df_temp[df_temp$crefo == crefo,]$anzma)
  anzm <- if(is.na(anzm)) NA else seq(anzm-3,anzm+3)
  
  
  df_gryear <- df_temp %>% 
    filter(if(!is.na(refo_d)) {refo_dummy==refo_d} else {refo_dummy==refo_dummy}) %>% 
    filter(if(!is.na(wz12_f)) {wz12_fct==wz12_f} else {wz12_fct==wz12_fct}) %>% 
    filter(if(!any(is.na(anzm))) {anzma %in% anzm} else {anzma==anzma}) %>% 
    select(gryear)
  
  
  df_crefo[i,"gryear"] <- median(df_gryear$gryear, na.rm = TRUE)
  
  if(i %% 1000 == 0) {print(i)}
  
  
}

df_gryear <- df_crefo

write_delim(df_gryear, path = file.path(getwd(), '02_Data', '10_Imputation', 'df_gryear.txt'), delim = '\t')


# Size
df_crefo <- df_panel %>% filter(is.na(csize_fct)) %>% distinct(crefo) %>% mutate(anzma = as.numeric(NA))

for (i in 1:nrow(df_crefo)){
  
  crefo <- df_crefo[i,]$crefo
  
  refo_d <- unique(df_temp[df_temp$crefo == crefo,]$refo_fct)
  wz12_f <- unique(df_temp[df_temp$crefo == crefo,]$wz12_fct)
  gry <- unique(df_temp[df_temp$crefo == crefo,]$gryear)
  gry <- if(is.na(gry)) NA else seq(gry-3,gry+3)
  
  
  df_anzma <- df_temp %>% 
    filter(if(!is.na(refo_d)) {refo_fct==refo_d} else {refo_fct==refo_fct}) %>% 
    filter(if(!is.na(wz12_f)) {wz12_fct==wz12_f} else {wz12_fct==wz12_fct}) %>% 
    filter(if(!any(is.na(gry))) {gryear %in% gry} else {gryear==gryear}) %>% 
    select(anzma)
  

  df_crefo[i,"anzma"] <- median(df_anzma$anzma, na.rm = TRUE)
  
  if(i %% 1000 == 0) {print(i)}
  
  
} 

df_csize <- df_crefo
df_csize <- df_csize  %>%  mutate(csize_fct = factor(case_when(
  (anzma <= 10)  ~ 'Micro-enterprise',
  ((anzma > 10) & (anzma < 50))  ~ 'Small enterprise', 
  ((anzma >= 50) & (anzma < 250))  ~ 'Medium-sized enterprise', 
  (anzma >= 250)  ~ 'Large enterprise'
), levels = c('Large enterprise', 'Medium-sized enterprise', 'Small enterprise', 'Micro-enterprise')))
df_csize %>% tab(csize_fct)


write_delim(df_csize, path = file.path(getwd(), '02_Data', '10_Imputation', 'df_csize.txt'), delim = '\t')

### Determine Sample Size #################################################
# Drop one duplicate
df_panel %>% 
  filter(insolv==1) %>% 
  group_by(crefo) %>% 
  mutate(n_obs = n()) %>% 
  ungroup() %>% 
  filter(n_obs > 1) %>% 
  select(crefo, jahr_rd, rechdat, exitdat, insolv, p_bonitaet, ch_bonitaet)

df_panel <- df_panel %>% 
  filter(!(crefo==8150104052 & rechdat=="2019-05-13" & exitdat=="2019-07-15"))

# Drop observations for which we do not know the size
df_panel <- df_panel %>% 
  filter(!is.na(csize_fct))


# or imputation
df_csize <- read_delim(file.path(getwd(), "02_Data", "10_Imputation", "df_csize.txt"), delim = "\t")
df_gryear <- read_delim(file.path(getwd(), "02_Data", "10_Imputation", "df_gryear.txt"), delim = "\t")

df_panel <- df_panel %>% coalesce_join(df_csize, by = "crefo") %>% 
  coalesce_join(df_gryear, by = "crefo")

# and drop insolvencies 4 months after update
df_panel <- df_panel %>% filter(!((insolv==0) & !is.na(d_exitdat))) 
  
### Some minor cleaning ###################################################
# Drop observations for which we do not know the size
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



df_panel %>% tab(csize_fct)
df_panel %>% filter(is.na(age))  
df_panel %>% filter(is.na(age))  %>% tab(insolv)


### Estimation ############################################################


# Formula
model_match <- as.formula(paste('treat_final', paste('ch_bonitaet', 
                                                     'p_bonitaet',
                                                     'avg_bonitaet',
                                                     'n_downgrade',
                                                     'age',
                                                     #'refo_dummy',
                                                     sep = ' + '), sep = ' ~ '))


df_match <- df_panel %>% 
  group_nest(wz12_fct, csize_fct, keep = TRUE) 

# Further split by boni prior crisis
median_pre_crisis_boni <- median(df_panel$avg_bonitaet) 
df_panel <- df_panel %>% mutate(viability = ifelse(avg_bonitaet > median_pre_crisis_boni, "unviable", "viable")) 
df_panel %>% tab(viability)

df_match <- df_panel %>% 
  group_nest(wz12_fct, csize_fct, viability, keep = TRUE) 

# Calculate standard deviation of ch_bonitaet
sd_boni <- sd(df_panel$ch_bonitaet)

system.time({
  for (i in 1:nrow(df_match)){
    
    df_temp <- df_match %>% 
      filter(row_number() == i) %>% 
      select(data) %>% 
      unnest(cols = data) %>% 
      select(crefo, jahr, treat_final, insolv, p_bonitaet, ch_bonitaet, avg_bonitaet, n_downgrade, age) %>% 
      filter(complete.cases(.)) %>% 
      mutate(id = paste(crefo, jahr, sep  = '_')) %>%
      distinct(id, .keep_all = TRUE) %>% 
      filter(!is.na(age)) %>% 
      column_to_rownames('id')
    
    
    #temp_caliper <- floor(sd_boni*(4/log(nrow(df_temp))))
    
    
    match_res <- matchit(formula=model_match, 
                         data=df_temp, 
                         method="nearest",
                         distance = "mahalanobis",
                         #ratio = 1,
                         ratio=floor(nrow(df_temp[df_temp$treat_final==0,])/nrow(df_temp[df_temp$treat_final==1,])),  #6, # nrow(df_panel[df_panel$treat_final==0,])/nrow(df_panel[df_panel$treat_final==1,])
                         replace=TRUE,
                         #discard='control', 
                         #exact = ~ refo_dummy,
                         caliper = c(ch_bonitaet = 8), #c(ch_bonitaet = temp_caliper), # sd(df_panel$ch_bonitaet)*0.25, https://www.lexjansen.com/pharmasug/2006/PublicHealthResearch/PR05.pdf
                         std.caliper=FALSE,
                         #min.controls = 0,
                         #max.controls = 6
                         
    )
    
    
    
    
    df_panel_match <- match.data(match_res) %>% 
      as_tibble() %>% 
      mutate(id = paste(crefo, jahr, sep  = '_')) %>% 
      select(id, everything())
    df_match[i, 'data_match'] <- nest(df_panel_match, data = everything())
    
    
    df_matrix_match <- match_res$match.matrix %>% as_tibble(rownames = 'treated')
    df_match[i, 'matrix_match'] <- nest(df_matrix_match, data = everything())
    
    
    cont_mean <- mean(df_panel_match[df_panel_match$treat_final==0,]$insolv)
    df_match[i, 'cont_mean'] <- cont_mean
    
    
    cont_mean_w <- weighted.mean(x = df_panel_match[df_panel_match$treat_final==0,]$insolv, w = df_panel_match[df_panel_match$treat_final==0,]$weights)
    df_match[i, 'cont_mean_w'] <- cont_mean_w
    
    
    treat_mean <- mean(df_panel_match[df_panel_match$treat_final==1,]$insolv)
    df_match[i, 'treat_mean'] <- treat_mean
    
    
    mean_diff <- treat_mean - cont_mean
    df_match[i, 'mean_diff'] <- mean_diff
    
    mean_w_diff <- treat_mean - cont_mean_w
    df_match[i, 'mean_w_diff'] <- mean_w_diff
    
    
    # wtd_t_test <- wtd.t.test(x = df_panel_match[df_panel_match$treat_final==1,]$insolv, weight = df_panel_match[df_panel_match$treat_final==1,]$weights,
    #                          y = df_panel_match[df_panel_match$treat_final==0,]$insolv, weighty = df_panel_match[df_panel_match$treat_final==0,]$weights, 
    #                          samedata = FALSE
    # )
    # #df_match[i, 'wtd_t_test'] <- wtd_t_test
    # 
    # 
    # p_value_t <- wtd_t_test$coefficients[[3]]
    # df_match[i, 'p_value_t'] <- p_value_t
    
    
    # ch2_test <- prop.test(x = c(sum(df_panel_match[df_panel_match$treat_final==0,]$insolv),
    #                             sum(df_panel_match[df_panel_match$treat_final==1,]$insolv)),
    #                       n = c(nrow(df_panel_match[df_panel_match$treat_final==0,]),
    #                             nrow(df_panel_match[df_panel_match$treat_final==1,])))
    # #df_match[i, 'ch2_test'] <- nest(ch2_test)
    # 
    # p_value_ch2 <- ch2_test$p.value
    # df_match[i, 'p_value_ch2'] <- p_value_ch2
    
    
    tryCatch({df_match[i, 'p_value_ch2'] <- chisq.test(df_panel_match$insolv, df_panel_match$treat_final)$p.value},
             error = function(e){NA})
    #df_match[i, 'p_value_ch2'] <- p_value_ch2
    
    sample_w <- svydesign(id~1, weights=~weights, data=df_panel_match)
    tryCatch({df_match[i, 'p_value_ch2_w'] <- svychisq(~insolv+treat_final, sample_w, statistic='F')$p.value[[1]]},
             error = function(e){NA})
    #df_match[i, 'p_value_ch2'] <- p_value_ch2
    
    
    print(i)
    
  }
})

# Quick overview of results
df_match %>% mutate_if(is_double, function(x) round(x,3)) %>% print(n=52)

# Insolvency backlog
df_match %>% mutate_if(is_double, function(x) round(x,3)) %>% filter(mean_w_diff<0)

# Significant difference
df_match %>% mutate_if(is_double, function(x) round(x,3)) %>% filter(p_value_ch2_w<=0.1)
df_match %>% mutate_if(is_double, function(x) round(x,3)) %>% filter(p_value_ch2_w<=0.1) %>% filter(mean_w_diff>0)

# Get matches given treatment 
df_match %>% filter(row_number() == 7) %>% select(data_match) %>% .[[1]] %>% .[[1]] %>% 
  filter(id %in% (df_match %>% filter(row_number() == 7) %>% select(matrix_match) %>% .[[1]] %>% .[[1]] %>% 
                    filter(treated == "2012235156_2020") %>% as_vector())) %>% 
  arrange(desc(treat_final))

# Analyze weights
df_match %>% filter(row_number() == 7) %>% select(data_match) %>% .[[1]] %>% .[[1]] %>% filter(treat_final==0) %>% 
  group_by(insolv) %>% summarise(mean_b = mean(weights), max_b = max(weights))


# Get treatments given matches
df_match %>% filter(row_number() == 7) %>% select(matrix_match) %>% .[[1]] %>% .[[1]]  %>% filter_all(any_vars(grepl("8190860514_2018", .)))
id1 <- df_match %>% filter(row_number() == 7) %>% select(matrix_match) %>% .[[1]] %>% .[[1]]  %>% filter_all(any_vars(grepl("8190860514_2018", .))) %>% 
  select(treated)


df_match %>% filter(row_number() == 7) %>% select(data_match) %>% .[[1]] %>% .[[1]] %>% 
  filter(id %in% as_vector(id1))

save(df_match, file = file.path(getwd(), '05_Model', 'df_match_V1.RData'))






### Balance ###############################################################

# Formula
model_match <- as.formula(paste('treat_final', paste('ch_bonitaet', 
                                                     'p_bonitaet',
                                                     'avg_bonitaet',
                                                     'n_downgrade',
                                                     'age',
                                                     #'refo_dummy',
                                                     sep = ' + '), sep = ' ~ '))


df_match <- df_panel %>% 
  group_nest(wz12_fct, csize_fct, keep = TRUE)

system.time({
  for (i in 1:2){
    
    df_temp <- df_match %>% 
      filter(row_number() == i) %>% 
      select(data) %>% 
      unnest(cols = data) %>% 
      select(crefo, jahr, treat_final, insolv, p_bonitaet, ch_bonitaet, avg_bonitaet, n_downgrade, age) %>% 
      filter(complete.cases(.)) %>% 
      mutate(id = paste(crefo, jahr, sep  = '_')) %>%
      distinct(id, .keep_all = TRUE) %>% 
      filter(!is.na(age)) %>% 
      column_to_rownames('id')
    
    
    #temp_caliper <- floor(sd_boni*(4/log(nrow(df_temp))))
    
    
    match_res <- matchit(formula=model_match, 
                         data=df_temp, 
                         method="nearest",
                         distance = "mahalanobis",
                         #ratio = 1,
                         ratio=floor(nrow(df_temp[df_temp$treat_final==0,])/nrow(df_temp[df_temp$treat_final==1,])),  #6, # nrow(df_panel[df_panel$treat_final==0,])/nrow(df_panel[df_panel$treat_final==1,])
                         replace=TRUE,
                         #discard='control', 
                         #exact = ~ refo_dummy,
                         caliper = c(ch_bonitaet = 8), #c(ch_bonitaet = temp_caliper), # sd(df_panel$ch_bonitaet)*0.25, https://www.lexjansen.com/pharmasug/2006/PublicHealthResearch/PR05.pdf
                         std.caliper=FALSE,
                         #min.controls = 0,
                         #max.controls = 6
                         
    )
    
    
    balance_object <- summary(match_res, standardize=TRUE, pair.dist=TRUE)
    balance_stats <- as_tibble(balance_object$sum.all, rownames = "variable") %>% mutate(type = "all") %>% 
    bind_rows(as_tibble(balance_object$sum.matched, rownames = "variable") %>% mutate(type = "matched")) %>% 
    bind_rows(as_tibble(balance_object$reduction, rownames = "variable") %>% mutate(type = "improvement"))
    
    df_match[i, 'balance_stats'] <- nest(.data = balance_stats, data = everything())
    
    
    print(i)
    
  }
})

save(df_match, file = file.path(getwd(), '05_Model', 'df_balance.RData'))

df_balance <- df_match %>% 
  #filter(row_number() %in% c(1,2)) %>% 
  select(wz12_fct, csize_fct, balance_stats) %>% 
  unnest(cols = balance_stats, keep_empty = TRUE) %>% 
  filter(type=="improvement") %>% 
  select_if(~sum(!is.na(.))>0) %>% 
  mutate_if(is.double, function(x) round(x,2)) %>% 
  select(-c(type)) %>%
  select(wz12_fct, csize_fct, variable, "Std. Mean Diff.", "eCDF Mean") %>% 
  pivot_wider(names_from = variable, values_from = c("Std. Mean Diff.", "eCDF Mean"), names_sep = ":")
  
df_balance %>% 
  kable(format = 'latex', booktabs = TRUE, linesep = "", format.args = list(big.mark = ","))

### Viability Extension ###################################################

system.time({
  for (i in 1:3){
    
    df_temp <- df_match %>% 
      filter(row_number() == i) %>% 
      select(data) %>% 
      unnest(cols = data) %>% 
      select(crefo, jahr, treat_final, insolv, p_bonitaet, ch_bonitaet, avg_bonitaet, n_downgrade, age) %>% 
      filter(complete.cases(.)) %>% 
      mutate(id = paste(crefo, jahr, sep  = '_')) %>%
      distinct(id, .keep_all = TRUE) %>% 
      filter(!is.na(age)) %>% 
      column_to_rownames('id')
    
    
    #temp_caliper <- floor(sd_boni*(4/log(nrow(df_temp))))
    
    
    tryCatch({
      match_res <- matchit(
        formula=model_match, 
        data=df_temp, 
        method="nearest",
        distance = "mahalanobis",
        #ratio = 1,
        ratio=floor(nrow(df_temp[df_temp$treat_final==0,])/nrow(df_temp[df_temp$treat_final==1,])),  #6, # nrow(df_panel[df_panel$treat_final==0,])/nrow(df_panel[df_panel$treat_final==1,])
        replace=TRUE,
        #discard='control', 
        #exact = ~ refo_dummy,
        caliper = c(ch_bonitaet = 8), #c(ch_bonitaet = temp_caliper), # sd(df_panel$ch_bonitaet)*0.25, https://www.lexjansen.com/pharmasug/2006/PublicHealthResearch/PR05.pdf
        std.caliper=FALSE,
        #min.controls = 0,
        #max.controls = 6
      )
      
      df_panel_match <- match.data(match_res) %>% 
        as_tibble() %>% 
        mutate(id = paste(crefo, jahr, sep  = '_')) %>% 
        select(id, everything())
      df_match[i, 'data_match'] <- nest(df_panel_match, data = everything())
      
      
      df_matrix_match <- match_res$match.matrix %>% as_tibble(rownames = 'treated')
      df_match[i, 'matrix_match'] <- nest(df_matrix_match, data = everything())
      
      
      cont_mean_w <- weighted.mean(x = df_panel_match[df_panel_match$treat_final==0,]$insolv, w = df_panel_match[df_panel_match$treat_final==0,]$weights)
      df_match[i, 'cont_mean_w'] <- cont_mean_w
      
      
      treat_mean <- mean(df_panel_match[df_panel_match$treat_final==1,]$insolv)
      df_match[i, 'treat_mean'] <- treat_mean
      
      
      mean_w_diff <- treat_mean - cont_mean_w
      df_match[i, 'mean_w_diff'] <- mean_w_diff
      
      tryCatch({
        sample_w <- svydesign(id~1, weights=~weights, data=df_panel_match)
        df_match[i, 'p_value_ch2_w'] <- svychisq(~insolv+treat_final, sample_w, statistic='F')$p.value[[1]]  
      }, 
      error = function(e){
        df_match[i, 'p_value_ch2_w'] <- as.double(NA)}
      )
      
      
      
    },
    error = function(e){
      df_match[i, 'data_match'] <<- NA
      df_match[i, 'matrix_match'] <<- NA
      df_match[i, 'cont_mean_w'] <<- NA
      df_match[i, 'treat_mean'] <<- NA
      df_match[i, 'mean_w_diff'] <<- NA
      df_match[i, 'p_value_ch2_w'] <<- NA}
    )
    
    
    print(i)
    
  }
})

# Quick overview of results
df_match %>% mutate_if(is_double, function(x) round(x,3)) %>% print(n=52)

# Insolvency backlog
df_match %>% mutate_if(is_double, function(x) round(x,3)) %>% filter(mean_w_diff<0)

# Significant difference
df_match %>% mutate_if(is_double, function(x) round(x,3)) %>% filter(p_value_ch2_w<=0.1)
df_match %>% mutate_if(is_double, function(x) round(x,3)) %>% filter(p_value_ch2_w<=0.1) %>% filter(mean_w_diff>0)

# Get matches given treatment 
df_match %>% filter(row_number() == 7) %>% select(data_match) %>% .[[1]] %>% .[[1]] %>% 
  filter(id %in% (df_match %>% filter(row_number() == 7) %>% select(matrix_match) %>% .[[1]] %>% .[[1]] %>% 
                    filter(treated == "2012235156_2020") %>% as_vector())) %>% 
  arrange(desc(treat_final))

# Analyze weights
df_match %>% filter(row_number() == 7) %>% select(data_match) %>% .[[1]] %>% .[[1]] %>% filter(treat_final==0) %>% 
  group_by(insolv) %>% summarise(mean_b = mean(weights), max_b = max(weights))


# Get treatments given matches
df_match %>% filter(row_number() == 7) %>% select(matrix_match) %>% .[[1]] %>% .[[1]]  %>% filter_all(any_vars(grepl("8190860514_2018", .)))
id1 <- df_match %>% filter(row_number() == 7) %>% select(matrix_match) %>% .[[1]] %>% .[[1]]  %>% filter_all(any_vars(grepl("8190860514_2018", .))) %>% 
  select(treated)


df_match %>% filter(row_number() == 7) %>% select(data_match) %>% .[[1]] %>% .[[1]] %>% 
  filter(id %in% as_vector(id1))

save(df_match, file = file.path(getwd(), '05_Model', 'df_match_invalid.RData'))



## CEM ====================================================================


## Data Preparation =======================================================
### HR-firms Only ##########################################################
table(!is.na(df_panel$hrnummer))


load(file = file.path(getwd(), '02_Data', '03_SanityChecks', 'df_hr.RData'))
df_panel <- df_panel %>% 
  coalesce_join(df_hr, by = 'crefo')

table(!is.na(df_panel$hrnummer))

df_temp <- df_panel %>% 
  filter(!is.na(hrnummer))

df_res <- insolv_ym(df_temp)
sapply(df_res %>% select(contains('rate')), function(x) mean(x, na.rm = TRUE))
df_res <- insolv_ym(df_panel)
sapply(df_res %>% select(contains('rate')), function(x) mean(x, na.rm = TRUE))

# Check average insolvencies in pre and post Corona samples
df_panel_pre <- df_temp %>% 
  filter(treat_final == 0)
mean(df_panel_pre$insolv, na.rm = TRUE)
# 0.01539

df_panel_post <- df_temp %>% 
  filter(treat_final == 1)
mean(df_panel_post$insolv, na.rm = TRUE)
# 0.01055

# Reduction of observations due to deletion of firms w/o hrnummer
dim_s14 <- nrow(df_temp)
df_drop <- df_drop %>% 
  bind_rows(tibble(N = dim_s14, Remark = 'Drop of firms not listed in HR'))


# Number of missings among HR frims
sapply(df_temp, function(x) sum(is.na(x)))

df_panel <- df_temp



### Sample ################################################################
# Problem: Possibly insolvencies of large enterprises oversampled in corona period
crosstab(df_panel_match$csize_fct, 
         c(df_panel_match$insolv, df_panel_match$treat_final),
         prop.r = TRUE,
         drop.levels = TRUE,
         plot = FALSE
         )

df_panel_sample <- df_panel %>%
  #elect(-c(treat, bonitaet, q_p_bonitaet)) %>% 
  mutate(kreis = as.factor(kreis)) %>% 
  group_by(insolv) %>% 
  nest() %>% 
  ungroup() %>% 
  mutate(frac = c(0.1, 1)) %>% 
  mutate(samp = map2(data, frac, sample_frac)) %>% 
  select(-c(data, frac)) %>% 
  unnest(samp)




crosstab(dep = df_panel_sample$treat_final,
         indep = df_panel_sample$insolv,
         #weight =
         prop.r = TRUE,
         drop.levels = TRUE,
         plot = FALSE)




### Complete Cases ########################################################
sapply(df_panel, function(x) sum(is.na(x)))

df_panel_comp <- df_panel %>%
  select(crefo, jahr_rd, treat_final, treat_fct, insolv, insolv_fct, p_bonitaet, p_bonitaet_fct, 
         ch_bonitaet, bonitaet, refo_fct, ctype_fct, csize_fct, wz_fct, updays, count, ) %>% 
  filter(complete.cases(.))


crosstab(dep = df_panel_comp$treat_final,
         indep = df_panel_comp$insolv,
         #weight =
         prop.r = TRUE,
         drop.levels = TRUE,
         plot = FALSE)

crosstab(dep = df_panel$treat_final,
         indep = df_panel$insolv,
         #weight =
         prop.r = TRUE,
         drop.levels = TRUE,
         plot = FALSE)

#df_panel <- df_panel_comp



sapply(df_panel, class)






## Cutpoint Definition ====================================================



# Use quantile information of pre-Corona observations for cut point definition of change in credit rating
quant_boni <- quantile(df_panel %>% .$ch_bonitaet, probs = c(0, 0.05, seq(0.1,0.9,length.out = 9), 0.95, 1))
cp_ch_bonitaet <- c(-Inf, 
  unique(quant_boni)[2:5]-1,
  0,
  unique(quant_boni)[6:(length(unique(quant_boni))-1)],
  Inf)

quant_boni <- quantile(df_panel %>% filter(ch_bonitaet>0) %>% .$ch_bonitaet, probs = c(0.25, 0.5, 0.75))
cp_ch_bonitaet <- c(-Inf, 
                    -1,
                    0,
                    unique(quant_boni),
                    Inf)

# Cutpoint list
cp_cem <- list(ch_anzma = c(-Inf, -1, 0, Inf), 
               ch_bonitaet = cp_ch_bonitaet,
               anzma = c(0, 9, 49, 99, 249, Inf))

# Matching data
df_panel_match <- df_panel %>%
  mutate(ch_anzma_fct = addNA(cut(ch_anzma, breaks = cp_cem$ch_anzma)),
         csize_fct = addNA(csize_fct),
         ctype_fct = addNA(ctype_fct),
         refo_fct = addNA(refo_fct),
         wz_fct = addNA(wz_fct),
         wz5_fct = addNA(wz5_fct),
         wz12_fct = addNA(wz12_fct),
         ch_bonitaet_fct = cut(ch_bonitaet, breaks = cp_ch_bonitaet),
         anzma_fct = addNA(cut(ch_anzma, breaks = cp_cem$anzma)),
         ) %>% 
  select(crefo, jahr_rd, treat_final, insolv, ch_bonitaet, ch_bonitaet_fct, 
         p_bonitaet, p_bonitaet_fct, q_p_bonitaet_fct, anzma_fct, ch_anzma_fct, 
         csize_fct, ctype_fct, refo_fct, wz_fct, wz5_fct, wz12_fct)

# Formula
model_match <- as.formula(paste('treat_final', paste('ch_bonitaet', 
                                                     #'q_p_bonitaet_fct', 
                                                     'p_bonitaet_fct',
                                                     #'ch_bonitaet', 
                                                     #'p_bonitaet', 
                                                     'ch_anzma_fct', 
                                                     #'anzma_fct', 
                                                     'csize_fct', 
                                                     'ctype_fct', 
                                                     #'refo_fct', 
                                                     #'wz5_fct',
                                                     'wz12_fct',
                                                     sep = ' + '), sep = ' ~ '))


# Distribution of control variables
crosstab(dep = df_panel_match$treat_final, 
         indep = df_panel_match$ch_anzma_fct,  
         prop.r = TRUE, 
         drop.levels = TRUE,
         plot = FALSE)
crosstab(dep = df_panel_match$treat_final, 
         indep = df_panel_match$csize_fct,  
         prop.r = TRUE, 
         drop.levels = TRUE,
         plot = FALSE)
crosstab(dep = df_panel_match$treat_final, 
         indep = df_panel_match$ctype_fct,  
         prop.r = TRUE, 
         drop.levels = TRUE,
         plot = FALSE)
crosstab(dep = df_panel_match$treat_final, 
         indep = df_panel_match$wz12_fct,  
         prop.r = TRUE, 
         drop.levels = TRUE,
         plot = FALSE)

## Estimaton ==============================================================

system.time({
  match_res <- matchit(formula = model_match, 
                       data=as.data.frame(df_panel_match), 
                       method="cem", 
                       cutpoints=cp_cem
                       )
})


# Matching results
match_res

# Get matching results (note that non-matches have been dropped)
df_panel_match <- match.data(match_res) %>% 
  as_tibble()

# Save matching result
save(df_panel_match, file = file.path(getwd(), '05_Model', 'cem_hr_1237.RData'))


# Potential number of subclasses
9*4*4*5*4*5
# Actual number of subclasses
# Check if groups drop if there do not fall any observations into the strata
length(table(df_panel_match$subclass))


# Recalibrate weights
df_panel_match <- df_panel_match %>% 
  mutate(weights2 = ifelse(weights==1, weights, (weights*nrow(df_panel_match[df_panel_match$treat_final==1,]))*nrow(df_panel_match)))

# Compare means
mean(as_vector(df_panel_match[df_panel_match$treat_final==0, 'insolv']))
# Average insolvency rate per month in control sample with overall number of 
sum(as_vector(df_panel_match[df_panel_match$treat_final==0, 'insolv']))/nrow(df_panel_match)/33 # 34 months from 2017-07-01 to 2020-03-31

mean(as_vector(df_panel_match[df_panel_match$treat_final==1, 'insolv']))
# Average insolvency rate per month in control sample with overall number of 
sum(as_vector(df_panel_match[df_panel_match$treat_final==1, 'insolv']))/nrow(df_panel_match)/3 # 3 months from from 2020-04-01 to 2020-06-30


mean(as_vector(df_panel_match[df_panel_match$treat_final==0, 'insolv']))
weighted.mean(x = as_vector(df_panel_match[df_panel_match$treat_final==0, 'insolv']),
              w = as_vector(df_panel_match[df_panel_match$treat_final==0, 'weights']))

mean(as_vector(df_panel_match[df_panel_match$treat_final==0&df_panel_match$csize_fct=='Micro-enterprise', 'insolv']))
weighted.mean(x = as_vector(df_panel_match[df_panel_match$treat_final==0&df_panel_match$csize_fct=='Micro-enterprise', 'insolv']),
              w = as_vector(df_panel_match[df_panel_match$treat_final==0&df_panel_match$csize_fct=='Micro-enterprise', 'weights']))

mean(as_vector(df_panel_match[df_panel_match$treat_final==0&df_panel_match$csize_fct=='Small enterprise', 'insolv']))
weighted.mean(x = as_vector(df_panel_match[df_panel_match$treat_final==0&df_panel_match$csize_fct=='Small enterprise', 'insolv']),
              w = as_vector(df_panel_match[df_panel_match$treat_final==0&df_panel_match$csize_fct=='Small enterprise', 'weights']))

mean(as_vector(df_panel_match[df_panel_match$treat_final==0&df_panel_match$csize_fct=='Medium-sized enterprise', 'insolv']))
weighted.mean(x = as_vector(df_panel_match[df_panel_match$treat_final==0&df_panel_match$csize_fct=='Medium-sized enterprise', 'insolv']),
              w = as_vector(df_panel_match[df_panel_match$treat_final==0&df_panel_match$csize_fct=='Medium-sized enterprise', 'weights']))

mean(as_vector(df_panel_match[df_panel_match$treat_final==0&df_panel_match$csize_fct=='Large enterprise', 'insolv']))
weighted.mean(x = as_vector(df_panel_match[df_panel_match$treat_final==0&df_panel_match$csize_fct=='Large enterprise', 'insolv']),
              w = as_vector(df_panel_match[df_panel_match$treat_final==0&df_panel_match$csize_fct=='Large enterprise', 'weights']))

# Estimate PD per strata
df_temp <- tibble(subclass = as.numeric(names(table((match_res$subclass)))),
                  n = as.numeric(table(match_res$subclass)))

df_subclass <- df_panel_match %>% 
  left_join(df_temp, by = 'subclass') %>% 
  select(subclass, weights, n, insolv, treat_final) %>% 
  filter(treat_final==0) %>% 
  group_by(subclass) %>% 
  summarise(
    n = mean(n),
    n_pre = n(),
    est_PD = mean(insolv)) %>%
  #mutate(est_PD_norm = vec_norm(est_PD)) %>% 
  ungroup() %>% 
  mutate(n_post = n - n_pre) %>% 
  select(subclass, n, n_pre, n_post, everything()) %>% 
  arrange(desc(est_PD)) 

# Merge estimated PD to panel
df_panel_match <- df_panel_match %>% left_join(df_subclass, by = 'subclass') 
  

df_panel_pan <- df_panel_match %>% 
  filter(treat_final == 1)

df_panel_pre <- df_panel_match %>% 
  filter(treat_final == 0)

mean(df_panel_pan$insolv)/4 # Wrong
mean(df_panel_pan$est_PD)/4 # Wrong
sum(df_panel_pan$insolv)/nrow(df_panel_match)/4 # Right: rate per month
sum(df_panel_pan$est_PD)/nrow(df_panel_match)/4 # Right: rate per month
sum(df_panel_pan$insolv)/nrow(df_panel_match) # Right: rate as per 4 months
sum(df_panel_pan$est_PD)/nrow(df_panel_match) # Right: rate as per 4 months
# Insolvency rate is small in corona sample based on counterfactual estimation.
# Find a way to align the two above -> work with distance? -< ?cem
# or maybe within subclasses some distance measure based on boni and change in boni
# How do insolvencies within both samples look like
mean(df_panel_pre$insolv)/30
mean(df_panel_pre$est_PD)/30
sum(df_panel_pre$insolv)/nrow(df_panel_match)/30
sum(df_panel_pre$est_PD)/nrow(df_panel_match)/30
sum(df_panel_pre$insolv)/nrow(df_panel_match)/8.25
sum(df_panel_pre$est_PD)/nrow(df_panel_match)/8.25

(sum(df_panel_pre$est_PD)/nrow(df_panel_match)/30) / (sum(df_panel_pan$est_PD)/nrow(df_panel_match)/4)
(sum(df_panel_pre$est_PD)/nrow(df_panel_match)/7.5) / (sum(df_panel_pan$est_PD)/nrow(df_panel_match))
## Strata Analysis ========================================================
### Merge Small Strata ####################################################
df_panel_strata <- df_panel_match %>% 
  select(ch_bonitaet, ch_bonitaet_fct, 
         p_bonitaet, p_bonitaet_fct, 
         #q_p_bonitaet_fct, q_p_bonitaet_fct, 
         ch_anzma_fct, csize_fct, ctype_fct, 
         wz5_fct, wz12_fct, 
         subclass, weights, est_PD, insolv, treat_final, n, n_pre, n_post)

df_panel_strata %>% 
  arrange(desc(est_PD), subclass)



df_panel_strata %>% 
  filter(ch_bonitaet_fct == '(0,4]' & 
           q_p_bonitaet_fct == '3. Quartile' & 
           csize_fct == 'Large enterprise' & 
           ctype_fct == 'Incumbent firm' & 
           wz5_fct == 'Business-related services')

# AS FOR NOW NO FURTHER AGGREGATION IS DONE



### Strata Descriptives ###################################################


df_panel_strata %>% 
  tab(n_pre) %>% 
  mutate(n_pre = as.numeric(n_pre)) %>% 
  arrange(n_pre) %>% 
  mutate(p_cum = round(cumsum(p), 5))
# Less than 2% of observations fall into strata with at most 5 control observations


df_strata <- df_panel_match %>% 
  select(p_bonitaet_fct, ch_bonitaet_fct, ch_anzma_fct, ctype_fct, csize_fct, wz12_fct, n, weights, n_pre, n_post, est_PD) %>% 
  group_by(p_bonitaet_fct, ch_bonitaet_fct, ch_anzma_fct, ctype_fct, csize_fct, wz12_fct) %>% 
  distinct() %>% 
  ungroup() %>% 
  arrange(desc(est_PD))

df_strata %>% 
  arrange(n_pre, desc(est_PD))

df_strata %>% 
  tab(n_pre) %>% 
  mutate(n_pre = as.numeric(n_pre)) %>% 
  arrange(n_pre) %>% 
  mutate(p_cum = round(cumsum(p), 5))
# ~ 13% of strata have at most 5 control observations (~ 3% of strata has only one control observation)

#save(df_strata, file = 'Q:\\Meine Bibliotheken\\Research\\SME_Corona\\02_Data\\01_PanelData\\df_strata.RData')



### Strata Distribution ###################################################

##############################
#1) in-group shares by decile of estimated insolvency rate
q_est_PD <- df_strata %>% 
  filter(est_PD > 0) %>% 
  select(est_PD) %>% 
  mutate(q_est_PD = ntile(est_PD, 4))

df_temp <- df_panel_match %>% 
  mutate(insolv_fct = factor(ifelse(insolv==0, 'Non-insolvent', 'Insolvent'), 
                             levels = c('Non-insolvent', 'Insolvent')),
         treat_fct = factor(ifelse(treat_final==0, 'Pre-suspension', 'Suspension period'),
                            levels = c('Pre-suspension', 'Suspension period'))) %>% 
  select(treat_fct, est_PD) %>% 
  left_join(q_est_PD, by = 'est_PD')


df_dist <- df_temp %>% 
  group_by(treat_fct) %>% 
  mutate(n_sample = n()) %>% 
  ungroup() %>% 
  select(est_PD, treat_fct, n_sample, q_est_PD) %>% 
  group_by(treat_fct, q_est_PD) %>% 
  summarise(
    n_sample = mean(n_sample),
    n = n(),
    rel_n = n/n_sample) %>% 
  ungroup()

df_dist %>% 
  pivot_wider(id_cols = c(treat_fct, q_est_PD), names_from = treat_fct, values_from = rel_n) %>% 
  mutate(rel_n_diff = `Suspension period` - `Pre-suspension`) %>% 
  ggplot() +
  geom_line(aes(x = q_est_PD, y = rel_n_diff))

df_dist %>% 
  filter(!is.na(q_est_PD)) %>%
  #mutate(est_PD_q = cut(est_PD))
  ggplot(aes(x = q_est_PD, y = rel_n, color = treat_fct)) +
  geom_point(alpha = 0.6, stat = 'identity') +
  scale_y_log10() +
  theme_jod
  
#2) in-group densities
df_panel_match <- df_panel_match %>% 
  mutate(insolv_fct = factor(ifelse(insolv==0, 'Non-insolvent', 'Insolvent'), 
                             levels = c('Non-insolvent', 'Insolvent')),
         treat_fct = factor(ifelse(treat_final==0, 'Pre-suspension', 'Suspension period'),
                            levels = c('Pre-suspension', 'Suspension period')))


df_panel_match %>% 
  #filter(est_PD>0) %>% 
  ggplot(aes(x=est_PD, group=treat_fct, fill=treat_fct)) +
  stat_bin(aes(y = ..density..), position = position_dodge(), alpha = 0.6, bins = 50) +
  scale_y_sqrt() +
  theme_jod
  
df_temp <- df_panel_match %>% 
  group_by(treat_fct) %>% 
  mutate(n_sample = n()) %>% 
  ungroup() %>% 
  select(est_PD, treat_fct, n_sample) %>% 
  group_by(treat_fct, est_PD) %>% 
  summarise(
    n_sample = mean(n_sample),
    n = n(),
    rel_n = n/n_sample) %>% 
  ungroup()

df_temp %>% 
  pivot_wider(id_cols = c(treat_fct, est_PD), names_from = treat_fct, values_from = rel_n) %>% 
  mutate(rel_n_diff = `Suspension period` - `Pre-suspension`) %>% 
  filter(est_PD>0) %>% 
  ggplot() +
  geom_line(aes(x = est_PD, y = rel_n_diff))
  filter(est_PD>0) %>% 
  ggplot() +
  geom_point(aes(x=est_PD, y=rel_n, color=treat_fct), stat="identity", position='dodge', lwd=0.5) +
  scale_y_log10() + 
  theme_jod




# Merge Matching Results Back to Panel ------------------------------------

# Merge variables back
df_panel <- df_panel %>% 
  mutate(ch_anzma_fct = cut(ch_anzma, breaks = cp_cem$ch_anzma),
         ch_bonitaet_fct = cut(ch_bonitaet, breaks = cp_ch_bonitaet),
         anzma_fct = cut(anzma, breaks = cp_cem$anzma)) %>% 
  left_join(df_panel_match %>% select(crefo, jahr, distance, weights, subclass, n, n_pre, n_post, est_PD), by = c('crefo', 'jahr'))


# Regression --------------------------------------------------------------






## Initial Model ==========================================================


model1 <- paste('insolv', paste('ch_bonitaet', 'r_p_bonitaet', 'ch_anzma', 'csize', 'ctype', 'refo', 'wz', sep = ' + '), sep = ' ~ ')

probit_prePan <- glm(formula = model1, family = binomial(link = 'probit'), data = df_panel, subset = (treat_final == 0))
summary(probit_prePan)

probit_Pan <- glm(formula = model1, family = binomial(link = 'probit'), data = df_panel, subset = (treat_final == 1))
summary(probit_Pan)



## Interesection between ch_bonitaet and r_p_bonitaet ======================



model2 <- paste('insolv', paste('ch_bonitaet*r_p_bonitaet', 'r_p_bonitaet', 'ch_anzma', 'csize', 'ctype', 'refo', 'wz', sep = ' + '), sep = ' ~ ')

probit_prePan <- glm(formula = model2, family = binomial(link = 'probit'), data = df_panel, subset = (treat_final == 0))
summary(probit_prePan)

probit_Pan <- glm(formula = model2, family = binomial(link = 'probit'), data = df_panel, subset = (treat_final == 1))
summary(probit_Pan)

glm_margins(probit_prePan)
glm_margins(probit_Pan)

system.time({
  probit_prePan_ame <- glm_margins(probit_prePan, type = 'response')
  probit_Pan_ame <- glm_margins(probit_Pan, type = 'response')
  
})



## Save Results ===========================================================
save(df_panel, file = file.path(getwd(), '02_Data', '01_PanelData', 'df_panel_estimated.RData'))
# Save dropping list
write_delim(df_drop, path = file.path(getwd(), '02_Data', '03_SanityChecks', 'df_drop.txt'), delim = '\t')


# Save panel data with matching results
save(df_panel, file = file.path(getwd(), '02_Data', '01_PanelData', 'df_panel_clean_res.RData'))
#write_delim(df_panel, path = file.path(getwd(), '02_Data', '01_PanelData', 'df_panel2010_2020.txt'), delim = '\t')



# -------------------------------------------------------------------------

# cem package instead of matchit
imbalance(group = df_panel_match$treat_final,  data = as.data.frame(df_panel_match))

df_panel_match <- df_panel_match %>% 
  select(-c(crefo, jahr, jahr_rd, ch_bonitaet,p_bonitaet_fct, p_bonitaet))

system.time({
  match_res2 <- cem(
    treatment = "treat_final",
    data = as.data.frame(df_panel_match), 
    drop = 'insolv',
    cutpoints = cp_cem
  )
})

att(match_res2, insolv ~ treat_final, data = as.data.frame(df_panel_match), model="logit")

match_res2
