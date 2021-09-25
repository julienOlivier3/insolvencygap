# CLEANING
setwd("Q:\\Meine Bibliotheken\\Research\\SME_Corona")
source(file.path(getwd(), '03_Code', '02_R', 'step0_setup.R'))

# -------------------------------------------------------------------------
# #########################################################################
# -------------------------------------------------------------------------




# Load Appended Panel -----------------------------------------------------

#load(file = file.path(getwd(), '02_Data', '01_PanelData', 'df_panel_2017_2020_clean.RData'))
#load(file = file.path(getwd(), '02_Data', '01_PanelData', 'df_panel_clean_fct.RData'))

# DROP
df_drop <- read_delim(file = file.path(getwd(), '02_Data', '03_SanityChecks', 'df_drop.txt'), delim = '\t')
df_drop <- df_drop[1:10,]


df_panel_pre <- df_panel %>% 
  filter(treat_final == 0)
mean(df_panel_pre$insolv, na.rm = TRUE)


df_panel_post <- df_panel %>% 
  filter(treat_final == 1)
mean(df_panel_post$insolv, na.rm = TRUE)


# 1) Drop of Observations Prior to 2017-07-01 -----------------------------
# DO!
df_temp <- df_panel %>% 
  mutate(keep = case_when(
    (insolv == 0) & (rechdat >= '2017-07-01') & (rechdat <= '2020-07-31') ~ 1,
    insolv == 1 ~ 1,
    (insolv == 0) & (rechdat < '2017-07-01') ~ 0,
    (insolv == 0) & (rechdat > '2020-07-31') ~ 0
  )) %>% 
  filter(keep == 1) %>% 
  select(-c(keep))



# Check average insolvencies in pre and post Corona samples
df_panel_pre <- df_temp %>% 
  filter(treat_final == 0)
mean(df_panel_pre$insolv, na.rm = TRUE)


df_panel_post <- df_temp %>% 
  filter(treat_final == 1)
mean(df_panel_post$insolv, na.rm = TRUE)

df_temp %>% tab(insolv)
plot_insolv(df_temp, end = '2020-07-31')


dim_s11 <- nrow(df_temp)
df_drop <- df_drop %>%
  bind_rows(tibble(N = dim_s11, Remark = 'Drop of observations and insolvencies prior to 2017-07-01'))

df_panel <- df_temp

#X# 2) Drop of Observations w\o Update for long Time ---------------------------
# DO!
# Potentially one should drop observations which have not been updated for more than a certain threshhold of days
quantile(df_panel$updays, probs = seq(0.1,0.9, by = 0.1))
range(df_panel$updays)
quantile(df_panel$updays, probs = seq(0.9,1, by = 0.01))
# There are some observations which have not been updated for an extremely long time. Drop these!

df_temp <- df_panel %>% 
  filter(updays <= 360*3)




# Check average insolvencies in pre and post Corona samples
df_panel_pre <- df_temp %>% 
  filter(treat_final == 0)
mean(df_panel_pre$insolv, na.rm = TRUE)

df_panel_post <- df_temp %>% 
  filter(treat_final == 1)
mean(df_panel_post$insolv, na.rm = TRUE)


df_temp %>% tab(insolv)
plot_insolv(df_temp, end = '2020-07-31')

# DROP
dim_s12 <- nrow(df_temp)
df_drop <- df_drop %>% 
  bind_rows(tibble(N = dim_s12, Remark = 'Drop observations which have not been updated for more than 3 years.'))


df_panel <- df_temp







# 3) Recode Insolvencies Observed 4 Months after Rechdat ---------------------
# DO!
#load(file = file.path(getwd(), '02_Data', '01_PanelData', 'df_ins_destatis2.RData'))

df_panel %>% 
  filter(d_exitdat < -360/3) %>% 
  select(crefo, jahr_rd, jahr, treat_final, rechdat, exitdat, d_exitdat, insolv, bonitaet, ch_bonitaet)

id <- df_panel %>% 
  filter(d_exitdat < -360/3) %>% 
  select(crefo) %>% 
  as_vector()

# We recode observations where exitdat is more than 4 months after rechdat as survivors even if they have failed by then
df_temp <- df_panel %>% 
  mutate(insolv = case_when(
    (insolv == 0) ~ 0,
    (insolv == 1) & (d_exitdat < -360/3) ~ 0,
    (insolv == 1) & (d_exitdat >= -360/3) ~ 1
  )) %>% 
  mutate(exitdat = if_else(insolv==1, exitdat, as.Date(NA)))

# Also drop firms which have been recoded as non-insolvent having now a research date prior to 2017-07-01
df_temp <- df_temp %>% 
   mutate(keep = case_when(
    (insolv == 0) & (rechdat >= '2017-07-01') & (rechdat <= '2020-07-31') ~ 1,
    insolv == 1 ~ 1,
    (insolv == 0) & (rechdat < '2017-07-01') ~ 0,
    (insolv == 0) & (rechdat > '2020-07-31') ~ 0
    )) %>% 
    filter(keep == 1) %>% 
    select(-c(keep))



# Check average insolvencies in pre and post Corona samples
df_panel_pre <- df_temp %>% 
  filter(treat_final == 0)
mean(df_panel_pre$insolv, na.rm = TRUE)


df_panel_post <- df_temp %>% 
  filter(treat_final == 1)
mean(df_panel_post$insolv, na.rm = TRUE)

df_temp %>% tab(insolv)
plot_insolv(df_temp)

# DROP
dim_s13 <- nrow(df_temp)
df_drop <- df_drop %>% 
  bind_rows(tibble(N = dim_s13, Remark = 'Drop observations which have been recoded as non-insolvent due to maximum time lag of 4 months but with rechdat prior to 2017-07-01-'))



df_panel <- df_temp




#X# 4) Drop non-HR firms ----------------------------------------------------
# DO!

df_temp <- df_panel %>% 
  filter(!is.na(hrnummer))



df_temp %>% tab(insolv)
plot_insolv(df_temp)

# Check average insolvencies in pre and post Corona samples
df_panel_pre <- df_temp %>% 
  filter(treat_final == 0)
mean(df_panel_pre$insolv, na.rm = TRUE)


df_panel_post <- df_temp %>% 
  filter(treat_final == 1)
mean(df_panel_post$insolv, na.rm = TRUE)

# DROP
dim_s14 <- nrow(df_temp)
df_drop <- df_drop %>% 
  bind_rows(tibble(N = dim_s14, Remark = 'Drop firms not in HR.'))


df_panel <- df_temp



#X# 5) Drop Hybrid Observations ------------------------------------------------
# Check if there exist insolvencies after 2020-04-01 but with rechdat before 2020-04-01
#load(file = file.path(getwd(), '02_Data', '01_PanelData', 'df_ins_destatis4.RData'))

df_panel %>% 
  filter(exitdat >= '2020-04-01') %>% 
  filter(rechdat < '2020-04-01') %>% 
  select(crefo, rechdat, exitdat, d_exitdat, insolv, treat_final)
# Yes there are some but they all count to the non-treated group
# Clearly these observations need to be dropped as their exit information does not relate to pandemic period
id <- df_panel %>% 
  filter(exitdat >= '2020-04-01') %>% 
  filter(rechdat < '2020-04-01') %>% 
  select(crefo, rechdat)
# DO!

# Hybrid observations are those where rechdat and exitdat fall in different samples. 
# Drop these!
df_temp <- df_panel %>% 
  anti_join(id, by = c('crefo', 'rechdat'))


# Check average insolvencies in pre and post Corona samples
df_panel_pre <- df_temp %>% 
  filter(treat_final == 0)
mean(df_panel_pre$insolv, na.rm = TRUE)


df_panel_post <- df_temp %>% 
  filter(treat_final == 1)
mean(df_panel_post$insolv, na.rm = TRUE)

df_temp %>% tab(insolv)
plot_insolv(df_temp)


# Reduction of panel due to deletion of non-existent and non-active observations 
dim_s15 <- nrow(df_temp)
df_drop <- df_drop %>% 
  bind_rows(tibble(N = dim_s15, Remark = 'Drop of hybrid insolvencies where exitdat and rechtdat do not fall in the same period'))


df_panel <- df_temp





# 7) Drop Observations between 2020-01-01 and 2020-04-01 ----------------

df_temp <- df_panel %>% 
  mutate(keep = case_when(
    (insolv == 0) & (rechdat >= '2020-01-01') & (rechdat < '2020-04-01') ~ 0,
    (insolv == 1) & (exitdat >= '2020-01-01') & (exitdat < '2020-04-01') ~ 0,
    (insolv == 0) & (rechdat < '2020-01-01') ~ 1,
    (insolv == 0) & (rechdat >= '2020-04-01') ~ 1,
    (insolv == 1) & (exitdat < '2020-01-01') ~ 1,
    (insolv == 1) & (exitdat >= '2020-04-01') ~ 1
  )) %>% 
  filter(keep == 1) %>% 
  select(-c(keep))

# Check insolvency rate
df_temp %>% tab(insolv)
df_res <- insolv_ym(df_temp)
plot_insolv(df_temp)



# Reduction of panel du to deletion of non-existent and non-active observations 
dim_s16 <- nrow(df_temp)
df_drop <- df_drop %>% 
  bind_rows(tibble(N = dim_s16, Remark = 'Drop of margin observations between 2020-01-01 and 2020-03-31.'))


df_panel <- df_temp


#X# 8) Drop Insolvencies in July 2020 ------------------------------------------

df_temp <- df_panel %>% 
  filter(!(exitdat == '2020-07-01') | is.na(exitdat))

# Check insolvency rate
df_res <- insolv_ym(df_temp)
sapply(df_res %>% select(contains('rate')), function(x) mean(x, na.rm = TRUE))

# Reduction of panel due to deletion of non-existent and non-active observations 
dim_s13 <- nrow(df_temp)
df_drop <- df_drop %>% 
  bind_rows(tibble(N = dim_s13, Remark = 'Drop of insolvencies in July 2020.'))




df_panel <- df_temp


# Save Data ---------------------------------------------------------------

# Save cleaned panel data
#save(df_panel, file = file.path(getwd(), '02_Data', '01_PanelData', 'df_panel_clean.RData'))
write_delim(df_panel, path = file.path(getwd(), '02_Data', '01_PanelData', 'df_panel_clean_final.txt'), delim = '\t')

# Save dropping list
write_delim(df_drop, path = file.path(getwd(), '02_Data', '03_SanityChecks', 'df_drop.txt'), delim = '\t')

