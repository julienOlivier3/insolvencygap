# ESTIMATION
setwd("Q:\\Meine Bibliotheken\\Research\\SME_Corona")
source(file.path(getwd(), '03_Code', '02_R', 'step0_setup.R'))

# -------------------------------------------------------------------------
# #########################################################################
# -------------------------------------------------------------------------

# Load Appended & Cleaned Panel -------------------------------------------

# Credit Rating Data
df_panel <- read_delim(file = file.path(getwd(), '02_Data', '01_PanelData', '01_Backup', 'df_panel_clean_final.txt'),
                       delim = '\t')
#df_panel <- read_delim(file = file.path(getwd(), '02_Data', '01_PanelData',  'df_panel_clean_final.txt'),
#                       delim = '\t')

# Create list of crefos in pre-crisis period for which no insolvency information exists
df_crefo <- df_panel %>% 
  filter(treat_final == 0) %>%
  filter(boni_orig>=500) %>%
  group_by(crefo) %>% 
  filter(all(insolv==0)) %>% 
  ungroup() %>% 
  distinct(crefo) 


# Insolvency Information
df_insolv <- read_delim(file = file.path(getwd(), '02_Data', '09_Insolvency', '01_Treatment', 'df_insolv.txt'), 
                        delim = '\t') %>% 
  rename('crefo' = 'X1')
df_insolv2 <- read_delim(file = file.path(getwd(), '02_Data', '09_Insolvency', '02_BadRating', 'df_insolv.txt'), 
                         delim = '\t') %>% 
  rename('crefo' = 'X1') %>% 
  filter((crefo %in% as_vector(df_crefo)))

df_insolv <- df_insolv %>% bind_rows(df_insolv2) %>% distinct()

# Matching results
#load(file.path(getwd(), '02_Data', '08_Matched', 'df_match_nn_rel.RData'))


# DROP
df_drop <- read_delim(file = file.path(getwd(), '02_Data', '03_SanityChecks', 'df_drop.txt'), delim = '\t')
df_drop <- df_drop[1:13,]


# Drop observations for which we do not the size
#df_panel <- df_panel %>% 
#  filter(!is.na(csize_fct))


# Clean Insolvencies ------------------------------------------------------


## Descriptives ===========================================================



# For some observations treatment assignment needs to be adjusted 
# (since treat_final variable [skript: step5_cleaning1.R] suffers some minor inconsistency)
#df_panel <- df_panel %>% 
#  mutate(treat_final = ifelse(rechdat >= "2020-04-01", 1, 0))

# Some information concerning variables
nrow(df_insolv)
df_insolv %>% tab(response)
df_insolv %>% tab(insolv)
df_insolv %>% tab(exitdat)

# Clean date format
df_insolv <- df_insolv %>% 
  #mutate(exitdat = dmy(exitdat)) %>%  # +1
  mutate(exitdat = dmy(exitdat)) %>% 
  mutate(exitdat = purrr::pmap(list(exitdat), function(x) clean_exitdat(x))) %>% 
  unnest(exitdat) %>% 
  select(crefo, insolv, exitdat, insolv_text)

# Extract first date of insolvency process
df_insolv <- df_insolv %>% 
  mutate(exitdat.x2 = purrr::pmap(list(insolv_text), function(x) extract_firstdate(x))) %>% 
  unnest(exitdat.x2) %>% 
  #mutate(exitdat.x2 = exitdat.x2) # +1 
  mutate(exitdat.x2 = purrr::pmap(list(exitdat.x2), function(x) clean_exitdat(x))) %>% 
  unnest(exitdat.x2)

# Are there insolvent firms w/o exitdat info?
df_insolv %>% 
  filter(insolv==1) %>% 
  filter(is.na(exitdat))
# There are none!


# How is the year distribution of newly scraped insolvency infos
df_insolv <- df_insolv %>% mutate(exit_year = year(exitdat))
df_insolv %>% tab(exit_year)
# Majority insolvent in 2020 (as expected)

# Do we have several insolvency dates for those filings prior to 2020?
df_temp <- df_insolv %>% filter(exit_year==2015)
# No, it does not seem to be the case! Apparently the latest event in the insolvency process refers to exitdat



# Merge df_insolv and df_panel
df_temp <- df_insolv %>% 
  left_join(df_panel, by = 'crefo')

# Correct MUP exitdat to start of month
#df_temp <- df_temp %>% mutate(exitdat.y = dmy(paste('01', month(exitdat.y), year(exitdat.y), sep = "-")))
# Calculate gap between firmenwissen exitdat and MUP exitdat
df_temp <- df_temp %>% mutate(date_gap = as.numeric(exitdat.x-exitdat.y))
df_temp <- df_temp %>% mutate(date_gap2 = as.numeric(exitdat.x2-exitdat.y))

# Look at distribution of time gap
df_temp %>% tab(date_gap) %>% arrange(date_gap)
df_temp %>% tab(date_gap2) %>% arrange(date_gap2)
df_temp %>% 
  select(date_gap, date_gap2) %>% 
  pivot_longer(cols = c(date_gap, date_gap2)) %>% 
  ggplot() + geom_histogram(aes(x=value)) +
  facet_wrap(.~name)
# As expected time gap is positive since exitdat.x refers to the latest event in the insolvency process
# Continue working with exitdat.x2

# Look at outliers
df_temp %>% 
  filter(date_gap2>1000)
# Wrong assignment of treat_final

df_temp %>% 
  filter(date_gap2 < -1000)
# several insolvency processes

df_temp <- df_temp %>% 
  select(-c(exitdat.x, insolv_text, date_gap)) %>% 
  select(crefo, insolv.x, exitdat.x2, exit_year, date_gap2, everything()) %>% 
  rename(c(exitdat.x = exitdat.x2,date_gap = date_gap2))

df_temp %>% filter(is.na(insolv.x)) %>% distinct(crefo, .keep_all = TRUE) %>% tab(csize_fct)
df_temp %>% filter(is.na(insolv.x)) %>% distinct(crefo, .keep_all = TRUE) %>% tab(wz12_fct)

## Step 1: Insolvency Date prior to 2020-04-01 ============================
df_temp %>% filter(exitdat.x<"2020-04-01") %>% distinct(crefo, .keep_all = TRUE) 
df_temp %>% filter(exitdat.x<"2020-04-01") %>% distinct(crefo, .keep_all = TRUE) %>% filter(!is.na(exitdat.y))
# This the case for 762 firms (4 of these have another exitdat according to MUP)
df_temp1 <- df_temp %>% filter(exitdat.x<"2020-04-01")

df_temp1_final <- df_temp %>% 
  filter(exitdat.x<"2020-04-01") %>% 
  mutate(exitdat = exitdat.x) %>% 
  select(crefo, rechdat, exitdat, everything()) %>% 
  mutate(d_exitdat = as.numeric(exitdat - rechdat)) %>% 
  mutate(insolv = ifelse((0 <= d_exitdat) & (d_exitdat <= 360/3), 1, 0),
         exitdat = ymd(ifelse(insolv == 1, as.character(exitdat), NA))) %>% 
  select(crefo, rechdat, exitdat, d_exitdat, insolv, p_bonitaet, ch_bonitaet, everything()) %>% 
  filter(d_exitdat>=0) %>% 
  select(colnames(df_panel))

df_temp1_final %>% select(crefo, insolv, rechdat, exitdat, d_exitdat) %>% filter(insolv==1)



## Step 2: Insolvency Date after 2020-04-01 ===============================
df_temp %>% filter(exitdat.x>="2020-04-01" | exitdat.y>="2020-04-01") %>% filter(!(crefo %in% df_temp1$crefo)) %>% distinct(crefo, .keep_all = TRUE) 
df_temp %>% filter(exitdat.x>="2020-04-01"| exitdat.y>="2020-04-01") %>% filter(!(crefo %in% df_temp1$crefo)) %>% distinct(crefo, .keep_all = TRUE) %>% 
  mutate(exitdat.x = as.character(paste(month(exitdat.x), year(exitdat.x), sep = "-")),
         exitdat.y = as.character(exitdat.y)) %>% tab(exitdat.x) 

df_temp2 <- df_temp %>% filter(exitdat.x>="2020-04-01" | exitdat.y>="2020-04-01") %>% filter(!(crefo %in% df_temp1$crefo))


df_temp2_final <- df_temp %>% 
  filter(exitdat.x>="2020-04-01" | exitdat.y>="2020-04-01") %>% 
  filter(!(crefo %in% df_temp1$crefo)) %>% 
  mutate(exitdat = ymd(ifelse(is.na(exitdat.x), as.character(exitdat.y), as.character(exitdat.x)))) %>% 
  mutate(d_exitdat = as.numeric(exitdat - rechdat)) %>%
  select(crefo, rechdat, exitdat, d_exitdat, everything()) %>%  
  mutate(insolv = ifelse(0 <= d_exitdat & d_exitdat <= 360/3, 1, 0),
         exitdat = ymd(ifelse(insolv == 1, as.character(exitdat), NA))) %>% 
  select(crefo, rechdat, exitdat, d_exitdat, insolv, p_bonitaet, ch_bonitaet, everything()) %>% 
  filter(d_exitdat>=0) %>%
  select(colnames(df_panel))

df_temp2_final  %>% select(crefo, insolv, rechdat, exitdat, d_exitdat) %>% filter(insolv==1)

# Check: Have we captured all insolvencies?
id1 <- df_temp %>% filter((insolv.x==1) | (insolv.y==1)) %>% distinct(crefo) %>% as_vector()
id2 <- unique(as_vector(c(df_temp1$crefo, df_temp2$crefo)))
setdiff(id1, id2); setdiff(id2, id1)
# Yes they are captured

## Step 3: No Insolvency ==================================================
id_crefo1 <- df_temp %>% filter((is.na(exitdat.x)) & (is.na(exitdat.y))) %>% 
  filter(!(crefo %in% df_temp1$crefo)) %>% 
  #filter(!(crefo %in% df_temp2$crefo)) %>% 
  select(crefo) %>% as_vector()

id_crefo2 <- df_temp %>% filter((is.na(exitdat.x)) & (is.na(exitdat.y))) %>% 
  filter(!(crefo %in% df_temp1$crefo)) %>% 
  filter(!(crefo %in% df_temp2$crefo)) %>% 
  select(crefo) %>% as_vector()

id_crefo <- setdiff(id_crefo1, id_crefo2)

df_temp3_final <- df_temp %>% filter((is.na(exitdat.x)) & (is.na(exitdat.y))) %>% 
  filter(!(crefo %in% df_temp1$crefo)) %>% 
  filter(!(crefo %in% df_temp2$crefo)) %>% 
  mutate(exitdat = NA,
         insolv = 0,
         d_exitdat = NA) %>% 
  select(colnames(df_panel))
  





# Clean df_panel ----------------------------------------------------------
# Check if all crefos have been captured
id1 <- unique(as_vector(df_temp$crefo))
id2 <- unique(as_vector(c(df_temp1$crefo, df_temp2$crefo, df_temp3_final$crefo)))
setdiff(id1, id2); setdiff(id2, id1)
# Yes they are captured

  
df_panel <- 
  df_panel %>% 
  mutate(d_exitdat = -d_exitdat) %>% # Clean d_exitdat by reversing sign (makes more sense this way)
  filter(!(crefo %in% df_temp$crefo)) %>% 
  bind_rows(df_temp1_final) %>% 
  bind_rows(df_temp2_final) %>% 
  bind_rows(df_temp3_final) %>% 
  mutate(treat_final = ifelse(rechdat >= "2020-04-01", 1, 0)) %>% 
  mutate(insolv_fct = factor(ifelse(insolv==0, 'Non-insolvent', 'Insolvent'), 
                             levels = c('Non-insolvent', 'Insolvent')),
         treat_fct = factor(ifelse(treat_final==0, 'Pre-suspension period', 'Suspension period'),
                            levels = c('Pre-suspension period', 'Suspension period')))
  
    



# Some Sanity Checks ------------------------------------------------------

table(df_panel$treat_final, df_panel$insolv)
# 15,595 insolvency filings before crisis, 1,231 insolvencies in crisis 
# 15,436 insolvency filings before crisis, 2,303 insolvencies in crisis 
# 16,501 insolvency filings before crisis, 2,548 insolvencies in crisis 

summary(df_panel$rechdat)
summary(df_panel[df_panel$insolv==1,]$rechdat)
summary(df_panel[df_panel$insolv==0,]$rechdat)
summary(df_panel[df_panel$treat_final==0,]$rechdat)
summary(df_panel[df_panel$treat_final==1,]$rechdat)
# The earliest observations are coming from July of 2017 if not insolvent
# If insolvent observation can be prior to July 2017. Observations far in the past, will be treated as
# non-insovent later on and thus be dropped.
# Drop insolvencies with rechdat in hybrid period
id1 <- df_panel %>% filter(rechdat>"2019-12-31" & rechdat < "2020-04-01") %>% select(crefo) %>% as_vector()
df_panel <- df_panel %>% filter(!((crefo %in% id1) & (insolv==1)))
#df_panel <- df_panel %>% filter(!((crefo == 5190664659) & (rechdat=="2020-01-10"))) #only relevanf for V2

summary(df_panel$exitdat)
# The earliest insolvencies are observed in July 2017 and the latest in Novenber 2020

summary(df_panel$updays)
# Only consider observations where time between two updates amounts at most 3 years
# Drop weird observation
#df_panel <- df_panel %>% 
#  filter(!(crefo==2310336354))

summary(df_panel[df_panel$insolv==1,]$d_exitdat)
# Only insolvencies with at most 4 month lag to closest rechdat should be considered -> at least rechdat must preceed exitdat

df_panel %>% group_by(crefo) %>% filter(any(insolv==1)) %>% mutate(check = insolv - lag(insolv)) %>%  ungroup() %>% tab(check)
# After insolvencies no more observations follow (given check never equals -1)



df_temp <- insolv_ym(df_panel)
plot_insolv(df_panel)

# Save Data ---------------------------------------------------------------

# Save cleaned panel data
#save(df_panel, file = file.path(getwd(), '02_Data', '01_PanelData', 'df_panel_clean.RData'))
write_delim(df_panel, path = file.path(getwd(), '02_Data', '01_PanelData', 'df_panel_clean_final_V1.txt'), delim = '\t')

#df_temp <- df_panel


# Bad Ratings prior to Crisis  --------------------------------------------
## Descriptives ===========================================================

df_insolv2 <- read_delim(file = file.path(getwd(), '02_Data', '09_Insolvency', '02_BadRating', 'df_insolv.txt'), 
                        delim = '\t') %>% 
  rename('crefo' = 'X1')

# Some information concerning variables
nrow(df_insolv2)
df_insolv2 %>% tab(response)
df_insolv2 %>% tab(insolv)
df_insolv2 %>% tab(exitdat)

# Clean date format
df_insolv2 <- df_insolv2 %>% 
  mutate(exitdat = dmy(exitdat)) %>%  # +1
  #mutate(exitdat = dmy(exitdat)) %>% 
  #mutate(exitdat = purrr::pmap(list(exitdat), function(x) clean_exitdat(x))) %>% 
  #unnest(exitdat) %>% 
  select(crefo, insolv, exitdat, insolv_text)

# Extract first date of insolvency process
df_insolv2 <- df_insolv2 %>% 
  mutate(exitdat.x2 = purrr::pmap(list(insolv_text), function(x) extract_firstdate(x))) %>% 
  unnest(exitdat.x2) %>% 
  mutate(exitdat.x2 = exitdat.x2) # +1 
#mutate(exitdat.x2 = purrr::pmap(list(exitdat.x2), function(x) clean_exitdat(x))) %>% 
#unnest(exitdat.x2)

# Are there insolvent firms w/o exitdat info?
df_insolv2 %>% 
  filter(insolv==1) %>% 
  filter(is.na(exitdat))
# There are none!  


# How is the year distribution of newly scraped insolvency infos
df_insolv2 <- df_insolv2 %>% mutate(exit_year = year(exitdat))
df_insolv2 %>% tab(exit_year)
df_insolv2 <- df_insolv2 %>% mutate(exit_year = year(exitdat.x2))
df_insolv2 %>% tab(exit_year)
# Majority prior to 2020 (as expected)

# Merge df_insolv and df_panel
df_temp <- df_insolv2 %>% 
  left_join(df_panel, by = 'crefo')

df_temp <- df_temp %>% 
  select(-c(exitdat.x, insolv_text)) %>% 
  select(crefo, insolv.x, exitdat.x2, exit_year, everything()) %>% 
  rename(c(exitdat.x = exitdat.x2))



## Step 1: Insolvency Date after 2020-04-01 ===============================
df_temp %>% filter(exitdat.x>="2020-04-01") %>% distinct(crefo, .keep_all = TRUE) 
df_temp %>% filter(exitdat.x>="2020-04-01") %>% distinct(crefo, .keep_all = TRUE) %>% 
  mutate(exitdat.x = as.character(paste(month(exitdat.x), year(exitdat.x), sep = "-"))) %>% tab(exitdat.x) 

df_temp1 <- df_temp %>% filter(exitdat.x>="2020-04-01")


df_temp1_final <- df_temp %>% 
  filter(exitdat.x>="2020-04-01") %>% filter(!is.na(exitdat.y))
  mutate(exitdat = ymd(ifelse(is.na(exitdat.x), as.character(exitdat.y), as.character(exitdat.x)))) %>% 
  mutate(d_exitdat = as.numeric(exitdat - rechdat)) %>%
  select(crefo, rechdat, exitdat, d_exitdat, everything()) %>%  
  mutate(insolv = ifelse(0 <= d_exitdat & d_exitdat <= 360/3, 1, 0),
         exitdat = ymd(ifelse(insolv == 1, as.character(exitdat), NA))) %>% 
  select(crefo, rechdat, exitdat, d_exitdat, insolv, p_bonitaet, ch_bonitaet, everything()) %>% 
  filter(d_exitdat>=0) %>%
  select(colnames(df_panel))

df_temp2_final %>% select(crefo, insolv, exitdat, d_exitdat) 

# Check: Have we captured all insolvencies?
id1 <- df_temp %>% filter((insolv.x==1) | (insolv.y==1)) %>% distinct(crefo) %>% as_vector()
id2 <- unique(as_vector(c(df_temp1$crefo, df_temp2$crefo)))
setdiff(id1, id2); setdiff(id2, id1)
# Yes they are captured