# MISSING VLAUES
setwd("Q:\\Meine Bibliotheken\\Research\\SME_Corona")
source(file.path(getwd(), '03_Code', '02_R', 'step0_setup.R'))

# -------------------------------------------------------------------------
# #########################################################################
# -------------------------------------------------------------------------


# Load Appended & Cleaned Panel -------------------------------------------

df_panel <- read_delim(file = file.path(getwd(), '02_Data', '01_PanelData', 'df_panel_2016_2020_clean_incl_ins.txt'),
                       delim = '\t')

# DROP
df_drop <- read_delim(file = file.path(getwd(), '02_Data', '03_SanityChecks', 'df_drop.txt'), delim = '\t')
df_drop <- df_drop[1:10,]

# Drop Irrelevant Variables -----------------------------------------------
df_panel <- df_panel %>% 
  select(-c(update, faulig, giftig, tot))

m1 <- sapply(df_panel, function(x) sum(is.na(x)))
m1


## Add Bonitaet Information Prior to Most Recent Update ####################

df_boni <- read_delim(file = file.path(getwd(), '02_Data', '01_PanelData', 'df_boni_3year.txt'),
                       delim = '\t')

# Fill missings
df_boni <- df_boni %>% 
  group_by(crefo) %>% 
  mutate(
    avg_bonitaet = ifelse(is.nan(avg_bonitaet), NA, avg_bonitaet)
  ) %>% 
  fill(avg_bonitaet, .direction = 'downup') %>%
  fill(n_downgrade, .direction = 'downup') %>% 
  ungroup()
  
  
df_panel <- df_panel %>% 
  left_join(df_boni, by = c('crefo', 'jahr_rd' = 'jahr'))


df_panel %>% filter(is.na(avg_bonitaet))

df_boni %>% filter(crefo==2010001919)

## Missings in grddat =====================================================
df_panel <- df_panel %>% 
  group_by(crefo) %>% 
  mutate(grddat = fmode(grddat, ties = 'max')) %>% 
  ungroup()

df_panel <- df_panel %>% 
  mutate(gryear = ifelse(is.na(gryear), year(grddat), gryear))

m <- sapply(df_panel, function(x) sum(is.na(x)))
m

# Improvement by imputation
(m1-m)/m1

# Impute by erstdat
df_dat <- read_dta("K:\\MUP\\Paneldaten\\Daten\\Original\\daten_w58.dta", encoding = 'UTF-8')
df_temp <- df_dat %>% 
  mutate(grddat_nchar = nchar(gruenddat)) %>% 
  mutate(grddat = ifelse(grddat_nchar!=8, NA, gruenddat)) %>% 
  mutate(grddat = ifelse(str_sub(grddat, start = -4) == '0000', paste0(str_sub(grddat, 1, 4), '0101'), grddat),
         grddat = ifelse(str_sub(grddat, start = -2) == '00', paste0(str_sub(grddat, 1, 6), '01'), grddat)) %>% 
  mutate(grddat = ymd(grddat),
         erstdat = ymd(erstdat)) %>% 
  select(crefo, grddat, erstdat, grddat_nchar)

df_temp <- df_temp %>% 
  mutate(grddat = if_else(is.na(grddat), erstdat, grddat))

sum(is.na(df_temp$grddat))
# No more missings with imputation by erstdat

# Conduct imputation
df_panel <- df_panel %>% 
  coalesce_join(df_temp[,c('crefo', 'grddat')], by = c('crefo'), join = dplyr::left_join) %>% 
  mutate(gryear = ifelse(is.na(gryear), year(grddat), gryear))


m <- sapply(df_panel, function(x) sum(is.na(x)))
m

# Improvement by imputation
(m1-m)/m1



## Missing in wz ==========================================================
# Make number of digits in wz consistent across panel years

df_wz <- read_dta(file.path(getwd(), '02_Data', '01_PanelData', 'df_sample_crefo_industrycodes.dta'))

map_branche <- stack(attr(df_wz$branche, 'labels')) %>% 
  mutate(ind_en = c('Other', 'Manufacturing (leading technology)', 'Manufacturing (technology intensive)', 'Manufacturing (non-technological)',
                    'Business-related services (technology intensive)', 'Business-related services (knowledge intensive)', 'Business-related services (non-technological)',
                    'Consumer-related services', 'Energy', 'Construction', 'Trade', 'Traffic', 'Postal Service', 'Insurance & Banking',
                    'Consulting'))

map_branche_kantar <- stack(attr(df_wz$bran_kantar, 'labels')) %>% 
  mutate(ind_en = c('Others', 'Mechanical engineering', 'Manufacturing of chemical or pharmaceutical products', 'Manufacturing of data processing equipment', 
                    'Food production', 'Other manufacturing industries', 'Wholesale and retail trade (incl. car repair)', 'Accommodation & catering',
                    'Insurance & banking', 'Logistics & transport (incl. postal service)', 'Creative industry & entertainment', 'Other business-related services',
                    'Health and social services'))

map_industry <- stack(attr(df_wz$branche_corona, 'labels')) %>% 
  mutate(ind_en = c('Other', 'Business-related services', 'Consumer-related services', 'Trade', 'Manufacturing/construction'))

df_wz <- df_wz %>% 
  mutate(#branche = factor(branche, labels = map_branche$ind_en),
         branche_corona = factor(branche_corona, labels = map_industry$ind_en),
         bran_kantar = factor(bran_kantar, labels = map_branche_kantar$ind_en)) %>% 
  rename(wz12_fct = bran_kantar, wz5_fct = branche_corona) %>% 
  select(crefo, wzdig5, wz12_fct, wz5_fct, cri, agriculture, energy, public)

df_panel <- df_panel %>% 
  coalesce_join(df_wz, by = 'crefo')

m <- sapply(df_panel, function(x) sum(is.na(x)))
m

# Improvement by imputation
(m1-m)/m1

## Missings in refo =======================================================
df_panel <- df_panel %>% 
  group_by(crefo) %>% 
  mutate(refo = fmode(refo, ties = 'max')) %>% 
  ungroup()

m <- sapply(df_panel, function(x) sum(is.na(x)))
m

# Improvement by imputation
(m1-m)/m1


## Missing in kreis =======================================================
df_panel <- df_panel %>% 
  group_by(crefo) %>% 
  mutate(kreis = fmode(kreis, ties = 'max')) %>% 
  ungroup()

m <- sapply(df_panel, function(x) sum(is.na(x)))
m

# Improvement by imputation
(m1-m)/m1


## Missing in hrnummer ====================================================
df_panel <- df_panel %>% 
  group_by(crefo) %>% 
  mutate(hrnummer = fmode(hrnummer, ties = 'max')) %>% 
  ungroup()

m <- sapply(df_panel, function(x) sum(is.na(x)))
m

# Improvement by imputation
(m1-m)/m1

# Info in panel
df_w58 <- read_delim("Q:\\Meine Bibliotheken\\Research\\SME_Corona\\02_Data\\02_Waves\\vvc58.txt", delim = '\t')
df_panel <- df_panel %>% 
  coalesce_join(df_w58 %>% filter(welle==58) %>% select(crefo, hrnummer), by = 'crefo')

m <- sapply(df_panel, function(x) sum(is.na(x)))
m

# Improvement by imputation
(m1-m)/m1

# Info in hr file
load(file = file.path(getwd(), '02_Data', '03_SanityChecks', 'df_hr.RData'))
df_panel <- df_panel %>% 
  coalesce_join(df_hr, by = 'crefo')

m <- sapply(df_panel, function(x) sum(is.na(x)))
m

# Improvement by imputation
(m1-m)/m1

## Missing in anzma, umsatz ===============================================

### Imputation by Extrapolation ###########################################


df_panel <- df_panel %>% 
  group_by(crefo) %>% 
  mutate(anzma = floor(na.approx(anzma, maxgap = 2, rule = 2, na.rm = FALSE)),
         umsatz = floor(na.approx(umsatz, maxgap = 2, rule = 2, na.rm = FALSE))) %>%
  ungroup()


df_panel <- df_panel %>% 
  group_by(crefo) %>% 
  fill(anzma, .direction = 'downup') %>% 
  fill(umsatz, .direction = 'downup') %>% 
  ungroup()

m <- sapply(df_panel, function(x) sum(is.na(x)))
m

# Improvement by imputation
(m1-m)/m1



### Imputation by ORBIS ###################################################

# 1
# Get info from ORBIS
df_bvdid <- df_panel %>% 
  select(crefo, rechdat, anzma, umsatz, stamm, bilanz) %>% 
  filter(is.na(anzma) | is.na(umsatz)) %>% 
  select(crefo) %>% 
  distinct() %>% 
  mutate(BvD_ID = str_c('DE', crefo)) 

#write_delim(df_bvdid, path = file.path('Q:\\Meine Bibliotheken\\Research\\SME_Corona\\02_Data\\06_Orbis\\df_bvdid.txt'), delim = '\t')

# Impute Orbis info
df_orbis <- read_csv(file.path(getwd(), '02_Data', '06_Orbis', 'result.csv'), na = c('NULL'))

df_bvdid <- df_bvdid %>% 
  rename(BVD_ID = BvD_ID)

df_orbis <- df_orbis %>% 
  left_join(df_bvdid, by = 'BVD_ID') %>% 
  select(crefo, everything()) %>% 
  select(-c(BVD_ID)) %>% 
  mutate(jahr_rd = year(CLOSDATE))

df_orbis <- df_orbis %>% 
  group_by(crefo) %>% 
  mutate(anzma = floor(na.approx(EMPL, maxgap = 2, rule = 2, na.rm = FALSE)),
         umsatz = floor(na.approx(OPRE, maxgap = 2, rule = 2, na.rm = FALSE))) %>%
  ungroup()

df_orbis <- df_orbis %>% 
  group_by(crefo) %>% 
  fill(anzma, .direction = 'downup') %>% 
  fill(umsatz, .direction = 'downup') %>% 
  ungroup() %>% 
  select(crefo, jahr_rd, anzma, umsatz) %>% 
  distinct()

df_orbis <- df_orbis %>% 
  group_by(crefo, jahr_rd) %>% 
  summarise(anzma = floor(mean(anzma)),
            umsatz = floor(mean(umsatz))) %>% 
  ungroup()

# Impute in panel if missing in the same year
df_panel <- df_panel %>% 
  coalesce_join(df_orbis, by = c('crefo', 'jahr_rd'))

# Impute with last observation from Orbis
df_orbis_last <- df_orbis %>% 
  group_by(crefo) %>% 
  filter(row_number() == n()) %>% 
  ungroup() %>% 
  select(-c(jahr_rd))


df_panel <- df_panel %>% 
  coalesce_join(df_orbis_last, by = c('crefo'))

m <- sapply(df_panel, function(x) sum(is.na(x)))
m

# Improvement by imputation
(m1-m)/m1


# 2

df_bvdid <- read_delim(file.path(getwd(), '02_Data', '06_Orbis', 'df_bvdid2.txt'), delim = '\t')
df_orbis <- read_csv(file.path(getwd(), '02_Data', '06_Orbis', 'result2.csv'), na = c('NULL'))

df_orbis <- df_orbis %>% 
  rename(c(BvD_ID =BVDID, umsatz = turnover, anzma = EMPL)) %>% 
  left_join(df_bvdid, by = 'BvD_ID') %>% 
  mutate(jahr = year(CLOSDATE)) %>% 
  select(crefo, jahr, anzma, umsatz, totalassets) %>% 
  mutate(csize_fct = factor(case_when(
    (anzma <= 10) | (umsatz <= 2000000) | (totalassets <= 2000000) ~ 'Micro-enterprise',
    ((anzma > 10) & (anzma < 50)) | ((umsatz > 2000000) & (umsatz <= 10000000)) | ((totalassets > 2000000) & (totalassets <= 10000000)) ~ 'Small enterprise', 
    ((anzma >= 50) & (anzma < 250)) | ((umsatz > 10000000) & (umsatz <= 50000000)) | ((totalassets > 10000000) & (totalassets <= 43000000)) ~ 'Medium-sized enterprise', 
    (anzma >= 250) | (umsatz > 50000000) | (totalassets > 43000000) ~ 'Large enterprise'
  ), levels = c('Large enterprise', 'Medium-sized enterprise', 'Small enterprise', 'Micro-enterprise')))

# Only choose latest information available
df_orbis <- df_orbis %>% 
  filter(!((crefo == 7270000355) & (is.na(anzma)))) %>% # select uniplast in Dettingen and not its duplicate
  filter(!((crefo == 8210040928) & (is.na(anzma)))) %>%  # select Waldner in Wangen and not its duplicate
  filter(!(crefo == 8150005377)) %>% # drop unclear duplicate
  as_tsibble(key = 'crefo', index = 'jahr') %>% 
  as_tibble() %>% 
  group_by(crefo) %>% 
  filter(row_number() == n())

df_panel %>% tab(csize_fct)

df_panel <- df_panel %>% 
  coalesce_join(df_orbis %>% select(-c(jahr)), by = c('crefo'))

df_panel %>% tab(csize_fct)

### Imputation by Older MUP Information ###################################
#load(file = file.path(getwd(), '02_Data', '01_PanelData','df_panel_clean_miss_temp.RData'))

df <- read_delim(file = "C:\\Users\\JDO\\Desktop\\df_panel2010_2019.txt", delim = '\t')



df_panel <- df_panel %>% 
  coalesce_join(df %>% select(crefo, jahr, anzma, umsatz, hkredit), by = c('crefo', 'jahr'))

m <- sapply(df_panel, function(x) sum(is.na(x)))
m

df_miss <- df_panel %>% 
  filter(is.na(anzma)|is.na(umsatz))

id <- df_miss %>% 
  select(crefo) %>% 
  distinct() %>% as_vector

df_miss_panel <- df %>% 
  filter(crefo %in% id) %>% 
  select(crefo, jahr, anzma, umsatz)

# Imputation
df_miss_panel <- df_miss_panel %>% 
  group_by(crefo) %>% 
  mutate(anzma = floor(na.approx(anzma, maxgap = 3, rule = 2, na.rm = FALSE)),
         umsatz = floor(na.approx(umsatz, maxgap = 3, rule = 2, na.rm = FALSE))) %>%
  ungroup()

df_miss_panel <- df_miss_panel %>% 
  group_by(crefo) %>% 
  fill(anzma, .direction = 'downup') %>% 
  fill(umsatz, .direction = 'downup') %>% 
  ungroup()

# Impute exact by crefo and jahr
df_panel <- df_panel %>% 
  coalesce_join(df_miss_panel, by = c('crefo', 'jahr'))

m <- sapply(df_panel, function(x) sum(is.na(x)))
m

# Impute with last available info
df_miss_panel2 <- df_miss_panel %>% 
  group_by(crefo) %>% 
  filter(row_number() == n()) %>% 
  ungroup() %>% 
  filter(!(is.na(anzma) & is.na(umsatz)))

df_miss_panel2 %>% tab(jahr)

df_miss_panel2 <- df_miss_panel2 %>% 
  select(-c(jahr))

df_panel <- df_panel %>% 
  coalesce_join(df_miss_panel2, by = c('crefo'))

m <- sapply(df_panel, function(x) sum(is.na(x)))
m


### Recalculate Changes ###################################################
#load(file = file.path(getwd(), '02_Data', '01_PanelData','df_panel_clean_miss_temp2.RData'))


df_panel <- df_panel %>% 
  group_by(crefo) %>% 
  mutate(ch_anzma = if_else(is.na(ch_anzma), anzma - lag(anzma), ch_anzma),
         ch_umsatz = if_else(is.na(ch_umsatz), umsatz - lag(umsatz), ch_umsatz)) %>%
  ungroup()

  
m <- sapply(df_panel, function(x) sum(is.na(x)))
m

# Improvement by imputation
(m1-m)/m1








# Improvements ------------------------------------------------------------
df_imp <- (m1-m)/m1
df_imp <- tibble(var = names(df_imp), imp = df_imp)
df_imp %>% arrange(desc(imp))



# Adding Variables --------------------------------------------------------
## How many times have observations been observed =========================
df_panel <- df_panel %>% 
  group_by(crefo) %>% 
  mutate(count = n()) %>% 
  ungroup()


crosstab(dep = df_panel$insolv,
         indep = df_panel$count,
         prop.r = TRUE,
         drop.levels = TRUE,
         plot = TRUE)


# Some Sanity Checks ------------------------------------------------------
summary(df_panel$rechdat)
summary(df_panel[df_panel$insolv==1,]$rechdat)
summary(df_panel[df_panel$insolv==0,]$rechdat)
# The earliest observations are coming from July of 2017 if not insolvent
# If insolvent observation can be prior to July 2017. Observations far in the past, will be treated as
# non-insovent later on and thus be dropped.

summary(df_panel$exitdat)
# The earliest insolvencies are observed in July 2017

summary(df_panel$updays)
# Only consider observations where time between two updates amounts at most 3 years

summary(df_panel[df_panel$insolv==1,]$d_exitdat)
# Only insolvencies with at most 4 month lag to closest rechdat should be considered -> at least rechdat must preceed exitdat


# Select final variables --------------------------------------------------

colnames(df_panel)
df_panel <- df_panel %>% 
  select(crefo, jahr_rd, treat_final, insolv, p_bonitaet, ch_bonitaet, bonitaet, 
         anzma, ch_anzma, umsatz, ch_umsatz, wz, refo, gryear, kreis, 
         rechdat, exitdat, grddat, d_exitdat, exit, auftrag, p_auftrag, entwick, p_entwick, update, updays, count, jahr,
         stamm, kurteil, zweise, hkredit, bilanz , hrnummer)


crosstab(dep = df_panel$treat_final,
         indep = df_panel$insolv,
         #weight =
         prop.r = TRUE,
         drop.levels = TRUE,
         plot = FALSE)




# Choose Correct Observation if two Observations closely preceede ---------
df_temp <- df_panel2 %>% 
  mutate(d_exitdat = -d_exitdat) %>% 
  group_by(crefo) %>% 
  filter(any(insolv==1)) %>% 
  filter(all(d_exitdat<120)) %>% 
  ungroup() %>%  
  filter(count>1) %>% 
  select(crefo, treat_final, rechdat, exitdat, d_exitdat, insolv, boni_orig, p_bonitaet, ch_bonitaet, count, everything()) %>%
  arrange(crefo)

# All of these (insolvent) observations have been osberved twice
df_temp %>% tab(count)

# Drop observation with improvement in boni
#df_temp <- df_temp %>% filter(ch_bonitaet>=0)

# Function to assign insolvency to correct date
df_temp <- df_temp %>% group_by(crefo) %>% mutate(insolv = case_when(
  (ch_bonitaet == 0) & all(ch_bonitaet == 0) & (row_number() == min(row_number())) ~ 1,
  (ch_bonitaet == 0) & all(ch_bonitaet == 0) & (row_number() == max(row_number())) ~ 0,
  any(ch_bonitaet == 0) & !(all(ch_bonitaet == 0)) & ch_bonitaet == max(ch_bonitaet) ~ 1,
  any(ch_bonitaet == 0) & !(all(ch_bonitaet == 0)) & ch_bonitaet == min(ch_bonitaet) ~ 0,
  all(ch_bonitaet > 0) & ch_bonitaet == min(ch_bonitaet) ~ 1,
  all(ch_bonitaet > 0) & ch_bonitaet == max(ch_bonitaet) ~ 0,
  any(ch_bonitaet < 0) & ch_bonitaet == max(ch_bonitaet) ~ 1,
  any(ch_bonitaet < 0) & ch_bonitaet == min(ch_bonitaet) ~ 0)
                                       ) %>% 
  fill(exitdat, .direction = "updown") %>% 
  mutate(exitdat = ymd(ifelse(insolv==1, as.character(exitdat), NA))) %>% 
  filter(!(is.na(exitdat) & d_exitdat == min(d_exitdat))) %>% 
  ungroup()

# Now join firm info from df_panel
df_temp_final <- df_temp %>% 
  select(crefo, rechdat, insolv, exitdat) %>% 
  left_join(df_panel %>% select(-c(insolv, exitdat)), by = c("crefo", "rechdat")) %>%  # df_panel <- read_delim(file = file.path(getwd(), '02_Data', '01_PanelData', '01_Backup', 'df_panel_clean_final.txt'), delim = '\t')
  select(colnames(df_panel)) %>% 
  filter(!is.na(ch_bonitaet))

# Sanity Checks
id <- df_panel %>% group_by(crefo) %>% 
  mutate(count = n()) %>%  
  filter(any(insolv==1)) %>% 
  filter(all(d_exitdat<120)) %>% 
  ungroup() %>%  
  filter(count>1) %>% 
  select(crefo, treat_final, rechdat, exitdat, d_exitdat, insolv, boni_orig, p_bonitaet, ch_bonitaet, count) %>% 
  select(crefo)

df_temp_final
df_temp_final %>% filter(!(crefo %in% as_vector(id)))

# Clean df_panel
df_panel <- df_panel %>% 
  filter(!(crefo %in% as_vector(unique(df_temp_final$crefo)))) %>% 
  bind_rows(df_temp_final)
  

# Save Panel --------------------------------------------------------------
write_delim(df_panel, path = file.path(getwd(), '02_Data', '01_PanelData', 'df_panel_clean_wo_miss.txt'), delim = '\t')

# Save dropping list
write_delim(df_drop, path = file.path(getwd(), '02_Data', '03_SanityChecks', 'df_drop.txt'), delim = '\t')
