# CLEANING
setwd("Q:\\Meine Bibliotheken\\Research\\SME_Corona")
source(file.path(getwd(), '03_Code', '02_R', 'step0_setup.R'))

# -------------------------------------------------------------------------
# #########################################################################
# -------------------------------------------------------------------------




# Load Appended Panel -----------------------------------------------------

#load(file = file.path(getwd(), '02_Data', '01_PanelData', 'df_panel_2017_2020_clean.RData'))
df_panel <- read_delim(file = file.path(getwd(), '02_Data', '01_PanelData', 'df_panel_2016_2020_clean.txt'),
                       delim = '\t')



# DROP
df_drop <- read_delim(file = file.path(getwd(), '02_Data', '03_SanityChecks', 'df_drop.txt'), delim = '\t')
df_drop <- df_drop[0,]


dim_s1 <- nrow(df_panel) + 6484347
df_drop <- df_drop %>% 
  bind_rows(tibble(N = dim_s1, Remark = 'Panel observations with at least one update in credit rating in panel years 2016 - 2020 (waves 50, 52, 54, 56, 58)'))


dim_s2 <- nrow(df_panel)
df_drop <- df_drop %>% 
  bind_rows(tibble(N = dim_s2, Remark = 'Drop of observations with missing information regarding their credit rating.'))



# Drop Observations with no Rechdat Information ---------------------------

sapply(df_panel, function(x) sum(is.na(x)))

df_panel <- df_panel %>% 
  filter(!is.na(rechdat))


dim_s3 <- nrow(df_panel)
df_drop <- df_drop %>% 
  bind_rows(tibble(N = dim_s3, Remark = 'Drop of panel observations for which no rechdat information is possible.'))


sapply(df_panel, function(x) sum(is.na(x)))








# Analysis of Insolvencies ------------------------------------------------

## Add Insolvency Information =============================================

# Add exit information (latest wave 54)
df_panel <- df_panel %>% 
  mutate(exit = NA,
         exitdat = NA) %>% 
  mutate(exit = as.double(exit),
         exitdat = as.Date(exitdat))

for (i in seq(58,54,-2)){
  # Define year corresponding to wave
  jahr <- w_y_map[as.character(i)][[1]]
  
  # Read dates dta file
  df_temp <- read_delim(paste0("H:\\Large_Datasets\\SME_Corona\\df_temp", i, ".csv"), 
                        delim = '\t')
  df_temp <- df_temp %>% rename(crefo = X1)
  
  
  # Merge grddat, rechdat, gryear to panel
  df_panel <- df_panel %>% 
    as_tibble() %>% 
    coalesce_join(df_temp, by = c("crefo", "jahr"), join = dplyr::left_join)
  
  print(jahr)
}

# Add Insolvency Variable


df_panel <- df_panel %>% 
  mutate(insolv = ifelse((exit != 2) | is.na(exit), 0, 1))



## Drop Insolvencies Prior to 2017-07-01 ==================================


table(year(df_panel$exitdat))

df_temp <- df_panel %>% 
  filter(exitdat >= '2017-07-01'|is.na(exitdat)) %>% 
  filter(exitdat < '2020-07-31'|is.na(exitdat))

df_panel %>% 
  filter(exitdat<'2017-07-01') %>% 
  select(crefo, jahr, rechdat, bonitaet, exit, exitdat, everything())
# These are insolvencies in the past which remain in the panel

df_panel %>% 
  filter(exitdat>'2020-07-31') %>% 
  select(crefo, jahr, rechdat, bonitaet, exit, exitdat, everything()) %>% 
  tab(exit)
# These are non-existent firms

df_panel <- df_temp

# Sanity check
summary(df_panel$exitdat)
# Possibly one could include insolvencies at 2020-08-01 since these can be considered as insolvencies in July 2020

# Reduction of panel due to past insolvencies
dim_s4 <- nrow(df_panel)
df_drop <- df_drop %>% 
  bind_rows(tibble(N = dim_s4, Remark = 'Reduction of insolvencies with exitdat < 2017-07-01 and exitdat > 2020-07-31.'))

write_delim(df_panel, path = file.path(getwd(), '02_Data', '01_PanelData', 'df_panel_2016_2020_clean.txt'), delim = '\t')

## Adjust Exitdat =========================================================
df_panel <- df_panel %>% 
  mutate(exitdat = purrr::pmap(list(exitdat), function(x) clean_exitdat(x))) %>% 
  unnest(exitdat)


write_delim(df_panel, path = file.path(getwd(), '02_Data', '01_PanelData', 'df_panel_2016_2020_clean.txt'), delim = '\t')

## Assign Insolvency to the Most Suitable Rechdat =========================

# 1) Get data set with all relevant insolvencies and their rating history
df_panel_ins <- df_panel %>% 
  group_by(crefo) %>% 
  filter(any(insolv == 1))


# It seems as if many of the insolvent firms have already had a bonitaet of 500
# when their insolvency happens to be in 2018 or 2017. It is likely that in the data delivery of 2016 (or even earlier)
# they have moved to 500. Let's get this data and check this presumption.
id <- unique(df_panel_ins$crefo)

# Ensure that df_panel is kept as the load command overwrites it!
df_temp3 <- df_panel

load("Q:\\Meine Bibliotheken\\Research\\SME_Corona\\02_Data\\01_PanelData\\01_Backup\\df_panel2010_2020.R")

df_panel_10_20_ins <- df_panel %>% 
  as_tibble() %>% 
  #head(100000) %>% 
  filter(crefo %in% id) %>% 
  select(crefo, jahr, bonitaet, rechdat) %>% 
  left_join(df_panel_ins %>% select(crefo, jahr, exitdat, insolv), by = c('crefo', 'jahr')) %>% 
  mutate(temp=duplicated(.)) %>% 
  filter(!temp) %>% 
  select(-c(temp)) %>% 
  as_tsibble(key = crefo, index = jahr)

#save(df_panel_10_20_ins, file = file.path("Q:\\Meine Bibliotheken\\Research\\SME_Corona\\02_Data\\01_PanelData\\df_panel2010_2020_ins.RData"))
# Add rechdat for previous waves (latest wave 46)
# Read rechdat panel
df_temp <- read_delim(paste0("H:\\Large_Datasets\\SME_Corona\\df_dat_", "all", ".csv"), delim = '\t')
df_temp <- df_temp %>% rename(crefo = X1)

df_panel_10_20_ins <- df_panel_10_20_ins %>% 
  as_tibble() %>% 
  coalesce_join(df_temp, by = c('crefo', 'jahr'))




# 2) Add information from uneven waves. This is necessary as it decreases the days between rechdat an insolvency
df_panel_10_20_ins <- df_panel_10_20_ins %>% as_tibble() %>% 
  filter(jahr >= 2015) %>% 
  mutate(welle = pmap_dbl(list(jahr), function(x) min(as.numeric(names(w_y_map[w_y_map==x]))))) %>% 
  select(crefo, jahr, welle, everything())

for (i in seq(57,47,-2)){
  # Define year corresponding to wave
  jahr <- w_y_map[as.character(i)][[1]]

  df_temp <- read_dta(paste0("K:\\MUP\\Paneldaten\\Daten\\Original\\daten_w", i, ".dta"), 
         encoding = 'UTF-8', col_select = c('crefo', 'welle', 'rechdat'))
  # Make some adjustments in the file
  df_temp <- df_temp %>% 
    mutate(
      rechdat = ymd(rechdat),
      jahr = jahr,
      exitdat = NA,
      insolv = NA
    ) %>% 
    mutate(exitdat = as.Date(exitdat),
           insolv = as.double(insolv))

  df_temp <- df_temp %>% 
    left_join(read_dta(paste0("K:\\MUP\\Paneldaten\\Rating\\Aufbereitet\\rating_w", i, ".dta"), 
                                encoding = 'UTF-8', col_select = c('crefo', 'welle', 'bonitaet')) %>% 
                         mutate(bonitaet = na_if(bonitaet, 0)), 
              by = c('crefo', 'welle')) %>% 
    select(crefo, jahr, welle, bonitaet, rechdat, exitdat, insolv) %>% 
    filter(crefo %in% id)

# Merge grddat, rechdat, gryear to panel
  df_panel_10_20_ins <- df_panel_10_20_ins %>% 
    bind_rows(df_temp)

  print(i)
}

df_panel_10_20_ins <- df_panel_10_20_ins %>% 
  as_tsibble(key = crefo, index = welle)

save(df_panel_10_20_ins, file = file.path("Q:\\Meine Bibliotheken\\Research\\SME_Corona\\02_Data\\01_PanelData\\df_panel2010_2020_ins.RData"))

load(file = file.path("Q:\\Meine Bibliotheken\\Research\\SME_Corona\\02_Data\\01_PanelData\\df_panel2010_2020_ins2.RData"))

# Drop observations without rechdat
df_panel_10_20_ins %>% 
  as_tibble() %>% 
  group_by(crefo) %>% 
  filter(all(is.na(rechdat)))
# No firms drop (only observation of the firm in a specific wave)
df_panel_10_20_ins <- df_panel_10_20_ins %>% 
  as_tibble() %>% 
  filter(!is.na(rechdat))
  


# 3) Impute bonitaet, 
df_panel_10_20_ins <- df_panel_10_20_ins %>% 
  as_tibble() %>% 
  group_by(crefo) %>% 
  #mutate(bonitaet = floor(na.approx(bonitaet, maxgap = 3, rule = 2))) %>% 
  fill(bonitaet, .direction = 'down') %>% 
  fill(exitdat, .direction = 'downup') %>% 
  mutate(
    boni_orig = bonitaet,
    bonitaet = ifelse(bonitaet==0, NA, bonitaet),
    bonitaet = ifelse(bonitaet==600, 500, bonitaet)
  ) %>% 
  mutate(
    updays = as.double(difftime(rechdat, lag(rechdat), units = 'days')),
    boni_update = ifelse(bonitaet == lag(bonitaet), 0, 1),
    update = case_when(
      (updays == 0) & (boni_update == 0) ~ 0,
      (updays > 0) | (boni_update == 1) ~ 1),
    p_bonitaet = ifelse(update == 0, NA, lag(bonitaet))
  ) %>% 
  ungroup() %>% 
  filter(update==1)




# 4) Assign two ch_bonitaet to insolvency observation
# 1. Change in boni PRIOR to insolvency
# 2. Change in boni CLOSEST to (but possibly after) insolvency
# Note: Only 1 of the two can be used further since there is no such distinction for non-insolvent observations!!
# Note: This step is about assigning the insolvency to the right period of observation. So I create 2 insolvs and 2 exitdat variables
#    determine period with maximum in change in bonitaet, 
#    determine period with minimum in time delta between rechdat and exitdat
#    determine period with minimum in time delta conditioned that rechdat is prior to exitdat

maxnegative <- function(x) max(x[x < 0], na.rm = TRUE)

df_panel_10_20_ins <- df_panel_10_20_ins %>% 
  group_by(crefo) %>% 
  #mutate(p_bonitaet = ifelse(p_bonitaet == bonitaet, lag(bonitaet), p_bonitaet)) %>% 
  mutate(ch_bonitaet = bonitaet - p_bonitaet,
         max_ch_bonitaet = ifelse(ch_bonitaet == max(ch_bonitaet, na.rm = TRUE), 1, 0)) %>% 
  mutate(d_exitdat = as.numeric(rechdat - exitdat),
         min_d_exitdat = ifelse(abs(d_exitdat) == min(abs(d_exitdat), na.rm = TRUE), 1, 0),
         min_neg_d_exitdat = ifelse(d_exitdat == maxnegative(d_exitdat), 1, 0)) %>% 
  ungroup()


df_panel_10_20_ins <- df_panel_10_20_ins %>% 
  select(crefo, jahr, welle, rechdat, exitdat, update, d_exitdat, updays, insolv, boni_orig, p_bonitaet, bonitaet, ch_bonitaet, min_d_exitdat, max_ch_bonitaet, min_neg_d_exitdat)

save(df_panel_10_20_ins, file = file.path("Q:\\Meine Bibliotheken\\Research\\SME_Corona\\02_Data\\01_PanelData\\df_panel2010_2020_ins.RData"))

load(file = file.path("Q:\\Meine Bibliotheken\\Research\\SME_Corona\\02_Data\\01_PanelData\\df_panel2010_2020_ins.RData"))

# 5) Check insolvencies without exitdat
df_panel_10_20_ins %>% 
  filter(is.na(exitdat)) %>% 
  group_by(crefo) %>% 
  filter(row_number() == n()) %>% 
  ungroup() %>% 
  select(bonitaet) %>% summary()
# To a large extent these seem to be firms which have cured after their insolvency. Drop these!
id1 <- df_panel_10_20_ins %>% 
  as_tibble() %>% 
  filter(is.na(exitdat)) %>% 
  select(crefo) %>%
  distinct() %>% 
  as_vector()

dim_s5 <- nrow(df_panel) - nrow(df_panel_ins %>% filter((crefo %in% id1)))
df_drop <- df_drop %>% 
  bind_rows(tibble(N = dim_s5, Remark = 'Drop of supposedly insolvencies, however, without exitdat (mostly these seem to be firms which cured after a filing'))


# Clean panel with insolvency information respectively
df_panel_10_20_ins <- df_panel_10_20_ins %>% 
  filter(!(crefo %in% id1))

df_panel_ins <- df_panel_ins %>% 
  filter(!(crefo %in% id1))



# Check if there are still missings in change bonitaet at date at which insolvency is assigned to
df_panel_10_20_ins %>% filter(min_neg_d_exitdat==1) %>%  tab(ch_bonitaet)
# Yes there are still some

# Drop these
id2 <- df_panel_10_20_ins %>% 
  filter(min_neg_d_exitdat == 1) %>% 
  filter(is.na(ch_bonitaet)) %>% 
  select(crefo) %>% as_vector()

dim_s6 <- df_drop$N[5] - nrow(df_panel_ins %>% ungroup() %>% filter((crefo %in% id2)))
df_drop <- df_drop %>% 
  bind_rows(tibble(N = dim_s6, Remark = 'Drop of insolvencies for which no change in boni can be assigned after correction of assignment of exitdat to rechdat'))


# Clean panel with insolvency information respectively
df_panel_10_20_ins <- df_panel_10_20_ins %>% 
  filter(!(crefo %in% id2))

df_panel_ins <- df_panel_ins %>% 
  filter(!(crefo %in% id2))


# 5) Assign insolvencies to right rechdat
df_panel_10_20_ins <- df_panel_10_20_ins %>% 
  mutate(exitdat = if_else(min_neg_d_exitdat==1, exitdat, as.Date(NA)),
         insolv = ifelse(min_neg_d_exitdat==1,1,0)) %>% 
  select(crefo, jahr, welle, rechdat, insolv, exitdat, p_bonitaet, boni_orig, bonitaet, ch_bonitaet, d_exitdat, updays)


# Check if there are different exit dates
df_panel_10_20_ins %>% 
  group_by(crefo) %>% 
  mutate(n_exit = length(unique(na.omit(exitdat)))) %>% 
  ungroup() %>% 
  filter(insolv==1) %>% 
  tab(n_exit)
# There are no observations with two distinct exitdats. Great!

# However there are some with no insolvency any longer (due to data preparation)
df_panel_10_20_ins %>% 
  group_by(crefo) %>% 
  filter(all(insolv==0))
# Drop these
id3 <- df_panel_10_20_ins %>% 
  group_by(crefo) %>% 
  filter(all(insolv==0)) %>%
  select(crefo) %>% 
  unique() %>% 
  as_vector()

dim_s7 <- df_drop$N[6] - nrow(df_panel_ins %>% ungroup() %>% filter((crefo %in% id3)))
df_drop <- df_drop %>% 
  bind_rows(tibble(N = dim_s7, Remark = 'Drop of insolvencies for which exitdat has been lost due to data preparation'))


# Clean panel with insolvency information respectively
df_panel_10_20_ins <- df_panel_10_20_ins %>% 
  filter(!(crefo %in% id3))

df_panel_ins <- df_panel_ins %>% 
  filter(!(crefo %in% id3))

df_panel_10_20_ins <- df_panel_10_20_ins %>% 
  as_tsibble(key = crefo, index = welle)




# 6) Clean double counted insolvencies 
# Happens when d_exitdat is the same for two observations of one firms. This in turn happens when rechdat is the same 
# but the bonitaet assigned to rechdat differs compared to bonitaet in period before. So although rechdat is the same
# there has been conducted an update in bonitaet. Overall, this affects a minority of cases.
df_panel_10_20_ins <- df_panel_10_20_ins %>% 
  as_tibble() %>% 
  group_by(crefo, rechdat, insolv) %>% 
  filter(ch_bonitaet == max(ch_bonitaet)) %>% 
  ungroup() %>% 
  group_by(crefo)

# Some remain since ch_bonitaet is the same: Select the one with highest bonitaet
df_panel_10_20_ins <- df_panel_10_20_ins %>% 
  as_tibble() %>% 
  group_by(crefo, rechdat, insolv, ch_bonitaet) %>% 
  filter(bonitaet == max(bonitaet)) %>% 
  ungroup() %>% 
  group_by(crefo)

# Some remain since bonitaet is the same: Select the one with lowest (=500) boni_orig
df_panel_10_20_ins <- df_panel_10_20_ins %>% 
  as_tibble() %>% 
  group_by(crefo, rechdat, insolv, ch_bonitaet, bonitaet) %>% 
  filter(boni_orig == min(boni_orig)) %>% 
  ungroup() %>% 
  group_by(crefo)

# Some remain since boni_orig is the same: Select the latest
df_panel_10_20_ins <- df_panel_10_20_ins %>%  
  ungroup() %>% 
  arrange(crefo, desc(welle)) %>% 
  distinct(crefo, rechdat, insolv, exitdat, p_bonitaet, boni_orig, bonitaet, ch_bonitaet, d_exitdat, .keep_all=TRUE) %>%
  as_tsibble(key = crefo, index=welle) 



# 7) Drop double years
# First drop non-insolvent double years
df_panel_10_20_ins <- df_panel_10_20_ins %>% 
  as_tibble() %>% 
  group_by(crefo, jahr, insolv) %>% 
  filter(abs(ch_bonitaet) == max(abs(ch_bonitaet))) %>% 
  ungroup()

# Some remain since ch_bonitaet is the same: Select the latest rechdat
df_panel_10_20_ins <- df_panel_10_20_ins %>% 
  as_tibble() %>% 
  group_by(crefo, jahr, insolv, ch_bonitaet) %>% 
  filter(rechdat == max(rechdat)) %>% 
  ungroup() %>% 
  group_by(crefo)

# Some remain since drop and decrease in ch_bonitaet is the same (absolute value becomes obsulete): Select the latest rechdat
df_panel_10_20_ins <- df_panel_10_20_ins %>%  
  ungroup() %>% 
  arrange(crefo, desc(rechdat)) %>% 
  distinct(crefo, jahr, insolv, .keep_all=TRUE) %>%
  as_tsibble(key = crefo, index=welle) 


# Second drop 
df_temp <- df_panel_10_20_ins %>% 
  as_tibble() %>% 
  group_by(crefo, jahr) %>% 
  mutate(n = n()) %>% 
  ungroup() %>% 
  group_by(crefo) %>% 
  filter(n>1) #%>% 
  #filter(d_exitdat<0) # Some observations prior to insolvency but in the same year remain
# For now we drop these!

# Second clean double years with one of the observations being an insolvency
df_panel_10_20_ins <- df_panel_10_20_ins %>% 
  as_tibble() %>% 
  group_by(crefo, jahr) %>% 
  mutate(n = n(),
         n = case_when(n == 1 ~ 1,
                   (n > 1) & is.na(exitdat) ~ 0,
                   (n > 1) & !is.na(exitdat) ~ 1)) %>% 
  ungroup() %>% 
  filter(n==1) %>%
  select(-c(n)) %>% 
  group_by(crefo)


# 8) Recalculate updays
df_panel_10_20_ins <- df_panel_10_20_ins %>% 
  group_by(crefo) %>% 
  mutate(updays2 = as.double(rechdat - lag(rechdat)),
         updays = ifelse(is.na(updays2), updays, updays2)) %>% 
  select(-c(updays2))

df_panel_10_20_ins <- df_panel_10_20_ins %>% 
  ungroup() %>% 
  select(-c(welle))


save(df_panel_10_20_ins, file = file.path("Q:\\Meine Bibliotheken\\Research\\SME_Corona\\02_Data\\01_PanelData\\df_panel2010_2020_ins_final.RData"))

load(file = file.path("Q:\\Meine Bibliotheken\\Research\\SME_Corona\\02_Data\\01_PanelData\\df_panel2010_2020_ins_final.RData"))


# 8) Drop observations after insolvency
df_panel_10_20_ins <- df_panel_10_20_ins %>% 
  group_by(crefo) %>% 
  filter(d_exitdat<0)
  

# 8) Merge df_panel_10_20_ins back to df_panel_ins which is later appended to df_panel
df_panel_ins <- df_panel_10_20_ins %>%
  ungroup() %>% 
  left_join(df_panel_ins %>% select(-c(rechdat, insolv, exitdat, p_bonitaet, boni_orig, bonitaet, ch_bonitaet, updays)),
            by = c("crefo", "jahr"))



## Append Insolvencies Back to Panel ======================================

# Get df_panel back
df_panel <- df_temp3

# Drop insolvencies from df_panel
df_panel <- df_panel %>% 
  filter(!(crefo %in% id))

# Drop firms with exitdat but without insolvency
df_panel %>% 
  filter(!is.na(exitdat)) %>% 
  select(crefo, exit, exitdat, everything()) %>% 
  tab(exit)
# These are firms with exit status 1 (does not exist) and 3 (non-active). Drop these!
df_panel <- df_panel %>% 
  filter(is.na(exitdat))

dim_s8 <- NA
df_drop <- df_drop %>% 
  bind_rows(tibble(N = dim_s8, Remark = 'Drop of observations with exitdat but without insolvency (non-active, non-existent firms).'))


setdiff(colnames(df_panel_ins), colnames(df_panel))

# Add d_exitdat to df_panel
df_panel <- df_panel %>% 
  mutate(d_exitdat = NA,
         d_exitdat = as.double(d_exitdat))

setdiff(colnames(df_panel), colnames(df_panel_ins))


# Adjust column order in df_panel_ins
df_panel_ins <- df_panel_ins %>%
  ungroup() %>% 
  select(colnames(df_panel))

all.equal(sapply(df_panel, function(x) class(x)), sapply(df_panel_ins, function(x) class(x)))
which(sapply(df_panel, function(x) class(x)) != sapply(df_panel_ins, function(x) class(x)))

# APPEND
df_panel <- df_panel %>% 
  bind_rows(df_panel_ins) %>% 
  as_tsibble(key = crefo, index = jahr) %>% 
  as_tibble()


dim_s9 <- nrow(df_panel)
df_drop <- df_drop %>% 
  bind_rows(tibble(N = dim_s9, Remark = 'Increase of insolvent observations since for insolvent firms all waves (and not only years) have been taken into consideration.'))






# Add Year of Observation -------------------------------------------------


df_panel <- df_panel %>% 
  mutate(jahr_rd = year(rechdat)) %>% 
  select(crefo, jahr_rd, everything(), jahr)


# Drop of Observations with Dubious Rechdat -------------------------------


df_panel %>% tab(jahr_rd)

df_panel <- df_panel %>% 
  mutate(keep = case_when(
    (insolv == 0) & (rechdat >= '2017-01-01') & (rechdat <= '2020-07-31') ~ 1,
    insolv == 1 ~ 1,
    (insolv == 0) & (rechdat < '2017-01-01') ~ 0,
    (insolv == 0) & (rechdat > '2020-07-31') ~ 0
  )) %>% 
  filter(keep == 1) %>% 
  select(-c(keep))


# Sanity check
df_panel %>% tab(jahr_rd)

# Reduction of panel due to deletion of observations without update by Creditreform
dim_s10 <- nrow(df_panel)
df_drop <- df_drop %>% 
  bind_rows(tibble(N = dim_s10, Remark = 'Reduction of panel observations with rechdat < 2017-07-01 and rechdat > 2020-07-31.'))



# Create Treatment Variable -----------------------------------------------
# An observation counts as treated if 
# - it is not insolvent and its rechdat is later than or equal to 2020-04-01
# - it is insolvent and the date of insolvency is later than or equal to 2020-04-01 (regardles of the rechdat)

# An observation counts as non-treated if 
# - it is not insolvent and its rechdat is before 2020-04-01
# - it is insolvent and the date of insolvency is before 2020-04-01 (regardles of the rechdat which may even be before 2017-07-01)


df_panel <- df_panel %>% 
  mutate(treat_final = case_when(
    (insolv == 0) & (rechdat < '2020-04-01') ~ 0,
    (insolv == 1) & (exitdat < '2020-04-01') ~ 0,
    (insolv == 0) & (rechdat >= '2020-04-01') ~ 1,
    (insolv == 1) & (exitdat >= '2020-04-01') ~ 1,
  ))


crosstab(dep = df_panel$treat_final,
         indep = df_panel$insolv,
         prop.r = TRUE, 
         drop.levels = TRUE,
         plot = FALSE)


# Save Data ---------------------------------------------------------------

# Save cleaned panel data
write_delim(df_panel, path = file.path(getwd(), '02_Data', '01_PanelData', 'df_panel_2016_2020_clean_incl_ins.txt'), delim = '\t')

# Save dropping list
write_delim(df_drop, path = file.path(getwd(), '02_Data', '03_SanityChecks', 'df_drop.txt'), delim = '\t')

