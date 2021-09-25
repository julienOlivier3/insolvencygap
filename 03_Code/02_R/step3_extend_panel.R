setwd("Q:\\Meine Bibliotheken\\Research\\SME_Corona")
source(file.path(getwd(), '03_Code', '02_R', 'step0_setup.R'))

# -------------------------------------------------------------------------
# #########################################################################
# -------------------------------------------------------------------------

# Setup -------------------------------------------------------------------
# Number of splits
n_split <- 50
  
# Number of subsplits
n_subsplit <- 10


# Panel Manipulations -----------------------------------------------------



# Full panel
#load(file.path(getwd(), '02_Data', '01_PanelData', 'df_panel2010_2020.R'))
# Sample
#load(file.path(getwd(), '02_Data', '01_PanelData', 'df_panel2010_2020_sample.RData')) 

for (k in 1:n_split){
df_panel_sample <- read_delim(file = file.path(getwd(), '02_Data', '01_PanelData', '02_Chunks', '01_Raw', paste0('df_panel', k, '.txt')), delim = '\t') 
  



# Vector of unique crefos
vec_crefo <- unique(df_panel_sample$crefo)

for (j in 1:n_subsplit){
  

id_crefo <- vec_crefo[ntile(vec_crefo, n = n_subsplit)==j]

df_panel <- df_panel_sample %>% 
  as_tibble() %>% 
  filter(crefo %in% id_crefo)





# Recherchedatum ----------------------------------------------------------

# Add rechdat for previous waves (latest wave 46)
for (i in seq(56,50,-2)){
  # Define year corresponding to wave
  jahr <- as.character(i)][[1]]
  
  # Read dates dta file
  #df_temp <- read_dta(paste0("K:\\MUP\\Paneldaten\\Daten\\Original\\daten_w", i, ".dta"),
  #                     encoding = 'UTF-8')
  df_temp <- read_delim(paste0("H:\\Large_Datasets\\SME_Corona\\df_dat", i, ".csv"), delim = '\t')
  
  # Make some adjustments in the file
  df_temp <- df_temp %>% 
    mutate(#grddat = ymd(gruenddat),
           rechdat = ymd(rechdat),
           #gryear = year(grddat),
           jahr = jahr) %>% 
    select(crefo, jahr, rechdat, 
           #grddat, gryear
           )
  
  # Merge grddat, rechdat, gryear to panel
  df_panel <- df_panel %>% 
    as_tibble() %>% 
    coalesce_join(df_temp, by = c("crefo", "jahr"), join = dplyr::left_join)
  
  print(jahr)
}




# Exitmerkmal -------------------------------------------------------------

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
  df_temp <- read_dta(paste0("K:\\MUP\\Paneldaten\\Exitmerkmal\\exitmerkmal_W", i, ".dta"), 
                             encoding = 'UTF-8')
  
  # Make some adjustments in the file
  df_temp <- df_temp %>% 
    mutate(exit = as.double(exit),
           exitdat = ymd(paste0(exit_datum, '01')),
           jahr = jahr) %>% 
    select(crefo, jahr, exit, exitdat)
  
  # Merge grddat, rechdat, gryear to panel
  df_panel <- df_panel %>% 
    as_tibble() %>% 
    coalesce_join(df_temp, by = c("crefo", "jahr"), join = dplyr::left_join)
  
  print(jahr)
}




# Add Variables -----------------------------------------------------------



# Reduce panel size based on year (one year prior to last information is available -> so deltas can be calculated)
start_year <- 2017
df_panel <- df_panel %>% 
  as_tsibble(key = crefo, index = jahr) %>% 
  filter_index(start_year~.)

# Clean bonitaet (determine mising as 0 and define bonitaet = 600 as 500), 
# indicator for update of data, number of days between updates, change in bonitaet, anzma, umsatz, gesch?ftsgang prior to change in bonitaet
df_panel <- df_panel %>%  
  mutate(bonitaet = ifelse(bonitaet==0, NA, bonitaet),
         bonitaet = ifelse(bonitaet==600, 500, bonitaet)) %>% 
  group_by_key() %>% 
  mutate(
    updays = difference(rechdat, lag = 1),
    ch_bonitaet = difference(x=bonitaet, lag = 1),
    ch_anzma = difference(x=anzma, lag = 1),
    ch_umsatz = difference(x=umsatz, lag = 1),
    p_auftrag = lag(auftrag),
    p_entwick = lag(entwick)
    #updays = rechdat - lag(rechdat)),
         ) %>% 
  ungroup() %>%
  mutate(
         updays = ifelse((updays == 'NA days'), NA, updays),
         update = case_when(
           updays == 0 ~ 0,
           updays > 0 ~ 1),
         ch_bonitaet = ifelse(update==0, NA, ch_bonitaet),
         ch_anzma = ifelse(update==0, NA, ch_anzma),
         ch_umsatz = ifelse(update==0, NA, ch_umsatz)
         ) %>%  
  select(crefo, jahr, rechdat, update, updays, bonitaet, ch_bonitaet, exit, exitdat, everything()) 


# Drop observations without any change in boni information
# df_panel <- df_panel %>%
#   filter(!is.na(ch_bonitaet))

# Introduce treatment label
df_panel <- df_panel %>% 
  mutate(treat = ifelse(rechdat>"2020-04-01", 1, 0)) %>% 
  select(crefo, jahr, treat, rechdat, update, updays, grddat, gryear,
         bonitaet, ch_bonitaet, exit, exitdat, anzma, ch_anzma, umsatz, 
         ch_umsatz, auftrag, p_auftrag, entwick, p_entwick, wz, kreis, refo, everything())

write_delim(df_panel, path = file.path(getwd(), '02_Data', '01_PanelData', '02_Chunks', paste0('df_panel', k, '_', j, '.txt')), delim = '\t')

print(paste0(k, '_', j))

}
}

# CONTINUE HERE (next step only after all deltas of interest have been calculated, e.g. ch_anzma)
# Drop all observations with missing values in ch_bonitaet
# df_panel <- df_panel %>% 
#   filter(!is.na(ch_bonitaet))
# 
# table(df_panel$treat)
# 
# df_temp <- df_panel %>% 
#   mutate(pos_ch_boni = ch_bonitaet>0)
# 
# table(df_temp$treat, df_temp$pos_ch_boni)
# 
# # Check exit companies in 2020
# df_panel[!is.na(df_panel$exitdat) & (year(df_panel$exitdat) == 2020),]