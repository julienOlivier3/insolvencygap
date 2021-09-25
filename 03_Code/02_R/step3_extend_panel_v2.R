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

# Read rechdat panel
df_temp <- read_delim(paste0("H:\\Large_Datasets\\SME_Corona\\df_dat_", "all", ".csv"), delim = '\t')
df_temp <- df_temp %>% rename(crefo = X1)

# Panel Manipulations -----------------------------------------------------


for (k in 15:n_split){
  df_panel_sample <- read_delim(file = file.path(getwd(), '02_Data', '01_PanelData', '02_Chunks', '01_Raw', paste0('df_panel', k, '.txt')), delim = '\t') 
  
  
  
  # Recherchedatum ----------------------------------------------------------
  
  
  
  
  df_panel_sample <- df_panel_sample %>% 
    as_tibble() %>% 
    coalesce_join(df_temp, by = c("crefo", "jahr"), join = dplyr::left_join)
  
  
  
  
  # Vector of unique crefos
  vec_crefo <- unique(df_panel_sample$crefo)
  
  for (j in 1:n_subsplit){
    
    
    id_crefo <- vec_crefo[ntile(vec_crefo, n = n_subsplit)==j]
    
    df_panel <- df_panel_sample %>% 
      as_tibble() %>% 
      filter(crefo %in% id_crefo)
    
    
    
    # Add Variables -----------------------------------------------------------
    
    
    
    # Reduce panel size based on year (one year prior to last information is available -> so deltas can be calculated)
    #start_year <- 2015
    df_panel <- df_panel %>% 
      as_tsibble(key = crefo, index = jahr) 
    
    # Clean bonitaet (determine mising as 0 and define bonitaet = 600 as 500), 
    # indicator for update of data, number of days between updates, change in bonitaet, anzma, umsatz, gesch?ftsgang prior to change in bonitaet
    df_panel <- df_panel %>%  mutate(
      boni_orig = bonitaet,
      bonitaet = ifelse(bonitaet==0, NA, bonitaet),
      bonitaet = ifelse(bonitaet==600, 500, bonitaet)
    ) %>% 
      group_by_key() %>% 
      mutate(
        updays = as.double(difftime(rechdat, lag(rechdat), units = 'days')),
        update = case_when(
          updays == 0 ~ 0,
          updays > 0 ~ 1),
        p_bonitaet = lag(bonitaet)
      ) %>% 
      ungroup() %>% 
      filter(update==1) %>% 
      group_by_key() %>% 
      mutate(p_bonitaet = ifelse(p_bonitaet == bonitaet, lag(bonitaet), p_bonitaet)) %>% 
      mutate(    
        ch_bonitaet = bonitaet - p_bonitaet,
        ch_anzma = difference(x=anzma, lag = 1),
        ch_umsatz = difference(x=umsatz, lag = 1)
      ) %>%  
      select(crefo, jahr, rechdat, update, updays, p_bonitaet, bonitaet, ch_bonitaet, anzma, ch_anzma, umsatz, ch_umsatz, everything()) 
    
    
    
    
    
    write_delim(df_panel, path = file.path("H:\\Large_Datasets\\SME_Corona", '02_Chunks', paste0('df_panel', k, '_', j, '.txt')), delim = '\t')
    
    print(paste0(k, '_', j))
    
  }
}

