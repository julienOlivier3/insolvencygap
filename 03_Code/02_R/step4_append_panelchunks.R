# MIGRATED TO PYTHON

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


# Append Chunks -----------------------------------------------------------
df_list <- list()
col_types <- cols(
  crefo = col_double(),
  jahr = col_double(),
  treat = col_double(),
  rechdat = col_date(),
  update = col_double(),
  updays = col_double(),
  grddat = col_date(),
  gryear = col_double(),
  bonitaet = col_double(),
  ch_bonitaet = col_double(),
  exit = col_double(),
  exitdat = col_date(),
  anzma = col_double(),
  ch_anzma = col_double(),
  umsatz = col_double(),
  ch_umsatz = col_double(),
  auftrag = col_double(),
  p_auftrag = col_double(),
  entwick = col_double(),
  p_entwick = col_double(),
  wz = col_double(),
  kreis = col_double(),
  refo = col_double(),
  wz08 = col_double(),
  stamm = col_double(),
  kurteil = col_double(),
  zweise = col_double(),
  hkredit = col_double(),
  bilanz = col_character(),
  bilwelle = col_double(),
  faulig = col_double(),
  giftig = col_double(),
  tot = col_double(),
  hrnummer = col_character()
  )


for (k in 1:n_split){
  
  for (j in 1:n_subsplit){
    df_temp <- read_delim(file = file.path(getwd(), '02_Data', '01_PanelData', '02_Chunks', paste0('df_panel', k, '_', j, '.txt')), 
                          delim = '\t', 
                          col_types = col_types) %>% 
      filter(!is.na(ch_bonitaet))
      
    # df_panel <- df_panel %>% 
    #   bind_rows(df_temp)
    df_list[[k]] <- df_temp
    
    print(paste0(k, '_', j))
  }
}

df_panel <- bind_rows(df_list)

crosstab(df_panel$jahr, df_panel$exit)
crosstab(df_panel$jahr, df_panel$tot)
crosstab(df_panel$jahr, df_panel$faulig)
crosstab(df_panel$jahr, df_panel$giftig)


save(df_panel, file = file.path(getwd(), '02_Data', '01_PanelData', 'df_panel_2017_2020_clean.RData'))


name_list <- list()
for (k in 1:n_split){
  
  for (j in 1:n_subsplit){
    name_list <- list.append(name_list, file.path(getwd(), '02_Data', '01_PanelData', '02_Chunks', paste0('df_panel', k, '_', j, '.txt')))
    
    print(paste0(k, '_', j))
  }
}


lapply(X = name_list, FUN = function(x) read_delim(file = x, delim = '\t', col_types = col_types))

       