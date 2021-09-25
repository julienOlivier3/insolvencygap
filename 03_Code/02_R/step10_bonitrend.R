# DESCRIPTIVES 
setwd("Q:\\Meine Bibliotheken\\Research\\SME_Corona")
source(file.path(getwd(), '03_Code', '02_R', 'step0_setup.R'))

# -------------------------------------------------------------------------
# #########################################################################
# -------------------------------------------------------------------------



# Load Appended & Cleaned Panel -------------------------------------------

df_panel <- read_delim(file = file.path(getwd(), '02_Data', '01_PanelData', '01_Backup', 'df_panel_clean_final.txt'),
                       delim = '\t')

# DROP
df_drop <- read_delim(file = file.path(getwd(), '02_Data', '03_SanityChecks', 'df_drop.txt'), delim = '\t')
df_drop <- df_drop[1:13,]

# Select relevant crefos
id <- df_panel %>% 
  select(crefo) %>% 
  distinct() %>% 
  as_vector()

# Load panel history
load(file = file.path("Q:\\Meine Bibliotheken\\Research\\SME_Corona\\02_Data\\01_PanelData\\01_Backup\\df_panel2010_2020.R"))

# Select crefos
df_panel <- df_panel %>% 
  filter(crefo %in% id)

# Only look at evolution 3 years prior to observation (earliest year of interest is 2014) and calc change boni
df_panel <- df_panel %>% 
  filter(jahr >= 2013) %>% 
  group_by(crefo) %>% 
  mutate(ch_bonitaet = bonitaet - lag(bonitaet)) %>% 
  filter(jahr >= 2014)

df_panel <- df_panel %>% 
  distinct(crefo, jahr, .keep_all = TRUE) %>% 
  as_tsibble(key = 'crefo', index='jahr') 


df_panel <- df_panel %>% 
  ungroup() %>% 
  select(crefo, jahr, bonitaet, ch_bonitaet)


# Correct bonitaet
df_panel <- df_panel %>% 
  mutate(
    bonitaet = ifelse(bonitaet==0, NA, bonitaet),
    bonitaet = ifelse(bonitaet==600, 500, bonitaet)
    )


# Now loop over relevant years and calculate mean boni and number of downgrades

start_year <- c(2014, 2015, 2016, 2017)
end_year <- c(2016, 2017, 2018, 2019)

df_boni = tibble(crefo = numeric(), 
                 jahr = numeric(),
                 avg_bonitaet = numeric(),
                 n_downgrade = numeric())

for (i in 1:4){
  
df_boni <-  df_boni %>% bind_rows(
  df_panel %>% 
    #head(100) %>% 
    filter_index(start_year[i] ~ end_year[i]) %>% 
    as_tibble() %>% 
    group_by(crefo) %>% 
    summarise(avg_bonitaet = mean(bonitaet, na.rm = TRUE),
              n_downgrade = sum(ch_bonitaet > 0, na.rm = TRUE)) %>% 
    mutate(jahr = end_year[i] + 1)
)

print(i)
    
  
}

df_boni <- df_boni %>% 
  arrange(crefo) %>% 
  #head(1000) %>% 
  as_tsibble(key = 'crefo', index = 'jahr') %>% 
  fill_gaps(.full=TRUE)

df_boni <- df_boni %>% 
  as_tibble()

## Save Panel =============================================================
#save(df_boni, file = file.path(getwd(), '02_Data', '01_PanelData', 'df_boni_3year.RData'))
write_delim(df_boni, path = file.path(getwd(), '02_Data', '01_PanelData', 'df_boni_3year.txt'), delim = '\t')
