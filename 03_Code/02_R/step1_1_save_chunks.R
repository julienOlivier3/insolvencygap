setwd("Q:\\Meine Bibliotheken\\Research\\SME_Corona")
source(file.path(getwd(), '03_Code', '02_R', 'step0_setup.R'))


# -------------------------------------------------------------------------
# #########################################################################
# -------------------------------------------------------------------------

load(file.path(getwd(), '02_Data', '01_PanelData', 'df_panel2010_2020.R'))

# Number of splits
n_split <- 50

vec_crefo <- unique(df_panel$crefo)

for (k in 17:n_split){
  
  
  id_crefo <- vec_crefo[ntile(vec_crefo, n = n_split)==k]
  
  df_temp <- df_panel %>% 
    as_tibble() %>% 
    filter(crefo %in% id_crefo)
  
write_delim(df_temp, path = file.path(getwd(), '02_Data', '01_PanelData', '02_Chunks', '01_Raw', paste0('df_panel', k, '.txt')), delim = '\t')

print(k)

}
