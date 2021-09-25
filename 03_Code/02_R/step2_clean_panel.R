setwd("Q:\\Meine Bibliotheken\\Research\\SME_Corona")
source(file.path(getwd(), '03_Code', '02_R', 'step0_setup.R'))

# -------------------------------------------------------------------------
# #########################################################################
# -------------------------------------------------------------------------

# Panel -------------------------------------------------------------------

load(file.path(getwd(), '02_Data', '01_PanelData', '01_Backup', 'df_panel2010_2020.R'))



# Drop duplicates
# Create dropping df
# DROP
s1_dim <- nrow(df_panel)
df_drop <- tibble(N = s1_dim, Remark = 'Number of observations in self-made Panel 58')


# Drop crefo-jahr duplicates
df_panel <- df_panel %>% 
  distinct(across(matches('crefo|jahr')), .keep_all = 'TRUE')

# DROP
s2_dim <- nrow(df_panel)
df_drop <- df_drop %>% 
  bind_rows(tibble(N = s2_dim, Remark = 'Drop due to duplicates in crefo-jahr combination'))

# Arrange by crefo, jahr
df_panel <- df_panel %>% 
  as_tsibble(key = crefo, index = jahr)

# Save panel with data from 2010-2020
save(df_panel, file = file.path(getwd(), '02_Data', '01_PanelData', 'df_panel2010_2020.R'))
#write_delim(df_panel, path = file.path(getwd(), '02_Data', '01_PanelData', 'df_panel2010_2020.txt'), delim = '\t')

# Save dropping list
write_delim(df_drop, path = file.path(getwd(), '02_Data', '03_SanityChecks', 'df_drop.txt'), delim = '\t')


# Create sample
df_panel_sample <- df_panel[1:1000000,]
# Save sample panel with data from 2010-2020
save(df_panel_sample, file = file.path(getwd(), '02_Data', '01_PanelData', 'df_panel2010_2020_sample.RData'))


