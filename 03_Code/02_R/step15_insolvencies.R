# MISSING VLAUES
setwd("Q:\\Meine Bibliotheken\\Research\\SME_Corona")
source(file.path(getwd(), '03_Code', '02_R', 'step0_setup.R'))

# -------------------------------------------------------------------------
# #########################################################################
# -------------------------------------------------------------------------


# Load Appended & Cleaned Panel -------------------------------------------

#load(file = file.path(getwd(), '02_Data', '01_PanelData', 'df_ins_destatis.RData')) # after general cleaning
#load(file = file.path(getwd(), '02_Data', '01_PanelData', 'df_ins_destatis2.RData')) # after deleting exit %in% c(1,3) 
#load(file = file.path(getwd(), '02_Data', '01_PanelData', 'df_ins_destatis3.RData')) # after deleting observations which have not been updated for more than 3 years
load(file = file.path(getwd(), '02_Data', '01_PanelData', 'df_ins_destatis4.RData')) # before deleting hybrids


# Reduce to firms with hrnummer
table(is.na(df_panel$hrnummer))

# Get hrnummer for prior waves
load("C:\\Users\\jdo\\Desktop\\df_panel2010_2020.R")
df_hr <- df_panel %>% 
  as_tibble() %>% 
  select(crefo, hrnummer)

df_hr <- df_hr %>% 
  filter(!is.na(hrnummer)) %>% 
  group_by(crefo) %>% 
  mutate(n = n()) %>% 
  ungroup()

df_hr %>% rename(no = n) %>% tab(no)
# all crefo - hrnummer combinations are unqiue

#save(df_hr, file = file.path(getwd(), '02_Data', '03_SanityChecks', 'df_hr.RData'))
load(file = file.path(getwd(), '02_Data', '03_SanityChecks', 'df_hr.RData'))


# Merge hrnummer
df_panel <- df_panel %>% 
  coalesce_join(df_hr, by = 'crefo')

table(is.na(df_panel$hrnummer))

# For some obervations exitdat and rechdat are not the same

df_panel %>% 
  filter(insolv==1) %>% 
  mutate(jahr_exit = year(exitdat)) %>% 
  select(crefo, rechdat, jahr_rd, exitdat, jahr_exit) %>% 
  filter(jahr_exit != jahr_rd)

# Create df with number of exits by date of exit
df_ins <- df_panel %>% 
  filter(!is.na(hrnummer)) %>% # only observations with hrnummer
  #filter((d_exitdat <= 360/3) | (is.na(d_exitdat))) %>%  # only observations where rechdat does not exceed 4 months after exitdat
  mutate(jahr_exit = year(exitdat)) %>% 
  mutate(jahr_rd2 = ifelse((jahr_rd == jahr_exit) | is.na(jahr_exit), jahr_rd, jahr_exit)) %>% 
  select(crefo, jahr_rd, jahr_exit, jahr_rd2, everything()) %>% 
  #group_by(treat_final) %>%
  #mutate(anzun = n()) %>%
  #ungroup() %>%
  mutate(anzun = n()) %>% 
  select(crefo, rechdat, jahr_rd, exitdat, jahr_exit, jahr_rd2, anzun, insolv, treat_final, exit) %>% 
  filter(insolv==1) %>% 
  group_by(exitdat) %>% 
  summarise(anzinsolv = n(),
            anzun_year = unique(anzun),
            insolv_intensity = round(anzinsolv/anzun_year, 5))
