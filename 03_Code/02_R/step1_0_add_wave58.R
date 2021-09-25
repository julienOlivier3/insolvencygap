setwd("Q:\\Meine Bibliotheken\\Research\\SME_Corona")
source(file.path(getwd(), '03_Code', '02_R', 'step0_setup.R'))

# -------------------------------------------------------------------------
# #########################################################################
# -------------------------------------------------------------------------

# Panel -------------------------------------------------------------------

df_panel <- read_delim('Q:\\Meine Bibliotheken\\Research\\SME_Corona\\02_Data\\01_PanelData\\df_panel2010_2019.txt',
                       delim = '\t')

# For testing
# df_panel <- read_dta("K:\\MUP\\panel56.dta", 
#                      skip = 0,
#                      n_max=100000, 
#                      encoding = 'UTF-8')
# # Filter by jahr
# df_panel <- df_panel %>% 
#   as_tsibble(key = 'crefo', index = jahr) %>% 
#   filter_index('2010'~.) %>% 
#   as_tibble()

# Change data types
df_panel <- df_panel %>% 
  mutate(
    #crefo = as.character(crefo),
    grddat = ymd(grddat),
    gryear = year(grddat)) 



# Add columns not in wave 58
df_panel <- df_panel %>% 
  mutate(hrnummer = NA,
         rechdat = NA) %>% 
  mutate(hrnummer = as.character(hrnummer),
         rechdat = as.Date(rechdat))


# Add wave 58
df_w58 <- read_delim("Q:\\Meine Bibliotheken\\Research\\SME_Corona\\02_Data\\02_Waves\\vvc58.txt", 
                     delim = '\t', col_types = cols(amt = col_character()))

dim(df_w58)

# Drop duplicate
df_w58 <- df_w58 %>% 
  distinct(crefo, .keep_all = 'TRUE')

dim(df_w58) # There have been 11 duplicates (now deleted)

# Clean wave 58
df_w58 <- df_w58 %>% 
  select(intersect(colnames(df_panel), colnames(df_w58)), hrnummer, rechdat) %>% 
  mutate(jahr = 2020,
         wz08 = NA,
         stamm = NA,
         kurteil = NA,
         zweise = NA,
         hkredit = NA,
         auftrag = NA,
         entwick = NA,
         grddat = NA,
         bilanz = NA,
         bilwelle = NA,
         gryear = NA) %>% 
  mutate(kreis = as.double(kreis),
         wz = as.double(wz),
         wz08 = as.double(wz08),
         stamm = as.double(stamm),
         gryear = as.double(gryear),
         kurteil = as.double(kurteil),
         zweise = as.double(zweise),
         hkredit = as.double(hkredit),
         auftrag = as.double(auftrag),
         entwick = as.double(entwick),
         grddat = as.double(grddat),
         bilanz = as.character(bilanz),
         bilwelle = as.double(bilwelle),
         rechdat = ymd(rechdat),
         grddat = ymd(grddat)) %>% 
  select(colnames(df_panel))



# Bind wave 58 and arrange by crefo and jahr
df_panel <- df_w58 %>% 
  bind_rows(df_panel) #%>% 
  arrange(crefo, jahr)

# Save panel with data from 2010-2020
save(df_panel, file = file.path(getwd(), '02_Data', '01_PanelData', '01_Backup', 'df_panel2010_2020.R'))
#write_delim(df_panel, path = file.path(getwd(), '02_Data', '01_PanelData', 'df_panel2010_2020.txt'), delim = '\t')
