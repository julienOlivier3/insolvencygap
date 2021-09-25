# DATA TYPES
setwd("Q:\\Meine Bibliotheken\\Research\\SME_Corona")
source(file.path(getwd(), '03_Code', '02_R', 'step0_setup.R'))

# -------------------------------------------------------------------------
# #########################################################################
# -------------------------------------------------------------------------


# Load Appended & Cleaned Panel -------------------------------------------
#load(file = file.path(getwd(), '02_Data', '01_PanelData', 'df_panel_clean_miss.RData'))
df_panel <- read_delim(file = file.path(getwd(), '02_Data', '01_PanelData', 'df_panel_clean_wo_miss.txt'),
                       delim = '\t')

# DROP
df_drop <- read_delim(file = file.path(getwd(), '02_Data', '03_SanityChecks', 'df_drop.txt'), delim = '\t')
df_drop <- df_drop[1:10,]


## Define Data Types ======================================================

# Boni, liability, age, size
df_panel <- df_panel %>% 
  mutate(
    p_bonitaet_fct = factor(case_when(
      p_bonitaet < 200 ~ '[100 - 200)',
      (p_bonitaet >= 200) & (p_bonitaet < 300)  ~ '[200 - 300)',
      (p_bonitaet >= 300) & (p_bonitaet < 400)  ~ '[300 - 400)',
      p_bonitaet >= 400 ~ '[400 - 500]'
    ), levels = c('[100 - 200)', '[200 - 300)', '[300 - 400)', '[400 - 500]')),
    q_p_bonitaet_fct = factor(case_when(
      p_bonitaet <= quantile(p_bonitaet, probs = 0.25, na.rm = TRUE) ~ '1. Quartile',
      p_bonitaet > quantile(p_bonitaet, probs = 0.25, na.rm = TRUE) & p_bonitaet <= quantile(p_bonitaet, probs = 0.5, na.rm = TRUE) ~ '2. Quartile',
      p_bonitaet > quantile(p_bonitaet, probs = 0.5, na.rm = TRUE) & p_bonitaet <= quantile(p_bonitaet, probs = 0.75, na.rm = TRUE) ~ '3. Quartile',
      p_bonitaet > quantile(p_bonitaet, probs = 0.75, na.rm = TRUE) ~ '4. Quartile'
    ), levels = c('1. Quartile', '2. Quartile', '3. Quartile', '4. Quartile')),
    refo_fct = factor(case_when(
      refo %in% c(1, 2, 3, 4, 5, 6, 7, 8) ~ 'full liability',
      refo %in% c(9, 10, 11, 12, 13, 82) ~ 'limited liability',
    ), levels = c('limited liability', 'full liability')),
    ctype_fct = factor(case_when(
      gryear >= 2016 ~ 'Start-up',
      (gryear < 2016) & (gryear >= 2010)  ~ 'Young firm',
      gryear < 2010 ~ 'Incumbent firm'
    ), levels = c('Incumbent firm', 'Young firm', 'Start-up')),
    csize_fct = factor(case_when(
      (anzma <= 10) | (umsatz <= 2000000) ~ 'Micro-enterprise',
      ((anzma > 10) & (anzma < 50)) | ((umsatz > 2000000) & (umsatz <= 10000000)) ~ 'Small enterprise', 
      ((anzma >= 50) & (anzma < 250)) | ((umsatz > 10000000) & (umsatz <= 50000000)) ~ 'Medium-sized enterprise', 
      (anzma >= 250) | (umsatz > 50000000) ~ 'Large enterprise'
    ), levels = c('Large enterprise', 'Medium-sized enterprise', 'Small enterprise', 'Micro-enterprise'))
  ) 





# Industry (high granularity)
df_panel <- df_panel %>% 
  mutate(wz_fct = case_when( 
                         wz>=1000 & wz<= 4000	~	'Agriculture'	,
                         wz>=5000 & wz<= 9999	~	'Mining'	,
                         wz>=10000 & wz<= 12999	~	'Food'	,
                         wz>=13000 & wz<= 18999	~	'Clothing/Printing'	,
                         wz>=19000 & wz<= 19999	~	'Oil'	,
                         wz>=20000 & wz<= 20999	~	'Chemicals'	,
                         wz>=21000 & wz<= 21999	~	'Pharmaceuticals'	,
                         wz>=22000 & wz<= 23999	~	'Plastics'	,
                         wz>=24000 & wz<= 25999	~	'Metal/Steel'	,
                         wz>=26000 & wz<= 27999	~	'Electronics/Optics'	,
                         wz>=28000 & wz<= 28999	~	'Mechnical Engineering'	,
                         wz>=29000 & wz<= 30999	~	'Automotive'	,
                         wz>=31000 & wz<= 32999	~	'Furniture'	,
                         wz>=33000 & wz<= 34999	~	'Repair/Installation'	,
                         wz>=35000 & wz<= 39999	~	'Utilities'	,
                         wz>=40000 & wz<= 44999	~	'Construction'	,
                         wz>=45000 & wz<= 45999	~	'Car repair'	,
                         wz>=46000 & wz<= 46999	~	'Wholesale'	,
                         (wz>=47000 & wz<= 47199) | (wz>=47400 & wz<= 47729) | (wz>=47760 & wz<= 47799) | (wz>=47820 & wz<= 47899) | (wz>=47990 & wz<= 47999)	~	'Retail (closed)'	,
                         (wz>=47200 & wz<= 47299) | (wz>=47300 & wz<= 47399) | (wz>=47730 & wz<= 47759) | (wz>=47800 & wz<= 47819) | (wz>=47900 & wz<= 47919)	~	'Retail (open)'	,
                         wz>=49000 & wz<= 53999	~	'Transport'	,
                         wz>=55000 & wz<= 55999	~	'Hotels'	,
                         wz>=56000 & wz<= 56999	~	'Restaurants'	,
                         wz>=58000 & wz<= 60999	~	'Media'	,
                         wz>=61000 & wz<= 63999	~	'Software/Telco'	,
                         wz>=64000 & wz<= 66999	~	'Financial service/Banks'	,
                         wz>=68000 & wz<= 68999	~	'Real estate'	,
                         (wz>=69000 & wz<= 71999) | (wz>=73000 & wz<= 73999)	~	'Consulting'	,
                         wz>=72000 & wz<= 72999	~	'Research'	,
                         wz>=73000 & wz<= 74999	~	'Creative Services'	,
                         wz>=75000 & wz<= 75999	~	'Veterinary'	,
                         wz>=77000 & wz<= 78999	~	'Leasing'	,
                         wz>=79000 & wz<= 79999	~	'Travel'	,
                         wz>=80000 & wz<= 82999	~	'Facilities'	,
                         wz>=84000 & wz<= 84999	~	'Public service'	,
                         wz>=85000 & wz<= 85999	~	'Education'	,
                         wz>=86000 & wz<= 88999	~	'Health'	,
                         wz>=90000 & wz<= 93999	~	'Entertainment'	,
                         (wz>=94000 & wz<= 94999)   | (wz>=99000 & wz<= 99999)	~	'Interest groups'	,
                         wz>=95000 & wz<= 98999	~	'Personal service'	
                         ))


# Factors for Insolvency and Treatment Dummies

df_panel <- df_panel %>% 
  mutate(insolv_fct = factor(ifelse(insolv==0, 'Non-insolvent', 'Insolvent'), 
                             levels = c('Non-insolvent', 'Insolvent')),
         treat_fct = factor(ifelse(treat_final==0, 'Pre-suspension period', 'Suspension period'),
                            levels = c('Pre-suspension period', 'Suspension period')))


## Save Panel =============================================================
save(df_panel, file = file.path(getwd(), '02_Data', '01_PanelData', 'df_panel_clean_fct.RData'))
#write_delim(df_panel, path = file.path(getwd(), '02_Data', '01_PanelData', 'df_panel_clean_fct.txt'), delim = '\t')


