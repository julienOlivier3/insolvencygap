# RESULTS
# Setup -------------------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(here, tidyverse)
source(file.path(here(), '03_Code', '02_R', 'step0_setup.R'))

# -------------------------------------------------------------------------
# #########################################################################
# -------------------------------------------------------------------------


# Load Strata PDs and Survey ----------------------------------------------
load(file = file.path(getwd(), '02_Data', '01_PanelData', 'df_panel_estimated.RData'))
# Create Factors for Insolvency and Treatment Dummies 
# 1)
df_panel <- df_panel %>% 
  mutate(insolv_fct = factor(ifelse(insolv==0, 'Non-insolvent', 'Insolvent'), 
                             levels = c('Non-insolvent', 'Insolvent')),
         treat_fct = factor(ifelse(treat_final==0, 'Pre-suspension', 'Suspension period'),
                            levels = c('Pre-suspension', 'Suspension period')))

# 2)
load(file = file.path(getwd(), '02_Data', '01_PanelData', 'df_strata.RData'))

# 3)
df_surv <- read_dta(file.path(here(), '02_Data', '07_Survey', '01_Gründungspanel', 'gpges_kern_w1-w13.dta'))
# Mapping crefo gpkey
df_crefo1 <- read_dta(file.path(getwd(), '02_Data', '07_Survey', "gp_sges.dta"))
df_crefo2 <- read_dta(file.path(getwd(), '02_Data', '07_Survey', 'crefo_gpkey_w13_erst.dta'))
map_gp_crefo <- df_crefo1 %>% 
  #filter(smpl19 > 0) %>% 
  select(gpkey, crefo) %>% 
  bind_rows(df_crefo2) 

# Prepare survey
df_surv <- df_surv %>% 
  filter(jahr==2019) %>% 
  select(gpkey, branche11, gr_jahr, unt_bet:strat_sonst) 

# Merge crefo
df_surv <- df_surv %>% 
  left_join(map_gp_crefo, by = 'gpkey') %>% 
  select(crefo, everything())


table(is.na(df_surv$crefo))
# Crefo exists for all



# Merge MUP Variables -----------------------------------------------------

# Get latest info from MUP only
df_surv <- df_surv %>% 
  left_join(df_panel , by = 'crefo') %>% 
  group_by(crefo) %>% 
  filter(row_number() == n()) %>% 
  ungroup()

colnames(df_surv)



## Non-Mergers ============================================================


# Some survey observations are not included in df_panel
df_surv_uk <- df_surv %>% 
  filter(is.na(rechdat)) %>% 
  select(crefo)


# Check if they can be obtained from current wave
df_w58 <- read_delim("Q:\\Meine Bibliotheken\\Research\\SME_Corona\\02_Data\\02_Waves\\vvc58.txt", delim = '\t')

df_w58 <- df_w58 %>% 
  filter(welle == 58)

# Join wave 58 info
df_surv_uk <- df_surv_uk %>% 
  left_join(df_w58, by = 'crefo')

# Keep relevant variables
df_surv_uk <- df_surv_uk %>% 
  mutate(archivdat = ymd(archivdat),
         rechdat = ymd(rechdat),
         jahr = 2020) %>% 
  select(intersect(colnames(.), colnames(df_panel)), archivdat)

# Save
save(df_surv_uk, file = file.path(getwd(), '02_Data', '02_Waves', 'df_surv_uk.RData'))

# Get variables from prior dates
load("C:\\Users\\jdo\\Desktop\\df_panel2010_2020.R")
df_surv_uk2 <- df_panel %>% 
  filter(crefo %in% df_surv_uk$crefo)



# Get rechdat from prior dates
# Add rechdat for previous waves (latest wave 46)
for (i in seq(52,46,-2)){
  # Define year corresponding to wave
  jahr <- w_y_map[as.character(i)][[1]]
  
  # Read dates dta file
  df_temp <- read_dta(paste0("K:\\MUP\\Paneldaten\\Daten\\Original\\daten_w", i, ".dta"),
                      encoding = 'UTF-8')
  
  # Make some adjustments in the file
  df_temp <- df_temp %>% 
    mutate(grddat = ymd(gruenddat),
           archivdat = ymd(archivdat),
           rechdat = ymd(rechdat),
           gryear = year(grddat),
           jahr = jahr) %>% 
    select(crefo, jahr, grddat, rechdat, archivdat, gryear)
  
  # Merge grddat, rechdat, gryear to panel
  df_surv_uk2 <- df_surv_uk2 %>% 
    as_tibble() %>% 
    coalesce_join(df_temp, by = c("crefo", "jahr"), join = dplyr::left_join)
  
  print(jahr)
}


df_surv_uk2 <- df_surv_uk2 %>% 
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
  df_surv_uk2 <- df_surv_uk2 %>% 
    as_tibble() %>% 
    coalesce_join(df_temp, by = c("crefo", "jahr"), join = dplyr::left_join)
  
  print(jahr)
}



# Calculate further variables
df_surv_uk2 <- df_surv_uk2 %>% 
  mutate(bonitaet = ifelse(bonitaet==0, NA, bonitaet),
       bonitaet = ifelse(bonitaet==600, 500, bonitaet)) %>% 
  group_by(crefo) %>% 
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
  select(crefo, jahr, treat, rechdat, update, updays, grddat, gryear, bonitaet, ch_bonitaet, 
         exit, exitdat, 
         anzma, ch_anzma, umsatz, ch_umsatz, auftrag, p_auftrag, entwick, p_entwick, wz, kreis, refo, everything())

load(file = file.path(getwd(), '02_Data', '02_Waves', 'df_surv_uk2.RData'))

df_surv_uk2 <- df_surv_uk2 %>%
  mutate(
    treat_final = ifelse(rechdat>="2020-04-01", 1, 0),
    jahr_rd = year(rechdat),
    insolv = ifelse(exit==2, 1, 0),
    p_bonitaet = bonitaet - ch_bonitaet,
    d_exitdat = as.double(rechdat - exitdat)
    ) %>% 
  mutate(
    p_bonitaet_fct = factor(case_when(
      p_bonitaet < 200 ~ '[100 - 200)',
      (p_bonitaet >= 200) & (p_bonitaet < 300)  ~ '[200 - 300)',
      (p_bonitaet >= 300) & (p_bonitaet < 400)  ~ '[300 - 400)',
      p_bonitaet >= 400 ~ '[400 - 500]'
    ), levels = c('[100 - 200)', '[200 - 300)', '[300 - 400)', '[400 - 500]')),
    q_p_bonitaet_fct = factor(case_when(
      p_bonitaet <= 233 ~ '1. Quartile',
      p_bonitaet > 233 & p_bonitaet <= 272 ~ '2. Quartile',
      p_bonitaet > 272 & 292 ~ '3. Quartile',
      p_bonitaet > 292 ~ '4. Quartile'
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
      (anzma < 10) | (umsatz <= 2000000) ~ 'Micro-enterprise',
      ((anzma >= 10) & (anzma < 50)) | (umsatz <= 10000000) ~ 'Small enterprise', 
      ((anzma >= 50) & (anzma < 250)) | (umsatz <= 43000000) ~ 'Medium-sized enterprise', 
      (anzma >= 250) | (umsatz > 43000000) ~ 'Large enterprise'
    ), levels = c('Large enterprise', 'Medium-sized enterprise', 'Small enterprise', 'Micro-enterprise'))
  ) %>% 
  mutate(wz_fct = case_when( wz>=1000 & wz<= 4000	~	'Agriculture'	,
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
  )) %>% 
  select(-c(treat)) %>% 
  filter(update == 1) %>% 
  group_by(crefo) %>% 
  filter(row_number() == n()) %>% 
  mutate(count = n()) %>% 
  ungroup() %>% 
  mutate(insolv_fct = factor(ifelse(insolv==0, 'Non-insolvent', 'Insolvent'), 
                             levels = c('Non-insolvent', 'Insolvent')),
         treat_fct = factor(ifelse(treat_final==0, 'Pre-suspension', 'Suspension period'),
                            levels = c('Pre-suspension', 'Suspension period')))


# Industry (Low granularity)
df_industry <- read_dta(file.path(getwd(), '02_Data', '01_PanelData', 'Branchenabgrenzung.dta'))
map_industry <- stack(attr(df_industry$branche_corona, 'labels')) %>% 
  mutate(ind_en = c('Other', 'Business-related services', 'Consumer-related services', 'Trade', 'Manufacturing/construction'))
df_industry <- df_industry %>% 
  mutate(wz5_fct = factor(branche_corona, labels = map_industry$ind_en)) %>% 
  rename(wz = wzdig5) %>% 
  select(wz, wz5_fct) %>% 
  bind_rows(tibble(wz = 45340, wz5_fct = 'Trade'))

df_surv_uk2 <- df_surv_uk2 %>% 
  left_join(df_industry, by = 'wz')

# Change in bonitaet as factor
quant_boni <- quantile(df_panel %>% filter(treat_final==0) %>% .$ch_bonitaet, probs = c(0, 0.05, seq(0.1,0.9,length.out = 9), 0.95, 1))
cp_ch_bonitaet <- c(-Inf, 
                    unique(quant_boni)[2:5]-1,
                    0,
                    unique(quant_boni)[6:(length(unique(quant_boni))-1)],
                    Inf)

cp_cem <- list(ch_anzma = c(-Inf, -1, 0, Inf), ch_bonitaet = cp_ch_bonitaet)

df_surv_uk2 <- df_surv_uk2 %>% 
  mutate(ch_anzma_fct = addNA(cut(ch_anzma, breaks = cp_cem$ch_anzma)),
         csize_fct = addNA(csize_fct),
         ctype_fct = addNA(ctype_fct),
         refo_fct = addNA(refo_fct),
         wz_fct = addNA(wz_fct),
         ch_bonitaet_fct = cut(ch_bonitaet, breaks = cp_ch_bonitaet)
  ) %>% 
  filter(!is.na(bonitaet)) %>% 
  filter(!is.na(ch_bonitaet))


# Merge strata info
df_surv_uk2 <- df_surv_uk2 %>% 
  left_join(df_strata, by = c('q_p_bonitaet_fct', 'ch_bonitaet_fct', 'ch_anzma_fct', 'ctype_fct', 'csize_fct', 'wz5_fct'))

# Add columns as missings
df_surv_uk2 <- df_surv_uk2 %>% 
  mutate(distance = NA,
         weights = NA)

df_surv_uk2 <- df_surv_uk2 %>% 
  select(df_surv %>% select(crefo, jahr_rd:treat_fct) %>% colnames())

# Save
save(df_surv_uk2, file = file.path(getwd(), '02_Data', '02_Waves', 'df_surv_uk2.RData'))


## Full Survey ============================================================
load(file = file.path(getwd(), '02_Data', '02_Waves', 'df_surv_uk2.RData'))

df_surv <- df_surv %>% 
  coalesce_join(df_surv_uk2, by = c('crefo'))

# Adjust dtypes
df_surv <- df_surv %>% 
  mutate_if(~(class(.) == 'haven_labelled'), stata2factor)

df_surv %>% 
  mutate(data_exist = !is.na(rechdat)) %>% 
  tab(data_exist)
# For 58% (~ 2350 UN) estimeates exist


df_surv %>% 
  tab(treat_final)
# For 13% (> 523 UN) updated bonitaet info is available

df_surv %>% 
  filter(treat_final==1) %>% 
  mutate(est_PD_gr0 = est_PD > 0) %>% 
  tab(est_PD_gr0)
# For 24% (> 125 UN) the estimated PD is greater than 0

# Reduce to observations with updated bonitaet only
df_surv_post <- df_surv %>% 
  filter(rechdat >= '2020-04-01') 


# Add manually est_PD to 26 missing cases with nearest neighboor
df_surv_post %>% filter(is.na(est_PD)) %>% select(crefo, est_PD, contains('_fct')) %>% select(-c(refo_fct, wz_fct, p_bonitaet_fct))

df_panel %>% select(est_PD, treat_final, q_p_bonitaet_fct, ctype_fct, csize_fct, wz5_fct, ch_anzma_fct, ch_bonitaet_fct) %>% 
  filter(treat_final==0) %>% 
  filter(q_p_bonitaet_fct == '1. Quartile',
         ctype_fct  ==  'Young firm',
         csize_fct == 'Micro-enterprise',
         #wz5_fct == 'Manufacturing/construction',
         #ch_anzma_fct == '(0,Inf]',
         ch_bonitaet_fct == '(-33,-8]') %>% 
  summarise(temp = mean(est_PD, na.rm = TRUE))

# 1)
df_surv_post[df_surv_post$crefo==8010163540,'est_PD'] <- 2.553771069e-4
# 2)
df_surv_post[df_surv_post$crefo==8030348713,'est_PD'] <- 1.776514479e-4
# 3)
df_surv_post[df_surv_post$crefo==7110371917,'est_PD'] <- 0
# 4)
df_surv_post[df_surv_post$crefo==7290526820,'est_PD'] <- 0


  
df_surv_post %>% 
  filter(ch_bonitaet!=0) %>% 
  ggplot() +
  geom_jitter(aes(x = ch_bonitaet, y = est_PD, shape = insolv_fct, color = oef_intens, size = oef_intens)) +
  scale_shape_discrete(name = "") +
  scale_color_gradient(low = 'grey', high = 'red', name = 'State aid intensity') +
  xlab('Change in credit rating') +
  ylab('Estimated PD') +
  scale_y_log10(lim = c(-0.01, 0.15)) + 
  theme_jod + 
  theme(panel.grid.minor.y = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5)) 


df_surv_post %>% 
  tab(csize_fct)

df_surv_post %>% 
  tab(ctype_fct)

df_surv_post %>% 
  tab(oef_kr)
df_surv_post %>% 
  tab(oef_zu)
df_surv_post %>% 
  tab(oef_wstf)
df_surv_post %>% 
  tab(oef_stst)
df_surv_post %>% 
  tab(oef_sonst)

# Calculate new variable for subsidy intensity
df_surv_post <- df_surv_post %>% 
  mutate_at(vars(contains('oef_')), .funs = function(x) as.numeric(as.character(x))) %>% 
  rowwise() %>% 
  mutate(oef_intens = sum(c_across(oef_zu:oef_sonst), na.rm = TRUE)) %>% 
  ungroup()

df_surv_post %>% 
  tab(oef_intens)

df_surv_post %>% 
  tab(strat_marktor)
