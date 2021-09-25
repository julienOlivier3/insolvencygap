# ESTIMATION
setwd("Q:\\Meine Bibliotheken\\Research\\SME_Corona")
source(file.path(getwd(), '03_Code', '02_R', 'step0_setup.R'))

# -------------------------------------------------------------------------
# #########################################################################
# -------------------------------------------------------------------------

# Load Appended & Cleaned Panel -------------------------------------------

df_panel <- read_delim(file = file.path(getwd(), '02_Data', '01_PanelData', 'df_panel_clean_final.txt'),
                       delim = '\t',
                       col_types = cols(exitdat = col_date()))
load(file.path(getwd(), '05_Model', 'df_match.RData'))
df_match <- df_match %>% 
  mutate(mean_diff = -1*mean_diff,
         mean_w_diff = -1*mean_w_diff)

# DROP
#df_drop <- read_delim(file = file.path(getwd(), '02_Data', '03_SanityChecks', 'df_drop.txt'), delim = '\t')
#df_drop <- df_drop[1:13,]

# Some minor cleaning
# Drop observations for which we do not know the size
df_panel <- df_panel %>% 
  filter(!is.na(csize_fct))

# or imputation
df_csize <- read_delim(file.path(getwd(), "02_Data", "10_Imputation", "df_csize.txt"), delim = "\t")
df_gryear <- read_delim(file.path(getwd(), "02_Data", "10_Imputation", "df_gryear.txt"), delim = "\t")

df_panel <- df_panel %>% coalesce_join(df_csize, by = "crefo") %>% 
  coalesce_join(df_gryear, by = "crefo")
# and then drop
df_panel <- df_panel %>% 
  filter(!is.na(csize_fct))


df_panel <- df_panel %>% 
  mutate(age = ifelse((jahr_rd + 1 - gryear) <= 0, 1, jahr_rd + 1 - gryear))

df_panel <- df_panel %>% 
  mutate(umsatz = ifelse(umsatz > 200000000000, NA, umsatz))

df_panel <- df_panel %>% 
  mutate(anzma = ifelse(anzma == 0, NA, anzma))

df_panel <- df_panel %>% 
  mutate(refo_dummy = case_when(refo_fct == 'limited liability' ~ 1,
                                refo_fct == 'full liability' ~ 0))   

# Drop one duplicate
df_panel %>% 
  filter(insolv==1) %>% 
  group_by(crefo) %>% 
  mutate(n_obs = n()) %>% 
  ungroup() %>% 
  filter(n_obs > 1) %>% 
  select(crefo, jahr_rd, rechdat, exitdat, insolv, p_bonitaet, ch_bonitaet)
  
df_panel <- df_panel %>% 
  filter(!(crefo==8150104052 & rechdat=="2019-05-13" & exitdat=="2019-07-15"))

# Clean WZ descriptions
df_match <- df_match %>% mutate(wz12_fct = case_when(wz12_fct == 'Others' ~ 'Others',
                                         wz12_fct == 'Mechanical engineering' ~ 'Mechanical engineering',
                                         wz12_fct == 'Manufacturing of chemical or pharmaceutical products' ~ 'Chemicals & pharmaceuticals', 
                                         wz12_fct == 'Manufacturing of data processing equipment' ~ 'Manufacturing of data processing equipment', 
                                         wz12_fct == 'Food production' ~ 'Food production',
                                         wz12_fct == 'Other manufacturing industries' ~ 'Manufacturing',
                                         wz12_fct == 'Wholesale and retail trade (incl. car repair)' ~ 'Wholesale & retail trade',
                                         wz12_fct == 'Accommodation & catering' ~ 'Accommodation & catering',
                                         wz12_fct == 'Insurance & banking' ~ 'Insurance & banking',
                                         wz12_fct == 'Logistics & transport (incl. postal service)' ~ 'Logistics & transport',
                                         wz12_fct == 'Creative industry & entertainment' ~ 'Creative industry & entertainment', 
                                         wz12_fct == 'Other business-related services' ~ 'Business-related services',
                                         wz12_fct == 'Health and social services' ~ 'Health & social services')) 

# Colors
display.brewer.all(n=10, select = "RdYlGn")
brewer.pal(n=10, name = "RdYlGn")

viridisLite::viridis(3, alpha = 1, begin = 0, end = 1, direction = 1, option = "D")


# Descriptives ------------------------------------------------------------

## In-text figures ========================================================
# Number of rating updates
nrow(df_panel)

# Number of distinct firms
length(unique(df_panel$crefo))

# Number of insolvencies
length((df_panel[df_panel$insolv==1,]$crefo))
length(unique(df_panel[df_panel$insolv==1,]$crefo))

# Number of non-insolvent rating updates
length((df_panel[df_panel$insolv==0,]$crefo))
length(unique(df_panel[df_panel$insolv==0,]$crefo))

nrow(df_panel) == length((df_panel[df_panel$insolv==1,]$crefo)) + length((df_panel[df_panel$insolv==0,]$crefo))


# Time between two updates
summary(df_panel$updays)/30


# Insolvency rates
df_match %>% filter(csize_fct == "Micro-enterprise") %>% 
  filter(wz12_fct %in% c("Accommodation & catering", "Logistics & transport", "Creative industry & entertainment")) %>% 
  select(wz12_fct, csize_fct, cont_mean_w, treat_mean) %>% 
  mutate_if(is_double, function(x) round(x,6)*100)

# Average rates
df_match %>%
  mutate(N = pmap_dbl(.l = list(data_match), .f = function(x) nrow(unnest(x)))) %>% 
  select(wz12_fct, csize_fct, N, mean_w_diff) %>% 
  group_by(csize_fct) %>% 
  summarise(avg_ins_rate = weighted.mean(mean_w_diff, N)) %>% 
  ungroup() %>% 
  mutate_if(is_double, function(x) round(x,6)*100)

# Outliers among large firms
df_match %>% filter(csize_fct == "Large enterprise") %>% 
  filter(wz12_fct %in% c("Food production")) %>% 
  select(data_match) %>% 
  unnest(cols = data_match) %>% 
  filter(insolv==1)

df_match %>% filter(csize_fct == "Large enterprise") %>% 
  filter(wz12_fct %in% c("Manufacturing of data processing equipment")) %>% 
  select(data_match) %>% 
  unnest(cols = data_match) 
  filter(insolv==1)

  

## Descriptive Statistics =================================================

df_panel %>% ggplot() + 
  geom_histogram(aes(umsatz)) + 
  scale_y_log10() +
  theme_jod



# Define survey object required for significance calculation
svyd <- svydesign(ids=~1, data = df_panel)

# Define function for test of independence depending on type of variable
indep_test <- function(x, split_var = 'treat_final', kind = ""){
  if(max(x, na.rm = TRUE) == 1){
    if(kind=="survey"){
      svychisq(~x+treat_final, design = svyd, statistic='F')$p.value[[1]] # not working
    }
    else{
      chisq.test(x, df_panel[[split_var]])$p.value
    }
    
  }
  else{
    t.test(x~df_panel[[split_var]])$p.value
  }
}

### Insolvent vs non-insolvent ##############################################

df3_1 <- df_panel %>%
  filter(insolv==0) %>% 
  mutate(umsatz = umsatz/1000000) %>% 
  select(insolv, ch_bonitaet, p_bonitaet, n_downgrade, avg_bonitaet, age, refo_dummy, anzma, umsatz) %>% 
  map_df(.f = ~ broom::tidy(summary(.x)), .id = "variable") %>% 
  left_join(df_panel %>%  filter(insolv==0) %>% 
              select(insolv, ch_bonitaet, p_bonitaet, n_downgrade, avg_bonitaet, age, refo_dummy, anzma, umsatz) %>% 
              map_df(.f = function(x) sum(!is.na(x))) %>% pivot_longer(everything(), names_to = "variable", values_to = "N")) %>% 
  left_join(df_panel %>%  filter(insolv==0) %>% 
              distinct(crefo, .keep_all = TRUE) %>% 
              select(insolv, ch_bonitaet, p_bonitaet, n_downgrade, avg_bonitaet, age, refo_dummy, anzma, umsatz) %>% 
              map_df(.f = function(x) sum(!is.na(x))) %>% pivot_longer(everything(), names_to = "variable", values_to = "N_firms")) %>% 
  left_join(df_panel %>% filter(insolv==0) %>%
              select(insolv, ch_bonitaet, p_bonitaet, n_downgrade, avg_bonitaet, age, refo_dummy, anzma, umsatz) %>% 
              map_df(.f = function(x) sd(x, na.rm=TRUE)) %>% pivot_longer(everything(), names_to = "variable", values_to = "sd")) %>% 
  select(variable, N, N_firms, minimum, q1, median, mean, q3, maximum, sd) %>% 
  mutate_if(is.numeric, round, 4) %>% as.data.frame()




df3_2 <- df_panel %>%
  filter(insolv==1) %>% 
  mutate(umsatz = umsatz/1000000) %>% 
  select(insolv, ch_bonitaet, p_bonitaet, n_downgrade, avg_bonitaet, age, refo_dummy, anzma, umsatz) %>% 
  map_df(.f = ~ broom::tidy(summary(.x)), .id = "variable") %>% 
  left_join(df_panel %>%  filter(insolv==1) %>% 
              select(insolv, ch_bonitaet, p_bonitaet, n_downgrade, avg_bonitaet, age, refo_dummy, anzma, umsatz) %>% 
              map_df(.f = function(x) sum(!is.na(x))) %>% pivot_longer(everything(), names_to = "variable", values_to = "N")) %>% 
  left_join(df_panel %>%  filter(insolv==1) %>% 
              distinct(crefo, .keep_all = TRUE) %>% 
              select(insolv, ch_bonitaet, p_bonitaet, n_downgrade, avg_bonitaet, age, refo_dummy, anzma, umsatz) %>% 
              map_df(.f = function(x) sum(!is.na(x))) %>% pivot_longer(everything(), names_to = "variable", values_to = "N_firms")) %>% 
  left_join(df_panel %>% filter(insolv==1) %>%
              select(insolv, ch_bonitaet, p_bonitaet, n_downgrade, avg_bonitaet, age, refo_dummy, anzma, umsatz) %>% 
              map_df(.f = function(x) sd(x, na.rm=TRUE)) %>% pivot_longer(everything(), names_to = "variable", values_to = "sd")) %>% 
  select(variable, N, N_firms, minimum, q1, median, mean, q3, maximum, sd) %>% 
  mutate_if(is.numeric, round, 4) %>% as.data.frame()



# Difference in means
df3_diff_sig <- df_panel %>% 
  select(insolv, ch_bonitaet, p_bonitaet, n_downgrade, avg_bonitaet, age, refo_dummy, anzma, umsatz) %>% 
  map_df(.f = function(x) indep_test(x, split_var='insolv')) %>% 
  pivot_longer(everything(), names_to = "variable", values_to = "value") %>% 
  mutate_if(is_double, function(x) case_when(x < 0.1 & x >= 0.05 ~ '*',
                                             x < 0.05 & x >= 0.01 ~ '**',
                                             x <= 0.01 ~ '***',
                                             x >= 0.1 ~ ""))


df3_diff <- df3_diff_sig %>% 
  mutate(value = df3_1$mean-df3_2$mean) %>% 
  mutate_if(is_double, function(x) round(x, 4)) %>% 
  mutate_if(is_double, function(x) sprintf("%0.4f", x))


df3_diff <- df3_diff %>% 
  bind_rows(df3_diff_sig) %>% 
  group_by(variable) %>% 
  mutate_at(.vars = vars(contains('value')), .funs = function(x) paste0(x, collapse = "")) %>% 
  ungroup() %>% 
  distinct()

df_final <- df3_1 %>% 
  select(variable, N, N_firms, minimum, mean, maximum) %>% 
  left_join(df3_2 %>% select(variable, N, N_firms, minimum, mean, maximum), by = "variable") %>% 
  left_join(df3_diff, by = "variable")

# Print results in latex format  
df_final %>%
  kable(format = 'latex', booktabs = TRUE, linesep = "", format.args = list(big.mark = ","))



### Pre-Crisis vs Crisis ##################################################



df2_1 <- df_panel %>%
  filter(treat_final==0) %>% 
  mutate(umsatz = umsatz/1000000) %>% 
  select(insolv, ch_bonitaet, p_bonitaet, n_downgrade, avg_bonitaet, age, refo_dummy, anzma, umsatz) %>% 
  map_df(.f = ~ broom::tidy(summary(.x)), .id = "variable") %>% 
  left_join(df_panel %>%  filter(treat_final==0) %>% 
              select(insolv, ch_bonitaet, p_bonitaet, n_downgrade, avg_bonitaet, age, refo_dummy, anzma, umsatz) %>% 
              map_df(.f = function(x) sum(!is.na(x))) %>%
              pivot_longer(everything(), names_to = "variable", values_to = "N")) %>% 
  left_join(df_panel %>%  filter(treat_final==0) %>% 
              distinct(crefo, .keep_all = TRUE) %>% 
              select(insolv, ch_bonitaet, p_bonitaet, n_downgrade, avg_bonitaet, age, refo_dummy, anzma, umsatz) %>% 
              map_df(.f = function(x) sum(!is.na(x))) %>%
              pivot_longer(everything(), names_to = "variable", values_to = "N_firms")) %>% 
  left_join(df_panel %>% filter(treat_final==0) %>%
              select(insolv, ch_bonitaet, p_bonitaet, n_downgrade, avg_bonitaet, age, refo_dummy, anzma, umsatz) %>% 
              map_df(.f = function(x) sd(x, na.rm=TRUE)) %>% 
              pivot_longer(everything(), names_to = "variable", values_to = "sd")) %>% 
  select(variable, N, N_firms, minimum, q1, median, mean, q3, maximum, sd) %>% 
  mutate_if(is.numeric, round, 4) %>% as.data.frame()

xtable::xtable(df2_1, digits=4)

# Insolvency rate
df_panel %>% 
  filter(treat_final==0) %>% 
  group_by(crefo) %>% 
  summarise(ins = max(insolv)) %>% 
  ungroup() %>% 
  summarise(ins_rate = mean(ins))
  

df2_2 <- df_panel %>%
  filter(treat_final==1) %>% 
  mutate(umsatz = umsatz/1000000) %>% 
  select(insolv, ch_bonitaet, p_bonitaet, n_downgrade, avg_bonitaet, age, refo_dummy, anzma, umsatz) %>% 
  map_df(.f = ~ broom::tidy(summary(.x)), .id = "variable") %>% 
  left_join(df_panel %>%  filter(treat_final==1) %>% 
              select(insolv, ch_bonitaet, p_bonitaet, n_downgrade, avg_bonitaet, age, refo_dummy, anzma, umsatz) %>% 
              map_df(.f = function(x) sum(!is.na(x))) %>% pivot_longer(everything(), names_to = "variable", values_to = "N")) %>% 
  left_join(df_panel %>%  filter(treat_final==1) %>% 
              distinct(crefo, .keep_all = TRUE) %>% 
              select(insolv, ch_bonitaet, p_bonitaet, n_downgrade, avg_bonitaet, age, refo_dummy, anzma, umsatz) %>% 
              map_df(.f = function(x) sum(!is.na(x))) %>% pivot_longer(everything(), names_to = "variable", values_to = "N_firms")) %>% 
  left_join(df_panel %>% filter(treat_final==1) %>%
              select(insolv, ch_bonitaet, p_bonitaet, n_downgrade, avg_bonitaet, age, refo_dummy, anzma, umsatz) %>% 
              map_df(.f = function(x) sd(x, na.rm=TRUE)) %>% pivot_longer(everything(), names_to = "variable", values_to = "sd")) %>% 
  select(variable, N, N_firms, minimum, q1, median, mean, q3, maximum, sd) %>% 
  mutate_if(is.numeric, round, 4) %>% as.data.frame()

xtable::xtable(df2_2, digits = 4)

# Insolvency rate
df_panel %>% 
  filter(treat_final==1) %>% 
  group_by(crefo) %>% 
  summarise(ins = max(insolv)) %>% 
  ungroup() %>% 
  summarise(ins_rate = mean(ins))

# Difference in means
df2_diff_sig <- df_panel %>% 
  select(insolv, ch_bonitaet, p_bonitaet, n_downgrade, avg_bonitaet, age, refo_dummy, anzma, umsatz) %>% 
  map_df(.f = function(x) indep_test(x, split_var='treat_final')) %>% 
  pivot_longer(everything(), names_to = "variable", values_to = "value") %>% 
  mutate_if(is_double, function(x) case_when(x < 0.1 & x >= 0.05 ~ '*',
                                             x < 0.05 & x >= 0.01 ~ '**',
                                             x <= 0.01 ~ '***',
                                             x >= 0.1 ~ ""))


df2_diff <- df2_diff_sig %>% 
  mutate(value = df2_1$mean-df2_2$mean) %>% 
  mutate_if(is_double, function(x) round(x, 4)) %>% 
  mutate_if(is_double, function(x) sprintf("%0.4f", x))

df2_diff <- df2_diff %>% 
  bind_rows(df2_diff_sig) %>% 
  group_by(variable) %>% 
  mutate_at(.vars = vars(contains('value')), .funs = function(x) paste0(x, collapse = "")) %>% 
  ungroup() %>% 
  distinct()

df_final <- df2_1 %>% 
  select(variable, N, N_firms, minimum, mean, maximum) %>% 
  left_join(df2_2 %>% select(variable, N, N_firms, minimum, mean, maximum), by = "variable") %>% 
  left_join(df2_diff, by = "variable")

# Print results in latex format  
df_final %>%
  kable(format = 'latex', booktabs = TRUE, linesep = "", format.args = list(big.mark = ","))


### Check Insolvency Rates over Time ######################################

df_res <- insolv_ym(df_panel)
df_res %>% print(n=40) %>% 
  mutate_if(is.numeric, round, 4) %>% as.data.frame()

plot_insolv(df_panel, end = '2020-07-31')

df_panel %>% 
  filter(exitdat >= '2020-05-01') %>% 
  filter(exitdat < '2020-07-01') %>% 
  select(exitdat, rechdat) %>% 
  mutate(exitdat2 = ymd(paste(year(exitdat),month(exitdat),01, '-'))) %>% 
  mutate(drop = ifelse(as.numeric(exitdat2-rechdat)<0,1,0)) %>% tab(drop)
  

## Size & Sector Distribution =============================================


df1 <- df_panel %>% 
  distinct(crefo, .keep_all = TRUE) %>% 
  select(csize_fct, wz12_fct) %>% 
  mutate(csize_fct = factor(ifelse(is.na(csize_fct), 'Unknown', csize_fct), 
                           levels = c( 'Micro-enterprise', 'Small enterprise', 'Medium-sized enterprise', 'Large enterprise')))

order_large_to_small <- names(sort(table(df1$wz12_fct), decreasing = TRUE))
df1 <- df1 %>% mutate(wz12_fct = factor(wz12_fct, levels = order_large_to_small))
  
df1 <- crosstab(dep = df1$wz12_fct, indep = df1$csize_fct, prop.r = TRUE, cell.layout = TRUE)
xtable::xtable(df1)



## Rating & PD ============================================================
pd <- read.xlsx("K:\\!Projekte\\BMWI-2020-Corona-KantarMUPWEB_133244\\Daten\\Creditreform\\Boni PD 03-19 zu 03-20 (punktgenau).xlsx",
                sheetIndex=1)

pd %>% as_tibble() %>% 
  mutate(Bonitätsklasse = case_when(Boni >= 100 & Boni <= 149 ~ '100-149',
                                    Boni >= 150 & Boni <= 199 ~ '150-199',
                                    Boni >= 200 & Boni <= 249 ~ '200-249',
                                    Boni >= 250 & Boni <= 299 ~ '250-299',
                                    Boni >= 300 & Boni <= 349 ~ '300-349',
                                    Boni >= 350 & Boni <= 499 ~ '350-499',
                                    Boni >= 500 ~ '500',
                                    #Boni == 600 ~ '600'
                                    )) %>% 
  group_by(Bonitätsklasse) %>% 
  summarise(Ausfallwahrscheinlichkeit = mean(PD.Boni.2.0)) %>% 
  mutate(colorgrad = as.numeric(rownames(.))^2) %>% 
  ggplot() +
  geom_bar(aes(x=Bonitätsklasse, y=Ausfallwahrscheinlichkeit, fill = colorgrad), stat='identity') +
  xlab('Credit rating index') +
  ylab('Average probability of default') +
  scale_fill_gradient2(low = 'darkgreen', mid = 'yellow', high = 'red',
                       midpoint = 20
  ) +
  theme_jod + 
  theme(legend.position = 'none')

pd %>%
  mutate(colorgrad = as.numeric(rownames(.))^4) %>% 
  ggplot() +
  geom_histogram(aes(x=Boni, y=PD.Boni.2.0, fill = colorgrad), stat='identity') +
  xlab('Credit rating index') +
  ylab('Probability of default') +
  xlim(c(100,601)) +
  scale_x_continuous(breaks = c(100,200,300,400,500,600)) +
  scale_fill_gradient2(low = 'darkgreen', mid = 'yellow', high = 'red',
                       midpoint = 300
  ) +
  theme_jod+ 
  theme(legend.position = 'none')



## Change in Rating Index ==================================================

tikz(file.path(getwd(), "04_Writing", "01_Figures", "fig_dist.tex"),
     height = 3.5,
     width = 6)

df_panel %>% 
  mutate(treat_fct = factor(ifelse(treat_final==0, 'pre-crisis', 'crisis'),
                     levels = c('pre-crisis', 'crisis'))) %>% 
  ggplot(aes(x=ch_bonitaet)) + 
  #geom_histogram(aes(y=..density..), na.rm = TRUE, colour="black", fill="white", bins = 50) +
  geom_histogram(aes(y=..density..), na.rm = TRUE, colour="white", fill="#00204D", bins = 50) +
  #geom_histogram(na.rm = TRUE, colour="black", fill="white", bins = 50) +
  #geom_density(alpha=.2, fill="#FF6666") +
  facet_wrap(~treat_fct, nrow = 1) + 
  #labs(title = 'Changes in Credit Rating', subtitle = 'According to Creditreform\'s Date of Reassesment') +
  #xlab('Credit rating update') +
  #xlab(bquote("Credit rating update" ~  "("*italic(Delta*r[t])*")")) + 
  xlab("Credit rating update ($\\Delta r_t$)") + 
  ylab('Density') +
  #scale_y_log10() +
  scale_x_continuous(breaks = c(-350, -300, -250, -200, -150, -100, -50, 0, 50, 100, 150, 200, 250, 300, 350), 
                     lim = c(-360, 360)
                     ) + 
  scale_y_continuous(lim = c(0,0.05), expand = c(0, 0)) + 
  theme_jod +
  geom_hline(yintercept = 0, color = "grey90", size = 0.5)

dev.off()

# Skewness  
df_panel %>% 
  group_by(treat_final) %>% 
  summarise(skew = skewness(ch_bonitaet)) %>% 
  ungroup()


## OECD table =============================================================
df_oecd <- read.xlsx(file = file.path(getwd(), '04_Writing', 'tab_OECD.xlsx'), sheetIndex = 2) %>% 
  as_tibble() %>% 
  mutate(overall_impulse = Immediate.fiscal.impulse + Deferral + Other.liquidity.guarantee) %>% 
  select(-c('Last.update')) %>% 
  mutate_if(.predicate = is_double, .funs = function(x) x*100)

order_large_to_small <- df_oecd %>% arrange(overall_impulse) %>% select(Country) %>% as_vector()


tikz(file.path(getwd(), "04_Writing", "01_Figures", "fig_oecd.tex"),
     height = 3.5,
     width = 6)

df_oecd %>%
  mutate(Country = factor(Country, 
                          levels = order_large_to_small)) %>% 
  pivot_longer(cols = c(Immediate.fiscal.impulse, Deferral, Other.liquidity.guarantee), 
               names_to = "Fiscal response", 
               values_to = "\\% of 2019 GDP") %>%
  ggplot() +
  geom_bar(aes(x=Country, y=`\\% of 2019 GDP`, fill=`Fiscal response`), 
           position='stack', 
           stat = 'identity') +
  scale_fill_viridis(discrete = TRUE, 
                     option = "E", 
                     breaks = c("Other.liquidity.guarantee", "Immediate.fiscal.impulse", "Deferral"),
                     labels = c("Other liquidity/guarantee", "Immediate fiscal impulse", "Deferral")) +
  scale_y_continuous(limits = c(0,50), expand = c(0,0)) +
  coord_flip() +
  theme_jod +
  theme(legend.justification = "left", 
        legend.title = element_blank(),
        legend.box.margin=margin(-5,0,-10,0),) +
  guides(fill = guide_legend(title.position = "top"))

dev.off()


## Economist table ========================================================

df_econ <- read.xlsx(file = file.path(getwd(), '04_Writing', 'tab_Economist.xlsx'), sheetIndex = 1) %>% 
  as_tibble() 

#order_large_to_small <- df_oecd %>% arrange(overall_impulse) %>% select(Country) %>% as_vector()


tikz(file.path(getwd(), "04_Writing", "01_Figures", "fig_econ.tex"),
     height = 3.5,
     width = 6)

df_econ %>%
  mutate(Country = factor(Country,
                          levels = c("Japan", "United States", "UK", "France", "Germany")[5:1])) %>%
  ggplot() +
  geom_bar(aes(x=Country, y=Change, fill=Months), 
           position = "dodge",
           stat = 'identity',
           width = 0.5) +
  scale_fill_viridis(discrete = TRUE,
                     option = "E",
                     breaks = c("Jan_Aug", "Aug"),
                     labels = c("Jan-Aug 2020, \\% decrease on same period a year earlier",
                                "Aug 2020, \\% decrease on a year earlier")) +
  scale_y_continuous(limits = c(-45, 0), expand = c(0,0)) +
  coord_flip() +
  ylab("Change (in \\%)") +
  theme_jod +
  theme(legend.justification = "left", 
        legend.title = element_blank(),
        legend.box.margin=margin(-5,0,-10,0)) +
  guides(fill = guide_legend(title.position = "top", nrow = 2))

dev.off()




## Data flow visualization ================================================

viridis_map <- viridis.map %>% as_tibble()

viridis_map_E <- viridis_map %>% filter(opt=="E")
rgb(viridis_map_E$R, viridis_map_E$G, viridis_map_E$B, maxColorValue = 360)


tikz(file.path(getwd(), "04_Writing", "01_Figures", "fig_data.tex"),
     height = 4,
     width = 6)
grViz("
    digraph data_viz {
    
    
    forcelabels=true;
    
        # arrow node statements
        node [shape = plaintext,
              fontwidth = 2,
              fontname = ModernComputer,
              width = 1.5,
              penwidth = 2,
              color = '#7C7B78']
        f [label = < <B>Micro level</B> >]
        g [label = < <B>Sector-size level</B> >]


        # table node statements
        node [shape = cylinder,
              fontname = ModernComputer,
              width = 2,
              penwidth = 2,
              color = '#7C7B78']
        a [label = 'Mannheim Enterprise Panel']
        

        # cylinder node statements
        node [shape = cylinder,
              fontname = ModernComputer,
              width = 2,
              penwidth = 2,
              color = '#7C7B78']
        a [label = 'Mannheim Enterprise Panel']
        

        
        # box node statements
        node [shape = box,
              fontname = ModernComputer,
              width = 3,
              penwidth = 2,
              color = '#7C7B78']
        b [label = <Survey Data <BR/> <FONT POINT-SIZE='10'><i>N </i> = 2,344 firms</FONT>>]
        c [label = <Credit Rating Data <BR/> <FONT POINT-SIZE='10'><i>N </i> ~ 2.4M rating updates</FONT>>]
        d [label = 'COVID-19 Exposure Index']
        e [label = 'Insolvency Gap Estimation']
        
        
       

        # edge statements
        f -> b [style=invis]
        g -> d [style=invis]
        a -> b [
        minlen=2
        labeldistance=5
        labelangle=0
        headlabel=<
                    <table bgcolor='white' border='0'>
                      <tr>
                        <td>Sampling</td>
                      </tr>
                    </table>
                  >
              ]
        
        

        a -> c [
        minlen=2
        labeldistance=5
        labelangle=0
        headlabel=<
                    <table bgcolor='white' border='0'>
                      <tr>
                        <td>Actively rated firms</td>
                      </tr>
                    </table>
                  >
              ]
        b -> d [
        minlen=2
        labeldistance=4
        labelangle=0
        headlabel=<
                    <table bgcolor='white' border='0'>
                      <tr>
                        <td>Regression</td>
                      </tr>
                    </table>
                  >
              ]
        c -> e [
        minlen=2
        labeldistance=4
        labelangle=0
        headlabel=<
                    <table bgcolor='white' border='0'>
                      <tr>
                        <td>Matching &amp; Outcome Analysis</td>
                      </tr>
                    </table>
                  >
              ]


        # define ranks
        subgraph {
            rank = same; a
        }

        subgraph {
            rank = same; f; b; c
        }
        subgraph {
            rank = same; g; d; e
        }
        
        
    }
")
dev.off()





## Matching visualization =================================================
# Choose examples
# Get matches given treatment 
data_match <- df_match %>% 
  filter(wz12_fct == "Accommodation & catering" & csize_fct == "Micro-enterprise") %>% 
  select(data_match) %>% 
  unnest(cols = data_match) %>% 
  mutate(PD = as.double(NA))
  

crefo_id <- data_match %>%  
  filter(treat_final==1) %>% 
  select(id) %>% as_vector()

matrix_match <- df_match %>% 
  filter(wz12_fct == "Accommodation & catering" & csize_fct == "Micro-enterprise") %>% 
  select(matrix_match) %>% 
  unnest(cols = matrix_match)

for (i in 1:length(crefo_id)){
  crefos <- matrix_match %>% filter(treated == crefo_id[i]) %>% as_vector()
  pd <- sum(data_match %>% filter(id %in% crefos) %>% select(insolv))/length(crefos)
  data_match[data_match$id == crefo_id[i], "PD"] <- pd
  
  if(i %% 1000 == 0) print(i)
  
}





# 2012360926_2020, 2012580541_2020 # non-insolvent
data_match %>%
  arrange(desc(PD)) %>%
  filter(PD<0.5) %>% 
  filter(insolv==0) %>% 
  print(n=40)
# 2012157100_2020, 4250624378_2020 # insolvent
data_match %>%
  arrange(desc(PD)) %>% 
  print(n=40)


id1 <- data_match %>% 
  filter(id %in% (matrix_match %>%
                  filter(treated == "2012572315_2020") %>% as_vector())) %>% 
  arrange(desc(treat_final)) %>% 
  select(id)

id2 <- data_match %>% 
  filter(id %in% (matrix_match %>%
                    filter(treated == "2012395229_2020") %>% as_vector())) %>% 
  arrange(desc(treat_final)) %>% 
  select(id)

df_match %>% filter(wz12_fct == "Accommodation & catering" & csize_fct == "Micro-enterprise") %>% 
  select(data) %>% 
  unnest(cols = data) %>% 
  mutate(id = paste(crefo, jahr, sep  = '_')) %>% 
  select(id, crefo, rechdat, exitdat, insolv, treat_final) %>% 
  filter(id %in% c(data_match %>% 
           filter(PD > 0.6) %>% filter(treat_final==1) %>% select(id) %>% as_vector())) %>% 
  print(n=168)


# Create shading data
dateRanges <- data.frame(
  from=as.Date(c('2020-01-01')), 
  to=as.Date(c('2020-04-01'))
) %>% as_tibble()

dateRanges2 <- data.frame(
  label = c("Matched control observations from pre-crisis period", "Firm observed in crisis"),
  from=as.Date(c('2017-07-01' ,'2020-04-01'))
  #from=as.Date(c('2017-07-01' ,'2020-04-01')), 
  #to=as.Date(c('2020-01-01', '2020-07-31'))
) %>% as_tibble()


Sys.setlocale("LC_ALL", "English")

df_plot <- df_match %>% dplyr::filter(wz12_fct == "Accommodation & catering" & csize_fct == "Micro-enterprise") %>% 
  select(data) %>% 
  unnest(cols = data) %>% 
  mutate(id = paste(crefo, jahr, sep  = '_')) %>% 
  select(id, crefo, rechdat, exitdat, insolv, treat_final) %>% 
  mutate(exitdat = ymd(ifelse(is.na(exitdat), as.character(rechdat + 30*4), as.character(exitdat)))) %>% 
  dplyr::filter(id %in% as_vector(id1)) %>% 
  mutate(example = "Example I: non-insolvent firm in crisis period") %>% 
  mutate(insolv = factor(insolv, levels = c(0,1), labels = c("non-insolvent", "insolvent"))) %>% 
  arrange(treat_final, desc(rechdat)) %>% 
  bind_rows(df_match %>% dplyr::filter(wz12_fct == "Accommodation & catering" & csize_fct == "Micro-enterprise") %>% 
              select(data) %>% 
              unnest(cols = data) %>% 
              mutate(id = paste(crefo, jahr, sep  = '_')) %>% 
              select(id, crefo, rechdat, exitdat, insolv, treat_final) %>% 
              mutate(exitdat = ymd(ifelse(is.na(exitdat), as.character(rechdat + 30*4), as.character(exitdat)))) %>% 
              dplyr::filter(id %in% as_vector(id2)) %>% 
              mutate(example = "Example II: insolvent firm in crisis period") %>% 
              mutate(insolv = factor(insolv, levels = c(0,1), labels = c("non-insolvent", "insolvent"))) %>% 
              arrange(treat_final, desc(rechdat))) %>% 
  mutate(rechdat_aes = "rating update") %>% 
  #group_by(insolv, treat_final) %>% 
  #slice_head(n = 10) %>% 
  #ungroup() %>% 
  mutate(Firm = factor(group_indices(., factor(crefo, levels = unique(crefo)))))


# Some manipulation
df_plot <- df_plot %>% 
  mutate(rating2exit = as.numeric(exitdat - rechdat)) %>% 
  mutate(exitdat = case_when(rating2exit < 100 ~ exitdat + 40,
                             rating2exit >= 100 ~ exitdat))


tikz(file.path(getwd(), "04_Writing", "01_Figures", "fig_matching.tex"),
     height = 3.5,
     width = 6)

df_plot %>% 
  ggplot() +
  scale_y_date(date_breaks = "3 months", limits = ymd(c('2017-07-01','2020-12-31')),
               date_labels = "%b. %Y") +
  geom_linerange(aes(x = Firm, ymin = rechdat, ymax = exitdat-7, color = insolv, group = insolv), size = 1) +
  geom_point(aes(x = Firm, y = exitdat, color = insolv, shape = insolv, group = insolv), size = 2) +
  geom_point(aes(x = Firm, y = rechdat, shape = rechdat_aes, group = insolv), size = 2) +
  geom_hline(yintercept = ymd("2020-07-31"), linetype = "dashed") +
  scale_shape_manual(name = "Treatment & State",
                     values = c(18,1,13), 
                     breaks= c("rating update", "non-insolvent", 'insolvent'),
                     labels = c("rating update", "non-insolvent", 'insolvent')) + 
  scale_color_manual(name = "Treatment & State",
                     values = c("black", "#66BD63", "#D73027"), 
                     breaks= c("rating update", "non-insolvent", 'insolvent'),
                     labels = c("rating update", "non-insolvent", 'insolvent')) + 
  geom_rect(data = dateRanges, aes(ymin = from, ymax = to, xmin = -Inf, xmax = Inf), alpha = 0.4) +
  #scale_color_viridis(discrete = TRUE, 
  #                   option = "E",
  #                   breaks = c("non-insolvent", "insolvent"),
  #                   labels = c("non-insolvent", "insolvent")) +
  ylab("Date") +
  xlab("Firm identifier") +
  coord_flip() +
  facet_wrap(~example, nrow=2, scales = "free_y") +
  guides(color = FALSE,
         shape = guide_legend(title.position = "top", 
                              override.aes = list(color = c("black", "#66BD63", "#D73027")))) +
  theme_jod +
  theme(panel.grid.minor.y = element_blank(),
        legend.justification = "left", 
        legend.title = element_blank(),
        #plot.margin=unit(c(0.5,0.5,1.5,0.5),"cm")
        ) +
  scale_x_discrete(breaks = 1:12,
                   labels = c(
                     "Control 5", "Control 4", "Control 3", "Control 2", "Control 1", "1",
                     "Control 10", "Control 9", "Control 8", "Control 7", "Control 6", "2"
                              ))

dev.off()

df_plot
grid.locator(unit="native") 
bottom_y <- 450
bottom_y2 <- 450 
grid.brackets(125, bottom_y, 610, bottom_y, lwd=1, curvature = 0.2, h = -0.05)
grid.brackets(657, bottom_y, 721, bottom_y, lwd=1, curvature = 0.8, h = -0.05)
#grid.text(x = 500, y = bottom_y2, label = "Matched observation from pre-crisis period", gp=gpar(fontsize=20))







# Matching ----------------------------------------------------------------
#load(file.path(getwd(), '05_Model', 'df_match.RData')) # very good
#load(file.path(getwd(), '05_Model', 'df_match_imp.RData')) # very good
#load(file.path(getwd(), '05_Model', 'df_match_drop_ins.RData')) # very good
#load(file.path(getwd(), '05_Model', 'df_match_imp_drop_ins.RData')) # best
#load(file.path(getwd(), '05_Model', 'df_match_V1.RData')) # not so good
#load(file.path(getwd(), '05_Model', 'df_match_calipered.RData')) # ok

# Clean WZ descriptions
df_match <- df_match %>% mutate(wz12_fct = case_when(wz12_fct == 'Others' ~ 'Others',
                                                     wz12_fct == 'Mechanical engineering' ~ 'Mechanical engineering',
                                                     wz12_fct == 'Manufacturing of chemical or pharmaceutical products' ~ 'Chemicals & pharmaceuticals', 
                                                     wz12_fct == 'Manufacturing of data processing equipment' ~ 'Manufacturing of data processing equipment', 
                                                     wz12_fct == 'Food production' ~ 'Food production',
                                                     wz12_fct == 'Other manufacturing industries' ~ 'Manufacturing',
                                                     wz12_fct == 'Wholesale and retail trade (incl. car repair)' ~ 'Wholesale & retail trade',
                                                     wz12_fct == 'Accommodation & catering' ~ 'Accommodation & catering',
                                                     wz12_fct == 'Insurance & banking' ~ 'Insurance & banking',
                                                     wz12_fct == 'Logistics & transport (incl. postal service)' ~ 'Logistics & transport',
                                                     wz12_fct == 'Creative industry & entertainment' ~ 'Creative industry & entertainment', 
                                                     wz12_fct == 'Other business-related services' ~ 'Business-related services',
                                                     wz12_fct == 'Health and social services' ~ 'Health & social services'))

# Quick overview of results
df_match %>% mutate_if(is_double, function(x) round(x,4)) %>% arrange(csize_fct) %>% print(n=52)





## Main ===================================================================
load(file.path(getwd(), '05_Model', 'df_match.RData')) # very good
df_match <- df_match %>% 
  mutate(mean_diff = -1*mean_diff,
         mean_w_diff = -1*mean_w_diff)

# Clean WZ descriptions
df_match <- df_match %>% mutate(wz12_fct = case_when(wz12_fct == 'Others' ~ 'Others',
                                                     wz12_fct == 'Mechanical engineering' ~ 'Mechanical engineering',
                                                     wz12_fct == 'Manufacturing of chemical or pharmaceutical products' ~ 'Chemicals & pharmaceuticals', 
                                                     wz12_fct == 'Manufacturing of data processing equipment' ~ 'Manufacturing of data processing equipment', 
                                                     wz12_fct == 'Food production' ~ 'Food production',
                                                     wz12_fct == 'Other manufacturing industries' ~ 'Manufacturing',
                                                     wz12_fct == 'Wholesale and retail trade (incl. car repair)' ~ 'Wholesale & retail trade',
                                                     wz12_fct == 'Accommodation & catering' ~ 'Accommodation & catering',
                                                     wz12_fct == 'Insurance & banking' ~ 'Insurance & banking',
                                                     wz12_fct == 'Logistics & transport (incl. postal service)' ~ 'Logistics & transport',
                                                     wz12_fct == 'Creative industry & entertainment' ~ 'Creative industry & entertainment', 
                                                     wz12_fct == 'Other business-related services' ~ 'Business-related services',
                                                     wz12_fct == 'Health and social services' ~ 'Health & social services'))

### Actual vs Counterfactual ##############################################

df_plot <- df_match %>% dplyr::select(wz12_fct, csize_fct, cont_mean_w, treat_mean) %>% 
  pivot_longer(cols = c(cont_mean_w, treat_mean), names_to = 'period', values_to = 'ins_rate') %>% 
  mutate(period = ifelse(period == "treat_mean", "Actual", "Counterfactual")) %>% 
  mutate(csize_fct = factor(csize_fct, levels = c('Micro-enterprise', 'Small enterprise', 'Medium-sized enterprise', 'Large enterprise')))

sector_level <- df_plot %>% mutate(wz12_fct = str_replace_all(wz12_fct, "&", "\\\\&")) %>% 
  filter(csize_fct == "Micro-enterprise") %>% 
  filter(period == "Counterfactual") %>% 
  arrange(ins_rate) %>% 
  dplyr::select(wz12_fct) %>% 
  as_vector()



tikz(file.path(getwd(), "04_Writing", "01_Figures", "fig_gap.tex"),
     height = 3.5,
     width = 6.5)

df_plot %>% mutate(wz12_fct = str_replace_all(wz12_fct, "&", "\\\\&")) %>% 
  mutate(wz12_fct = factor(wz12_fct, levels = sector_level)) %>% 
  ggplot() + 
  geom_bar(aes(wz12_fct, ins_rate, fill = period), stat='identity', position = 'dodge') +
  scale_y_continuous(labels = c(0.01, 0.02), 
                     breaks = c(0.01, 0.02),
                     expand = c(0,0), 
                     limits = c(0,max(df_plot$ins_rate)*1.1)) + 
  coord_flip() +
  xlab('Sector') +
  ylab('Insolvency rate') +
  facet_grid(~csize_fct, labeller = label_wrap_gen(width=10)) +
  scale_fill_viridis(discrete = TRUE, 
                     option = "E", 
                     breaks = c("Counterfactual", "Actual"),
                     labels = c("Counterfactual", "Actual")) +
  theme_jod +
  theme(axis.text.y = element_text(size=7),
        legend.justification = "left", 
        legend.title = element_blank())
dev.off()

### Insolvency Gap ########################################################
df_gap <- df_match %>% dplyr::select(wz12_fct, csize_fct, mean_w_diff) %>% 
  pivot_wider(names_from = csize_fct, values_from = c(mean_w_diff)) %>% 
  dplyr::select(wz12_fct, "Micro-enterprise", "Small enterprise", "Medium-sized enterprise", "Large enterprise") %>% 
  arrange(factor(wz12_fct, levels = sector_level[length(sector_level):1])) %>% 
  mutate_if(is_double, function(x) round(x, 4)) %>% 
  mutate_if(is_double, function(x) sprintf("%+0.4f", x))

df_gap


df_gap_sig <- df_match %>% dplyr::select(wz12_fct, csize_fct, p_value_ch2_w) %>% 
  pivot_wider(names_from = csize_fct, values_from = p_value_ch2_w) %>% 
  dplyr::select(wz12_fct, "Micro-enterprise", "Small enterprise", "Medium-sized enterprise", "Large enterprise") %>% 
  arrange(factor(wz12_fct, levels = sector_level[length(sector_level):1])) %>% 
  #mutate_if(is_double, function(x) round(x, 4))
  mutate_if(is_double, function(x) case_when(x < 0.1 & x >= 0.05 ~ '*',
                                             x < 0.05 & x >= 0.01 ~ '**',
                                             x <= 0.01 ~ '***',
                                             x >= 0.1 ~ ""))
df_gap_final <- df_gap %>% 
  bind_rows(df_gap_sig) %>% 
  group_by(wz12_fct) %>% 
  mutate_at(.vars = vars(contains('enterprise')), .funs = function(x) paste0(x, collapse = "")) %>% 
  ungroup() %>% 
  distinct() %>% 
  mutate_at(.vars = vars(contains('enterprise')), .funs = function(x) na_if(x, "+0.0000NA")) %>% 
  mutate_at(.vars = vars(contains('enterprise')), .funs = function(x) replace_na(x, "0.0000")) 

# Print results in latex format  
df_gap_final %>%
  kable(format = 'latex', booktabs = TRUE, linesep = "\\addlinespace")


# in pp
df_gap <- df_match %>% dplyr::select(wz12_fct, csize_fct, mean_w_diff) %>%
  mutate(mean_w_diff = mean_w_diff*100) %>% 
  pivot_wider(names_from = csize_fct, values_from = c(mean_w_diff)) %>% 
  dplyr::select(wz12_fct, "Micro-enterprise", "Small enterprise", "Medium-sized enterprise", "Large enterprise") %>% 
  arrange(factor(wz12_fct, levels = sector_level[length(sector_level):1])) %>% 
  mutate_if(is_double, function(x) round(x, 4)) %>% 
  mutate_if(is_double, function(x) sprintf("%+0.4f", x))

df_gap


df_gap_sig <- df_match %>% dplyr::select(wz12_fct, csize_fct, p_value_ch2_w) %>% 
  pivot_wider(names_from = csize_fct, values_from = p_value_ch2_w) %>% 
  dplyr::select(wz12_fct, "Micro-enterprise", "Small enterprise", "Medium-sized enterprise", "Large enterprise") %>% 
  arrange(factor(wz12_fct, levels = sector_level[length(sector_level):1])) %>% 
  #mutate_if(is_double, function(x) round(x, 4))
  mutate_if(is_double, function(x) case_when(x < 0.1 & x >= 0.05 ~ '*',
                                             x < 0.05 & x >= 0.01 ~ '**',
                                             x <= 0.01 ~ '***',
                                             x >= 0.1 ~ ""))
df_gap_final <- df_gap %>% 
  bind_rows(df_gap_sig) %>% 
  group_by(wz12_fct) %>% 
  mutate_at(.vars = vars(contains('enterprise')), .funs = function(x) paste0(x, collapse = "")) %>% 
  ungroup() %>% 
  distinct() %>% 
  mutate_at(.vars = vars(contains('enterprise')), .funs = function(x) na_if(x, "+0.0000NA")) %>% 
  mutate_at(.vars = vars(contains('enterprise')), .funs = function(x) replace_na(x, "0.0000")) %>% 
  mutate_at(.vars = vars(contains('enterprise')), .funs = function(x) if_else(str_detect(x, "\\+.{1,}\\*{1,}"), paste0("\textcolor{red}{" ,x ,"}"), x)) %>% 
  mutate_at(.vars = vars(contains('enterprise')), .funs = function(x) if_else(str_detect(x, "-.{1,}\\*{1,}"), paste0("\textcolor{green}{" ,x ,"}"), x))

# Print results in latex format  
df_gap_final %>%
  dplyr::select(-c(contains("Large"))) %>% 
  kable(format = 'latex', booktabs = TRUE, linesep = "\\addlinespace", escape = FALSE)

# in %
df_gap <- df_match %>% dplyr::select(wz12_fct, csize_fct, cont_mean_w, treat_mean, mean_w_diff) %>% 
  mutate(percent_w_diff = (cont_mean_w-treat_mean)/treat_mean*100) %>% 
  dplyr::select(wz12_fct, csize_fct, percent_w_diff) %>% 
  pivot_wider(names_from = csize_fct, values_from = c(percent_w_diff)) %>% 
  dplyr::select(wz12_fct, "Micro-enterprise", "Small enterprise", "Medium-sized enterprise", "Large enterprise") %>% 
  arrange(factor(wz12_fct, levels = sector_level[length(sector_level):1])) %>% 
  mutate_if(is_double, function(x) round(x, 4)) %>% 
  mutate_if(is_double, function(x) sprintf("%+0.4f", x))

df_gap


df_gap_sig <- df_match %>% dplyr::select(wz12_fct, csize_fct, p_value_ch2_w) %>% 
  pivot_wider(names_from = csize_fct, values_from = p_value_ch2_w) %>% 
  dplyr::select(wz12_fct, "Micro-enterprise", "Small enterprise", "Medium-sized enterprise", "Large enterprise") %>% 
  arrange(factor(wz12_fct, levels = sector_level[length(sector_level):1])) %>% 
  #mutate_if(is_double, function(x) round(x, 4))
  mutate_if(is_double, function(x) case_when(x < 0.1 & x >= 0.05 ~ '*',
                                             x < 0.05 & x >= 0.01 ~ '**',
                                             x <= 0.01 ~ '***',
                                             x >= 0.1 ~ ""))
df_gap_final <- df_gap %>% 
  bind_rows(df_gap_sig) %>% 
  group_by(wz12_fct) %>% 
  mutate_at(.vars = vars(contains('enterprise')), .funs = function(x) paste0(x, collapse = "")) %>% 
  ungroup() %>% 
  distinct() %>% 
  mutate_at(.vars = vars(contains('enterprise')), .funs = function(x) na_if(x, "+0.0000NA")) %>% 
  mutate_at(.vars = vars(contains('enterprise')), .funs = function(x) replace_na(x, "0.0000")) 

# Print results in latex format  
df_gap_final %>%
  kable(format = 'latex', booktabs = TRUE, linesep = "\\addlinespace")



# gap by sector
df_gap_sector <- df_match %>% 
  mutate(N =sapply(df_match$data_match, function(x) NROW(x))) %>% 
  dplyr::select(wz12_fct, csize_fct, cont_mean_w, treat_mean, mean_w_diff, N) %>% 
  group_by(wz12_fct) %>% 
  summarise(
    sec_cont_mean_w = weighted.mean(cont_mean_w, N),
    sec_treat_mean = weighted.mean(treat_mean, N),
    sec_mean_w_diff = weighted.mean(mean_w_diff, N)) %>% 
  mutate(mean_factor = (sec_cont_mean_w-sec_treat_mean)/sec_treat_mean + 1) %>% 
  dplyr::select(-c(sec_mean_w_diff)) %>% 
  mutate_if(is.double, function(x) round(x, 4))

df_gap_sector %>% 
  kable(format = 'latex', booktabs = TRUE, linesep = "\\addlinespace")


### Weighted Average Across Sectors #######################################



df_weights <- df_match %>%
  mutate(N = pmap_dbl(.l = list(data_match), .f = function(x) tryCatch({ nrow(unnest(x))}, error = function(e){NA}))) %>% 
  dplyr::select(wz12_fct, csize_fct, N, mean_w_diff) %>% 
  filter(!(csize_fct == "Large enterprise")) 

write.xlsx(df_weights, file = file.path(getwd(), "02_Data", "03_SanityChecks", "df_weights.xlsx"))

df_weights
  group_by(csize_fct) %>% 
  summarise(avg_ins_rate = weighted.mean(mean_w_diff, N, na.rm = TRUE)) %>% 
  
  ungroup() %>% 
  mutate_if(is_double, function(x) round(x,6)*100) %>% 
  mutate(csize_fct = factor(csize_fct, 
                            levels = c("Micro-enterprise", "Small enterprise", "Medium-sized enterprise", "Large enterprise"))) %>% 
  


### Balance Assessment ###################################################################
load(file.path(getwd(), '05_Model', 'df_balance.RData'))
df_match <- df_match %>% mutate(wz12_fct = case_when(wz12_fct == 'Others' ~ 'Others',
                                                     wz12_fct == 'Mechanical engineering' ~ 'Mechanical engineering',
                                                     wz12_fct == 'Manufacturing of chemical or pharmaceutical products' ~ 'Chemicals & pharmaceuticals', 
                                                     wz12_fct == 'Manufacturing of data processing equipment' ~ 'Manufacturing of data processing equipment', 
                                                     wz12_fct == 'Food production' ~ 'Food production',
                                                     wz12_fct == 'Other manufacturing industries' ~ 'Manufacturing',
                                                     wz12_fct == 'Wholesale and retail trade (incl. car repair)' ~ 'Wholesale & retail trade',
                                                     wz12_fct == 'Accommodation & catering' ~ 'Accommodation & catering',
                                                     wz12_fct == 'Insurance & banking' ~ 'Insurance & banking',
                                                     wz12_fct == 'Logistics & transport (incl. postal service)' ~ 'Logistics & transport',
                                                     wz12_fct == 'Creative industry & entertainment' ~ 'Creative industry & entertainment', 
                                                     wz12_fct == 'Other business-related services' ~ 'Business-related services',
                                                     wz12_fct == 'Health and social services' ~ 'Health & social services')) 

# Prepare df
df_balance <- df_match %>% 
  #filter(row_number() %in% c(1,2)) %>% 
  select(wz12_fct, csize_fct, balance_stats) %>% 
  unnest(cols = balance_stats, keep_empty = TRUE) %>% 
  filter(type=="improvement") %>% 
  select_if(~sum(!is.na(.))>0) %>% 
  mutate_if(is.double, function(x) round(x,0)) %>% 
  select(-c(type)) %>%
  select(wz12_fct, csize_fct, variable, "Std. Mean Diff.", "eCDF Mean", "Var. Ratio") %>% 
  pivot_wider(names_from = variable, values_from = c("Std. Mean Diff.", "eCDF Mean", "Var. Ratio"), names_sep = ":") %>% 
  mutate(csize_fct = str_remove(csize_fct, pattern = regex(".{1}enterprise"))) %>% 
  mutate(csize_fct = str_remove(csize_fct, pattern = regex("-sized"))) %>% 
  mutate(csize_fct = factor(csize_fct, levels = c("Micro", "Small", "Medium", "Large"))) %>% 
  arrange(wz12_fct, csize_fct)

# Number of worsenings
df_balance %>% 
  select(contains("Std. Mean Diff")) %>% 
  mutate_all(function(x) x < 0) %>% 
  summarise_all(cumsum) %>% 
  filter(row_number()==n()) %>% 
  sum()

df_balance %>% 
  select(contains("eCDF Mean")) %>% 
  mutate_all(function(x) x < 0) %>% 
  summarise_all(cumsum) %>% 
  filter(row_number()==n()) %>% 
  sum()

df_balance %>% 
  select(contains("Var.")) %>% 
  mutate_all(function(x) x < 0) %>% 
  summarise_all(cumsum) %>% 
  filter(row_number()==n()) %>% 
  sum()

# Prepare df (cont')
df_balance <- df_match %>% 
  #filter(row_number() %in% c(1,2)) %>% 
  select(wz12_fct, csize_fct, balance_stats) %>% 
  unnest(cols = balance_stats, keep_empty = TRUE) %>% 
  filter(type=="improvement") %>% 
  select_if(~sum(!is.na(.))>0) %>% 
  mutate_if(is.double, function(x) round(x,0)) %>% 
  select(-c(type)) %>%
  select(wz12_fct, csize_fct, variable, "eCDF Mean", "Var. Ratio") %>% 
  pivot_wider(names_from = variable, values_from = c("eCDF Mean", "Var. Ratio"), names_sep = ":") %>% 
  mutate(csize_fct = str_remove(csize_fct, pattern = regex(".{1}enterprise"))) %>% 
  mutate(csize_fct = str_remove(csize_fct, pattern = regex("-sized"))) %>% 
  mutate(csize_fct = factor(csize_fct, levels = c("Micro", "Small", "Medium", "Large"))) %>% 
  arrange(wz12_fct, csize_fct) %>% 
  select(-c(contains("Var."))) %>% 
  left_join(df_match %>% 
              #filter(row_number() %in% c(1,2)) %>% 
              select(wz12_fct, csize_fct, balance_stats) %>% 
              unnest(cols = balance_stats, keep_empty = TRUE) %>% 
              filter(type=="matched") %>% 
              select_if(~sum(!is.na(.))>0) %>% 
              mutate_if(is.double, function(x) round(x,2)) %>% 
              select(-c(type)) %>%
              select(wz12_fct, csize_fct, variable, "eCDF Mean", "Var. Ratio") %>% 
              pivot_wider(names_from = variable, values_from = c("eCDF Mean", "Var. Ratio"), names_sep = ":") %>% 
              mutate(csize_fct = str_remove(csize_fct, pattern = regex(".{1}enterprise"))) %>% 
              mutate(csize_fct = str_remove(csize_fct, pattern = regex("-sized"))) %>% 
              mutate(csize_fct = factor(csize_fct, levels = c("Micro", "Small", "Medium", "Large"))) %>% 
              arrange(wz12_fct, csize_fct) %>% 
              select(-c(contains("eCDF"))), 
            by = c("wz12_fct", "csize_fct"))

df_balance %>% 
  kable(format = 'latex', booktabs = TRUE, linesep = "", format.args = list(big.mark = ","))



## Gap in Absolute Numbers ================================================
df_insYear <- readxl::read_excel(file.path(getwd(), "04_Writing", "02_Presentation", "Insolvenzanmeldungen.xlsx"), sheet = "Years")
#df_insMonth <- readxl::read_excel(file.path(getwd(), "Insolvenzanmeldungen.xlsx"), sheet = "Months")

# Color definition
colfunc <- colorRampPalette(c("black", "white"))
viridisLite::viridis(3, alpha = 1, begin = 0, end = 1, direction = 1, option = "D")
col_purpd <- '#002046'
col_purp <- add.alpha(col_purpd, alpha = 0.7)
col_purpL <- rgb(212, 44, 252, maxColorValue = 255)
col_yeld <- "#FDE725"
col_yel <- add.alpha(col_yeld, alpha = 0.4)
col_yelL <- rgb(253, 243, 146, maxColorValue = 255)


# Shaded areas
recession_GR <- data.frame(
  from=ymd(c('2009-04-01')), 
  to=ymd(c('2010-04-01'))
) %>% as_tibble()

recession_COVID <- data.frame(
  from=ymd(c('2020-04-01')), 
  to=ymd(c('2021-03-01'))
) %>% as_tibble()

recession_COVID_future <- tibble(from = seq(from = ymd("2021-02-28"), 
                                            to = ymd("2022-12-31"),
                                            by = "1 day")) %>% 
  mutate(to = lead(from,n = 1),
         color = colfunc(nrow(.)),
         color_id = 1:nrow(.))

# Backlog visualization
exp_growth <- data.frame(
  x = ymd(c('2020-01-01','2020-06-30','2020-09-30','2020-11-15','2020-12-31')),
  x1 = ymd(c('2020-06-30','2020-06-30','2020-09-30','2020-11-15','2020-12-31')),
  y_exp = c(18750, 19500, 23500, 32000, 16300+25000),
  y_exp1 = c(19500, 19500, 23500, 32000, 16300+25000),
  y_lin = c(18750, 17550, 16930, 16595, 16300)
) %>% as_tibble()

tikz(file.path(getwd(), "04_Writing", "01_Figures", "fig_gap_absolute.tex"),
     height = 3.5,
     width = 6)

df_insYear %>% 
  bind_rows(tibble("Year" = c(2021, 2022), "Filings" = c(NA, NA))) %>% 
  mutate(Year = as.Date(paste(Year,'12','31',sep = '-'))) %>% 
  ggplot() +
  scale_x_date(date_breaks = "1 year", 
               limits = ymd(c('2005-12-31','2022-12-31')),
               date_labels = c(as.character(2004:2022)), expand=c(0,0)) +
  geom_ribbon(data = exp_growth, aes(x=x, ymax=y_exp, ymin=y_lin), fill=col_purp, alpha=0.4) +
  geom_area(aes(x = Year, y = Filings), stat = "identity", color=col_yeld, fill=col_yel) +
  geom_point(aes(x = Year, y = Filings), stat = "identity", color=col_yeld, size=1.5) +
  scale_y_continuous(limits = c(0, 45000), 
                     labels=function(x) format(x, big.mark = ",", decimal.mark = ".", scientific = FALSE),  
                     expand = c(0, 0)) + 
  geom_point(aes(x = ymd('2020-12-31'), y = 16300+25000), size=1, color=col_purpd) +
  geom_point(data = exp_growth, aes(x = x1, y = y_exp1), size=0.4, linetype=3, color=col_purpd) +
  ylab("Number of insolvency filings") +
  xlab("Year") +
  geom_rect(data = recession_GR, aes(xmin = from, xmax = to, ymin = -Inf, ymax = Inf), alpha = 0.4, color="black") +
  geom_rect(data = recession_COVID, aes(xmin = from, xmax = to, ymin = -Inf, ymax = Inf), alpha = 0.4, color=NA) +
  geom_rect(data = recession_COVID_future, aes(xmin = from, xmax = to, ymin = -Inf, ymax = Inf, fill = color_id), alpha=0.8) +
  scale_fill_gradient(low = "dark grey", high = "white") +
  geom_vline(xintercept = ymd('2020-04-01'), color="black") + 
  geom_segment(x = ymd('2020-04-01'), xend = ymd('2021-03-01'), y = 0, yend = 0, color="black", size = 0.25) +
  geom_segment(x = ymd('2020-04-01'), xend = ymd('2021-03-01'), y = 45000, yend = 45000, color="black", size = 0.25) +
  geom_segment(x = ymd('2021-03-01'), xend = ymd('2022-12-31'), y = 0, yend = 0, color="black", linetype = "dotted", size = 0.25) +
  geom_segment(x = ymd('2021-03-01'), xend = ymd('2022-12-31'), y = 45000, yend = 45000, color="black", linetype = "dotted", size = 0.25) +
  annotate("text", label = "Great Recession", x = ymd('2009-01-15'), y = 17000, size = 2.5, angle = 90, fontface="italic") +
  annotate("text", label = "COVID-19 Crisis", x = ymd('2020-01-15'), y = 12000, size = 2.5, angle = 90, fontface="italic") +
  annotate("text", label = "Insolvency gap \n ($\\sim$ 25,000 firms)", x = ymd('2021-10-30'), y = 29000, size = 2.5, angle = 90, fontface="italic") +
  annotate("text", label = "Actual insolvencies \n ($\\sim$ 16,300 firms)", x = ymd('2021-10-30'), y = 8000, size = 2.5, angle = 90, fontface="italic") +
  theme_jod +
  theme(axis.text.x = element_text(family = "Arial", angle = 45, hjust = 1),
        legend.position = "none",
        panel.grid.major.x = element_blank())

dev.off()

grid.brackets(x1 = 730, y1 = 43, x2 = 730, y2 = 291, col="black")
grid.brackets(x1 = 730, y1 = 291, x2 = 730, y2 = 452, col="black")



## Unviable vs. Viable ===================================================================
load(file.path(getwd(), '05_Model', 'df_match_viability.RData')) # very good
df_match <- df_match %>% 
  mutate(mean_w_diff = -1*mean_w_diff)

df_match <- df_match %>% select(-c(p_value_ch2_w))

for (i in 1:nrow(df_match)){
  
  df_panel_match <- df_match %>% 
    filter(row_number() == i) %>% 
    select(data_match) %>% 
    unnest(cols = data_match)
  
  tryCatch({
    sample_w <- svydesign(id~1, weights=~weights, data=df_panel_match)
    df_match[i, 'p_value_ch2_w'] <- svychisq(~insolv+treat_final, sample_w, statistic='F')$p.value[[1]]  
  }, 
  error = function(e){
    df_match[i, 'p_value_ch2_w'] <- as.double(NA)})
  
  print(i)
  
}





# Clean WZ descriptions
df_match <- df_match %>% mutate(wz12_fct = case_when(wz12_fct == 'Others' ~ 'Others',
                                                     wz12_fct == 'Mechanical engineering' ~ 'Mechanical engineering',
                                                     wz12_fct == 'Manufacturing of chemical or pharmaceutical products' ~ 'Chemicals & pharmaceuticals', 
                                                     wz12_fct == 'Manufacturing of data processing equipment' ~ 'Manufacturing of data processing equipment', 
                                                     wz12_fct == 'Food production' ~ 'Food production',
                                                     wz12_fct == 'Other manufacturing industries' ~ 'Manufacturing',
                                                     wz12_fct == 'Wholesale and retail trade (incl. car repair)' ~ 'Wholesale & retail trade',
                                                     wz12_fct == 'Accommodation & catering' ~ 'Accommodation & catering',
                                                     wz12_fct == 'Insurance & banking' ~ 'Insurance & banking',
                                                     wz12_fct == 'Logistics & transport (incl. postal service)' ~ 'Logistics & transport',
                                                     wz12_fct == 'Creative industry & entertainment' ~ 'Creative industry & entertainment', 
                                                     wz12_fct == 'Other business-related services' ~ 'Business-related services',
                                                     wz12_fct == 'Health and social services' ~ 'Health & social services'))

# Strata for which no insolvency gap could have been calculated
df_match %>% filter(wz12_fct == "Creative industry & entertainment" & viability == "unviable", csize_fct == "Large enterprise") %>% 
  select(data) %>% unnest(cols = data) %>% tab(treat_final)

### Actual vs Counterfactual ##############################################

df_plot <- df_match %>% select(wz12_fct, csize_fct, viability, cont_mean_w, treat_mean
                               #, mean_w_diff, p_value_ch2_w
) %>% 
  pivot_longer(cols = c(cont_mean_w, treat_mean), names_to = 'period', values_to = 'ins_rate') %>% 
  mutate(period = ifelse(period == "treat_mean", "Actual", "Counterfactual")) %>% 
  mutate(csize_fct = factor(csize_fct, levels = c('Micro-enterprise', 'Small enterprise', 'Medium-sized enterprise', 'Large enterprise')))

sector_level <- df_plot %>% filter(csize_fct == "Micro-enterprise") %>% filter(period == "Counterfactual") %>% arrange(ins_rate) %>% select(wz12_fct) %>% distinct() %>% as_vector()

sector_level <- sapply(sector_level, function(x) str_replace(x, "\\\\&", "&"))

df_plot <- df_plot %>% 
  mutate(wz12_fct = factor(wz12_fct, levels = sector_level),
         viability = factor(viability, levels = c("viable", "unviable")))

# Short WZs
df_plot <- df_plot %>% 
  mutate(wz12_fct = case_when(wz12_fct == 'Others' ~ 'Others',
                              wz12_fct == 'Mechanical engineering' ~ 'Engineering',
                              wz12_fct == 'Chemicals & pharmaceuticals' ~ 'Pharma', 
                              wz12_fct == 'Manufacturing of data processing equipment' ~ 'Data equipment', 
                              wz12_fct == 'Food production' ~ 'Food',
                              wz12_fct == 'Manufacturing' ~ 'Manufacturing',
                              wz12_fct == 'Wholesale & retail trade' ~ 'Trade',
                              wz12_fct == 'Accommodation & catering' ~ 'Hotel',
                              wz12_fct == 'Insurance & banking' ~ 'Finance',
                              wz12_fct == 'Logistics & transport' ~ 'Logistics',
                              wz12_fct == 'Creative industry & entertainment' ~ 'Entertainment', 
                              wz12_fct == 'Business-related services' ~ 'Services',
                              wz12_fct == 'Health & social services' ~ 'Health'))

df_plot %>% 
  pivot_wider(names_from = period, values_from = ins_rate) %>% 
  mutate(gap = Counterfactual - Actual) %>% 
  filter(!(csize_fct == "Large enterprise")) %>% 
  ggplot() +
  geom_line(aes(x=wz12_fct, y=gap, color=viability, group=viability), size=1, linetype="dashed") +
  geom_point(aes(x=wz12_fct, y=gap, color=viability, group=viability), size=2) +
  geom_hline(yintercept=0, linetype="dotted") +
  ylab("Insolvency gap (in pp)") +
  xlab("Sector") +
  facet_wrap(.~csize_fct, scales = "free_x") +
  # scale_color_viridis(discrete = TRUE, 
  #                     option = "E", 
  #                     breaks = c("Strong financial standing", "Weak financial standing"),
  #                     labels = c("Strong financial standing", "Weak financial standing")) +
  # scale_y_continuous(labels = c(-0.5,0,0.5,1.0,1.5), 
  #                    breaks = c(-0.5,0,0.5,1.0,1.5),
  #                    lim=c(-0.5,1.5)) +
  # scale_x_discrete(expand = c(0.05,0.05))+
  theme_jod +
  theme(legend.justification = "left",
        legend.title = element_blank(),
        panel.grid.minor.y = element_blank()) +
  guides(color = guide_legend(title.position = "top"))
  
  

df_plot %>% ggplot() + 
  geom_bar(aes(wz12_fct, ins_rate, fill = period), stat='identity', position = 'dodge') +
  scale_y_continuous(labels = c(0.01, 0.02), 
                     breaks = c(0.01, 0.02),
                     expand = c(0,0), 
                     limits = c(0,max(df_plot$ins_rate)*1.1)) + 
  coord_flip() +
  xlab('Sector') +
  ylab('Insolvency rate') +
  facet_grid(viability~csize_fct, labeller = label_wrap_gen(width=10)) +
  scale_fill_viridis(discrete = TRUE, 
                     option = "E", 
                     breaks = c("Counterfactual", "Actual"),
                     labels = c("Counterfactual", "Actual")) +
  theme_jod +
  theme(axis.text.y = element_text(size=7),
        legend.justification = "left", 
        legend.title = element_blank())
### Insolvency Gap ########################################################

df_gap <- df_match %>% dplyr::select(wz12_fct, csize_fct, viability, mean_w_diff) %>% 
  pivot_wider(names_from = csize_fct, values_from = c(mean_w_diff)) %>% 
  select(wz12_fct, viability, "Micro-enterprise", "Small enterprise", "Medium-sized enterprise", "Large enterprise") %>% 
  arrange(factor(wz12_fct, levels = sector_level[length(sector_level):1])) %>% 
  mutate_if(is_double, function(x) round(x, 4)) %>% 
  mutate_if(is_double, function(x) sprintf("%+0.4f", x))

df_gap


df_gap_sig <- df_match %>% select(wz12_fct, csize_fct, viability, p_value_ch2_w) %>% 
  pivot_wider(names_from = csize_fct, values_from = p_value_ch2_w) %>% 
  select(wz12_fct, viability, "Micro-enterprise", "Small enterprise", "Medium-sized enterprise", "Large enterprise") %>% 
  arrange(factor(wz12_fct, levels = sector_level[length(sector_level):1])) %>% 
  #mutate_if(is_double, function(x) round(x, 4))
  mutate_if(is_double, function(x) case_when(x < 0.1 & x >= 0.05 ~ '*',
                                             x < 0.05 & x >= 0.01 ~ '**',
                                             x <= 0.01 ~ '***',
                                             x >= 0.1 ~ ""))
df_gap_final <- df_gap %>% 
  bind_rows(df_gap_sig) %>% 
  group_by(viability, wz12_fct) %>% 
  mutate_at(.vars = vars(contains('enterprise')), .funs = function(x) paste0(x, collapse = "")) %>% 
  ungroup() %>% 
  distinct() %>% 
  mutate_at(.vars = vars(contains('enterprise')), .funs = function(x) na_if(x, "+0.0000NA")) %>% 
  mutate_at(.vars = vars(contains('enterprise')), .funs = function(x) replace_na(x, "0.0000")) %>% 
  na_if("NANA") %>% 
  select(viability, everything()) %>% 
  arrange(desc(viability))

df_gap_final %>% 
  print(n=26)

# Print results in latex format  
df_gap_final %>%
  mutate(delim = "") %>% 
  select(-c(viability)) %>%
  select(delim, everything()) %>% 
  kable(format = 'latex', booktabs = TRUE, linesep = "\\addlinespace")











### Weighted Average Across Sectors #######################################
col_purpd <- '#002046'
col_purp <- add.alpha(col_purpd, alpha = 0.7)
col_purpL <- rgb(212, 44, 252, maxColorValue = 255)
col_yeld <- "#FDE725"
col_yeldd <- '#d7c101'
col_yel <- add.alpha(col_yeld, alpha = 0.4)
col_yelL <- rgb(253, 243, 146, maxColorValue = 255)



tikz(file.path(getwd(), "04_Writing", "01_Figures", "fig_weightavg.tex"),
     height = 3.5,
     width = 6)

df_match %>%
  mutate(N = pmap_dbl(.l = list(data_match), .f = function(x) tryCatch({ nrow(unnest(x))}, error = function(e){NA}))) %>% 
  dplyr::select(wz12_fct, csize_fct, viability, N, mean_w_diff) %>% 
  group_by(csize_fct, viability) %>% 
  summarise(avg_ins_rate = weighted.mean(mean_w_diff, N, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate_if(is_double, function(x) round(x,6)*100) %>% 
  mutate(csize_fct = factor(csize_fct, 
                            levels = c("Micro-enterprise", "Small enterprise", "Medium-sized enterprise", "Large enterprise"),
                            labels = c("Micro", "Small", "Medium", "Large")),
         viability = factor(viability, 
                            levels = c("viable", "unviable"), 
                            labels = c("Strong financial standing", "Weak financial standing"))) %>% 
  ggplot() +
  geom_line(aes(x=csize_fct, y=avg_ins_rate, color=viability, group=viability), size=1, linetype="dashed") +
  geom_point(aes(x=csize_fct, y=avg_ins_rate, color=viability, group=viability), size=2) +
  geom_hline(yintercept=0, linetype="dotted") +
  ylab("Average insolvency gap (in pp)") +
  xlab("Size of company") +
  scale_color_manual(values = c("Weak financial standing" = col_yeldd, "Strong financial standing" = col_purpd),
                     breaks = c("Strong financial standing", "Weak financial standing"),
                     labels = c("Strong financial standing", "Weak financial standing")) +
  # scale_color_viridis(discrete = TRUE, 
  #                     breaks = c("Strong financial standing", "Weak financial standing"),
  #                     labels = c("Strong financial standing", "Weak financial standing")) +
  scale_y_continuous(labels = c(-0.5,0,0.5,1.0,1.5), 
                   breaks = c(-0.5,0,0.5,1.0,1.5),
                   lim=c(-0.5,1.5)) +
  scale_x_discrete(expand = c(0.05,0.05))+
  theme_jod +
  theme(legend.justification = "left", 
        legend.title = element_blank(),
        panel.grid.minor.y = element_blank()) +
  guides(fill = guide_legend(title.position = "top"))
dev.off()

## Understand results =====================================================

# Food sector
df_match %>% filter(wz12_fct == "Food production" & csize_fct == "Medium-sized enterprise") %>% 
  select(data_match) %>% .[[1]] %>% .[[1]] %>% filter(insolv==1)

df_match %>% filter(wz12_fct == "Food production" & csize_fct == "Medium-sized enterprise") %>% 
  select(data) %>% .[[1]] %>% .[[1]] %>% filter(insolv==1) %>% select(crefo, insolv, treat_final, ch_bonitaet)

df_match %>% filter(wz12_fct == "Food production" & csize_fct == "Large enterprise") %>% 
  select(data_match) %>% .[[1]] %>% .[[1]] %>% filter(insolv==1)

df_panel_match <- df_match %>% filter(wz12_fct == "Food production" & csize_fct == "Large enterprise") %>% 
  select(data_match) %>% .[[1]] %>% .[[1]]

ch2 <- chisq.test(df_panel_match$insolv, df_panel_match$treat_final)
ch2$observed
ch2$expected

sample_w <- svydesign(id~1, weights=~weights, data=df_panel_match)
mean(df_panel_match$weights)
#sample_w <- svydesign(id~1, probs=~weights, data=df_panel_match)
summary(sample_w)
ch2_w <- svychisq(~insolv+treat_final, sample_w, statistic='F')


# Look at this problem: insolvents pre crisis not selected
df_match %>% filter(wz12_fct == "Mechanical engineering" & csize_fct == "Medium-sized enterprise") %>% select(data) %>% unnest(cols = data) %>% filter(insolv==1) %>% select(crefo, p_bonitaet, ch_bonitaet, treat_final) %>% print(n=24)

df_match %>% filter(wz12_fct == "Mechanical engineering" & csize_fct == "Medium-sized enterprise") %>% select(data_match) %>% unnest(cols = data_match) %>% filter(insolv==1) %>% print(n=24)

df_match <- df_match %>% bind_cols(tibble(SD = as.numeric(NA), fac = as.numeric(NA), caliper = as.numeric(NA))) %>% 
  mutate(SD_all = sd(df_panel$ch_bonitaet))

for (i in 1:nrow(df_match)){
  
  
  
  df_temp <- df_match %>% filter(row_number()==i) %>% 
    select(data) %>% 
    unnest(cols = data) %>% 
    select(ch_bonitaet) %>% 
    summarise(SD = sd(ch_bonitaet),
              fac = (4/log(nrow(.))),
              caliper = ceiling(fac*SD))
    df_match[i, c("SD", "fac", "caliper")] <- df_temp
  
}
df_match %>% select(wz12_fct, csize_fct, data, SD_all, SD, fac, caliper) %>% 
  mutate(caliper_all = ceiling(SD_all*fac)) %>% print(n=52)

0.2*sd(df_panel$ch_bonitaet)

df_temp <- df_match %>% 
  filter(wz12_fct == "Mechanical engineering" & csize_fct == "Medium-sized enterprise") %>% 
  select(data) %>% 
  unnest(cols = data) %>% 
  select(crefo, jahr, treat_final, insolv, p_bonitaet, ch_bonitaet, avg_bonitaet, n_downgrade, age) %>% 
  filter(complete.cases(.)) %>% 
  mutate(id = paste(crefo, jahr, sep  = '_')) %>%
  distinct(id, .keep_all = TRUE) %>% 
  filter(!is.na(age)) %>% 
  column_to_rownames('id')


model_match <- as.formula(paste('treat_final', paste('ch_bonitaet', 
                                                     'p_bonitaet',
                                                     'avg_bonitaet',
                                                     'n_downgrade',
                                                     'age',
                                                     #'refo_dummy',
                                                     sep = ' + '), sep = ' ~ '))

match_res <- matchit(formula=model_match, 
                     data=df_temp, 
                     method="nearest",
                     distance = "mahalanobis",
                     #ratio = 1,
                     ratio=floor(nrow(df_temp[df_temp$treat_final==0,])/nrow(df_temp[df_temp$treat_final==1,])),  #6, # nrow(df_panel[df_panel$treat_final==0,])/nrow(df_panel[df_panel$treat_final==1,])
                     replace=TRUE,
                     #discard='control', 
                     #exact = ~ refo_dummy,
                     caliper = c(ch_bonitaet = 16), # sd(df_panel$ch_bonitaet)*0.25, https://www.lexjansen.com/pharmasug/2006/PublicHealthResearch/PR05.pdf
                     std.caliper=FALSE,
                     #min.controls = 0,
                     #max.controls = 6
                     
)

df_panel_match <- match.data(match_res) %>% 
  as_tibble() %>% 
  mutate(id = paste(crefo, jahr, sep  = '_')) %>% 
  select(id, everything()) 


mean(df_panel_match[df_panel_match$treat_final==0,]$insolv)
sum(df_panel_match[df_panel_match$treat_final==0,]$insolv)/nrow(df_panel_match[df_panel_match$treat_final==0,])

cont_mean_w <- weighted.mean(x = df_panel_match[df_panel_match$treat_final==0,]$insolv, w = df_panel_match[df_panel_match$treat_final==0,]$weights)
sum(df_panel_match[df_panel_match$treat_final==0,]$insolv*df_panel_match[df_panel_match$treat_final==0,]$weights)/nrow(df_panel_match[df_panel_match$treat_final==0,])
sum(df_panel_match[df_panel_match$treat_final==0,]$insolv*df_panel_match[df_panel_match$treat_final==0,]$weights)/sum(df_panel_match[df_panel_match$treat_final==0,]$weights)


treat_mean <- mean(df_panel_match[df_panel_match$treat_final==1,]$insolv)
sum(df_panel_match[df_panel_match$treat_final==1,]$insolv)/nrow(df_panel_match[df_panel_match$treat_final==1,])
