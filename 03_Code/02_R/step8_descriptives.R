# DESCRIPTIVES 
setwd("Q:\\Meine Bibliotheken\\Research\\SME_Corona")
source(file.path(getwd(), '03_Code', '02_R', 'step0_setup.R'))

# -------------------------------------------------------------------------
# #########################################################################
# -------------------------------------------------------------------------



# Load Appended & Cleaned Panel -------------------------------------------

df_panel <- read_delim(file = file.path(getwd(), '02_Data', '01_PanelData', 'df_panel_clean_final.txt'),
                       delim = '\t')

# DROP
df_drop <- read_delim(file = file.path(getwd(), '02_Data', '03_SanityChecks', 'df_drop.txt'), delim = '\t')
df_drop <- df_drop[1:13,]


# Check Insolvency Rates over Time ----------------------------------------

# All insolvencies
df_res <- insolv_ym(df_panel)
plot_insolv(df_panel, end = '2020-07-31')

# Check Insolvencies by Industry and Size ---------------------------------

df_panel %>% 
  group_by(treat_final) %>% 
  summarise(N = n(),
            N_ins = sum(insolv==1),
            ins_rate = mean(insolv))

df_panel %>% 
  group_by(wz12_fct, csize_fct, treat_fct) %>% 
  summarise(N = n(),
            N_ins = sum(insolv==1),
            ins_rate = mean(insolv)) %>% 
  print(n=130)

df_panel %>% 
  group_by(wz12_fct, csize_fct, treat_fct) %>% 
  summarise(N = n(),
            N_ins = sum(insolv==1),
            ins_rate = mean(insolv)) %>% 
  #filter(treat_fct == "Pre-suspension period") %>% 
  filter(N_ins == 0)


df_panel %>% 
  group_by(wz12_fct, csize_fct, treat_fct) %>%
  summarise(N = n(),
            N_ins = sum(insolv==1),
            ins_rate = mean(insolv)) %>% 
  ungroup() %>% 
  group_by(wz12_fct, csize_fct, treat_fct) %>%
  ggplot(aes(x = wz12_fct, y = ins_rate)) + 
  geom_col(aes(fill = treat_fct)) +
  #geom_label(aes(fill = treat_fct, label = N_ins)) +
  coord_flip() +
  facet_wrap(.~csize_fct)



# Check Insolvency Distribution by Updays Decile --------------------------
df_temp <- df_panel %>% 
  mutate(q_updays = ntile(updays, n = 10)) %>% 
  select(crefo, jahr, bonitaet, ch_bonitaet, insolv_fct, updays, q_updays, exitdat, treat_fct, d_exitdat)

df_temp %>% 
  filter(updays>=0) %>% 
  ggplot(aes(x=updays)) + 
  geom_histogram(na.rm = TRUE, colour="black", fill="white", bins = 30) +
  #geom_histogram(na.rm = TRUE, colour="black", fill="white") +
  #geom_density(alpha=.2, fill="#FF6666") +
  facet_wrap(~treat_fct, nrow = 1, scales = 'free') + 
  labs(title = 'Time between updates in Creditreform database'
       #, subtitle = 'According to Creditreform\'s Date of Reassesment'
  ) +
  xlab('Days') +
  ylab('Count') +
  #scale_y_log10() +
  theme_jod
# Possibly drop observations with short update period in pre-suspension

crosstab(dep = df_temp$q_updays, 
         indep = df_temp$insolv_fct,
         prop.r = TRUE, 
         drop.levels = TRUE,
         plot = FALSE)
# The longer no update in database the higher the fraction of insolvencies. Except for the lowest decile.
# -> Creditreform updates once it gets insolcency-related info




# Firms with Bonitaet of 500+ ---------------------------------------------
# Do we see firms with 500+ boni which are still alive
df_temp <- df_panel %>% 
  mutate(bonitaet_gr500 = ifelse(bonitaet==500,1,0))

crosstab(dep = df_temp$bonitaet_gr500, 
         indep = df_temp$insolv,
         prop.r = TRUE, 
         drop.levels = TRUE,
         plot = FALSE)
# Yes we do!



# Change of Bonitaet  -----------------------------------------------------

# Plot change in rating prior and after suspension of obligation to file insolvencies
df_panel %>% 
  ggplot(aes(x=ch_bonitaet)) + 
  geom_histogram(aes(y=..density..), na.rm = TRUE, colour="black", fill="white", bins = 50) +
  #geom_histogram(na.rm = TRUE, colour="black", fill="white") +
  #geom_density(alpha=.2, fill="#FF6666") +
  facet_wrap(~treat_fct, nrow = 1) + 
  #labs(title = 'Changes in Credit Rating', subtitle = 'According to Creditreform\'s Date of Reassesment') +
  xlab('Change in credit rating') +
  ylab('Density') +
  #scale_y_log10() +
  theme_jod

# Plot change in rating versus insolvency variable
df_panel %>% 
  ggplot(aes(x=ch_bonitaet)) + 
  geom_histogram(aes(y=..density..), na.rm = TRUE, colour="black", fill="white", bins = 100) +
  #geom_histogram(na.rm = TRUE, colour="black", fill="white") +
  #geom_density(alpha=.2, fill="#FF6666") +
  facet_wrap(~insolv_fct, nrow = 1) + 
  labs(title = 'Changes in Credit Rating' 
       #subtitle = 'According to Survival Status'
       ) +
  xlab('Change in credit rating') +
  ylab('Density') +
  #scale_y_log10() +
  theme_jod



# Plot both differentiations together
df_panel %>% 
  ggplot(aes(x=ch_bonitaet)) + 
  geom_histogram(aes(y=..density..), na.rm = TRUE, colour="black", fill="white", bins = 30) +
  #geom_histogram(na.rm = TRUE, colour="black", fill="white") +
  #geom_density(alpha=.2, fill="#FF6666") +
  facet_wrap(treat_fct~insolv_fct, nrow = 1) + 
  labs(title = 'Changes in Credit Rating') +
  xlab('Change in credit rating') +
  ylab('Density') +
  #scale_y_log10() +
  theme_jod

# Plot both differentiations together but only for worsening in credit rating
df_panel %>% 
  filter(ch_bonitaet>0) %>% 
  ggplot(aes(x=ch_bonitaet)) + 
  geom_histogram(aes(y=..density..), na.rm = TRUE, colour="black", fill="white", bins = 30) +
  #geom_histogram(na.rm = TRUE, colour="black", fill="white") +
  #geom_density(alpha=.2, fill="#FF6666") +
  facet_wrap(treat_fct~insolv_fct, nrow = 1) + 
  labs(title = 'Changes in Credit Rating') +
  xlab('Change in credit rating') +
  ylab('Density') +
  #scale_y_log10() +
  theme_jod


# Plot distribution of change in credit rating according to prior bonitaet, post- & pre-treatment as well as survival status
df_panel %>% 
  ggplot(aes(x=ch_bonitaet, fill = insolv_fct)) + 
  geom_histogram(aes(y = ..density..), na.rm = TRUE, bins = 30, alpha = 0.6, position = 'identity') +
  #geom_histogram(na.rm = TRUE, colour="black", fill="white") +
  #geom_density(alpha=.2, fill="#FF6666") +
  facet_wrap(treat_fct~p_bonitaet_fct, nrow = 2) + 
  labs(title = 'Changes in Credit Rating'
       #, subtitle = 'According to Creditreform\'s Date of Reassesment, Credit Rating prior to Change and Survival Status'
       ) +
  xlab('Change in credit rating') +
  ylab('Density') +
  scale_fill_discrete(name = NULL) +
  #scale_y_log10() +
  theme_jod


# Plot distribution of change in credit rating according to prior bonitaet, post- & pre-treatment as well as survival status
df_temp <- df_panel %>% 
  group_by(treat_fct, insolv_fct, p_bonitaet_fct, ch_bonitaet) %>% 
  summarise(n = n()) %>% 
  ungroup()

df_temp %>% 
  ggplot(aes(x=ch_bonitaet, y = n, fill = insolv_fct)) + 
  geom_bar(na.rm = TRUE, 
           #binwidth = 10, 
           alpha = 0.6, stat = 'identity') +
  #geom_histogram(na.rm = TRUE, colour="black", fill="white") +
  #geom_density(alpha=.2, fill="#FF6666") +
  facet_wrap(treat_fct~p_bonitaet_fct, nrow = 2) + 
  labs(title = 'Changes in Credit Rating', subtitle = 'According to Creditreform\'s Date of Reassesment, Credit Rating prior to Change and Survival Status') +
  xlab('Change in credit rating') +
  ylab('Count') +
  scale_y_log10(breaks = logticks(df_temp$n, "breaks"),
                labels = logticks(df_temp$n, "labels"),
                limits = c(1, 10^6),
                expand = c(0, 0)
                ) +
  scale_fill_discrete(name = NULL) +
  theme_jod +
  theme(panel.grid.minor.y = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))



# Days between Exitdat and Rechdat ----------------------------------------
df_panel %>% 
  filter(insolv==1) %>% 
  ggplot() +
  geom_histogram(aes(x = d_exitdat), colour="black", fill="white", bins = 30) +
  facet_wrap(~treat_fct, scales = 'free_y') +
  labs(title = 'Time between Insolvency Filing and Update in Creditreform Database'
       #, subtitle = 'According to Creditreform\'s Date of Reassesment'
  ) +
  xlab('Days') +
  ylab('Count') +
  #scale_y_log10() +
  theme_jod

df_panel %>% 
  filter(treat_final==1) %>% 
  filter(insolv==1) %>% 
  select(treat_final, treat_fct) %>% 
  table(useNA = 'always')


# Revenues by Employee Number ---------------------------------------------


df_temp <- df_panel %>% filter(jahr_rd == 2019) %>% 
  group_by(anzma) %>% 
  summarise(mean_revenue = median(umsatz, na.rm = TRUE))


df_temp %>% 
  filter(anzma < 500) %>% 
  ggplot(aes(x=anzma, y = mean_revenue)) +
  geom_line()

write.xlsx(df_temp, file = file.path(getwd(), '02_Data', '03_SanityChecks', 'revnue_by_employee.xlsx'))

# Some More Descriptives --------------------------------------------------
# Group change in solvency variable
df_temp <- df_panel %>% 
  mutate(ch_bonitaet_q = ntile(ch_bonitaet, n = 10),
         ch_bonitaet_s = case_when(
           ch_bonitaet < 0 ~ 1,
           ch_bonitaet == 0 ~ 2,
           ch_bonitaet > 0 ~ 3)
         ) %>% 
  select(crefo, jahr, insolv, ch_bonitaet, ch_bonitaet_q, everything())

# Does an increase in credit rating correspond with a higher fraction of insolvenies?
crosstab(dep = df_temp$ch_bonitaet_s, 
         indep = df_temp$insolv,  
         prop.r = TRUE, 
         drop.levels = TRUE,
         plot = FALSE)
# Yes it does!

# Do we see a lower fraction of insolvencies in the post-Corona period (which would be in line with the policy instrument)
crosstab(dep = df_panel$treat_fct, 
         indep = df_panel$insolv_fct,  
         #weight =  
         prop.r = TRUE, 
         drop.levels = TRUE,
         plot = FALSE)
# Yes we do!



