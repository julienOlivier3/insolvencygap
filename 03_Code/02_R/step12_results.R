# RESULTS
setwd("Q:\\Meine Bibliotheken\\Research\\SME_Corona")
source(file.path(getwd(), '03_Code', '02_R', 'step0_setup.R'))

# -------------------------------------------------------------------------
# #########################################################################
# -------------------------------------------------------------------------

# Load Appended & Cleaned Panel -------------------------------------------
df_panel <- read_delim(file = file.path(getwd(), '02_Data', '01_PanelData', 'df_panel_clean_final_V2.txt'),
                       delim = '\t', 
                       col_types = cols(exitdat = col_date()))
load(file = file.path(getwd(), '02_Data', '01_PanelData', 'df_panel_clean_res.RData'))
load(file = 'Q:\\Meine Bibliotheken\\Research\\SME_Corona\\02_Data\\01_PanelData\\df_strata.RData')

# DROP
df_drop <- read_delim(file = file.path(getwd(), '02_Data', '03_SanityChecks', 'df_drop.txt'), delim = '\t')
df_drop <- df_drop[1:12,]


# Data Preparation --------------------------------------------------------
# Only matched observations
df_panel_match <- df_panel %>% 
  filter(!is.na(est_PD))


# Significance ------------------------------------------------------------
df_panel_match %>% 
  tab(csize_fct)

micro <- df_panel_match %>% 
  filter(csize_fct == 'Micro-enterprise') %>% 
  select(csize_fct, subclass, weights, insolv, treat_final, n, n_pre, n_post, est_PD) %>% 
  group_by(subclass, treat_final) %>% 
  summarise(weights = mean(weights),
            mean_ins = mean(insolv),
            n = mean(n),
            n_pre = mean(n_pre),
            n_post = mean(n_post),
            est_PD = mean(est_PD)) %>% 
  ungroup()

micro

df <- df_panel_match %>% 
  filter(csize_fct == 'Micro-enterprise') %>% 
  select(crefo, csize_fct, subclass, weights, insolv, treat_final, n, n_pre, n_post, est_PD)
  
wtd.t.test(x = as_vector(df[df$treat_final==0, 'insolv']), y = as_vector(df[df$treat_final==1, 'insolv']),
           weight = as_vector(df[df$treat_final==0, 'weights']), weighty = as_vector(df[df$treat_final==1, 'weights']))

# Actual vs. Counterfactual -----------------------------------------------
# Function to plot results
plot_ACvsCO <- function(grouping_var = wz_fct, y_axis_name = 'Industry', drop_NA = TRUE){
  
  variable <- quo(!!ensym(grouping_var))
  
df_plot <- df_panel_match %>% 
    group_by(!!variable) %>%
    mutate(n_group = n()) %>% 
    ungroup() %>% 
    filter(treat_final==1) %>%
    mutate(!!variable := factor(!!variable)) %>% 
    group_by(!!variable) %>% 
    summarise(Counterfactual = sum(est_PD)/mean(n_group),
              Actual = sum(insolv)/mean(n_group)) %>% 
  {if(drop_NA) filter(., !is.na(!!variable)) else .} 

df_avg <- df_panel_match %>% 
  filter(treat_final==1) %>% 
  {if(drop_NA) filter(., !is.na(!!variable)) else .} %>% 
  select(est_PD, insolv) %>% 
  summarise_all(sum)

  
df_plot <- df_plot %>%  
    pivot_longer(cols = c(Counterfactual, Actual), names_to = 'PD_type', values_to = 'PDs') %>% 
    bind_rows(tibble(!!variable := c("Average","Average"), 
                     PD_type = c("Counterfactual", "Actual"), 
                     PDs = c(df_avg$est_PD/nrow(df_panel_match), 
                             df_avg$insolv/nrow(df_panel_match)))) %>% 
    mutate(!!variable := factor(!!variable))

df_plot <- df_plot %>%
    mutate(
      avg = factor(ifelse(!!variable!='Average'|is.na(!!variable), 0, 1), levels = c(0,1)),
      #avg = factor(ifelse(is.na(avg), 0, avg)),
      PD_type = factor(PD_type, levels = c("Counterfactual", "Actual")),
      !!variable := factor(!!variable, levels = df_plot %>% filter(PD_type == 'Counterfactual') %>% arrange(PDs) %>% select(!!variable) %>% as_vector())
    )
  
  gglevels <- df_plot %>% filter(PD_type == 'Counterfactual') %>% arrange(PDs) %>% select(!!variable) %>% as_vector()
  ggface <- c(rep('plain', which(gglevels == "Average")-1), 'bold.italic', rep('plain', length(gglevels) - which(gglevels == "Average")))
  
  df_plot %>% 
    ggplot() +
    geom_bar(aes(!!variable, PDs, fill = PD_type, alpha = avg, color = avg), stat='identity', position = 'dodge') +
    scale_y_continuous(expand = c(0,0), limits = c(0,max(df_plot$PDs)*1.1)) + 
    coord_flip() +
    labs(title = "Actual and Estimated Probability of Default (PD) in the Suspension Period", 
         subtitle = 'Actual probabilities based on observed insolvency filings after 01-04-2020, counterfactual probabilites estimated with coarsened exact matches prior to 01-04-2020') +
    xlab(y_axis_name) +
    ylab('PD') +
    scale_fill_discrete(name = "", guide = guide_legend(reverse = TRUE)) +
    scale_alpha_discrete(range = c(0.5, 1), guide=FALSE) +
    scale_color_manual(values = c('white', 'black'), guide=FALSE) +
    #scale_y_sqrt(limits = c(0, 0.5), breaks = c(0, 0.01, 0.1, 0.5), labels = c(0, 0.01, 0.1, 0.5)) +
    theme_jod +
    theme(axis.text.y = element_text(
      face = ggface
    )
    )
  
}


## by wz ==================================================================
plot_ACvsCO(drop_NA = TRUE)
plot_ACvsCO(grouping_var = wz5_fct)
plot_ACvsCO(grouping_var = wz12_fct)

## by company size ========================================================
plot_ACvsCO(grouping_var = csize_fct, y_axis_name = 'Company size', drop_NA = FALSE)
plot_ACvsCO(grouping_var = anzma_fct, y_axis_name = 'Company size')

## by company age =========================================================
plot_ACvsCO(grouping_var = ctype_fct, y_axis_name = 'Company age')

## by liability ===========================================================
plot_ACvsCO(grouping_var = refo_fct, y_axis_name = 'Liability')

## by prior boni ==========================================================
plot_ACvsCO(grouping_var = ch_bonitaet_fct, y_axis_name = 'Change in credit rating')



# Estimated PDs by Bonitaet -----------------------------------------------

df_strata %>%
  ggplot() +
  geom_boxplot(aes(x = ch_bonitaet_fct, y = est_PD)) +
  facet_wrap(~p_bonitaet_fct) +
  xlab('Change in credit rating') +
  ylab('Estimated PD') +
  # scale_y_log10(
  #   #breaks = logticks(df_strata$est_PD, "breaks"),
  #   #labels = logticks(df_strata$est_PD, "labels"),
  #   #limits = c(0, 1.5),
  #   expand = c(0, 0)
  # ) +
  scale_fill_discrete(name = NULL) +
  theme_jod +
  theme(panel.grid.minor.y = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))
  



# -------------------------------------------------------------------------



# Look at distribution grouped by post and pre corona
df_panel_match %>% 
  mutate(r_bonitaet = case_when(bonitaet >= 100 & bonitaet <= 149 ~ '100-149',
                                bonitaet >= 150 & bonitaet <= 199 ~ '150-199',
                                bonitaet >= 200 & bonitaet <= 249 ~ '200-249',
                                bonitaet >= 250 & bonitaet <= 299 ~ '250-299',
                                bonitaet >= 300 & bonitaet <= 349 ~ '300-349',
                                bonitaet >= 350 & bonitaet <= 499 ~ '350-499',
                                bonitaet >= 500 ~ '500+'),
         treat_final = ifelse(treat_final==1, 'post-pandemic (est.)', 'pre-pandemic (act.)')) %>% 
  group_by(treat_final, r_bonitaet, ctype) %>% 
  summarise(counter_PD = mean(est_PD)) %>% 
  pivot_longer(cols = c(counter_PD), names_to = 'PD', values_to = 'PDs') %>% 
  ggplot() +
  geom_bar(aes(r_bonitaet, PDs, fill = treat_final), alpha = 0.5, stat='identity', position = 'dodge') +
  facet_wrap(~ctype) + 
  labs(title = "Probability for insolvency filings", 
       subtitle = 'Probabilities estimated with observations prior to 01-04-20') +
  xlab('Credit Rating') +
  ylab('Probability') +
  scale_fill_discrete(name = "") +
  #scale_y_sqrt(limits = c(0, 0.5), breaks = c(0, 0.01, 0.1, 0.5), labels = c(0, 0.01, 0.1, 0.5)) +
  theme_jod
# estimated insolvencies are consistently higher for observations in the corona crisis which is due to the fact that a latrger
# fraction of Corona observations fall into the high insolvency risk buckets. 




# Meaningless
df_panel %>% 
  filter(!is.na(est_PD)) %>% 
  mutate(r_bonitaet = case_when(bonitaet >= 100 & bonitaet <= 149 ~ '100-149',
                                bonitaet >= 150 & bonitaet <= 199 ~ '150-199',
                                bonitaet >= 200 & bonitaet <= 249 ~ '200-249',
                                bonitaet >= 250 & bonitaet <= 299 ~ '250-299',
                                bonitaet >= 300 & bonitaet <= 349 ~ '300-349',
                                bonitaet >= 350 & bonitaet <= 499 ~ '350-499',
                                bonitaet >= 500 ~ '500+')) %>% 
  group_by(r_bonitaet, ctype_fct) %>% 
  summarise(counter_PD_pre = weighted.mean(est_PD, w = n_pre),
            counter_PD_post = weighted.mean(est_PD, w = n_post)
  ) %>% 
  pivot_longer(cols = c(counter_PD_pre, counter_PD_post), names_to = 'PD', values_to = 'PDs') %>% 
  ggplot() +
  geom_bar(aes(r_bonitaet, PDs, fill = PD), alpha = 0.5, stat='identity', position = 'dodge') +
  facet_wrap(~ctype_fct) + 
  labs(title = "Probability for insolvency filings", 
       subtitle = 'Probabilities estimated with observations prior to 01-04-20') +
  xlab('Credit Rating') +
  ylab('Probability of Default') +
  scale_fill_discrete(name = "", labels = c("Counterfactual PD (suspension period)", "Counterfactual PD (pre-suspension)")) +
  #scale_y_sqrt(limits = c(0, 0.5), breaks = c(0, 0.01, 0.1, 0.5), labels = c(0, 0.01, 0.1, 0.5)) +
  theme_jod
# estimated insolvencies are consistently higher for observations in the corona crisis which is due to the fact that a 
# larger fraction of Corona observations fall into the high insolvency risk buckets. 





# Check pandemic observations
df_panel_pan <- df_panel_match %>% 
  filter(treat_final==1)

# Mean over mean
t1 <- c(1,0,0) 
t2 <- c(0,1,1,0,0,0) 
t3 <- c(1,0,0,1,1,1,0)
mean(t1);mean(t2);mean(t3)

mean(c(mean(t1),mean(t2),mean(t3)))
mean(c(t1,t2,t3))
# mean over mean is not the same if number of samples in strata differ
# Solution: weighted means!
weighted.mean(c(mean(t1),mean(t2),mean(t3)), w = c(3,6,7))




df_panel_pan %>% 
  mutate(insolvency = ifelse(insolv==1, 'yes', 'no')) %>% 
  ggplot() +
  geom_boxplot(aes(x=insolvency, y=est_PD))+
  labs(title = 'Estimated insolvency filings for insolvent and non-insolvent firms') +
  xlab('Actual insolvency') +
  ylab('Estimated PD') +
  #scale_y_sqrt() +
  theme_jod

df_panel_match %>% 
  sample_n(100000) %>% 
  mutate(obs = ifelse(treat_final==1,'pandemic', 'pre-pandemic'),
         insolv = ifelse(insolv==1,'yes', 'no')) %>% 
  ggplot() +
  geom_point(aes(x = ch_bonitaet, y = est_PD, color = obs, shape = insolv, size = insolv), alpha = 0.5) 


df_panel_match %>% 
  #filter(treat_final==1) %>% 
  mutate(obs = ifelse(treat_final==1,'pandemic', 'pre-pandemic'),
         insolv = ifelse(insolv==1,'yes', 'no')) %>% 
  ggplot() +
  geom_point(aes(x = ch_bonitaet, y = est_PD_norm, color = insolv, shape = insolv, size = insolv), alpha = 0.5) +
  facet_wrap(~obs)


# Determine thresshold above which insolvency assignment is optimal
df_panel_pre <- df_panel_match %>% 
  filter(treat_final==0)


mean(df_panel_pre$insolv)
mean(df_panel_pre$est_PD)
weighted.mean(df_panel_pre$est_PD, w = df_panel_pre$n_pre)
# Difference realted to whether one groups before calculating mean over mean (w/o grouping mean over mean is the same as simple mean)
df_panel_pre %>% 
  group_by(ctype) %>% 
  summarise(m_insolv = mean(insolv),
            m_est_PD = mean(est_PD),
            m_est_PD_weights = weighted.mean(est_PD, n_pre))
# No groupby does calculation correctly



mean(df_panel_pan$insolv)
mean(df_panel_pan$est_PD)
# Insolvency rate is small in corona sample based on counterfactual estimation.
# Find a way to align the two above -> work with distance? -< ?cem
# or maybe within subclasses some distance measure based on boni and change in boni
# How do insolvencies within both samples look like
mean(df_panel_pre$insolv)
mean(df_panel_pan$insolv)
mean(df_panel_pre$est_PD)

df_panel %>% 
  filter(subclass==1) %>% 
  mutate(neighboor_weight = #now calculate similarity to others based on boni)
           
           # Find optimal threshhold
           y_true <- df_panel_pre$insolv
         y_true_pred <- df_panel_pre$est_PD
         
         simple_roc <- function(labels, scores){
           labels <- labels[order(scores, decreasing=TRUE)]
           data.frame(TPR=cumsum(labels)/sum(labels), FPR=cumsum(!labels)/sum(!labels), labels)
         }
         
         roc_res <- simple_roc(y_true, y_true_pred)
         my_roc <- roc(y_true, y_true_pred)
         plot(my_roc, xlim = c(1,0))
         ins <- length(my_roc$cases)
         cont <- length(my_roc$controls)
         roc_thres <- coords(my_roc, x = "best", best.method = "closest.topleft", best.weights = c(2,ins/(ins+cont)), transpose = TRUE)
         
         df_panel_pre %>% 
           mutate(insolv_pred = ifelse(est_PD>roc_thres[1], 1, 0)) %>% 
           select(insolv, insolv_pred) %>% 
           table()
         
         TP <- 1387
         FP <- 27891
         FN <- 852
         F_1 <- TP/(TP + 0.5*(FP + FN))
         
         plot(x = roc_res$FPR, y = roc_res$TPR, type = 'l')
         
         https://blog.revolutionanalytics.com/2016/08/roc-curves-in-two-lines-of-code.html
         
