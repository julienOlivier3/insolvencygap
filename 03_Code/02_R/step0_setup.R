library(scales)
library(margins)
library(haven)
#library(pmdplyr)
library(lubridate)
library(tsibble)
library(rlist)
library(descr)
library(MatchIt)
library(cem)
library(scales)
library(pROC)
library(zoo)
library(collapse)
library(xlsx)
library(gridExtra)
library(weights)
library(survey)
library(viridis)
library(DiagrammeR)
library(knitr)
library(RColorBrewer)
library(ggpubr)
library(tikzDevice)
library(xlsx)
library(pBrackets)
library(igraph)
library(grid)
library(GISTools)
library(tidyverse)
#library(cobalt)
#library(Zelig)





# Setup -------------------------------------------------------------------
# ggplot Template
source(file = "Q:\\Meine Bibliotheken\\Research\\06_Environments\\ggplot_template.R")

# Print crefo nicely
options(pillar.sigfig=10)

# Create list updating when observations will be dropped


# Functions ---------------------------------------------------------------

# Function that joins in place
replace_subset <- function(df, df_subset, id_col_names = c()) {
  
  # work out which of the columns contain "new" data
  new_data_col_names <- colnames(df_subset)[which(!colnames(df_subset) %in% id_col_names)]
  
  # complete the df_subset with the extra columns from df
  df_sub_to_join <- df_subset %>%
    left_join(select(df, -new_data_col_names), by = c(id_col_names))
  
  # join and bind rows
  df_out <- df %>%
    anti_join(df_sub_to_join, by = c(id_col_names)) %>%
    bind_rows(df_sub_to_join)
  
  return(df_out)
  
}

# Function that joins if missing
coalesce_join <- function(x, y, by = NULL, suffix = c(".x", ".y"), join = dplyr::left_join, ...) {
  joined <- join(x, y, by = by, suffix = suffix, ...)
  # names of desired output
  cols <- union(names(x), names(y))
  
  to_coalesce <- names(joined)[!names(joined) %in% cols]
  suffix_used <- suffix[ifelse(endsWith(to_coalesce, suffix[1]), 1, 2)]
  # remove suffixes and deduplicate
  to_coalesce <- unique(substr(
    to_coalesce, 
    1, 
    nchar(to_coalesce) - nchar(suffix_used)
  ))
  
  coalesced <- purrr::map_dfc(to_coalesce, ~dplyr::coalesce(
    joined[[paste0(.x, suffix[1])]], 
    joined[[paste0(.x, suffix[2])]]
  ))
  names(coalesced) <- to_coalesce
  
  dplyr::bind_cols(joined, coalesced)[cols]
}

# Function return marginal effects of probit model
glm_margins <- function(model, type = "response"){
  reg_res <- summary(model)
  ame <- margins(model, type = type)
  
  res <- as_tibble(summary(ame)) %>% 
    arrange(match(factor,rownames(reg_res$coefficients)[2:length(rownames(reg_res$coefficients))])) %>% 
    mutate(sig = case_when(p < 0.01 ~ '***',
                           p >= 0.01 & p < 0.05 ~ '**',
                           p >= 0.05 & p < 0.1 ~ '*',
                           p >= 0.1 ~ '')) %>%
    dplyr::select(factor, AME, p, sig, SE) %>% 
    mutate_if(is.numeric, round, 5)# %>% 
  # mutate(sig = noquote(sig))
  
  print(as.data.frame(res), digits = 3, right = FALSE)
  
  return(res)
}

# Function normalizing vector to be between 0 and 1
vec_norm <- function(x){
  res  <-  (x - min(x))/(max(x) - min(x))

  return(res)
}

# Function to draw log tickmarks nicely
logticks <- function(datavar,type) {
  
  minimum <- 1/10^abs(floor(log10(min(datavar, na.rm=TRUE))))
  maximum <- 1*10^abs(floor(log10(max(datavar, na.rm=TRUE)))+1)
  multiple <- floor(log10(maximum/minimum))
  
  yourtickvector <- c()
  
  if (type=="breaks") {
    
    yourtickvector <- c(ifelse(minimum==1,0,minimum))
    
    for (x in seq(0,multiple)) {
      
      andadd <- seq(minimum*10^x,minimum*10^(x+1),minimum*10^x)[-1]
      
      yourtickvector <- c(yourtickvector,andadd)
      
    }
    
  } else if (type=="labels") {
    
    yourtickvector <- c(ifelse(minimum==1,0,minimum), rep("",8))
    
    for (x in seq(1,multiple)) {
      
      andadd <- c(minimum*10^x,rep("",8))
      
      yourtickvector <- c(yourtickvector,andadd)
      
    }
    
    yourtickvector <- c(yourtickvector,minimum*10^multiple)
    
  }
  
  return(yourtickvector)
  
}

# Function to plot table within pipe
tab <- function(.data, var){
  dtype <- .data %>% 
    select({{var}}) %>% 
    as_vector() %>% 
    class()
  
  .data %>% 
    select({{var}}) %>% 
    table(useNA = 'always') %>% 
    as_tibble() %>% 
    rename(!!quo(!!ensym(var)) := '.') %>% 
    mutate(p = round(n/sum(n), 5)) %>% 
    arrange(desc(p)) %>% 
    {if(dtype == 'numeric') mutate_all(., as.numeric) else .}
}

# Function to convert stata variables in factors
stata2factor <- function(x){
    map_tab <- stack(attr(x, 'labels'))
    
    if(length(map_tab$ind) == length(unique(x))){
      fac <- factor(x, labels = map_tab$ind)
    }
    
    else fac <- factor(x)
    
    return(fac)
}

# Function to calculate insolvency rate by year-month combination
insolv_ym <- function(.data){
  df_obs <- .data %>% 
    mutate(n = n(),
           n_firm = length(unique(crefo))) %>% 
    mutate(rechdat_ym = ymd(paste(year(rechdat), month(rechdat), '01','-')),
           rechdat_ym = yearmonth(rechdat_ym)) %>% 
    group_by(rechdat_ym) %>% 
    summarise(n_period = n(),
              n = mean(n),
              n_firm = mean(n_firm)) %>% 
    ungroup() %>% 
    as_tsibble(index=rechdat_ym) %>% 
    fill_gaps() %>% 
    as_tibble() 
  
  df_ins <- .data %>% 
    mutate(exitdat = ymd(paste(year(exitdat), month(exitdat), '01','-')),
           exitdat = yearmonth(exitdat)) %>% 
    group_by(exitdat, treat_final) %>% 
    summarise(n_ins = n()) %>% 
    ungroup() %>% 
    pivot_wider(names_from = treat_final, values_from = n_ins) %>% 
    filter(!is.na(exitdat)) %>% 
    ungroup() %>% 
    as_tsibble(index=exitdat) %>% 
    fill_gaps() %>% 
    as_tibble() 
  
  df_ins <- df_ins %>% 
    full_join(df_obs, by = c('exitdat'='rechdat_ym')) %>% 
    mutate(rate_control = round(`0`/n,5),
           rate_treat = round(`1`/n,5),
           rate_control2 = round(`0`/n_firm,5),
           rate_treat2 = round(`0`/n_firm,5)) %>% 
    rename(n_ins_control = `0`, n_ins_treat = `1`, date = exitdat) %>% 
    select(date, n, n_firm, n_period, n_ins_control, n_ins_treat, rate_control, rate_treat, rate_control2, rate_treat2) %>% 
    arrange(date)
  
  return(df_ins)
    
  
}

# Function to plot insolvencies over time as comparison to offical statistics
plot_insolv <- function(.dat, start = '2017-07-01', end = '2020-11-30'){
  
  df_stabu <- read.xlsx(file = file.path(getwd(), '02_Data', '03_SanityChecks', 'Vergleich_StaBu_MUP.xlsx'), sheetName = 'R') %>% 
    as_tibble() %>% 
    mutate(date = ymd(date)) %>% 
    mutate(source = 'DESTATIS')
  
  df_res <- insolv_ym(.dat) %>% 
    rowwise() %>% 
    mutate(n_ins = sum(n_ins_control, n_ins_treat, na.rm = T),
           n_active = n,
           ins_rate = sum(rate_control, rate_treat, na.rm = T)) %>% 
    ungroup() %>% 
    select(date, n_ins, n_active, ins_rate) %>% 
    mutate(date = ymd(paste(year(date), month(date), '01','-')),
           source = 'Creditreform Sample')
  
  df_diff <- df_stabu %>% 
    select(date, n_ins) %>% 
    full_join(df_res %>% select(date, n_ins), by = 'date') %>% 
    filter(date >= start) %>% 
    filter(date <= end) %>% 
    mutate(underestimation = 100*(n_ins.x - n_ins.y)/n_ins.x) %>% 
    select(date, underestimation)
  
  df_stabu <- df_stabu %>% 
    bind_rows(df_res) %>%  
    filter(date >= start) %>% 
    filter(date <= end) 
  
  coeff <- max(df_stabu$n_ins, na.rm = TRUE)/max(df_diff$underestimation, na.rm = TRUE)
  
  plot1 <- df_stabu %>% 
    ggplot() +
    geom_line(aes(x = date, y = n_ins, col = source)) +
    geom_line(aes(x = date, y = underestimation*coeff), data = df_diff, color = 'red', lty = 2) +
    xlab('Date') +
    ylab('Number of Monthly Insolvencies') +
    scale_y_continuous(
      
      # Features of the first axis
      name = "Number of Monthly Insolvencies",
      
      # Add a second axis and specify its features
      sec.axis = sec_axis(~./coeff, name="Missing Insolvencies in %")) +
    theme_jod
  
  plot2 <- df_stabu %>% 
    ggplot() +
    geom_line(aes(x = date, y = ins_rate, col = source)) +
    xlab('Date') +
    ylab('Monthly Insolvency Rate') +
    scale_y_continuous(labels = comma) + 
    theme_jod
  
  grid.arrange(plot1, plot2, ncol=1)
  
}

# Function that adjusts date to last day of month
clean_exitdat <- function(x){
  if(is.na(x)){
    res <- NA
  } 
  
  else {
    x <- ymd(x)
    day(x) <- days_in_month(x)
    res <- x
  }
}

# Function that takes first scraped date
extract_firstdate <- function(x){
  if(is.na(x)){
    return(NA)
  }
  
  else{
    res <- x %>% str_split("', ") %>% 
      unlist() %>% 
      as_tibble() %>% 
      filter(row_number() == n()) %>% 
      str_extract(pattern = regex("\\b\\d{1,2}.\\d{1,2}.\\d{2,4}\\b", ignore_case = TRUE)) %>% 
      dmy()
    return(res) 
  }
  
}


# Wave - Year mapping
w_y_map <- list('58' = 2020,
                '57' = 2019,
                '56' = 2019,
                '55' = 2018,
                '54' = 2018,
                '53' = 2017,
                '52' = 2017,
                '51' = 2016,
                '50' = 2016,
                '49' = 2015,
                '48' = 2015,
                '47' = 2014,
                '46' = 2014,
                '44' = 2013,
                '42' = 2012,
                '40' = 2011,
                '38' = 2010)



