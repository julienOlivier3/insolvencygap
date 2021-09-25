# MIGRATED TO PYTHON

setwd("Q:\\Meine Bibliotheken\\Research\\SME_Corona")
source(file.path(getwd(), '03_Code', '02_R', 'step0_setup.R'))

# -------------------------------------------------------------------------
# #########################################################################
# -------------------------------------------------------------------------

# Load Appended & Cleaned Panel -------------------------------------------

df_panel <- read_delim(file = file.path(getwd(), '02_Data', '01_PanelData', '01_Backup', 'df_panel_clean_final.txt'),
                       delim = '\t')


library(rvest)

clean_html <- function(html){
  res <- html %>% 
    str_replace_all("\n", " ") %>% 
    #str_remove_all("\t") %>% 
    trimws()
  return(res)
}

# Scraping 1 ---------------------------------------------------------------

landing <- "https://www.firmenwissen.de/musterfirmenprofil.html?crefoId="

urls <- paste0(landing, unique(df_panel$crefo))
crefos <- unique(df_panel$crefo)

urls[1:3]
format(crefos[1:3], scientific=FALSE)
df_panel %>% filter(insolv==1)


raw_html_dead <- read_html("https://www.firmenwissen.de/musterfirmenprofil.html?crefoId=2010001919")
raw_html_living <- read_html("https://www.firmenwissen.de/musterfirmenprofil.html?crefoId=2010000137")
raw_html_na <- read_html("https://www.firmenwissen.de/musterfirmenprofil.html?crefoId=2010000139")



insolv_scraper <- function(crefo, landing, encoding = "UTF-8"){
  
  url <- paste0(landing, crefo)
  
  raw_html <- tryCatch(
    {
      read_html(url, encoding = encoding) 
    },
    error = function(cond) {
      return(NA)
    }
  )
  
  if(is.na(raw_html)) {
    return(tibble(crefo = crefo, insolv = NA, exitdat = NA, insolv_text = NA))
  }
  
  else if(raw_html %>% 
            html_node('body') %>% 
            html_nodes(xpath =  "//*[@id='Insolvenzverfahren']") %>% 
            is_empty()){
    return(tibble(crefo = crefo, insolv = 0, exitdat = NA, insolv_text = NA))
  }
  
  else {
    
    temp <- raw_html %>% 
      html_node('body') %>% 
      html_nodes(xpath =  "//*[@id='Insolvenzverfahren']") %>% 
      xml_parent() %>% 
      html_nodes(xpath = "//div[contains(@class, 'announcements')]") %>% 
      html_text() %>% 
      clean_html()
    
    res <- tibble(crefo = crefo, insolv = 1) %>% 
      bind_cols(temp %>% 
                  paste(collapse = " ") %>% 
                  str_extract(pattern = regex("\\b\\d{1,2}.\\d{1,2}.\\d{2,4}\\b.{1,3}Insolvenzbekanntmachung", ignore_case = TRUE)) %>% 
                  str_extract(pattern = regex("\\b\\d{1,2}.\\d{1,2}.\\d{2,4}\\b", ignore_case = TRUE)) %>% 
                  enframe(name = NULL, value = "exitdat")) %>% 
      bind_cols(temp %>%  
                  tibble(text = .) %>% 
                  nest(data = everything()) %>% 
                  rename(insolv_text=data))
    
    
    return(res)
    
  }
  
}


  

df_insolv = NULL

for (i in c(2010001919, 2010000137, 2010000139, 2010061880)){
  temp <- insolv_scraper(i, landing = landing)
  df_insolv <- df_insolv %>% 
    bind_rows(temp)
}

  



for (i in 135:length(crefos)){
  
  Sys.sleep(runif(n = 1, min = 0, max = 0.5))
  
  temp <- insolv_scraper(crefos[i], landing = landing)
  df_insolv <- df_insolv %>% 
    bind_rows(temp)
  
  if(i %% 1000 == 0){
    print(i)
  }
  
  
}

save(df_insolv, file = file.path(getwd(), '02_Data', '09_Insolvency', 'df_insolv.RData'))
  
insolv_scraper(landing = 'Q:\\Meine Bibliotheken\\Research\\SME_Corona\\02_Data\\09_Insolvency\\01_html\\2010001919.html', crefo = '', encoding = 'latin-1')
read_html('Q:\\Meine Bibliotheken\\Research\\SME_Corona\\02_Data\\09_Insolvency\\01_html\\2010001919.html')


# Scraping 2 --------------------------------------------------------------

df_crefo <- df_panel %>% 
  filter(treat_final==0) %>% 
  filter(boni_orig>=500) %>%
  filter(insolv==0) %>% 
  distinct(crefo)

df_panel %>% 
  filter(treat_final==0) %>% 
  filter(boni_orig>=500) %>%
  group_by(crefo) %>% 
  filter(all(insolv==0)) %>%
  ungroup() %>% 
  distinct(crefo)

df_panel %>% 
  filter(boni_orig >= 500) %>% 
  group_by(crefo) %>% 
  filter(all(insolv==0)) %>% 
  ungroup() %>% 
  distinct(crefo)

write_delim(df_crefo, path = file.path(getwd(), "02_Data", "09_Insolvency", "crefos_badrating.txt"), delim = '\t')
