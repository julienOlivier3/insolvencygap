use "K:\MUP\panel56.dta" 



drop if jahr < 2010



export delimited "Q:\Meine Bibliotheken\Research\SME_Corona\02_Data\01_PanelData\df_panel2010_2019.txt", delimiter("tab")
