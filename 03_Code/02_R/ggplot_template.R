
# Packages ----------------------------------------------------------------
library(ggplot2)


# Colors ------------------------------------------------------------------
bb_blue_dark <- rgb(0, 69, 125, maxColorValue = 255)
bb_blue_medium <- rgb(102, 144, 177, maxColorValue = 255)
bb_blue_light <- rgb(204, 218, 229, maxColorValue = 255)

bb_red_dark <- rgb(230, 68, 79, maxColorValue = 255)
bb_red_medium <- rgb(235, 105, 114, maxColorValue = 255)
bb_red_light <- rgb(240, 143, 149, maxColorValue = 255)

bb_green_dark <- rgb(151, 191, 13, maxColorValue = 255)
bb_green_medium <- rgb(172, 204, 61, maxColorValue = 255)
bb_green_light <- rgb(193, 216, 110, maxColorValue = 255)

ml_green_dark <- "seagreen4"
ml_green_medium <- "seagreen3"
ml_green_light <- "seagreen2"
# ml_green_dark <- "aquamarine4"
# ml_green_medium <- "aquamarine3"
# ml_green_light <- "aquamarine2"

function_gradient_blue <- colorRampPalette(c(bb_blue_light, bb_blue_dark))
function_gradient_green <- colorRampPalette(c(ml_green_light, ml_green_dark))
function_gradient_redTOgreen <- colorRampPalette(c(bb_red_dark, ml_green_dark))
function_gradient_redTOwhiteTOgreen <- colorRampPalette(c(bb_red_dark, "white", ml_green_dark))
function_gradient_redTOblueTOgreen <- colorRampPalette(c(bb_red_dark, bb_blue_dark, ml_green_dark))



# Theme -------------------------------------------------------------------


theme_jod <- theme(
  panel.background = element_blank(),
  panel.grid.major.y = element_line(color = "grey90", size = 0.5),
  panel.grid.minor.y = element_line(color = "grey90", size = 0.5),
  panel.grid.major.x = element_line(color = "grey90", size = 0.5),
  panel.grid.minor.x =  element_blank(),
  #panel.grid.major.x = element_blank(),
  #panel.border = element_rect(fill = NA, color = "grey20"),
  axis.text.x = element_text(size = 7, angle = 45, hjust = 1),
  axis.text.y = element_text(size = 7),
  #axis.title = element_text(size = 12, face = "bold", margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt")),  # not working?
  axis.title.y = element_text(size = 9, vjust = 3,  margin = margin(t = 0, r = 0, b = 0, l = 5, unit = "pt")),
  axis.title.x = element_text(size = 9,  vjust = -.3),
  plot.title = element_text(size = 14 , face = "bold", hjust = 0),
  legend.position="top",
  legend.box = "horizontal",
  legend.key = element_blank(),
  legend.text = element_text(size = 9),
  legend.title = element_text(size = 9, face = "bold"),
  legend.spacing.x = unit(0.2, 'cm'),
  legend.margin=margin(0,0,0,0),
  legend.box.margin=margin(-5,0,-10,-5),
  #axis.ticks = element_blank(),
  strip.text.x = element_text(size = 10, color = "white"#, face = "bold"
                              ),    # changes facet labels
  strip.text.y = element_text(size = 10, color = "white"#, face = "bold"
                              ),
  #strip.background = element_rect(color="black", fill="grey", size=1, linetype="solid")
  strip.background = element_rect(fill="grey", color="black"),
  #plot.margin = unit(c(1,1,1,1), "cm"),
  panel.border = element_rect(fill=NA,color="black", size=0.5, 
                              linetype="solid")
)
