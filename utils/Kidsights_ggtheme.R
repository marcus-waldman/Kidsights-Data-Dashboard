color_values_Kidsights_qualitative<-function(){
  return(c("#0b3474", "#6baedb", "#999999","#D2D2D2"))
}

scale_color_Kidsights_qualitative <- function() {
  
  scale_color_manual(values = color_values_Kidsights_qualitative())
  
}


scale_fill_Kidsights_qualitative <- function() {
  
  scale_color_Kidsights_qualitative()
  
}

theme_Kidsights <- function(legend.pos="bottom", legend.title = element_blank(), 
                            style = "white", plot.background = NULL, 
                            base_font_size = 12, title_font_ratio = 1.2, subtitle_font_ratio = .8) {
  library(jtools)
  library(extrafont) #https://gradientdescending.com/adding-custom-fonts-to-ggplot-in-r/
  
  loadfonts(device = "win", quiet = T)
  thm = jtools::theme_nice(legend.pos=legend.pos, style = style, base_family = "Century Gothic", base_size = base_font_size)
  thm$legend.title = legend.title
  thm$text$size = base_font_size  
  thm$plot.title = element_text(size = floor(title_font_ratio*base_font_size), face = "bold", hjust = 0)
  thm$plot.subtitle = element_text(size = floor(subtitle_font_ratio*base_font_size), face = "italic", hjust = 0)
  if(!is.null(plot.background)){thm$plot.background = plot.background}
  return(thm)
  
}
