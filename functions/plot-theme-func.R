
# load the ggplot2 library
library(ggplot2)

# load a custom plotting theme
theme_meta <- 
  function(base_family = "") {
    theme(panel.background=element_rect(fill="white", linetype="solid"),
          panel.border = element_blank(),
          axis.line = element_line(size = 0.5),
          axis.text = element_text(colour="black",size=10),
          legend.key = element_rect(fill = NA))
  }