#' seastack theme
#'
#' a custom theme designed for seastack plots
#' @export
theme_seastack <- function(){
  theme_classic() %+replace%    #start with theme_classic and replace elements we want to change

    theme(
      # legend
      legend.position = "none",

      # panel spacing
      panel.spacing = unit(0.3, "cm"),

      # text elements
      axis.title.y = element_text(size = 14, face = "bold", margin = margin(0,15,0,0), angle = 90), # for some reason without specifying the angle it makes the y axis title horizontal
      axis.title.x = element_text(size = 14, face = "bold", margin = margin(10,0,0,0)),
      axis.text.y = element_text(size = 14),
      axis.text.x = element_blank(),
      strip.text = element_text(size = 12),

      # background
      strip.background = element_blank()
    )
}
