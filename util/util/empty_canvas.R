

empty_canvas <- function(label="Add text", colour="black", size=15) {
  ggplot(data.frame(x=1, y=1), aes(x, y) )   + 
    theme_void() + 
    annotate("text", x = 1, y = 1, label = label,  colour = colour, size = size)
  
}
