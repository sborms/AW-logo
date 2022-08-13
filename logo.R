library("ggplot2")
library("gridExtra")

theme_AW <- function(base_size = 12, base_family = "") {
  theme_grey(base_size = base_size, base_family = base_family) %+replace%
    theme(
      # Specify axis options
      axis.line = element_blank(),  
      axis.text.x = element_blank(),  
      axis.text.y = element_blank(),  
      axis.ticks = element_blank(),  
      axis.title.x = element_blank(),  
      axis.title.y = element_blank(), 
      # Specify legend options
      legend.position = "none",
      # Specify panel options
      panel.background = element_blank(),  
      panel.border = element_blank(),  
      panel.grid.major = element_blank(),  
      panel.grid.minor = element_blank(),  
      # Specify faceting options
      strip.background = element_rect(fill = "grey30", color = "grey10"),  
      strip.text.x = element_text(size = base_size*0.8, color = "white"),  
      strip.text.y = element_text(size = base_size*0.8, color = "white", angle = -90),  
      # Specify plot options
      plot.background = element_rect(color = "black", fill = "black"),  
      plot.margin = unit(rep(1, 4), "lines")
    )
}

####################################################################

x <- seq(-1.3, 1.3, length.out = 10000)
y1 <- -(sin(2 * x) * sin(20 * x))
y2 <- rep(NA, length(y1))
l <- seq(-0.25, 0.30, length.out = 500)
y2[4501:5000] <- l
y2[5001:5500] <- l[length(l):1]
w1 <- 1:length(x) %in% 4000:6000
w2 <- 1:length(x) %in% 4501:5500
data <- data.frame(x = x, y1 = y1, w1 = w1, y2 = y2, w2 = w2)

p <- ggplot(data) +
  geom_line(aes(x = x, y = y1, colour = (w1 == TRUE), group = 1), size = 1.5) + 
  geom_line(aes(x = x, y = y2, colour = (w2 == TRUE), group = 1), size = 1.5) + 
  theme_AW() +
  scale_colour_manual(values = c("white", "orange"))
p

ggsave("AW.jpg", p, "jpg", width = 2200, height = 1700, units = "px")