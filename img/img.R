library(hexSticker)
library(ggplot2)
library(showtext)
library(ggpubr)

p <- ggplot(data = data.frame(x = c(-3, 3)), aes(x)) +
  stat_function(fun = dnorm, n = 200, args = list(mean = 0, sd = 1), color="#f39c12", size = 1.5) +
  ylab("") +
  scale_y_continuous(breaks = NULL)+
  theme_void() + 
  theme_transparent()

sticker(p, package="normr", p_size=10, s_x=1, s_y=.75, s_width=1.3, s_height=1, h_fill="#000000", h_color="#f39c12",filename="normr-sticker.png")