library(tidyverse)
library(visdat)
library(cowplot)
library(ggrepel)
library(mapproj) # polar orthographic map projection
library(ggthemes) 
library(here)
library(extrafont)
library(extrafont)
library(knitr)
library(png)
library(grid)
# Get gegrid# Get geospatial data for Antarctica only
antarctica <- map_data("world", region = "Antarctica")

# map of antartica
df_penguinloc <-
  tibble(
    island = c("Dream", "Biscoe", "Torgersen"),
    lat_y = c(-64.7333, -65.4333, -64.7666636),
    long_x = c(-64.2333, -65.5000, -64.083333)
  ) 


df_penguinloc <- penguins %>% 
  group_by(island) %>%  summarise(amount=n()) %>% 
  left_join(df_penguinloc, by = "island")

# pic
img <- readPNG("./Data/penguin2.png")
g <- rasterGrob(img, interpolate=TRUE)

# barplot

d <- penguins %>% filter(year==2009) %>% 
  mutate(island = factor(island), 
         island = factor(island, levels = rev(levels(island)))) %>%  
ggplot() +
  stat_count(aes(island, fill = species), alpha = 0.8) +
  annotate("text", y=3,  x= "Torgersen", label= "Torgersen", color = "#1874CD")+
  annotate("text", y=3,  x= "Dream", label= "Dream", color = "#c02728")+
  annotate("text", y=3,  x= "Biscoe", label= "Biscoe", color = "#53868B")+
  scale_fill_manual(values = c("#66c2a5","#fc8d62","#8da0cb")) +
  scale_y_reverse()+
  labs(caption = "Source: Gorman, Williams and Fraser, 2014 | Graphics: Julián Avila-Jiménez") +
  theme_minimal() +
  theme(legend.position.inside = c(0.2,0.3),
        axis.title = element_blank(),
        axis.text.x = element_blank(),
        #panel.grid = element_blank(),
        plot.background = element_rect(fill="#f9f9f9", color = "#f9f9f9"))
d

# main map
p <- ggplot(antarctica, aes(long, lat, group = group)) +
  geom_polygon(fill = "#506B8E", alpha = .8) +
  coord_map("ortho", orientation = c(-90, 0, 0),
            xlim = c(-62, -55),
            ylim = c(-75, - 60)) +
  geom_text_repel(df_penguinloc, mapping=aes(long_x, lat_y, label = island), 
                  group=1, color = c("#53868B", "#c02728", "#1874CD"), 
                  box.padding = 0.5,
                  nudge_y = 1,  nudge_x = -2, min.segment.length = 0) +
  geom_point(df_penguinloc, mapping=aes(long_x, lat_y,  
                                        group = 1, 
                                        colour = island), 
             alpha =.7)+
  scale_color_manual(values = c("#53868B", "#c02728", "#1874CD"))+
    labs(title = "Penguins in Palmer Archipelago",
       subtitle = "Recorded penguins in 2009 and their nesting Islands") +
  theme_map() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 20),
        plot.subtitle =  element_text(hjust = 0.5),
        plot.background = element_rect(fill="#f9f9f9", color = "#f9f9f9"))
p

# inset map
inset <- ggplot(antarctica, aes(long, lat, group = group)) +
  geom_polygon(fill = "#506B8E", alpha = .5) +
  coord_map("ortho", orientation = c(-90, 0, 0)) +
  geom_point(df_penguinloc, mapping=aes(long_x, lat_y,  
                                        group = island, 
                                        colour = island), 
             alpha =.5, size = 1)+
  annotate("rect", color="black", fill = "transparent",
           xmin = -68, xmax = -54,
           ymin = -75, ymax = -60)+
  labs(title = "Antarctica") +
  theme_map() +
  theme(legend.position = "none",
        panel.grid.major.y = element_line(colour="grey"),
        plot.title = element_text(hjust = 0.5),
        plot.background = element_rect(fill="#f9f9f9", color = "#f9f9f9"))
inset


# draw together inset and main map
a <- ggdraw(p ) +
  draw_plot(inset, .47, .38, .5, .4)
a

# plot everything together
p1 <- plot_grid(a,d, ncol = 1, rel_widths = c(4, 2), rel_heights = c(2,1))+
  theme(plot.background = element_rect(fill="#f9f9f9")) +
  labs(title = "Penguins in Palmer Archipelago")

p1
