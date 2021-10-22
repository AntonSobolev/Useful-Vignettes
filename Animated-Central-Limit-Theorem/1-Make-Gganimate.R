if (!require("pacman")) install.packages("pacman")
p_load(data.table, dplyr, plyr, tidyr, devtools, ggpubr) # Packages for text mining
p_load(ggraph, igraph ,gganimate, graphlayouts, patchwork)
p_load(gifski, ggrepel,transformr)
p_load(png)  
theme_set(theme_pubr(border = TRUE))
# devtools::install_github('thomasp85/ggraph')
# devtools::install_github("thomasp85/transformr")

# https://www.r-bloggers.com/2021/09/animating-network-evolutions-with-gganimate/
`%+%` <- function(x,y){ paste0(x, y)}
## ------------------------------------------------------------------------
library(ggplot2)
library(gganimate)

path <- 'https://asobolev.com/files/0-classes/2021F-CyberPolicy/0-Data/ransomware-augmented.csv'
d <- fread(path)
true_mean <- mean(d$ransom)
set.seed(14)
sampled.results <- data.table() # create empty bin to store the results
for (i in 1:1000){
  n <- 20 # Define the size of the sample PLAY WITH THIS NUMBER!: make it small or large and check how the sampling distribution evolves
  sampled.ids <-  sample(d$id, n) # Sample from population data
  d.sample.i <- d[id %in% sampled.ids,] # Subset sampled cases
  mean.i <- mean(d.sample.i$ransom) # Calculate mean
  st.error.i <- sd(d.sample.i$ransom)/sqrt(n)
  sampled.results.i <- data.table(mean_i = mean.i, std_error_i = st.error.i, index = i) # collect the results
  sampled.results <- list(sampled.results, sampled.results.i)
  sampled.results <- rbindlist(sampled.results)
}

sampled.results[,lower_bound:= mean_i - std_error_i * 1.96]
sampled.results[,upper_bound:= mean_i + std_error_i * 1.96]
sampled.results[,`True Mean Within Confidence Intreval` := "No :("]
sampled.results[lower_bound < true_mean & upper_bound > true_mean,
                `True Mean Within Confidence Intreval`  := "Yes!"]

d.sampled.results.for.animation <- data.table()

for(index.i in sampled.results$index){
  d.i <- sampled.results[index %in% 1:index.i,]
  d.i[,frame_i := index.i]
  d.sampled.results.for.animation <- list(d.sampled.results.for.animation, d.i) %>% rbindlist()
  
}


# Confidence Interval

g1 <- ggplot(data = d.sampled.results.for.animation[index.i %in% 1:50]) +
  geom_segment(size=1, alpha = .4,
               aes(x=index, xend=index, y=lower_bound,  yend=upper_bound,
                   col = `True Mean Within Confidence Intreval`), 
  ) + 
  geom_hline(yintercept = true_mean, col = "darkgreen", alpha = .8, size = 1, linetype = 1) +
  geom_point(aes(x = index, y = mean_i), col = "blue", shape = 4, size =5 ) +
  xlab("Order of Samples") +ylab("Ransom") + ylim(0,20) +
  transition_states(
    frame_i,
    transition_length = 2,
    state_length = 1
  ) +
  enter_fade() + 
  exit_shrink() +
  ease_aes('sine-in-out')



  
p.anumated <- animate(g1, duration = 30, height = 1000, width = 1500,
                      fps = 5, res = 200,
                      renderer = gifski_renderer())
anim_save("output2.gif", animation = p.anumated, path = "0-Gif")

unique(d.sampled.results.for.animation$frame_i)


# Mean Histogram
frames.in.plot <- seq(1,100, 50)

g1 <- ggplot(data = d.sampled.results.for.animation[frame_i %in% frames.in.plot,],
             aes(x = mean_i)
             ) +
  geom_histogram(fill = "navy", alpha = .1, col = "black", binwidth = 0.1) + 
  geom_vline(xintercept = true_mean,
             col = "darkgreen", alpha = .8, size = 1, linetype = 1) +
  xlab("Mean Sample i") +
  transition_states(
    frame_i,
    transition_length = 1,
    state_length = 1
  ) +
  enter_fade() + 
  exit_shrink() +
  ease_aes('sine-in-out') + xlim(0,20)




p.anumated <- animate(g1, height = 1000, width = 1500,
                      fps = 20, res = 200, nframes = 100,
                      renderer = gifski_renderer()) 
anim_save("sampling-distribution-histogram.gif", animation = p.anumated, path = "0-Gif")


# Mean Histogram
frames.in.plot.1 <- seq(1,100, 10)
frames.in.plot.2 <- seq(200,1000, 100)
frames.in.plot <- c(frames.in.plot.1,frames.in.plot.2)

g1 <- ggplot(data = d.sampled.results.for.animation[frame_i %in% frames.in.plot,],
             aes(x = mean_i)
) +
  geom_density(fill = "orange", alpha = .1, col = "black") + 
  geom_vline(xintercept = true_mean,
             col = "darkgreen", alpha = .8, size = 1, linetype = 1) +
  xlab("Mean Sample i")  +
  transition_states(
    frame_i,
    transition_length = 1,
    state_length = 1
  ) +
  enter_fade() + 
  exit_shrink() +
  ease_aes('sine-in-out') + xlim(0,20) + ylim(0,1)




p.anumated <- animate(g1, height = 1000, width = 1500,
                      fps = 20, res = 200, nframes = 500,
                      renderer = gifski_renderer()) 
anim_save("sampling-density.gif", animation = p.anumated, path = "0-Gif")

