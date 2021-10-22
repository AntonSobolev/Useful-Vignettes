if (!require("pacman")) install.packages("pacman")
p_load(data.table, dplyr, plyr, tidyr, devtools, ggpubr) # Packages for text mining
p_load(ggraph, igraph ,gganimate, graphlayouts, patchwork)
p_load(gifski, ggrepel)
p_load(png)  
theme_set(theme_pubr(border = TRUE))
# devtools::install_github('thomasp85/ggraph')

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
for (i in 1:50){
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


g1 <- ggplot(data = d.sampled.results.for.animation) +
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





