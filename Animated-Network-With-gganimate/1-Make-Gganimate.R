if (!require("pacman")) install.packages("pacman")
p_load(data.table, dplyr, plyr, tidyr, devtools) # Packages for text mining
p_load(ggraph, igraph ,gganimate, graphlayouts, patchwork)
p_load(gifski, ggrepel)
p_load(png)  
# devtools::install_github('thomasp85/ggraph')

# https://www.r-bloggers.com/2021/09/animating-network-evolutions-with-gganimate/
`%+%` <- function(x,y){ paste0(x, y)}
## ------------------------------------------------------------------------
path <- "0-Data/cyberattacks-across-the-globe-cases.csv"
d <- fread(path)

path <- "0-Data/Contry-Code-Dictionary.csv"
dictionary <- fread(path)

d[,source:=mapvalues(source, from = dictionary$StateNme, to = dictionary$StateAbb)]
d[,target:=mapvalues(target, from = dictionary$StateNme, to = dictionary$StateAbb)]

countries.of.interest.out <- table(d$source) %>% sort( decreasing = T)
countries.of.interest.out <- names(countries.of.interest.out[1:5])
countries.of.interest.in <- table(d$target) %>% sort( decreasing = T)
countries.of.interest.in <- names(countries.of.interest.in[1:5])

countries.of.interest <- c(countries.of.interest.out,countries.of.interest.in) %>% unique()

# d <- d[source %in% countries.of.interest,]
# d <- d[target %in% countries.of.interest,]
d <- d[,.(weight = sum(num)), by = .(source, target, year)]
d <- unique(d)

rare.countries <- c("EST", "LEB")
d <- d[!source %in% rare.countries,]
d <- d[!target %in% rare.countries,]

d[,weight:=NULL]
# Create IDss
all.countries <- c(d$source, d$target) %>% unique()
all.ids <- 0:(length(all.countries)-1)

nodes.table <- data.table(name = all.countries, id = all.ids)

d.igraph <- list()
years <- unique(d$year) %>% sort()
# years <- 2005:2006
i <- 1
for(year.i in years){
# Make nodes
nodes.i <- c(d$source[d$year == year.i], d$target[d$year == year.i]) %>% unique()
ids.i <- mapvalues(nodes.i, from = all.countries, to = all.ids) %>% as.integer()
nodes.i <- data.table(name = nodes.i, id = ids.i)
# Make edges <-
edges.i <- d[year == year.i,]
edges.i[,year:=NULL]
d.igraph.i <- graph_from_data_frame(d = edges.i, vertices = nodes.table, directed=TRUE)
d.igraph[[i]] <- d.igraph.i
i <- i + 1
}
str(d.igraph)
d.igraph[[]]


names(d.igraph[2])
# invalid vertex id,
# xy <- layout_as_dynamic(d.igraph[[1]], alpha = 0.2)
xy <- layout_as_dynamic(d.igraph, alpha = 1)
# i = 1
# for(i in 1:length(xy)){
#   xy[[i]][1,1] <
# }



nodes_lst <- lapply(1:length(d.igraph), function(i) {
  cbind(igraph::as_data_frame(d.igraph[[i]], "vertices"),
        x = xy[[i]][, 1], y = xy[[i]][, 2], frame = i + 2000
  )
})

V(d.igraph)

edges_lst <- lapply(1:length(d.igraph), function(i) cbind(igraph::as_data_frame(d.igraph[[i]], "edges"), frame = i))
edges_lst <- lapply(1:length(d.igraph), function(i) {
  edges_lst[[i]]$x <- nodes_lst[[i]]$x[match(edges_lst[[i]]$from, nodes_lst[[i]]$name)]
  edges_lst[[i]]$y <- nodes_lst[[i]]$y[match(edges_lst[[i]]$from, nodes_lst[[i]]$name)]
  edges_lst[[i]]$xend <- nodes_lst[[i]]$x[match(edges_lst[[i]]$to, nodes_lst[[i]]$name)]
  edges_lst[[i]]$yend <- nodes_lst[[i]]$y[match(edges_lst[[i]]$to, nodes_lst[[i]]$name)]
  edges_lst[[i]]$id <- paste0(edges_lst[[i]]$from, "-", edges_lst[[i]]$to)
  edges_lst[[i]]$status <- TRUE
  edges_lst[[i]]
})

all_edges <- do.call("rbind", lapply(d.igraph, get.edgelist))
all_edges <- all_edges[!duplicated(all_edges), ]
all_edges <- cbind(all_edges, paste0(all_edges[, 1], "-", all_edges[, 2]))
i = 1
edges_lst <- lapply(1:length(d.igraph), function(i) {
  idx <- which(!all_edges[, 3] %in% edges_lst[[i]]$id)
  if (length(idx != 0)) {
    tmp <- data.frame(from = all_edges[idx, 1], to = all_edges[idx, 2], id = all_edges[idx, 3])
    tmp$x <- nodes_lst[[i]]$x[match(tmp$from, nodes_lst[[i]]$name)]
    tmp$y <- nodes_lst[[i]]$y[match(tmp$from, nodes_lst[[i]]$name)]
    tmp$xend <- nodes_lst[[i]]$x[match(tmp$to, nodes_lst[[i]]$name)]
    tmp$yend <- nodes_lst[[i]]$y[match(tmp$to, nodes_lst[[i]]$name)]
    tmp$frame <- i
    tmp$status <- FALSE
    edges_lst[[i]] <- rbind(edges_lst[[i]], tmp)
  }
  edges_lst[[i]]
})

edges_df <- do.call("rbind", edges_lst)
edges_df$frame <- edges_df$frame + 2000
unique(edges_df$frame)
nodes_df <- do.call("rbind", nodes_lst)
unique(nodes_df$frame)

major.countries<- c("USA", "RUS", "CHN")
nodes_df <- as.data.table(nodes_df)
edges_df <- as.data.table(edges_df)

nodes_df[,major:="No"]
nodes_df[name %in% major.countries,major:="Yes"]
table(nodes_df$major)

set.seed(1234)
edges_df[,ids:=1:nrow(edges_df)]
all.attacks <- edges_df$ids[edges_df$status == 1]
sampled.ids <- sample(all.attacks, 70)
edges_df[,cluster:="Intrusion"]
edges_df[ids %in% sampled.ids,cluster:="Infiltration"]
table(edges_df$cluster)

p <- ggplot() +
 
  geom_curve(
    data = edges_df,
    aes(x = x, xend = xend, y = y, yend = yend,
        group = id, alpha = status, col = cluster), 
    show.legend = FALSE,
    arrow = arrow(type = "closed", length = unit(0.2, "inches")), 
    curvature = 0.2
  ) + # , col = "blue", alpha = .5, arrow = arrow(type = "closed")
  geom_label(
    data = nodes_df, aes(x, y, group = name, label = name, fill = major) #, # , fill = as.factor(smoke)
    #hjust=0, vjust=0
  ) +
  # geom_point(
  #   data = nodes_df, aes(x, y, group = name), # , fill = as.factor(smoke)
  #   shape = 21, size = 4, show.legend = FALSE
  # ) +

  scale_fill_manual(values = c("white", "cornsilk2")) +
  scale_colour_manual(values = c("black", "blue")) +
  
  scale_alpha_manual(values = c(0, 1)) +
  ease_aes("quadratic-in-out") +
  transition_states(frame, state_length = 8, transition_length = 3,  wrap = T) +
  labs(title = " {closest_state}") + 
  guides() + 
  theme_void()

  
p.anumated <- animate(p, duration = 3, height = 1000, width =1500,
                      fps = 1, res = 200,
                      renderer = gifski_renderer())
anim_save("output2.gif", animation = p.anumated, path = "0-Gif")
