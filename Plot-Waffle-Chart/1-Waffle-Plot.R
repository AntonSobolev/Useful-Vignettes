# Description: This file contains the code to calculate the final grade for the Fall 2023 semester at TU.


source("https://asobolev.com/files/source.R")
p_load(tidyr)
p_load(waffle)

library(ggplot2)
library(dplyr)


library(ggplot2)
library(dplyr)
library(ggsci)   # For the jco color palette
library(ggpubr)  # For additional styling

create_combined_waffle_chart <- function(banana_per_day, apple_per_day) {
  if(length(banana_per_day) != length(apple_per_day)) {
    stop("The length of banana_per_day and apple_per_day vectors must be the same.")
  }

  # Create a data frame for bananas and apples
  day_sequence <- 1:length(banana_per_day)
  banana_data <- data.frame(Day = rep(day_sequence, banana_per_day), 
                            Fruit = "Banana", 
                            Count = unlist(mapply(seq, rep(1, length(banana_per_day)), banana_per_day)))
  apple_data <- data.frame(Day = rep(day_sequence, apple_per_day), 
                           Fruit = "Apple", 
                           Count = unlist(mapply(seq, banana_per_day + 1, banana_per_day + apple_per_day)))

  # Combine the data
  combined_data <- rbind(banana_data, apple_data)

  # Plot the data
  p <- ggplot(combined_data, aes(x = Day, y = Count, fill = Fruit)) +
    geom_tile(color = "darkgrey", size = 0.3, width = 0.7, height = 0.7,lineend = "round") +
    scale_fill_jco() + # Using the jco color palette
    labs(title = "Daily Consumption of Bananas and Apples Over Days", 
         x = "Day", y = "", fill = "Fruit") +
    theme_minimal() +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank()) +
    coord_fixed(ratio = 1) # Ensure squared tiles

  # Apply ggpubr styling
  ggpubr::ggpar(p, font.title = c(14, "bold", "darkblue"),
                font.x = c(12, "bold", "darkblue"),
                font.y = c(12, "bold", "darkblue"),
                font.legend = c(12, "bold", "darkblue"))
}

# Example usage
banana_per_day <- c(2, 3, 2, 1, 3)
apple_per_day <- c(3, 2, 4, 3, 1)
create_combined_waffle_chart(banana_per_day, apple_per_day)
