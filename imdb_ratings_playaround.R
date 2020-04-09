#load relevant package libraries
library(tidyverse)
library(viridis)  # for colour palette (colourblind and greyscale friendly)
library(png)
library(cowplot)
# set ID for The Simpsons - from https://www.imdb.com/title/tt0096697/
simpsons_id <- "tt0096697"
law_order_id <- "tt0098844"
law_order_svu <- "tt0203259"

# load ratings and episodes, change any \N to NA (have to do double slash to 'escape' the quotes)
ratings <- read_tsv("data/title.ratings.tsv/data.tsv", na = "\\N")
episodes <- read_tsv("data/title.episode.tsv/data.tsv", na = "\\N")
#episode_names <- read_tsv("data/title.basics.tsv/data.tsv", na = "\\N")
# note there will be ~61040 parsing errors

# join the simpons episodes with their ratings
# first create a new dataframe and put episodes into it
simpsons_episodes <- episodes %>%
  # filter this to only include episodes of the simpsons, by using its ID as the parent
  filter(parentTconst == simpsons_id) %>%
  # join the ratings dataframe onto the current one, where the episode IDs match (tconst)
  # using inner join so that it only includes the episodes with ratings (otherwise would include 
  # episodes that haven't aired yet)
  inner_join(ratings, by = "tconst")

# Looks like there are 678 episodes that have aired - wow!
# A check on Wikipedia says there are 679 to date (2020-04-08), but I found that Wikipedia treats
# Season 28 Epsiode "The Great Phatsby" as two separate episodes (12 and 13), IMDb treats it as one (12)
# so we're not missing an episode.

# theme for the plot
tile_theme = theme(
  #text = element_text(family = "Akbar"),
  plot.background = element_rect(fill = "#FFD90F", colour = NA),
  panel.background = element_rect(fill=NA),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  axis.ticks = element_blank(), 
  axis.text = element_text(color="black", size=8),
  axis.text.y  = element_text(hjust=1),
  legend.text = element_text(color="black", size=8),
  legend.position = "top",
  legend.background = element_rect(fill = NA),
  legend.title=element_text())

plot <- ggplot(data = simpsons_episodes, aes(x = seasonNumber, y = episodeNumber)) +
  geom_tile(aes(fill = averageRating), colour = "black") +
  geom_text(aes(label = format(averageRating, digits = 2)), size = 2) +
  coord_fixed() +
  scale_y_continuous(trans = "reverse",
                     expand = c(0,0),
                     breaks = 1:max(simpsons_episodes$episodeNumber),
                     name = "Episode") +
  scale_x_continuous(position = "top",
                     expand = c(0,0),
                     breaks = 1:max(simpsons_episodes$seasonNumber),
                     name = "Season") +
  scale_fill_viridis(name = "Rating") + tile_theme +
 guides(fill = guide_colourbar(barwidth = 10, barheight = 1, title.position = "top", title.hjust = 0.5,
                               frame.colour = "black", ticks.colour = "black"))

# get just the legend so we can plot it separately
legend <- get_legend(plot) # from cowplot

# read in the images
homer <- readPNG("images/homer_face_transparent.png") %>%
  rasterGrob(interpolate = TRUE)
logo <- readPNG("images/simpsons_logo_transparent.png") %>%
  rasterGrob(interpolate = TRUE)

#from cow_plot
bottom_row <- plot_grid(homer, plot + theme(legend.position="none"), ncol = 2, rel_widths = c(1, 4)) +
  theme(plot.background = element_rect(fill = "#FFD90F", colour = NA))
bottom_row

top_row <- plot_grid(logo, legend3, ncol = 2, rel_widths = c(1,1)) +
  theme(plot.background = element_rect(fill = "#FFD90F", colour = NA))
top_row

combine <- plot_grid(top_row, plot + theme(legend.position="none"), nrow = 2, rel_heights = c(1, 5), align = "hv") +
  theme(plot.background = element_rect(fill = "#FFD90F", colour = NA))
combine

legend2 <- add_sub(legend, "Good", x  = 0.5, y = 1, hjust = -2.2, size = 10)
legend3 <- add_sub(legend2, "Bad", x  = 0.5, y = 2, hjust = 4.4, size = 10)
plot(legend3)


num_eps <- simpsons_episodes %>%
  group_by(seasonNumber) %>%
  summarise(n = n()) %>%
  ungroup()

ggplot(data = ratings, aes(x = numVotes, y = averageRating)) + geom_point(alpha = 0.2)

## install.packages(devtools)
#devtools::install_github("Ryo-N7/tvthemes")
#loadfonts(device='win')
#windowsFont()
