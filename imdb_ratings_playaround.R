#load relevant package libraries
library(tidyverse)

# set ID for The Simpsons - from https://www.imdb.com/title/tt0096697/
simpsons_id <- "tt0096697"

# load ratings and episodes, change any \N to NA (have to do double slash to 'escape' the quotes)
ratings <- read_tsv("data/title.ratings.tsv/data.tsv", na = "\\N")
episodes <- read_tsv("data/title.episode.tsv/data.tsv", na = "\\N")

# join the simpons episodes with their ratings
# first create a new dataframe and put episodes into it
simpsons_episodes <- episodes %>%
  # filter this to only include episodes of the simpsons, by using its ID as the parent
  filter(parentTconst == simpsons_id) %>%
  # join the ratings dataframe onto the current one, where the episode IDs match (tconst)
  inner_join(ratings, by = "tconst")

ggplot(data = simpsons_episodes, aes(x = numVotes, y = averageRating)) + geom_point(alpha = 0.2)
  
