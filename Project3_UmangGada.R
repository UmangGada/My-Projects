### Introduction ###
# 
# In this project, I would be working with Spotify Data and perform some exploratory data analysis and visualizations on it.
# The Spotify Data has the following:
# Primary:
#   .	Track_id (unique id for each track)
#   .	Track_album_id (unique id for each track album)
#   .	Playlist_id (unique ids for each playlist)
# 
# Numerical: 
#   .	Track_popularity (Ranges from 0 to 100)
#   .	Danceability (Ranges from 0 to 1)
#   .	Energy (Ranges from 0 to 1)
#   .	Loudness (Float ranging from -46 to 2 approx.)
#   .	Speechiness (Ranges from 0 to 1)
#   .	Acousticness (Ranges from 0 to 1)
#   .	Instrumentalness (Ranges from 0 to 1)
#   .	Liveness (Ranges from 0 to 1)
#   .	Valence (Ranges from 0 to 1)
#   .	Tempo (Ranges from 0 to 1)
#   .	Duration_ms (Typically ranges from 45k to 500k)
# Dummy:
#   .	Mode (0 = minor, 1 = major)
# Categorical:
#   .	Key (All keys on octave encoded as values ranging from 0 to 11, with C as 0 , C# as 1 and so on)
#          .	Track_name(list of songs)
#          .	Track_artist(list of artists)
#          .	Track_album_name(list of albums)
#          .	Track_album_release_dates (Date of release in d/m/yyyy format, but may vary)
#          .	Playlist_name (list of playlists)
#          .	Playlist_genre (list of genres)
#          .	Playlist_subgenre (list of subgenres)
#
# The core idea is to create a data frame of the data and perform various analysis like summary, plotting missing data, drawing correlation between numerical regressors, 
# plotting time series of each regressor aggregated by years, and then generating top songs from top artists and top playlist subgenres.         


#setwd("C:/Users/umang/OneDrive/Documents/College files/Sem 4/STAT_611_R/Projects")

library(ggplot2) #to create neat graphs.
library(dplyr) #for data manipulation.
library(tidyr) #for data manipulation.
library(corrplot) #for plotting neat correlation plots
library(lubridate) #for manipulating and converting date
library(tidyverse) #for certain graphing elements

### Data Preparation ###

# 1.	Original data sourced from: https://github.com/rfordatascience/tidytuesday/blob/master/data/2020/2020-01-21/readme.md
# 
# 2.	I have used the dataset with all of its columns but will change it below as required.
# 
# 3.	I imported the data using read.csv and converted it into a dataframe. I did not find any empty cells, but I did reformat the album dates which were not uniform. 
#     I also converted instrumentalness into numeric form in Excel before importing to use it for a plot. I also added an extra column for separating year from album dates

spotify_data = read.csv("../Projects/spotify_songs.csv")
spotify_df = data.frame(spotify_data)
head(spotify_df) #with cleaned instrumentalness column

x = spotify_df$track_album_release_date
spotify_df$track_album_release_date = as.Date(parse_date_time(x, c('y', 'm/d/y', 'y-m')))
spotify_df$year = as.numeric(format(spotify_df$track_album_release_date, "%Y"))
head(spotify_df) #top 5 rows for clean data

### Exploratory Data Analysis ###

cat("Number of rows:", dim(spotify_df)[1], "\nNumber of columns:", dim(spotify_df)[2]) #total number of rows and columns

str(spotify_df, vec.len=1)

summary(spotify_df) #5-point summary for numerical features, class type for non numerical features
reduced_df = spotify_df[,-which(sapply(spotify_df, class) == "character")]

missing_by_column <- reduced_df %>% 
  is.na %>% 
  as_tibble %>% 
  mutate(row_number = 1:nrow(.)) %>% 
  gather(variable, is_missing, -row_number) 

ggplot(missing_by_column, aes(x = variable, y = row_number, fill = is_missing)) +
  geom_tile() + 
  theme_minimal() +
  scale_fill_manual(name = "",
                    values = c('steelblue', 'red'),
                    labels = c("Valid", "Missing")) +
  theme(axis.text.x  = element_text(angle=90, vjust=0.5, size = 8)) + 
  labs(x = "Columns",
       y = "Count")

# As we can see in the plot there are no missing values. We are good to continue with this data frame for correlation plots

remove(missing_by_column)

draw_corr <- function(reduced_df, cols, sort_col, threshold) {
  
  all_num_values <- reduced_df[, cols]
  cor_mtx <- cor(all_num_values, use="pairwise.complete.obs")
  
  cor_sorted <- as.matrix(sort(cor_mtx[, sort_col], decreasing = TRUE))
  
  cor_threshold <- names(which(apply(cor_sorted, 1, function(x) abs(x)>threshold)))
  cor_mtx <- cor_mtx[cor_threshold, cor_threshold]
  corrplot.mixed(cor_mtx, tl.col="black", order="hclust", tl.pos = "lt")
}
track_features <- c('valence', 'acousticness', 'danceability', 'energy', 'instrumentalness', 'key', 'liveness', 'loudness', 'speechiness', 'tempo')

draw_corr(reduced_df, track_features, "valence", 0.05) # plotting correlations greater than abs(0.05)

# We can see that there is a moderate positive linear relation between loudness and energy, which is to be expected as more energetic songs would
# produce louder noises. We can also see that valence has a slight positive linear relationship with danceability, which again can be expected as
# happier songs (high valence = happy, cheerful) may instigate people to dance.

n_cols = c('valence', 'acousticness', 'danceability', 'duration_ms', 'energy', 'instrumentalness', 'key', 'liveness', 'loudness', 'mode', 'track_popularity', 'speechiness', 'tempo')

draw_corr(reduced_df, n_cols, "track_popularity", 0.05) # plotting correlations against popularity

# We can see two noticeable correlations apart from the ones found previously. Energy has a moderate negative linear relation with acousticness,
# acoustic songs are usually played with a single instrument so it can be expected. We also have a slight negative linear realtionshup between
# acousticness and loudness, which is again understandable as loudness and energy are correlated.


by_years <- aggregate(valence~year, data = spotify_df, mean)
ggplot(data=by_years, aes(x=year, y=valence)) +
  geom_line(size=1.4) +
  ggtitle("Valence - Years Graph") + theme(plot.title = element_text(hjust = 0.5))+ 
  scale_y_continuous(name="Avg. Valence", limits=seq(0.0, 1.0))+
  scale_x_continuous(name="Years", breaks=seq(1950, 2020, 10))+
  geom_smooth(method=lm , color="blue", se=FALSE, show.legend = TRUE, linetype="dashed")+
  theme(
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "gray"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "gray"),
    axis.text.x = element_text(face = "bold", size = 11),
    axis.text.y = element_text(face = "bold", size = 11)
  )

# Here i have plotted valence throughout the years, Valence seemed to be decreasing gradually suggesting that sad, emotional songs have increased
# over the years, Although we can see that lately happier songs are making a comeback.


by_years <- aggregate(acousticness~year, data = spotify_df, mean)

ggplot(data=by_years, aes(x=year, y=acousticness)) +
  geom_line(size=1.4) +
  ggtitle("Acousticness - Years Graph") + theme(plot.title = element_text(hjust = 0.5))+ 
  scale_y_continuous(name="Avg. Acousticness", limits=seq(0.0, 1.0))+
  scale_x_continuous(name="Years", breaks=seq(1950, 2020, 10))+
  geom_smooth(method=lm , color="blue", se=FALSE, show.legend = TRUE, linetype="dashed")+
  theme(
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "gray"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "gray"),
    axis.text.x = element_text(face = "bold", size = 11),
    axis.text.y = element_text(face = "bold", size = 11)
  )

# We can also see that acoustic songs were quite popular in the 60s but have since dropped off. In recent times though it is again picking up with
# the artists.

by_years <- aggregate(energy~year, data = spotify_df, mean)

ggplot(data=by_years, aes(x=year, y=energy)) +
  geom_line(size=1.4) +
  ggtitle("Energy - Years Graph") + theme(plot.title = element_text(hjust = 0.5))+ 
  scale_y_continuous(name="Avg. Energy", limits=seq(0.0, 1.0))+
  scale_x_continuous(name="Years", breaks=seq(1950, 2020, 10))+
  geom_smooth(method=lm , color="blue", se=FALSE, show.legend = TRUE, linetype="dashed")+
  theme(
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "gray"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "gray"),
    axis.text.x = element_text(face = "bold", size = 11),
    axis.text.y = element_text(face = "bold", size = 11)
  )

# The energy of the songs have been increasing over the years, this could be because of the introduction of metal and rock genres. Lately a rise
# in acoustic songs has dropped the energy.

by_years <- aggregate(loudness~year, data = spotify_df, mean)

ggplot(data=by_years, aes(x=year, y=loudness)) +
  geom_line(size=1.4) +
  ggtitle("Loudness - Years Graph") + theme(plot.title = element_text(hjust = 0.5))+ 
  scale_y_continuous(name="Avg. Loudness")+
  scale_x_continuous(name="Years", breaks=seq(1950, 2020, 10))+
  geom_smooth(method=lm , color="blue", se=FALSE, show.legend = TRUE, linetype="dashed")+
  theme(
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "gray"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "gray"),
    axis.text.x = element_text(face = "bold", size = 11),
    axis.text.y = element_text(face = "bold", size = 11)
  )

# As with energy, the loudness of the songs have increased over the years. This could be because of the metal and rock genres introduced in the 80s.
# and probably disco songs in early 2000s.

by_years <- aggregate(liveness~year, data = spotify_df, mean)

ggplot(data=by_years, aes(x=year, y=liveness)) +
  geom_line(size=1.4) +
  ggtitle("Liveness - Years Graph") + theme(plot.title = element_text(hjust = 0.5))+ 
  scale_y_continuous(name="Avg. Liveness")+
  scale_x_continuous(name="Years", breaks=seq(1950, 2020, 10))+
  geom_smooth(method=lm , color="blue", se=FALSE, show.legend = TRUE, linetype="dashed")+
  theme(
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "gray"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "gray"),
    axis.text.x = element_text(face = "bold", size = 11),
    axis.text.y = element_text(face = "bold", size = 11)
  )

# Liveness seems to be increasing over the years but haven't quite reached the levels of 60s. This could be attributed to remixes and a possible 
# increase in studio albums but also an increase in tours. Studio albums may have been rarer in earlier days.

by_years <- aggregate(danceability~year, data = spotify_df, mean)

ggplot(data=by_years, aes(x=year, y=danceability)) +
  geom_line(size=1.4) +
  ggtitle("Danceability - Years Graph") + theme(plot.title = element_text(hjust = 0.5))+ 
  scale_y_continuous(name="Avg. Danceability")+
  scale_x_continuous(name="Years", breaks=seq(1950, 2020, 10))+
  geom_smooth(method=lm , color="blue", se=FALSE, show.legend = TRUE, linetype="dashed")+
  theme(
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "gray"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "gray"),
    axis.text.x = element_text(face = "bold", size = 11),
    axis.text.y = element_text(face = "bold", size = 11)
  )

# The danceability has also increased over the years but never reaching the heights of the 60s, remixes could again be a reason for increase in
# danceability.


by_years <- aggregate(speechiness~year, data = spotify_df, mean)

ggplot(data=by_years, aes(x=year, y=speechiness)) +
  geom_line(size=1.4) +
  ggtitle("Speechiness - Years Graph") + theme(plot.title = element_text(hjust = 0.5))+ 
  scale_y_continuous(name="Avg. Speechiness")+
  scale_x_continuous(name="Years", breaks=seq(1950, 2020, 10))+
  geom_smooth(method=lm , color="blue", se=FALSE, show.legend = TRUE, linetype="dashed")+
  theme(
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "gray"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "gray"),
    axis.text.x = element_text(face = "bold", size = 11),
    axis.text.y = element_text(face = "bold", size = 11)
  )

# speechiness has increased over the years, with the songs in 90s and 2000s peaking, this could be due to an increase in rap songs and the fame of
# rappers like 2pac, eminem, jay-z etc. The speechiness has dropped in the last decade but is again on the rise.

by_years <- aggregate(key~year, data = spotify_df, mean)

ggplot(data=by_years, aes(x=year, y=key)) +
  geom_line(size=1.4) +
  ggtitle("key - Years Graph") + theme(plot.title = element_text(hjust = 0.5))+ 
  scale_y_continuous(name="Avg. key")+
  scale_x_continuous(name="Years", breaks=seq(1950, 2020, 10))+
  geom_smooth(method=lm , color="blue", se=FALSE, show.legend = TRUE, linetype="dashed")+
  theme(
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "gray"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "gray"),
    axis.text.x = element_text(face = "bold", size = 11),
    axis.text.y = element_text(face = "bold", size = 11)
  )

# the key has not changed much on average, but it seems most of the songs have pitch classes somewhere between F/E-sharp/G-double-flat 
# and F-sharp/G-flat/E-double-sharp.

by_years <- aggregate(mode~year, data = spotify_df, mean)

ggplot(data=by_years, aes(x=year, y=mode)) +
  geom_line(size=1.4) +
  ggtitle("mode - Years Graph") + theme(plot.title = element_text(hjust = 0.5))+ 
  scale_y_continuous(name="Avg. mode")+
  scale_x_continuous(name="Years", breaks=seq(1950, 2020, 10))+
  geom_smooth(method=lm , color="blue", se=FALSE, show.legend = TRUE, linetype="dashed")+
  theme(
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "gray"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "gray"),
    axis.text.x = element_text(face = "bold", size = 11),
    axis.text.y = element_text(face = "bold", size = 11)
  )

# The mode has gradually decreased over the years, it seems the minor mode is increasing in songs but majority of the songs still have major mode.

by_years <- aggregate(tempo~year, data = spotify_df, mean)

ggplot(data=by_years, aes(x=year, y=tempo)) +
  geom_line(size=1.4) +
  ggtitle("Tempo - Years Graph") + theme(plot.title = element_text(hjust = 0.5))+ 
  scale_y_continuous(name="Avg. Tempo")+
  scale_x_continuous(name="Years", breaks=seq(1950, 2020, 10))+
  geom_smooth(method=lm , color="blue", se=FALSE, show.legend = TRUE, linetype="dashed")+
  theme(
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "gray"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "gray"),
    axis.text.x = element_text(face = "bold", size = 11),
    axis.text.y = element_text(face = "bold", size = 11)
  )

# the tempo of the songs has decreased a lot over the years. although lately it is picking up. This could be because of an increase in remixes, djs
# and beats in pop music. 

by_years <- aggregate(duration_ms~year, data = spotify_df, mean)

ggplot(data=by_years, aes(x=year, y=duration_ms)) +
  geom_line(size=1.4) +
  ggtitle("Duration_ms - Years Graph") + theme(plot.title = element_text(hjust = 0.5))+ 
  scale_y_continuous(name="Avg. Duration in ms")+
  scale_x_continuous(name="Years", breaks=seq(1950, 2020, 10))+
  geom_smooth(method=lm , color="blue", se=FALSE, show.legend = TRUE, linetype="dashed")+
  theme(
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "gray"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "gray"),
    axis.text.x = element_text(face = "bold", size = 11),
    axis.text.y = element_text(face = "bold", size = 11)
  )

# the song duration seems to have increased over the years although since the last decade it has been dropping off. I think this could be because
# of the popularity of metal and rock songs in the early 80s, 90s and 2000s where long guitar solos were common. The recent drop may be attributed
# to increase in pop songs and a rise in pop rock which have shorter durations.


by_years <- aggregate(instrumentalness~year, data = spotify_df, mean)

ggplot(data=by_years, aes(x=year, y=instrumentalness)) +
  geom_line(size=1.4) +
  ggtitle("Instrumentalness - Years Graph") + theme(plot.title = element_text(hjust = 0.5))+ 
  scale_y_continuous(name="Avg. Instrumentalness")+
  scale_x_continuous(name="Years", breaks=seq(1950, 2020, 10))+
  geom_smooth(method=lm , color="blue", se=FALSE, show.legend = TRUE, linetype="dashed")+
  theme(
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "gray"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "gray"),
    axis.text.x = element_text(face = "bold", size = 11),
    axis.text.y = element_text(face = "bold", size = 11)
  )
 
# The instrumentalness has been at about the same level excluding the huge spike in 60s. This could also depict that speech has been a greater part
# of songs as we moved through the decades, although with the recent rise of electronic music, the instrumentalness has seen a slight rise in the last
# decade.

cat("Unique Singers: ", length(unique(spotify_df[, "track_artist"]))) # number of unique artists

artist_counts <- as.data.frame(table(spotify_df['track_artist']))
top_artists <- subset(artist_counts, artist_counts[ , "Freq"] > 40)
sorted_artists <- top_artists[order(top_artists$Freq, decreasing = TRUE),]
sorted_artists <- sorted_artists[-c(1, 2),]

sorted_artists[1:10,] # top 10 artists with highest number of songs

songs <- as.data.frame(spotify_df[c('track_name', 'track_artist', 'track_popularity')])
top_songs <- subset(songs, songs[ , "track_popularity"] > 90)
sorted_songs <- top_songs[order(top_songs$track_popularity, decreasing = TRUE),]

popularity_table <- aggregate(track_popularity~track_artist, data = spotify_df, sum)
popularity_table <- popularity_table[order(popularity_table$track_popularity, decreasing = TRUE),]
popularity_table[1:10,] # top 10 most popular artists

cat(subset(artist_counts, artist_counts$Var1 == "Martin Garrix")$Freq)
garrix <- subset(spotify_df, spotify_df$track_artist == "Martin Garrix")
garrix <- garrix[c('track_name', 'track_popularity', 'year')]
garrix <- garrix[order(garrix$track_popularity, decreasing = TRUE),]
unique(garrix[1:10,]) # top 4 popular songs by garrix (since different playlist contain the same songs, we get a reduced unique list.)

ggplot(data = unique(garrix), aes(year,track_popularity, fill = factor(track_popularity))) + 
  geom_bar(position="stack", stat="identity") + theme_minimal() # plotting popularity vs year

# We can see from the plot that most of the popular songs by Martin garrix have come in the year 2019 followed by 2017. The proportion of popular songs
# is also high in 2019 as compared to 2016

remove(garrix) #removing subset to save space

cat(subset(artist_counts, artist_counts$Var1 == "The Chainsmokers")$Freq)
chain <- subset(spotify_df, spotify_df$track_artist == "The Chainsmokers")
chain <- chain[c('track_name', 'track_popularity', 'year')]
chain <- chain[order(chain$track_popularity, decreasing = TRUE),]
unique(chain[1:10,]) # top 3 the chainsmokers songs


ggplot(data = unique(chain), aes(year,track_popularity, fill = factor(track_popularity))) + 
  geom_bar(position="stack", stat="identity") + theme_minimal() 

# We can see that The chainsmokers have been as popular in 2016 as they were in 2019 although they seem to have more popular songs in 2016 and more
# songs in general in 2016. 2018 on the other hand wasn't as good for them.

remove(chain)

cat(subset(artist_counts, artist_counts$Var1 == "David Guetta")$Freq)
guetta <- subset(spotify_df, spotify_df$track_artist == "David Guetta")
guetta <- guetta[c('track_name', 'track_popularity', 'year')]
guetta <- guetta[order(guetta$track_popularity, decreasing = TRUE),]
unique(guetta[1:10,])

ggplot(data = unique(guetta), aes(year,track_popularity, fill = factor(track_popularity))) + 
  geom_bar(position="stack", stat="identity") + theme_minimal() 

# David Guetta has had a longer time in music industry as can be seen in the plot. It seems in 2018 he had his most popular songs. 2019 was also
# a good year for him. The other notable years for popularity were 2012 and 2014

remove(guetta)

playlistsg_counts <- as.data.frame(table(spotify_df['playlist_subgenre']))
top_playlistssg <- subset(playlistsg_counts, playlistsg_counts[ , "Freq"] > 40)
sorted_playlistssg <- top_playlistssg[order(top_playlistssg$Freq, decreasing = TRUE),]
sorted_playlistssg <- sorted_playlistssg[-c(1, 2),]

sorted_playlistssg[1:10,] # top 10 subgenres by count

cat(subset(playlistsg_counts, playlistsg_counts$Var1 == "indie poptimism")$Freq)
indie <- subset(spotify_df, spotify_df$playlist_subgenre == "indie poptimism")
indie <- indie[c('track_name', 'track_popularity', 'year')]
indie <- indie[order(indie$track_popularity, decreasing = TRUE),]
unique(indie[1:10,])

# All the indie songs in top 10 have been from 2019. Its possible that a lot of indie poptimism songs are produced every year.

remove(indie)

cat(subset(playlistsg_counts, playlistsg_counts$Var1 == "latin hip hop")$Freq)
latin <- subset(spotify_df, spotify_df$playlist_subgenre == "latin hip hop")
latin <- latin[c('track_name', 'track_popularity', 'year')]
latin <- latin[order(latin$track_popularity, decreasing = TRUE),]
unique(latin[1:10,])

# Most of the songs in latin hip hop top 10 have also been from 2019. Recent songs seem to be more popular. Interesting to note that Dance Monkey
# has a popularity of 100 which means it is very popular.

remove(latin)

cat(subset(playlistsg_counts, playlistsg_counts$Var1 == "neo soul")$Freq)
neo <- subset(spotify_df, spotify_df$playlist_subgenre == "neo soul")
neo <- neo[c('track_name', 'track_popularity', 'year')]
neo <- neo[order(neo$track_popularity, decreasing = TRUE),]
unique(neo[1:10,])

# The top 10 songs from neo soul are more spread out in years. It is surprising to see that All I want for Christmas is You, a song from 1994 is more
# popular than songs produced recently.

remove(neo)


remove(artist_counts, popularity_table, songs, sorted_artists, sorted_songs, top_artists, top_songs, playlistsg_counts, top_playlistssg, sorted_playlistssg)
# removing all subsets to save space.


### Summary ###
# I created a summary statistics for each column, following it up with missing points plot which suggested our dataset had no missing values
# We then looked for correlation among features and against track_popularity with most features quite independent of each other,
# I then proceeded with plotting each feature throughout time. I then finished my analysis with top 10 songs from the most popular artists and most
# most popular subgenres (I selected subgenres because there were only few genres available, and i thought a subgenre would filter songs much better.)

# This analysis helps see which songs or genres are rising in popularity. It also shows the artists that are most popular and how their popularity
# evolved through their career.

# I had actually planned and tried to run randomForest algorithm to be able to predict a song's popularity. Although i ended up facing an error in
# ranger function for which i could not find a solution.
# Predicting popularity would be the improvement on this analysis. 

### End of Project ###

