library(spotifyr)
library(dplyr)
library(ggplot2)
library(reshape2)
library(tidyr)
library(ggforce)
library(tibble)
library(plotly)
library(purrr)

# Set Spotify API credentials
Sys.setenv(SPOTIFY_CLIENT_ID = Sys.getenv("SPOTIFY_CLIENT_ID"))
Sys.setenv(SPOTIFY_CLIENT_SECRET = Sys.getenv("SPOTIFY_CLIENT_SECRET"))

# Authenticate with Spotify
access_token <- get_spotify_access_token()

playlist_id <- "1RhRAqYJA1mwmBtk4mXRju"
username <- "quintijn.kroesbergen"

all_audio_features_df <- get_playlist_audio_features(username, playlist_uris = c(playlist_id), authorization = access_token)

# Assign genres
all_audio_features_df$genre <- NA
all_audio_features_df$genre[1:107] <- 'Afro House'
all_audio_features_df$genre[108:206] <- 'Deep House'
all_audio_features_df$genre[207:284] <- 'Melodic House'

# Plotting energy vs valence by genre
ggplot(all_audio_features_df, aes(x = energy, y = valence, color = genre)) +
  geom_point() +
  labs(title = "Energy vs Valence by Genre",
       x = "Energy",
       y = "Valence",
       color = "Genre") +
  scale_color_manual(values = c('Afro House' = 'blue', 'Deep House' = 'red', 'Melodic House' = 'green')) +
  theme_minimal() +
  theme(legend.position = "right")

# Plotting energy vs valence for each genre in separate panels
ggplot(all_audio_features_df, aes(x = energy, y = valence)) +
  geom_point(aes(color = genre)) +  # Color points by genre
  facet_wrap(~ genre) +  # Create a separate plot for each genre
  labs(title = "Energy vs Valence for Each Genre",
       x = "Energy",
       y = "Valence",
       color = "Genre") +
  scale_color_manual(values = c('Afro House' = 'blue', 'Deep House' = 'red', 'Melodic House' = 'green')) +
  theme_minimal() +
  theme(legend.position = "right",
        strip.background = element_blank(),  
        strip.text = element_text(size = 12))  
genre_stats <- all_audio_features_df %>%
  group_by(genre) %>%
  summarise(
    energy_mean = mean(energy),
    energy_sd = sd(energy),
    valence_mean = mean(valence),
    valence_sd = sd(valence)
  )

# display genre stats
print(genre_stats)

# Assuming 'all_audio_features_df' is your dataset and 'energy' is the column of interest
# Assuming 'all_audio_features_df' is your dataset and 'tempo' is the column of interest
Q1_tempo <- quantile(all_audio_features_df$tempo, 0.25)
Q3_tempo <- quantile(all_audio_features_df$tempo, 0.75)
IQR_tempo <- Q3_tempo - Q1_tempo

# Define bounds for outliers
lower_bound_tempo <- Q1_tempo - 1.5 * IQR_tempo
upper_bound_tempo <- Q3_tempo + 1.5 * IQR_tempo

# Identify tempo outliers
tempo_outliers <- subset(all_audio_features_df, tempo < lower_bound_tempo | tempo > upper_bound_tempo)

# Assuming tempo_outliers is already defined and contains the outlier tracks
# Get the track name of the last track in tempo_outliers
last_track_name <- tail(tempo_outliers$track.name, 1)

# Display the name of the last track
print(last_track_name)

track_id <- "1MvLmHeLkaNgUScgbUVnWJ"
audio_analysis <- get_track_audio_analysis(track_id, authorization = access_token)
# Assuming `audio_analysis` has already been retrieved
str(audio_analysis$segments)
segments <- audio_analysis$segments


# Initialize an empty data frame to store the long-format pitch data
pitch_data_long <- tibble()

# Loop through each segment to transform pitch data into a long format
for(i in 1:nrow(segments)) {
  segment_pitch_data <- tibble(
    start = segments$start[i],
    duration = segments$duration[i],
    pitch_class = c("C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B"),
    intensity = segments$pitches[[i]]
  )
  pitch_data_long <- bind_rows(pitch_data_long, segment_pitch_data)
}

# Now, pitch_data_long should be in the correct format for plotting

print(head(pitch_data_long))
ggplot(pitch_data_long, aes(x = start, y = pitch_class, fill = intensity)) +
  geom_tile() + # Use geom_tile() to create the heatmap
  scale_fill_gradient(low = "blue", high = "red") + # Customize the color gradient
  labs(title = "Pitch Class Intensity Over Time",
       x = "Time (s)",
       y = "Pitch Class",
       fill = "Intensity") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), # Rotate x-axis labels if needed
        axis.title = element_text(size = 12),
        title = element_text(size = 14))

