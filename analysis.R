library(spotifyr)
library(dplyr)
library(ggplot2)
library(reshape2)
library(tidyr)
library(ggforce)

library(tibble)
# Set Spotify API credentials
Sys.setenv(SPOTIFY_CLIENT_ID = "8d5f768621d941aa9cd172a7a56d2bba")
Sys.setenv(SPOTIFY_CLIENT_SECRET = "1d506845301544549b103282e036e6da")

# Authenticate with Spotify
access_token <- get_spotify_access_token()

playlist_id <- "1RhRAqYJA1mwmBtk4mXRju"

# Fetch all playlist tracks
all_tracks_df <- data.frame()
offset <- 0
total_tracks <- 284  

# Fetch tracks in batches of 100
while (offset < total_tracks) {
  temp_tracks <- get_playlist_tracks(playlist_id = playlist_id, limit = 100, offset = offset, authorization = access_token)
  all_tracks_df <- rbind(all_tracks_df, temp_tracks)
  offset <- offset + 100
  if (nrow(temp_tracks) < 100) {
    break  # Exit loop if last batch of tracks is fetched
  }
}

# Extract track IDs
track_ids <- all_tracks_df$track.id

# Initialize an empty data frame for audio features
all_audio_features_df <- data.frame()

# Fetch audio features for all track IDs
for (i in seq(1, length(track_ids), by = 100)) {
  batch_track_ids <- track_ids[i:min(i+99, length(track_ids))]
  temp_features <- get_track_audio_features(batch_track_ids, authorization = access_token)
  all_audio_features_df <- rbind(all_audio_features_df, temp_features)
}

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
       y = "Valence") +
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




