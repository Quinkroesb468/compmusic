library(spotifyr)
library(dplyr)
library(ggplot2)
library(reshape2)
library(tidyr)
library(ggforce)

library(tibble)
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




