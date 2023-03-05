library(tidyverse)
library(spotifyr)
library(dplyr)
library(ggplot2)

# playlist with songs from 2023
now <- get_playlist_audio_features("", "0a1qCaoC6Zw4Ni5eV1PReR?si=e57ef18872744fde")
now |>
  summarise(
    mean_acousticness = mean(acousticness),
    sd_acousticness = sd(acousticness),
    median_acousticness = median(acousticness),
    mad_acousticness = mad(acousticness),
    mean_danceability = mean(danceability),
    sd_danceability = sd(danceability),
    median_danceability = median(danceability),
    mad_danceability = mad(danceability),
    mean_energy = mean(energy),
    sd_energy = sd(energy),
    median_energy = median(energy),
    mad_energy = mad(energy),
    mean_instrumentalness = mean(instrumentalness),
    sd_instrumentalness = sd(instrumentalness),
    median_instrumentalness = median(instrumentalness),
    mad_instrumentalness = mad(instrumentalness),
    mean_liveness = mean(liveness),
    sd_liveness = sd(liveness),
    median_liveness = median(liveness),
    mad_liveness = mad(liveness),
    mean_loudness = mean(loudness),
    sd_loudness = sd(loudness),
    median_loudness = median(loudness),
    mad_loudness = mad(loudness),
    mean_speechiness = mean(speechiness),
    sd_speechiness = sd(speechiness),
    median_speechiness = median(speechiness),
    mad_speechiness = mad(speechiness),
    mean_tempo = mean(tempo),
    sd_tempo = sd(tempo),
    median_tempo = median(tempo),
    mad_tempo = mad(tempo)
  )

# playlist with songs from 1900s
then <- get_playlist_audio_features("", "37i9dQZF1DXawmJ5HnBNtX?si=00449bb1fdce4780")
then |>
  summarise(
    mean_acousticness = mean(acousticness),
    sd_acousticness = sd(acousticness),
    median_acousticness = median(acousticness),
    mad_acousticness = mad(acousticness),
    mean_danceability = mean(danceability),
    sd_danceability = sd(danceability),
    median_danceability = median(danceability),
    mad_danceability = mad(danceability),
    mean_energy = mean(energy),
    sd_energy = sd(energy),
    median_energy = median(energy),
    mad_energy = mad(energy),
    mean_instrumentalness = mean(instrumentalness),
    sd_instrumentalness = sd(instrumentalness),
    median_instrumentalness = median(instrumentalness),
    mad_instrumentalness = mad(instrumentalness),
    mean_liveness = mean(liveness),
    sd_liveness = sd(liveness),
    median_liveness = median(liveness),
    mad_liveness = mad(liveness),
    mean_loudness = mean(loudness),
    sd_loudness = sd(loudness),
    median_loudness = median(loudness),
    mad_loudness = mad(loudness),
    mean_speechiness = mean(speechiness),
    sd_speechiness = sd(speechiness),
    median_speechiness = median(speechiness),
    mad_speechiness = mad(speechiness),
    mean_tempo = mean(tempo),
    sd_tempo = sd(tempo),
    median_tempo = median(tempo),
    mad_tempo = mad(tempo)
  )

# combine data sets with labeling variable.
songs <-
  bind_rows(
    now |> mutate(category = "Songs from 2023"),
    then |> mutate(category = "Songs from 2000")
  )

# bar plots to compare a continuous variable (energy) across categories
songs |>
  ggplot(aes(x = energy)) +
  geom_histogram(binwidth = 0.1, fill="steelblue") +
  facet_wrap(~category)

# scatter+line plot to compare danceability vs tempo
songs |> ggplot(aes(x = danceability, y = tempo)) +
         geom_point(size=0.5, color="steelblue") +
         geom_smooth(fill="darkred", color="red", size=0.8) +
         facet_wrap(~category)

# scatter plot to compare valence vs energy.
songs |>                    # Start with awards.
  mutate(
    mode = ifelse(mode == 0, "Minor", "Major")
  ) |>
  ggplot(                     # Set up the plot.
    aes(
      x = valence,
      y = energy,
      size = loudness,
      colour = mode
    )
  ) +
  geom_point() +              # Scatter plot.
  geom_rug(linewidth = 0.1) + # Add 'fringes' to show data distribution.
  geom_text(aes(x= valence, y=energy,label=label),
    data = tibble(label=c("slow", "energic"),
        category = c("Songs from 2023", "Songs from 2000"),
        valence = c(0.090, 0.123), energy = c(0.101, 0.967)),
    colour = "black",         # Override colour (not mode here).
    size = 3,                 # Override size (not loudness here).
    hjust = "left",           # Align left side of label with the point.
    vjust = "bottom",         # Align bottom of label with the point.
    nudge_x = -0.05,          # Nudge the label slightly left.
    nudge_y = 0.02            # Nudge the label slightly up.
  ) +
  facet_wrap(~ category) +    # Separate charts per playlist.
  scale_x_continuous(         # Fine-tune the x axis.
    limits = c(0, 1),
    breaks = c(0, 0.50, 1),   # Use grid-lines for quadrants only.
    minor_breaks = NULL       # Remove 'minor' grid-lines.
  ) +
  scale_y_continuous(         # Fine-tune the y axis in the same way.
    limits = c(0, 1),
    breaks = c(0, 0.50, 1),
    minor_breaks = NULL
  ) +
  scale_colour_brewer(        # Use the Color Brewer to choose a palette.
    type = "qual"            # Qualitative set.
    #palette = "Paired"        # Name of the palette is 'Paired'.
  ) +
  scale_size_continuous(      # Fine-tune the sizes of each point.
    trans = "exp",            # Use an exp transformation to emphasise loud.
    guide = "none"            # Remove the legend for size.
  ) +
  theme_light() +             # Use a simpler theme.
  labs(                       # Make the titles nice.
    x = "Valence",
    y = "Energy",
    colour = "Mode"
  )
