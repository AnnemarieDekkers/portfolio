remotes::install_github('jaburgoyne/compmus')

library(compmus)
library(tidyverse)
library(spotifyr)
library(compmus)
library(ggplot2)

# the 3 tools we need to make chromagram
# audio: El favor 
favor <-
  get_tidy_audio_analysis("684EjRHwNsZQ9hCQxL4NYL?si=73deb841a4134163") |>
  select(segments) |>
  unnest(segments) |>
  select(start, duration, pitches)
# audio: atrevete
atre <-
  get_tidy_audio_analysis("1q8NdCAQ9QUjpYiqzdd3mv?si=f7e00bf18e8841b9") |>
  select(segments) |>
  unnest(segments) |>
  select(start, duration, pitches)

# plot of el favor
favor |>
  mutate(pitches = map(pitches, compmus_normalise, "euclidean")) |>
  compmus_gather_chroma() |> 
  ggplot(
    aes(
      x = start + duration / 2,
      width = duration,
      y = pitch_class,
      fill = value
    )
  ) +
  geom_tile() +
  labs(x = "Time (s)", y = NULL, fill = "Magnitude") +
  theme_minimal() +
  scale_fill_viridis_c() +
  ggtitle("Chromagram from the song: El Favor")

# plot of atrevete
favor |>
  mutate(pitches = map(pitches, compmus_normalise, "euclidean")) |>
  compmus_gather_chroma() |> 
  ggplot(
    aes(
      x = start + duration / 2,
      width = duration,
      y = pitch_class,
      fill = value
    )
  ) +
  geom_tile() +
  labs(x = "Time (s)", y = NULL, fill = "Magnitude") +
  theme_minimal() +
  scale_fill_viridis_c() +
  ggtitle("Chromagram from the song: Atrevete-Te-Te")

