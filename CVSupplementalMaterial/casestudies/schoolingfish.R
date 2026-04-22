# Case study from Tunstrøm et al. 2013
# golden shiner analysis


library(jsonlite)
library(tidyverse)
library(shapeshiftr)



# data

raw <- fromJSON("CVSupplementalMaterial/schooling_frames.json")
head(str(raw))
str(raw[[1]])          # full structure of frame 1
names(raw[[1]])        # what fields exist inside a frame
length(raw[[1]])       # how many elements inside frame 1

# raw[[i]] is a named list with vectors px, py, vx, vy, onfish
# Fish count varies per frame (tracking drops partially-obscured fish)

extract_data_nlist <- function(df, id_col = "frame") { # every row produced from the inner list gets a "frame" column with the current list element number or name.
  purrr::imap_dfr(df, function(x, i) { # map this function over each element of a list & row binds, the i gives the index name, returns a tibble
    x <- tibble::as_tibble(x)
    x[[id_col]] <- i
    x
  })
}
fish_df <- extract_data_nlist(raw)
head(fish_df)


# Quick sanity check: fish counts per frame
fish_per_frame <- fish_df %>% count(frame)
summary(fish_per_frame$n)
ggplot(fish_per_frame, aes(as.numeric(frame), n)) +
  geom_line(linewidth = 0.3) +
  labs(x = "Frame", y = "Fish tracked", title = "Tracked fish per frame") +
  theme_bw()

cv_per_frame <- fish_df %>%
  group_by(frame) %>%
  summarise(
    n_fish = n(),
    cv_pop = cvpop(across(c(px, py))),
    cv_ind = cvind(across(c(px, py))),
    ratio  = cvratio(across(c(px, py))),
    # Polarization Op: magnitude of mean unit heading vector
    # vx/vy already unit vectors so no normalisation needed
    op = sqrt(mean(vx)^2 + mean(vy)^2),
    # Rotation Or: mean |cross(r_i, v_i)| / |r_i|
    # r_i = position relative to centroid, v_i = unit velocity
    # cross product z-component in 2D: rx*vy - ry*vx
    or = abs(mean(
      ((px - mean(px)) * vy - (py - mean(py)) * vx) /
        (sqrt((px - mean(px))^2 + (py - mean(py))^2) + 1e-10) #+ 1e-10 in the denominator just guards against division by zero for ny fishing heading directly towards centroid
    )),
    .groups = "drop"
  )

saveRDS(cvs_per_frame, "CVSupplementalMaterial/casestudies/cvs_fish.rds")

# ~300 fish




cvs_per_frame %>% mutate(frame = as.numeric(frame)) %>%
  pivot_longer(cols = c(cv_pop, cv_ind, ratio, op, or), names_to = "Type", values_to = "value") %>%
ggplot(aes(frame, value))+
  geom_smooth(aes(col = Type))+
  theme_bw()

# op > 0.65 & or < 0.35 ~ "polarized", (kind of like our elongation)
# op < 0.35 & or > 0.65 ~ "milling", # we woudn't be able to distinguish between milling and swarm,
# op < 0.35 & or < 0.35 ~ "swarm",

