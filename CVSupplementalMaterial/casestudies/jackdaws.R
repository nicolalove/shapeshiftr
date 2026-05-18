
library(tidyverse)
library(shapeshiftr)
library(patchwork)
library(ggsignif)
library(ggpubfigs)
#----Functions----
flock_labeller <- function(variable,value){
  return(flocks[value])
}
read_jackdaw <- function(path) {
  lines <- readLines(path)

  # Skip the header (line 1), process the rest
  lines <- lines[-1]

  flock_id  <- NA_character_
  rows      <- list()

  for (line in lines) {
    tokens <- strsplit(trimws(line), "\\s+")[[1]]

    if (length(tokens) == 3) {
      # Flock label line e.g. "transit flock 01"
      flock_id <- paste(tokens, collapse = "_")  # "transit_flock_01"
    } else if (length(tokens) == 8) {
      # Data row
      rows[[length(rows) + 1]] <- c(flock = flock_id, tokens)
    }
    # ignore blank lines or anything else
  }

  df <- as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)
  colnames(df) <- c("flock", "bird_id", "x", "y", "z", "t", "vx", "vy", "vz")

  # Convert numeric columns
  df <- df %>%
    mutate(across(c(bird_id, x, y, z, t, vx, vy, vz), as.numeric))

  df
}
#----TRANSITING FLOCKS----

transit <- read_jackdaw("CVSupplementalMaterial/casestudies/transitflocks.txt")
transit <- transit %>%
  group_by(flock, t) %>%
  mutate(
    n_birds    = n_distinct(bird_id)
  )
# how many birds per frame
transit %>%
  group_by(flock, t) %>%
  summarise(
    n_birds    = n_distinct(bird_id)
  ) %>%
  ggplot(aes(t, n_birds))+
  geom_line(aes(col = flock))+
  theme_bw()


# all frames all flocks
cv_transit_all <-  transit %>%
  group_by(flock, t) %>%
  filter(n() >= 10) %>% # need at least 10 birds to contribute
  summarise(
    n_birds = n(),
    cv_pop  = cvpop(across(c(x, y))),
    cv_ind  = cvind(across(c(x, y))),
    ratio   = cvratio(across(c(x, y))),
    .groups = "drop"
  )

cv_transit_binned <- cv_transit_all %>%
  mutate(t_bin = round(t, 1)) %>%       # bin to nearest 0.1s
  group_by(flock, t_bin) %>%
  summarise(
    n_frames = n(),
    n_birds  = median(n_birds),          # median birds across frames in bin
    cv_pop   = median(cv_pop),
    cv_ind   = median(cv_ind),
    ratio    = median(ratio),
    .groups  = "drop"
  )
mathematicapal <- c( "#8888d4","#229e0c", "black")

t_mins <- transit %>%
  group_by(flock) %>%
  summarise(t_min = min(t), .groups = "drop")

transitplot_lines <- ggplot(
  cv_transit_all %>%
    left_join(t_mins, by = "flock") %>%
    mutate(t_rel = t - t_min) %>%
    pivot_longer(c(cv_ind, cv_pop, ratio), names_to = "Type", values_to = "CV")
) +
  geom_line(aes(t_rel, CV, colour = Type)) +
  scale_color_manual(values = mathematicapal,
                     labels = c(bquote(CV[italic(indiv)]),
                                bquote(CV[italic(pop)]), "Ratio")) +
  facet_wrap(~flock,
            # labeller = as_labeller(c(transit_flock_01 = "Unsampled Flock 1"))
            ) +
  labs(x = "Time (s)", y = "CV", color = "") +
  theme_bw() +
  theme(legend.text = element_text(size = 12),
        strip.text  = element_text(size = 13))
transitplot_lines

#----MOBBING FLOCKS----

mob1 <- read_jackdaw("CVSupplementalMaterial/casestudies/mobbingflocks_0105.txt")
mob2 <- read_jackdaw("CVSupplementalMaterial/casestudies/mobbingflocks_0610.txt")
mob <- rbind(mob1, mob2)
mob <- mob %>%
  group_by(flock, t) %>%
  mutate(
    n_birds    = n_distinct(bird_id)
  )

# all flocks all frames
cv_mob_all <-  mob %>%
  group_by(flock, t) %>%
  filter(n() >= 10) %>%
  summarise(
    n_birds = n(),
    cv_pop  = cvpop(across(c(x, y))),
    cv_ind  = cvind(across(c(x, y))),
    ratio   = cvratio(across(c(x, y))),
    .groups = "drop"
  )

t_mins <- mob %>%
  group_by(flock) %>%
  summarise(t_min = min(t), .groups = "drop")

mobplot_lines <- ggplot(
  cv_mob_all %>%
    left_join(t_mins, by = "flock") %>%
    mutate(t_rel = t - t_min) %>%
    pivot_longer(c(cv_ind, cv_pop, ratio), names_to = "Type", values_to = "CV")
) +
  geom_line(aes(t_rel, CV, colour = Type)) +
  scale_color_manual(values = mathematicapal,
                     labels = c(bquote(CV[italic(indiv)]),
                                bquote(CV[italic(pop)]), "Ratio")) +
  facet_wrap(~flock,
             # labeller = as_labeller(c(transit_flock_01 = "Unsampled Flock 1"))
  ) +
  labs(x = "Time (s)", y = "CV", color = "") +
  theme_bw() +
  theme(legend.text = element_text(size = 12),
        strip.text  = element_text(size = 13))
mobplot_lines

#---- 2D Median CV Across Time ----
cv_transit_global <- cv_transit_all %>% group_by(flock) %>% summarise(
  n_birds = n(),
  med_cvpop = median(cv_pop),
  med_cvind = median(cv_ind),
  med_ratio = median(ratio),
 # mean_cvpop = mean(cv_pop),
  #mean_cvind = mean(cv_ind),
  #mean_ratio = mean(ratio),
  .groups = "drop"
)

cv_mob_global <- cv_mob_all %>% group_by(flock) %>% summarise(
  n_birds = n(),
  med_cvpop = median(cv_pop),
  med_cvind = median(cv_ind),
  med_ratio = median(ratio),
  # mean_cvpop = mean(cv_pop),
  # mean_cvind = mean(cv_ind),
  # mean_ratio = mean(ratio),
  .groups = "drop"
)

#saveRDS(cv_transit_all, "CVSupplementalMaterial/casestudies/cv_transit_all2D.rds")
#saveRDS(cv_mob_all, "CVSupplementalMaterial/casestudies/cv_mob_all2D.rds")


cv_transit_global$Behavior <- "Transitting"
cv_mob_global$Behavior <- "Mobbing"

cv_global_summary <- rbind(cv_transit_global, cv_mob_global)
cv_global_long <- cv_global_summary %>%
  pivot_longer(cols = c(med_cvpop, med_cvind, med_ratio),
               names_to = "Metric", values_to = "CV")
cv_global_long$Behavior <- factor(cv_global_long$Behavior,
                                    levels = c("Transitting", "Mobbing"))

# Compute cross-flock medians from the pivoted medians
cv_meds <- cv_global_long %>%
  group_by(Metric, Behavior) %>%
  summarise(med_CV = median(CV),
            se_CV = sd(CV)/ sqrt(n()),
            .groups = "drop")

# Wilcox test
anno_df <-  cv_global_summary %>%
  pivot_longer(cols = c(med_cvpop , med_cvind, med_ratio),
               #,mean_cvpop, mean_cvind, mean_ratio),
               names_to = "Metric",
               values_to = "CV") %>%
  group_by(Metric) %>%
  summarise(wtest = list(wilcox.test(CV~Behavior)),
            .groups = "drop") %>% # using wilcox because diff sample sizes, no normality
  mutate(tidied = map(wtest, broom::tidy)) %>%
  unnest(tidied) %>%
  mutate(p.adjusted = p.adjust(p.value, method = "BH"),
         dimension = "2D")

# Pick y-positions above each metric's data
y_pos <- cv_global_long %>%
  group_by(Metric) %>%
  summarise(y = max(CV, na.rm = TRUE) * 1.06, .groups = "drop") %>%
  pull(y)


twod<- ggplot(cv_global_long,
       aes(Metric, CV, colour = Behavior,
           group = interaction(Metric, Behavior))) +
  geom_violin(position = position_dodge(width = 0.7),
               alpha = 0.5) +
  geom_point(position = position_jitterdodge(jitter.width = 0.15,
                                             dodge.width  = 0.7),
             size = 2) +
  geom_errorbar(data = cv_meds,
                aes(x = Metric, ymin = (med_CV-se_CV), ymax=(med_CV+se_CV), group = Behavior),
                width=.2,
                position = position_dodge(width = 0.7), alpha = .8,
                col = "black", inherit.aes = F)+
  # Mean overlay — same Metric values, dodges correctly
  geom_point(data = cv_meds,
             aes(y = med_CV),
             position = position_dodge(width = 0.7), col = "black",
             alpha = .8,
             shape = 18, size = 3) +   # diamond shape distinguishes from jitter
  geom_signif(
    annotation = symnum(anno_df$p.adjusted,
                        cutpoints = c(0, 0.001, 0.01, 0.05, 1),
                        symbols   = c("***", "**", "*", "ns")) |> as.character(),
    y_position = y_pos,
    xmin = c(1, 2, 3) - 0.175,   # left violin of each dodged pair
    xmax = c(1, 2, 3) + 0.175,   # right violin
    tip_length = 0.01,
    textsize = 5,
    col = "black"
  ) +
  scale_color_manual(values = friendly_pal("contrast_three"))+
  scale_x_discrete(labels = c(
    med_cvind = expression(CV[italic(indiv)]),
    med_cvpop = expression(CV[italic(pop)]),
    med_ratio = "Ratio"
  )) +
  theme_bw(base_size = 14) +
  theme(axis.text.x = element_text(size = 13,color = "black"))+
  labs(x = "", colour = "Behavior"
       , subtitle = "2D Median CV per flock"
  )

twod


# running multiple hypothesis tests, the probability of getting at least one false positive by chance increases with each test you add =  family-wise error rate inflation problem.

#---- 3D Median CV Across Time -----
cv_transit_3d <- transit %>%
  group_by(flock, t) %>%
  filter(n() >= 10) %>%
  summarise(
    n_birds = n(),
    cv_pop  = {
      D <- as.vector(dist(cbind(x, y, z)))
      sd(D) / mean(D)
    },
    cv_ind  = {
      D_mat <- as.matrix(dist(cbind(x, y, z)))
      diag(D_mat) <- NA
      mean_per_bird <- rowMeans(D_mat, na.rm = TRUE)
      sd(mean_per_bird) / mean(mean_per_bird)
    },
    ratio = cv_ind / cv_pop,
    .groups = "drop"
  )

#saveRDS(cv_transit_3d, "CVSupplementalMaterial/casestudies/cv_transit_3d.rds")

cv_mob_3d <- mob %>%
  group_by(flock, t) %>%
  filter(n() >= 10) %>%
  summarise(
    n_birds = n(),
    cv_pop  = {
      D <- as.vector(dist(cbind(x, y, z)))
      sd(D) / mean(D)
    },
    cv_ind  = {
      D_mat <- as.matrix(dist(cbind(x, y, z)))
      diag(D_mat) <- NA
      mean_per_bird <- rowMeans(D_mat, na.rm = TRUE)
      sd(mean_per_bird) / mean(mean_per_bird)
    },
    ratio = cv_ind / cv_pop,
    .groups = "drop"
  )
#saveRDS(cv_mob_3d, "CVSupplementalMaterial/casestudies/cv_mob_3d.rds")


cv_transit_global3d <- cv_transit_3d %>% group_by(flock) %>% summarise(
  n_birds = n(),
  med_cvpop = median(cv_pop),
  med_cvind = median(cv_ind),
  med_ratio = median(ratio),
  # mean_cvpop = mean(cv_pop),
  # mean_cvind = mean(cv_ind),
  # mean_ratio = mean(ratio),
  .groups = "drop"
)
cv_mob_global3d <- cv_mob_3d %>% group_by(flock) %>% summarise(
  n_birds = n(),
  med_cvpop = median(cv_pop),
  med_cvind = median(cv_ind),
  med_ratio = median(ratio),
  # mean_cvpop = mean(cv_pop),
  # mean_cvind = mean(cv_ind),
  # mean_ratio = mean(ratio),
  .groups = "drop"
)
cv_transit_global3d$Behavior <- "Transitting"
cv_mob_global3d$Behavior <- "Mobbing"

cv_global_summary3d <- rbind(cv_transit_global3d, cv_mob_global3d)
cv_global_long3d <- cv_global_summary3d %>%
  pivot_longer(cols = c(med_cvpop, med_cvind, med_ratio),
               names_to = "Metric", values_to = "CV")

cv_global_long3d$Behavior <- factor(cv_global_long3d$Behavior,
                                       levels = c("Transitting", "Mobbing"))

# Compute cross-flock means from the pivoted medians
cv_meds3d <- cv_global_long3d %>%
  group_by(Metric, Behavior) %>%
  summarise(med_CV = median(CV),
            se_CV = sd(CV)/ sqrt(n()),
            .groups = "drop")

# Wilcox test
anno_df3d <-  cv_global_summary3d %>%
  pivot_longer(cols = c(med_cvpop , med_cvind, med_ratio),
               #,mean_cvpop, mean_cvind, mean_ratio),
               names_to = "Metric",
               values_to = "CV") %>%
  group_by(Metric) %>%
  summarise(wtest = list(wilcox.test(CV~Behavior)),
            .groups = "drop") %>% # using wilcox because diff sample sizes, no normality
  mutate(tidied = map(wtest, broom::tidy)) %>%
  unnest(tidied) %>%
  mutate(p.adjusted = p.adjust(p.value, method = "BH"),
         dimension = "3D")

# Pick y-positions above each metric's data
y_pos <- cv_global_long3d %>%
  group_by(Metric) %>%
  summarise(y = max(CV, na.rm = TRUE) * 1.06, .groups = "drop") %>%
  pull(y)

threed <- ggplot(cv_global_long3d,
       aes(Metric, CV, colour = Behavior,
           group = interaction(Metric, Behavior))) +
  geom_violin(position = position_dodge(width = 0.7),
               alpha = 0.5) +
  geom_point(position = position_jitterdodge(jitter.width = 0.15,
                                             dodge.width  = 0.7),
             size = 2) +
  geom_errorbar(data = cv_meds3d,
                aes(x = Metric, ymin = (med_CV-se_CV), ymax=(med_CV+se_CV), group = Behavior),
                width=.2,
                position = position_dodge(width = 0.7), col = "black", inherit.aes = F)+
  # Mean overlay — same Metric values, dodges correctly
  geom_point(data = cv_meds3d,
             aes(y = med_CV),
             position = position_dodge(width = 0.7), col = "black",
             shape = 18, size = 3) +   # diamond shape distinguishes from jitter
  geom_signif(
    annotation = symnum(anno_df3d$p.adjusted,
                        cutpoints = c(0, 0.001, 0.01, 0.05, 1),
                        symbols   = c("***", "**", "*", "ns")) |> as.character(),
    y_position = y_pos,
    xmin = c(1, 2, 3) - 0.175,   # left violin of each dodged pair
    xmax = c(1, 2, 3) + 0.175,   # right violin
    tip_length = 0.01,
    textsize = 5,
    col = "black"
  ) +
  scale_color_manual(values = friendly_pal("contrast_three"))+
  scale_x_discrete(labels = c(
    med_cvind = expression(CV[italic(indiv)]),
    med_cvpop = expression(CV[italic(pop)]),
    med_ratio = "Ratio"
  )) +
  theme_bw(base_size = 14) +
  theme(axis.text.x = element_text(size = 13, color = "black"))+
  labs(x = "", colour = "Behavior"
      , subtitle = "3D Median CV per flock"
      )
threed
twod  <- twod  + theme(plot.tag = element_text(face = "bold"),
                       legend.position = "bottom")
threed <- threed + theme(plot.tag = element_text(face = "bold"),
                         legend.position = "bottom")

 ((twod +
     scale_y_continuous(expand = expansion(mult = c(0.05, 0.12))))  |
     (threed +
        scale_y_continuous(expand = expansion(mult = c(0.05, 0.12))))) +
  plot_layout(guides = "collect") +
  plot_annotation(theme = theme(legend.position = "bottom",
  ), tag_levels = list(c("A", "B")))


