# Script to run simulations & build figures
# Note: we ran the parallelized simualtions the UBC sockeye server. We recommend reducing the number of iterations in the mclapply lines if running on a laptop

#----Libraries----
set.seed(123)
library(MASS)
library(shapeshiftr)
library(dplyr)
library(ggplot2)
library(parallel)
library(ggpubfigs)
library(ggridges)
library(patchwork)
#----Functions----
cv_names <- c(`CV_among` = bquote(CV[italic(indiv)]), `CV_herd` = bquote(CV[italic(pop)]),
              #`Ratio` = bquote(frac(CV[italic(indiv)],CV[italic(pop)]))
              `Ratio` = "Ratio"
)
cv_labeller <- function(variable,value){
  return(cv_names[value])
}

check_slope_significance <- function(data) {
  reg <- lm(CV ~ timepoint, data = data)
  slope <- round(summary(reg)$coefficients[2, 1],4)  # slope value
  se <- round(summary(reg)$coefficients[2, 2],4)  # standard error of slope
  p_value <- round(summary(reg)$coefficients[2, 4],4)  # p-value of slope
  significant <- ifelse(p_value < 0.05, "Significant", "Not Significant")
  return(c(slope = slope, se = se, p_value = p_value, significant = significant))
}
check_slope_significance2 <- function(data) {
  tryCatch({
    reg <- lm(CV ~ timepoint, data = data)
    sm  <- summary(reg)$coefficients
    tibble::tibble(
      slope = round(sm[2, 1], 4),
      se = round(sm[2, 2], 4),
      p_value = round(sm[2, 4], 4),
      significant = ifelse(sm[2, 4] < 0.05, "Significant", "Not Significant")
    )
  }, error = function(e) {
    tibble::tibble(slope = NA_real_, se = NA_real_, p_value = NA_real_, significant = NA_character_)
  })
}

herdCV <- function(points) {
  dist_matrix <- as.matrix(dist(points))
  lower_tri_indices <- lower.tri(dist_matrix)
  lower_tri_distances <- dist_matrix[lower_tri_indices]
  CV_lower_tri_distances <- round(sd(lower_tri_distances)/mean(lower_tri_distances), 4)
  # CV_lower_tri_distances <- round(log(sd(lower_tri_distances)) - log(mean(lower_tri_distances)), 4)
  return(CV_lower_tri_distances)
  #return(dist_matrix[lower_tri_indices])
}
amongCV <- function(points) {
  dist_matrix <- as.matrix(dist(points))
  # Initialize a list to store the pairwise distances for each individual
  individual_distances <- vector("list", nrow(points))
  names(individual_distances) <- rownames(points)
  # Loop through the lower triangle of the distance matrix
  for (i in 2:nrow(dist_matrix)) {
    for (j in 1:(i-1)) {
      # Add the distance to the list for both individuals
      individual_distances[[i]] <- c(individual_distances[[i]], dist_matrix[i, j])
      individual_distances[[j]] <- c(individual_distances[[j]], dist_matrix[i, j])
    }
  }
  # Initialize a vector to store mean values for each individual
  mean_values <- numeric(nrow(points))
  # Loop through each individual's distances
  for (i in 1:nrow(points)) {
    # Get the pairwise distances for the i-th individual
    pairwise_distances <- individual_distances[[i]]
    # Compute the mean
    mean_dist <- mean(pairwise_distances)

    if (mean_dist != 0) {
      mean_values[i] <- mean_dist
    } else {
      mean_values[i] <- NA  # Handle cases where the mean distance is zero
    }
  }
  CV_mean_values <- round(sd(mean_values, na.rm = T)/mean(mean_values, na.rm = T),4)
  #CV_mean_values <- round(log(sd(mean_values, na.rm = T)) - log(mean(mean_values, na.rm = T)),4)
  return(CV_mean_values)
}
generate_cluster_centers <- function(num_clusters, grid_size) {
  # Determine number of rows and columns to place clusters (sqrt for square grid-like placement)
  n_rowcol <- ceiling(sqrt(num_clusters))  # Number of rows/cols in the grid

  # Calculate the spacing between clusters
  spacing <- grid_size / (n_rowcol + 1)

  # Generate the coordinates for cluster centers
  coords <- expand.grid(x = seq(spacing, grid_size - spacing, length.out = n_rowcol),
                        y = seq(spacing, grid_size - spacing, length.out = n_rowcol))

  # Sample the number of clusters we need from the evenly spaced points
  cluster_centers <- coords[sample(nrow(coords), num_clusters), ]

  return(cluster_centers)
}
#----Initial Points----
n_timepoints <- 20
sample_sizes <- c(10, 50,75, 100, 500)
initial_sd <- 25
initial_sd_y <- 25
initial_sd_x <- 25
separation_in_sd <- 4
center = c(50,50)
sample_covariance_matrix <- matrix(c(initial_sd_x^2, 0, 0, (initial_sd_y )^2),
                                   ncol = 2)

slope = -1*(initial_sd*(1/separation_in_sd) - initial_sd)/(n_timepoints-1)
# there are only 19 timepoints that go through the simulation # 0.9868421

time_cov_matrices <- lapply(0:(n_timepoints-1), function(t) {
  matrix(c((initial_sd - slope * t)^2, 0, 0, (initial_sd - slope * t)^2 ), ncol = 2)
})

################ Simple Simulations #############################
# ---- Symmetrical -----
run_iteration <- function(iteration, sample_sizes, n_timepoints, sample_covariance_matrix, time_cov_matrices) {
  iteration_results <- list()
  for (n_individuals in sample_sizes) {
    # create bivariate normal distribution
    initial_points <- mvrnorm(n = n_individuals,
                              mu = c(50,50),
                              Sigma = sample_covariance_matrix)
    # Preallocate results
    resultsherd <- numeric(n_timepoints)
    resultsamong <- numeric(n_timepoints)
    ratio <- numeric(n_timepoints)
    points_list <- vector("list", n_timepoints)
    resultsherd[1] <- shapeshiftr::cvpop(initial_points)
    resultsamong[1] <- shapeshiftr::cvind(initial_points)
    ratio[1] <- shapeshiftr::cvratio(initial_points)
    #ratio[1] <- resultsamong[1] / resultsherd[1]
    points_list[[1]] <- data.frame(x = initial_points[, 1], y = initial_points[, 2], year = 1)
    for (t in 2:n_timepoints){
      # create bivariate normal distribution
      points <- mvrnorm(n = n_individuals, mu = center,
                        Sigma = time_cov_matrices[[t]])

      resultsherd[t] <- shapeshiftr::cvpop(points)
      resultsamong[t] <- shapeshiftr::cvind(points)
      ratio[t] <- shapeshiftr::cvratio(points)
      #ratio[t] <- resultsamong[t]/resultsherd[t]
      points_list[[t]] <- data.frame(x = points[, 1], y = points[, 2], year = t)
    }
    rm(all_points_df)
    all_points_df <- do.call(rbind, points_list)
    all_points_df$n_individuals <- n_individuals
    # Convert results to a data frame for plotting
    results_df <- data.frame(timepoint = 1:n_timepoints,
                             CV_herd = resultsherd,
                             CV_among = resultsamong,
                             Ratio = ratio
    )
    rdf <- results_df %>%
      pivot_longer(cols = c("CV_herd", "CV_among", "Ratio"),
                   names_to = "Type", values_to = "CV") %>%
      mutate(Type = factor(Type, levels = c("CV_among","CV_herd", "Ratio")))
    # CV names for labels
    cv_names <- c(`CV_among` = "Individual-Level CV", `CV_herd` = "Herd-Level CV",
                  `ratio` = "Ratio CVs")
    rdf <- rdf %>%
      group_by(Type) %>%
      mutate(slope_info = list(check_slope_significance(cur_data()))) %>%
      unnest_wider(slope_info)
    rdf$significant <- factor(rdf$significant, levels = c("Significant", "Not Significant"))
    rdf$n_individuals <- n_individuals

    # Store results for each sample size
    iteration_results[[as.character(n_individuals)]] <- list(
      results_df = rdf,
      points_df = all_points_df)
  }
  return(iteration_results)
}

# Run the parallelized iterations
num_cores <- 30  # Use one less than total cores available
samplesize_results <- mclapply(1:1000, run_iteration,
                               sample_sizes = sample_sizes,
                               n_timepoints = n_timepoints,
                               sample_covariance_matrix = sample_covariance_matrix,
                               time_cov_matrices = time_cov_matrices,
                               mc.cores = num_cores)

resultsdf <- do.call(rbind, lapply(seq_along(samplesize_results), function(iter_idx) {
  iter_name <- iter_idx
  #do.call(rbind,): combines all results_df into one df
  #lapply: loops through the names of the inner-outer list
  do.call(rbind, lapply(seq_along(samplesize_results[[iter_idx]]), function(n_ind_idx) {
    n_ind_name <- n_ind_idx
    # Extract results
    cbind(iteration = as.numeric(iter_name),
          samplesize_results[[iter_idx]][[n_ind_idx]]$results_df)
      }))
  }))
resultsdf$slope <- as.numeric(resultsdf$slope)
pointsdf <- do.call(rbind, lapply(seq_along(samplesize_results), function(iter_idx) {
  iter_name <- iter_idx
  do.call(rbind, lapply(seq_along(samplesize_results[[iter_idx]]), function(n_ind_idx) {
    n_ind_name <- n_ind_idx
    cbind(iteration = as.numeric(iter_name),
          samplesize_results[[iter_idx]][[n_ind_idx]]$points_df)
      }))
  }))
ratios_sym <- resultsdf %>% group_by(Type, n_individuals) %>%
  summarise(n_total = n(), # should be 100 per type
            nsig = sum(significant == "Significant"),  # Count of sig slopes
            prop_sig = round(nsig / n_total, 2)*100  # prop sig
  )
sym_rdf <- left_join(resultsdf, ratios_sym %>% dplyr::select(Type, n_individuals, prop_sig), by = c("Type", "n_individuals"))
maxylabel <- max(sym_rdf$slope, na.rm = TRUE)
label_df <- sym_rdf %>%
  group_by(n_individuals, Type) %>%
  summarise(n_total = n(), # should be 100 per type
            nsig = sum(significant == "Significant"),  # Count of sig slopes
            prop_sig = round(nsig / n_total, 2)*100,
            x_pos = maxylabel
  )
sym_rdf$scenario = "Symmetrical"

sym_plot<- ggplot(sym_rdf, aes(x = slope, y = as.factor(n_individuals), fill = stat(x))) +
  geom_density_ridges_gradient(scale = 2, rel_min_height = 0.01) +
  scale_fill_gradientn(
    colors = c("turquoise3","white", "goldenrod3"), #f65026 blood orange #00d1b5 cyan goldenrod2
    name = "Slope",
    limits = c(-0.01, 0.01),
    breaks = c(-0.01, 0, 0.01),
    labels = c("-0.01", "0", "0.01"),
    values = scales::rescale(c(-0.01, 0, .01)),
    oob = scales::squish,
  ) +
  geom_label(
    data = label_dfp,
    aes(
      x = .01-.001,
      y = as.factor(n_individuals),
      label = paste0(prop_sig, "%")
    ),
    inherit.aes = FALSE,
    size = 3,
    label.padding = unit(0.2, "lines")
  ) +
  facet_wrap(~Type, labeller = cv_labeller) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 1) +
  labs(
    x = "Slope of Linear Regression",
    y = "Sample Size"
  ) +
  theme_bw() +
  theme(
    strip.text = element_text(size = 10),
    legend.position = "bottom"
  )
sym_plot

# significance
sym_rdf %>% group_by(n_individuals, Type) %>%
  summarise(mean_slope = mean(slope),
            mean_se = mean(as.numeric(se))) %>%
  mutate(sig_to_noise = mean_slope / mean_se)


# ---- Elongation -----
# ---- Even Split -----
# ---- Uneven Split -----
# ---- Combine plots ----
((sym_plot+labs(subtitle = bquote(bold(A.)~Symmetrical))+guides(fill = "none")) +
   (elplot+labs(subtitle = bquote(bold(B.)~Elongation))+guides(fill = "none")) +
   (cplot+labs(subtitle = bquote(bold(C.)~"Even "*"split"))+guides(fill = "none"))+
   (teplot+labs(subtitle = bquote(bold(D.)~"Uneven "*"split")))) +
  plot_layout(guides = "collect", axis_titles = "collect") &theme(legend.position = "bottom")
################ Multiple Patches ###############################
# ---- Even Split ----
# ---- Power Analysis ----
