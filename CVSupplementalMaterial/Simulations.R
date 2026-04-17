# Script to run simulations & build figures
# Note: we ran the parallelized simulations the UBC sockeye server. We recommend reducing the number of iterations in the mclapply lines if running on a laptop

#----Libraries----
set.seed(123)
library(MASS)
library(shapeshiftr)
library(tidyr)
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
    data = label_df,
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
      center <- c(50, 50) # Center of ellipses
      points <- mvrnorm(n = n_individuals, mu = c(50, 50), Sigma = time_cov_matrices[[t]])

      resultsherd[t] <- shapeshiftr::cvpop(points)
      resultsamong[t] <- shapeshiftr::cvind(points)
      ratio[t] <- shapeshiftr::cvratio(points)
      points_list[[t]] <- data.frame(x = points[, 1], y = points[, 2], year = t)
    }
    rm(all_points_df)
    all_points_df <- do.call(rbind, points_list)
    all_points_df$n_individuals <- n_individuals
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

    iteration_results[[as.character(n_individuals)]] <- list(
      results_df = rdf,
      points_df = all_points_df)
  }
  return(iteration_results)
}

num_cores <- 30
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
  #do.call(rbind,): combines all results_df into one df
  #lapply: loops through the names of the inner-outer list
  do.call(rbind, lapply(seq_along(samplesize_results[[iter_idx]]), function(n_ind_idx) {
    n_ind_name <- n_ind_idx
    # Extract results
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

# ---- Even Split -----
run_iteration <- function(iteration, sample_sizes, n_timepoints, sample_covariance_matrix) {
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
    points_list[[1]] <- data.frame(x = initial_points[, 1], y = initial_points[, 2], year = 1,
                                   cluster_assignment = 1)
    center1 <- c(50,50)
    center2 <- c(50,400)
    cluster_assignment <- sample(1:2, n_individuals, replace = TRUE, prob = c(.5, .5))

    for (t in 2:n_timepoints) {
      # Define cluster centers
      alpha <- (t - 1) / (n_timepoints - 1)
      center2_t <- (1 - alpha) *c(50, 50) + alpha * center2
      points <- t(sapply(cluster_assignment, function(cluster) {
        if (cluster == 1) {
          mvrnorm(1, mu = center1, Sigma = sample_covariance_matrix)
        } else {
          mvrnorm(1, mu = center2_t, Sigma = sample_covariance_matrix)
        }
      }))

      resultsherd[t] <- shapeshiftr::cvpop(points)
      resultsamong[t] <- shapeshiftr::cvind(points)
      ratio[t] <- shapeshiftr::cvratio(points)
      points_list[[t]] <- data.frame(x = points[, 1], y = points[, 2], year = t,
                                     cluster_assignment = cluster_assignment)
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

num_cores <- 30  # Use one less than total cores
samplesize_results <- mclapply(1:1000, run_iteration,
                               sample_sizes = sample_sizes,
                               n_timepoints = n_timepoints,
                               sample_covariance_matrix = sample_covariance_matrix,
                               mc.cores = num_cores)

resultsdf <- do.call(rbind, lapply(seq_along(samplesize_results), function(iter_idx) {
  #seq_along(samplesize_results): Since mclapply returns a list without named indices, using seq_along() ensures compatibility.
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
  #seq_along(samplesize_results): Since mclapply returns a list without named indices, using seq_along() ensures compatibility.
  iter_name <- iter_idx
  #do.call(rbind,): combines all results_df into one df
  #lapply: loops through the names of the inner-outer list
  do.call(rbind, lapply(seq_along(samplesize_results[[iter_idx]]), function(n_ind_idx) {
    n_ind_name <- n_ind_idx
    # Extract results
    cbind(iteration = as.numeric(iter_name),
          samplesize_results[[iter_idx]][[n_ind_idx]]$points_df)
  }))
}))

ratios_even <- resultsdf %>% group_by(Type, n_individuals) %>%
  summarise(n_total = n(), # should be 100 per type
            nsig = sum(significant == "Significant"),  # Count of sig slopes
            prop_sig = round(nsig / n_total, 2)*100  # prop sig
  )
even_rdf <- left_join(resultsdf, ratios_even %>% dplyr::select(Type, n_individuals, prop_sig), by = c("Type", "n_individuals"))
maxylabel <- max(even_rdf$slope, na.rm = TRUE)
label_df <- even_rdf %>%
  group_by(n_individuals, Type) %>%
  summarise(n_total = n(), # should be 100 per type
            nsig = sum(significant == "Significant"),  # Count of sig slopes
            prop_sig = round(nsig / n_total, 2)*100,
            x_pos = maxylabel
  )
even_rdf$scenario = "Even split"

even_plot<- ggplot(even_rdf, aes(x = slope, y = as.factor(n_individuals), fill = stat(x))) +
  geom_density_ridges_gradient(scale = 2, rel_min_height = 0.01) +
  scale_fill_gradientn(
    colors = c("turquoise3","white", "goldenrod3"), #f65026 blood orange #00d1b5 cyan goldenrod2
    name = "Slope",
    values = scales::rescale(c(-0.01, 0, .01)),
    oob = scales::squish,
  ) +
  geom_label(
    data = label_df,
    aes(
      x = .01-.001,
      y = as.factor(n_individuals),
      label = paste0(prop_sig, "%")
    ),
    inherit.aes = FALSE,
    label.padding = unit(0.2, "lines"),
    size = 3
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
even_plot

# significance
even_rdf %>% group_by(n_individuals, Type) %>%
  summarise(mean_slope = mean(slope),
            mean_se = mean(as.numeric(se))) %>%
  mutate(sig_to_noise = mean_slope / mean_se)


# ---- Uneven Split -----
run_iteration <- function(iteration, sample_sizes, n_timepoints, sample_covariance_matrix) {
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
    points_list[[1]] <- data.frame(x = initial_points[, 1], y = initial_points[, 2], year = 1,
                                   cluster_assignment = 1)
    center1 <- c(50,50)
    center2 <- c(50,400)
    cluster_assignment <- sample(1:2, n_individuals, replace = TRUE, prob = c(.95, .05))

    for (t in 2:n_timepoints) {
      # Define cluster centers
      alpha <- (t - 1) / (n_timepoints - 1)
      center2_t <- (1 - alpha) *c(50, 50) + alpha * center2
      points <- t(sapply(cluster_assignment, function(cluster) {
        if (cluster == 1) {
          mvrnorm(1, mu = center1, Sigma = sample_covariance_matrix)
        } else {
          mvrnorm(1, mu = center2_t, Sigma = sample_covariance_matrix)
        }
      }))

      resultsherd[t] <- shapeshiftr::cvpop(points)
      resultsamong[t] <- shapeshiftr::cvind(points)
      ratio[t] <- shapeshiftr::cvratio(points)
      points_list[[t]] <- data.frame(x = points[, 1], y = points[, 2], year = t,
                                     cluster_assignment = cluster_assignment)
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

num_cores <- 30  # Use one less than total cores
samplesize_results <- mclapply(1:1000, run_iteration,
                               sample_sizes = sample_sizes,
                               n_timepoints = n_timepoints,
                               sample_covariance_matrix = sample_covariance_matrix,
                               mc.cores = num_cores)

resultsdf <- do.call(rbind, lapply(seq_along(samplesize_results), function(iter_idx) {
  #seq_along(samplesize_results): Since mclapply returns a list without named indices, using seq_along() ensures compatibility.
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
  #seq_along(samplesize_results): Since mclapply returns a list without named indices, using seq_along() ensures compatibility.
  iter_name <- iter_idx
  #do.call(rbind,): combines all results_df into one df
  #lapply: loops through the names of the inner-outer list
  do.call(rbind, lapply(seq_along(samplesize_results[[iter_idx]]), function(n_ind_idx) {
    n_ind_name <- n_ind_idx
    # Extract results
    cbind(iteration = as.numeric(iter_name),
          samplesize_results[[iter_idx]][[n_ind_idx]]$points_df)
  }))
}))

ratios_uneven <- resultsdf %>% group_by(Type, n_individuals) %>%
  summarise(n_total = n(), # should be 100 per type
            nsig = sum(significant == "Significant"),  # Count of sig slopes
            prop_sig = round(nsig / n_total, 2)*100  # prop sig
  )
uneven_rdf <- left_join(resultsdf, ratios_uneven %>% dplyr::select(Type, n_individuals, prop_sig), by = c("Type", "n_individuals"))
maxylabel <- max(uneven_rdf$slope, na.rm = TRUE)
label_df <- uneven_rdf %>%
  group_by(n_individuals, Type) %>%
  summarise(n_total = n(), # should be 100 per type
            nsig = sum(significant == "Significant"),  # Count of sig slopes
            prop_sig = round(nsig / n_total, 2)*100,
            x_pos = maxylabel
  )
uneven_rdf$scenario = "Uneven split"

uneven_plot<- ggplot(uneven_rdf, aes(x = slope, y = as.factor(n_individuals), fill = stat(x))) +
  geom_density_ridges_gradient(scale = 2, rel_min_height = 0.01) +
  scale_fill_gradientn(
    colors = c("turquoise3","white", "goldenrod3"), #f65026 blood orange #00d1b5 cyan goldenrod2
    values = scales::rescale(c(-0.05, 0, 0.05)),  # manually rescale to your actual data range
    limits = c(-0.05, 0.05),                      # specify full range of data
    breaks = scales::pretty_breaks(n = 3),
    name = "Slope"
  ) +
  geom_label(
    data = label_df,
    aes(
      x = x_pos-.001,
      y = as.factor(n_individuals),
      label.padding = unit(0.2, "lines"),
      label = paste0(prop_sig, "%")
    ),
    inherit.aes = FALSE,
    size = 3
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
uneven_plot

# significance
uneven_rdf %>% group_by(n_individuals, Type) %>%
  summarise(mean_slope = mean(slope),
            mean_se = mean(as.numeric(se))) %>%
  mutate(sig_to_noise = mean_slope / mean_se)




# ---- Combine plots ----
((sym_plot+labs(subtitle = bquote(bold(A.)~Symmetrical))+guides(fill = "none")) +
   (elplot+labs(subtitle = bquote(bold(B.)~Elongation))+guides(fill = "none")) +
   (even_plot+labs(subtitle = bquote(bold(C.)~"Even "*"split"))+guides(fill = "none"))+
   (uneven_plot+labs(subtitle = bquote(bold(D.)~"Uneven "*"split")))) +
  plot_layout(guides = "collect", axis_titles = "collect") &theme(legend.position = "bottom")
################ Multiple Patches ###############################
# ---- Even Split ----
cluster_range <- c(5, 10, 20)
max_distance <- 8 * initial_sd
run_iteration2 <- function(iteration, sample_sizes, n_timepoints, sample_covariance_matrix, asym = FALSE) {
  all_results <- list()
  all_points  <- list()
  keep_points <- iteration %in% c(1, 42, 647)


  for (n_individuals in sample_sizes) {
    for (num_clusters in cluster_range) {
      if (asym == TRUE){
        prop_core <- 0.8
        n_core    <- round(prop_core * n_individuals)
        n_other   <- n_individuals - n_core

        # Everyone in core cluster (cluster 1)
        cluster_assignment <- integer(n_individuals)
        cluster_assignment[1:n_core] <- 1

        # Remaining individuals randomly assigned to clusters 2:num_clusters
        cluster_assignment[(n_core + 1):n_individuals] <-
          sample(2:num_clusters, size = n_other, replace = TRUE)

        # Shuffle so ordering doesn't matter
        cluster_assignment <- sample(cluster_assignment, replace = FALSE)
      } else {
        # Balanced cluster membership
        q <- n_individuals %/% num_clusters
        r <- n_individuals %% num_clusters
        cluster_assignment <- c(
          rep.int(1:num_clusters, times = q),
          sample.int(num_clusters, size = r)
        )
        cluster_assignment <- sample(cluster_assignment, replace = FALSE)
      }

      # Randomize which individual is in which cluster (not: the first "x" go to cluster 1, etc)
      # Goal: for each cluster, quickly know which rows/individuals belong to that cluster.
      # groups those indices by the cluster label
      # draw one MVN per cluster per timepoint, but we still need to know which individuals to fill for that cluster. could use which(), but that's v slow
      idx_list <- split(seq_len(n_individuals), cluster_assignment)
      #target_centers <- matrix(round(runif(2*num_clusters, 50, max_distance)), ncol = 2)
      target_centers <- center + matrix(
        runif(2 * num_clusters, -max_distance, max_distance),
        ncol = 2
      )
      target_centers[1, ] <- center
      # Initial points
      initial_points <- MASS::mvrnorm(n = n_individuals, mu = center, Sigma = sample_covariance_matrix)

      # Preallocate
      resultsherd  <- numeric(n_timepoints)
      resultsamong <- numeric(n_timepoints)
      ratio        <- numeric(n_timepoints)
      points_list  <- vector("list", n_timepoints)

      # t = 1
      resultsherd[1]  <- shapeshiftr::cvpop(initial_points)
      resultsamong[1] <- shapeshiftr::cvind(initial_points)
      ratio[1]        <- shapeshiftr::cvratio(initial_points)

      points_list <- if (keep_points) vector("list", n_timepoints) else NULL # keeping the points for all the df is computationally expensive, so only keeping the first iteration for each sample size and scenario for visualization purposes
      if (keep_points) {
        points_list[[1]] <- data.frame(
          x = initial_points[, 1], y = initial_points[, 2],
          year = 1,
          n_individuals = n_individuals,
          num_clusters = num_clusters,
          cluster_assignment = 1,
          iteration = iteration
        ) }

      # t = 2..n_timepoints
      for (t in 2:n_timepoints) {
        f <- (t - 1) / (n_timepoints - 1)
        current_centers <- center + f * (target_centers - center)

        points <- matrix(NA_real_, nrow = n_individuals, ncol = 2)

        for (cl in seq_len(num_clusters)) {
          # idx might be NULL if that cluster didn't get any individuals (rare but possible when r=0 and q=0)
          idx <- idx_list[[as.character(cl)]]
          if (!is.null(idx) && length(idx) > 0) {
            points[idx, ] <- MASS::mvrnorm(
              n = length(idx),
              mu = current_centers[cl, ],
              Sigma = sample_covariance_matrix
            )
          }
        }

        resultsherd[t]  <- shapeshiftr::cvpop(points)
        resultsamong[t] <- shapeshiftr::cvind(points)
        ratio[t]        <- shapeshiftr::cvratio(points)

        if (keep_points) {
          points_list[[t]] <- data.frame(
            x = points[, 1], y = points[, 2],
            year = t,
            n_individuals = n_individuals,
            num_clusters = num_clusters,
            cluster_assignment = cluster_assignment,
            iteration = iteration
          )
        }

      }

      # Build long results (no pivot)
      rdf <- data.frame(
        timepoint = rep.int(seq_len(n_timepoints), times = 3),
        Type = factor(rep(c("CV_among", "CV_herd", "Ratio"), each = n_timepoints),
                      levels = c("CV_among", "CV_herd", "Ratio")),
        CV = c(resultsamong, resultsherd, ratio),
        n_individuals = n_individuals,
        num_clusters  = num_clusters,
        iteration     = iteration
      )

      # slope per Type (3 fits)
      slope_info <- lapply(split(rdf, rdf$Type), function(d) {
        # split by Type, essentially making 3 tiny dfs, lapply loops over each df and applies the functions in the curly brackets.
        info <- check_slope_significance2(d[, c("timepoint", "CV")])
        data.frame(
          Type = d$Type[1],
          slope = info$slope[[1]],
          se = info$se[[1]],
          p_value = info$p_value[[1]],
          significant = info$significant[[1]]
        )
      })
      slope_info <- do.call(rbind, slope_info)
      # do this instead of merge, merge takes a lot of time
      rdf$slope <- slope_info$slope[match(rdf$Type, slope_info$Type)]
      rdf$se <- slope_info$se[match(rdf$Type, slope_info$Type)]
      rdf$p_value <- slope_info$p_value[match(rdf$Type, slope_info$Type)]
      rdf$significant <- slope_info$significant[match(rdf$Type, slope_info$Type)]

      #rdf <- merge(rdf, slope_info, by = "Type", all.x = TRUE, sort = FALSE)
      rdf$significant <- factor(rdf$significant, levels = c("Significant", "Not Significant"))

      key <- paste(n_individuals, num_clusters, sep = "_")
      all_results[[key]] <- rdf
      if (keep_points) {
        all_points[[key]] <- do.call(rbind, points_list)
      }
    }
  }

  list(
    results_df = do.call(rbind, all_results),
    points_df  = if (keep_points) do.call(rbind, all_points) else NULL )
}

num_cores <- 3  # Use fewer cores on laptop, gets laggy
samplesize_results <- mclapply(1:10, run_iteration2,
                               sample_sizes = sample_sizes,
                               n_timepoints = n_timepoints,
                               sample_covariance_matrix = sample_covariance_matrix,
                               asym = FALSE,
                               mc.cores = num_cores)

# Extract results_df and points_df efficiently
resultsdf <- do.call(rbind, lapply(samplesize_results, `[[`, "results_df"))
resultsdf$slope <- as.numeric(resultsdf$slope)
head(resultsdf)

pointsdf <- do.call(rbind, lapply(samplesize_results, `[[`, "points_df"))
pointsdf$year <- as.numeric(pointsdf$year)

pal <- friendly_pal("contrast_three", 5, type = "continuous")
df_plot <- pointsdf %>%
  filter(year %in% c(20), iteration == 1) %>%
  mutate(cluster_id = interaction(num_clusters, cluster_assignment, drop = TRUE, sep = "__"))

cols <- df_plot %>%
  distinct(num_clusters, cluster_assignment) %>%
  group_by(num_clusters) %>%
  arrange(as.integer(cluster_assignment), .by_group = TRUE) %>%
  summarise(
    pal = list(setNames(
      ggpubfigs::friendly_pal("muted_nine", n(), type = "continuous"),
      paste(num_clusters, cluster_assignment, sep = "__")
    )),
    .groups = "drop"
  ) %>%
  pull(pal) %>%
  unlist()

# visualize what the different population patches look like on the landscape
pointsplot <- ggplot(df_plot, aes(x, y, color = cluster_id)) +
  geom_point(alpha = 0.7) +
  scale_color_manual(values = cols) +
  theme_bw() +
  facet_wrap(~num_clusters, nrow = 1) +
  guides(color = "none") +
  coord_equal()
#labs(title = "Uneven split")
pointsplot


maxylabel <- .02
total_slope <- resultsdf %>%
  dplyr::distinct(iteration, n_individuals, num_clusters, Type, slope, .keep_all = TRUE)

label_df <- total_slope %>%
  group_by(num_clusters, n_individuals, Type) %>%
  summarise(n_total = n(), # should be 1000 per type
            nsig = sum(significant == "Significant"),  # Count of sig slopes
            prop_sig = round(nsig / n_total, 2)*100,
            x_pos = maxylabel
  )

label_df2 <- total_slope %>%
  dplyr::mutate(
    expected_sign = dplyr::case_when(
      Type %in% c("CV_among", "Ratio") ~ 1,
      Type == "CV_herd"                ~  1
    ),
    sig = p_value < 0.05,
    correct_dir = sign(slope) == expected_sign,

    power_hit   = sig & correct_dir,
    wrong_dir   = sig & !correct_dir,
    missed      = !sig
  ) %>%
  dplyr::group_by(num_clusters, n_individuals, Type) %>%
  dplyr::summarise(
    n_total  = n(),
    n_power  = sum(power_hit),
    n_wrong  = sum(wrong_dir),
    n_missed = sum(missed),

    power_pct  = round(100 * n_power  / n_total, 0),
    wrong_pct  = round(100 * n_wrong  / n_total, 0),
    missed_pct = round(100 * n_missed / n_total, 0),

    x_pos = maxylabel,
    x_pos_min = -maxylabel,
    .groups = "drop"
  )

make_cluster_plot <- function(k) {
  df_k  <- total_slope %>% filter(num_clusters == k)
  lab_k <- label_df2    %>% filter(num_clusters == k)

  ggplot(df_k, aes(x = slope, y = factor(n_individuals), fill = after_stat(x))) +
    geom_density_ridges_gradient(scale = 2, rel_min_height = 0.01) +
    scale_fill_gradientn(
      colors = c("turquoise3", "white", "goldenrod3"),
      name = "Slope",
      values = scales::rescale(c(-0.02, 0, 0.02)),
      limits = c(-0.02, 0.02),
      oob = scales::squish
    ) +
    geom_label(
      data = lab_k,
      aes(
        x = x_pos,
        y = factor(n_individuals),
        label = paste0(
          "Missed: ", missed_pct, "%"
        )
      ),
      inherit.aes = FALSE,
      label.padding = unit(0.2, "lines"),
      size = 3
    ) +
    geom_label(
      data = lab_k,
      aes(
        x = x_pos_min,
        y = factor(n_individuals),
        label = paste0(
          "Power: ", power_pct, "%"
          # "Power: ", power_pct, "%\n",
          #"Wrong-dir: ", wrong_pct, "%"
        )
      ),
      inherit.aes = FALSE,
      label.padding = unit(0.2, "lines"),
      size = 3
    )+
    facet_wrap(~Type, labeller = ggplot2::labeller(Type = cv_names, .default = ggplot2::label_parsed)) +
    geom_vline(xintercept = 0, linetype = "dashed", linewidth = 1) +
    labs(
      title = paste0("Clusters: ", k),
      x = "Slope of Linear Regression",
      y = "Sample Size"
    ) +
    theme_bw() +
    theme(
      strip.text = element_text(size = 10),
      legend.position = "bottom"
    )+
    xlim(-.02, .025)
}

cluster_vals <- sort(unique(total_slope$num_clusters))
plots <- lapply(cluster_vals, make_cluster_plot)
names(plots) <- paste0("k_", cluster_vals)
combined <- wrap_plots(plots, ncol = 1, guides = "collect") +
  plot_layout(axis_titles = "collect")+
  patchwork::plot_annotation(tag_levels = 'A', title = "Even Split") &
  theme(legend.position = "bottom",
        plot.tag = element_text(face = 'bold'))
combined

