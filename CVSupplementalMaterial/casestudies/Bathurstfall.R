# Bathurst Fall Migration

library(ggpubfigs)
library(dplyr)
library(lubridate)
library(data.table)
library(sf)
library(shapeshiftr)
library(kableExtra)
library(parallel)
library(scales)
library(adehabitatHR)
library(viridis)
library(MASS)
library(grid)

#----Make Subsets----
# 2020 raw data cannot be shared, but this is how the pairwise data was created
daily <- st_cast(daily, "POINT")
fall <- daily %>% filter(season == "fall", year == 2020)
fall <- fall %>% mutate(validids = paste0(season,  ".", year)) %>%
  group_by(validids) %>%
  filter(n_distinct(AID) >= 8) %>%
  ungroup()
pairwise_fall <- shapeshiftr::iidist(fall, nest_by = c("yday", "season", "year"), idcol = "AID")
head(pairwise_fall)
#----Compute CVs per day----
cvs_yday <- shapeshiftr::cvmetrics_sf(
  pairwise_fall,
  distcol = "iidist",
  idcol   = "ID1",
  grp_by  = c("yday", "season", "year")
) # cv of inter-individual distances for every day in the fall of 2020

#saveRDS(cvs_yday, "CVSupplementalMaterial/casestudies/cvs_yday.rds")

long_cvsyday <- cvs_yday %>% pivot_longer(cols = c(cvpop,cvind, ratio), names_to = "Type", values_to = "CV") %>%
  mutate(Type = factor(Type, levels = c("cvind", "cvpop", "ratio")),
         yday = as.numeric(yday),
         year = as.numeric(year)
         )
mathematicapal <- c("#8888d4","#229e0c", "black")

cvlineplot <- ggplot(long_cvsyday) +
  geom_line(aes(yday, CV, color = Type)) +
  scale_color_manual(values = mathematicapal) +
  theme_bw()+
  guides(color = "none")
cvlineplot

#---- Make KDE insets ----
# can't share , but this is how the KDEs were created:
cents_sub <- daily %>% filter(season == "fall", year == 2020,
                              yday %in% c(250, 280, 295, 310, 325)) %>%
  mutate(id = paste0(yday, ".", year), ID = rep(seq(1, 38), each = 5)) %>%
  dplyr::select(yday, ID, year)

# Convert the kable table into a grob

vertices <- cents_sub %>% ungroup() %>% st_cast("POINT") #3581
coords <- st_coordinates(vertices)
dataframe_vert <- as.data.frame(st_drop_geometry(vertices))

sp_df <- SpatialPointsDataFrame(
  coords = coords,
  data = dataframe_vert,
  proj4string = CRS(st_crs(cents)$wkt)  # or could do proj4string = CRS("+init=epsg:3581")
)

sp_df <- sp_df[,"id"]

k_temp <- kernelUD(sp_df, h = 15000,
                   grid = 1000, extent = 6)
kd <- getverticeshr(k_temp, percent = 95)
kde <- st_set_crs(st_as_sf(kd), 3581)
isopleth_sf <- kde %>%
  separate(id, sep = "\\.", into = c("yday", "year"))
isopleth_sf <- isopleth_sf %>%
  mutate(across(c(yday, year), as.numeric)) %>% arrange(yday, year)
isopleth_sf_sub <- isopleth_sf %>% filter(year == 2020,yday %in% c(250, 280, 295, 310, 325)) %>% group_by(yday) %>% mutate(area =st_area(geometry),area_km2 = as.numeric(area) / 1e6)

isosubs <- isopleth_sf_sub %>% st_cast("POLYGON")

# Function to calculate density
get_density <- function(x, y, ...) {
  dens <- MASS::kde2d(x, y, ...)
  ix <- findInterval(x, dens$x)
  iy <- findInterval(y, dens$y)
  ii <- cbind(ix, iy)
  return(dens$z[ii])
}
#MASS::kde2d(x, y, ...): This calculates the 2D kernel density estimate for the points given by x and y coordinates. It returns a list containing:
#x: A vector of x-coordinates of the grid points where the density is estimated.
#y: A vector of y-coordinates of the grid points.
#z: A matrix where each entry is the estimated density at the corresponding grid point.

#findInterval(x, dens$x) and findInterval(y, dens$y): These functions determine which interval of the grid each data point falls into. The intervals are defined by the dens$x and dens$y vectors returned by kde2d(). findInterval() returns the index of the interval that each point falls into.

#cbind(ix, iy): This combines the indices for the x and y intervals into a matrix ii, where each row corresponds to a data point and contains its x and y interval indices.

#dens$z[ii]: This extracts the density values from the dens$z matrix at the positions corresponding to each data point's interval indices.

# Extract coordinates from jittered points
cents_sub$X <- st_coordinates(cents_sub)[,1]
cents_sub$Y <- st_coordinates(cents_sub)[,2]
cents_sub$density <- get_density(cents_sub$X, cents_sub$Y)
jitter <- cents%>% filter(year == 2020,yday %in% c(250, 280, 295, 310, 325)) %>% st_jitter(geometry, amount = 6000)
jitter$density <- cents_sub$density

cents%>% filter(year == 2020,yday %in% c(250, 280, 295, 310, 325)) %>% group_by(yday)%>% summarise(n = n_distinct(AID)) # 38 individuals


p1 <- ggplot() +
  geom_sf(data = isopleth_sf_sub %>% filter(year == 2020, yday %in% c(250, 280, 295, 310, 325)), fill = NA) +
  geom_sf(data = jitter, aes(color = density)) +
  #scale_color_gradientn(colors = c("#DDAA33","#BB5566","#004488" ))+
  scale_color_viridis_c(option = "viridis", direction = -1, name = "Density", labels = NULL) +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.ticks = element_blank())+
  facet_wrap(~yday)
p1


#overlay:

# Compute global bounding box
st_crs(isopleth_sf_sub)
global_bbox <- st_bbox(isopleth_sf_sub)  # Adjust if using multiple days
global_xlim <- c(global_bbox["xmin"], global_bbox["xmax"])
global_ylim <- c(global_bbox["ymin"], global_bbox["ymax"])
st_area(st_as_sfc(global_bbox))/1e6
st_transform(global_bbox, 4326)

# take out isopleths
make_day_grob <- function(day_points, day_polygons) {

  # day_data: the subset of your points for this yday
  # polygons_sf: the subset of polygons (isopleths) for this yday

  # Minimal plot focusing on the shapes/points, ignoring lat/lon
  p_day <- ggplot() +
    #geom_sf(data = day_polygons, fill = NA, size = 1) +
    geom_sf(data = day_points, aes(color = density)) +
    scale_color_viridis_c(option = "viridis", direction = -1) +
    coord_sf(xlim = global_xlim, ylim = global_ylim) +
    theme_void() + # remove axes, etc.
    theme(
      legend.position = "none",
      plot.background = element_rect(fill = "white",
                                     color = "black", linewidth = 0.5),
      panel.border = element_blank()
    )
  grob_day <- ggplotGrob(p_day)
  return(grob_day)
}

days_to_annotate <- c(250, 280, 295, 310, 325)
box_width <- 20  # Adjust this to control horizontal size
box_height <- 0.5 # Adjust this to control vertical size
grbs <- data.frame(
  yday = c(250, 280, 295, 310, 325),
  xmin = c(238 - box_width/2, 255 - box_width/2, 295 - box_width/2,
           330 - box_width/2, 345 - box_width/2),
  xmax = c(238 + box_width/2, 255 + box_width/2, 295 + box_width/2,
           330 + box_width/2, 345 + box_width/2),
  ymin = c(.75 - box_height/2, 1.25 - box_height/2,
           1.88 - box_height/2, 1.25 - box_height/2,
           .75 - box_height/2),
  ymax = c(.75 + box_height/2, 1.25 + box_height/2,
           1.89 + box_height/2, 1.25 + box_height/2,
           .75 + box_height/2))

centers <- data.frame(
  x_center = c(238, 255, 295, 330, 345),
  y_center = c(.75, 1.25, 1.88, 1.25, .75),
  x_end = c(250, 280, 295, 310, 325),
  y_end = c(long_cvsyday %>% filter(year == 2020,yday == 250, Type == "cvpop") %>% pull(CV),
            long_cvsyday %>% filter(year == 2020,yday == 280, Type == "cvpop") %>% pull(CV),
            long_cvsyday %>% filter(year == 2020,yday == 295, Type == "cvpop") %>% pull(CV),
            long_cvsyday %>% filter(year == 2020,yday == 310, Type == "cvpop") %>% pull(CV),
            long_cvsyday %>% filter(year == 2020,yday == 325, Type == "cvpop") %>% pull(CV)
  ))

p2 <- ggplot(long_cvsyday) +
  geom_line(aes(yday, CV, color = Type)) +
  scale_color_manual(values = mathematicapal) +
  theme_bw()+
  guides(color = "none")+
  scale_x_continuous(limits = c(230, 355))+
  ylim(0, 2.2)+
  labs(x = "Year-day")

for (i in seq_along(days_to_annotate)) {
  row <- grbs[i, ]
  day_i <- days_to_annotate[i]
  # 1) Subset your polygons & points for this day
  day_points <- jitter %>% filter(yday == day_i)
  #day_polygons <- isopleth_sf_sub %>% filter(yday == day_i)

  # 2) Create the grob
  grob_i <- make_day_grob(day_points, day_polygons)
  p2 <- p2 +
    geom_segment(
      data = centers %>% filter(x_end == day_i),
      aes(x = x_center, xend = x_end,
          y = y_center, yend = y_end),
      linetype = "dashed",
      inherit.aes = FALSE,
      color = "steelblue",
      position = "identity"
    ) +
    annotation_custom(grob_i,
                      xmin = row$xmin, xmax = row$xmax,
                      ymin = row$ymin, ymax = row$ymax)
}



temp <- ggplot(long_cvsyday) +
  geom_smooth(aes(yday, CV, color = Type)) +
  scale_color_manual(values = mathematicapal, labels = c(bquote(CV[indiv]), bquote(CV[group]), "Ratio")) +
  theme_bw()+
  facet_wrap(~year)+
  scale_x_continuous(limits = c(230, 355))+
  ylim(0, 2.2)+
  theme(legend.position = "bottom",
        legend.title = element_blank()
  )

legend = cowplot::get_plot_component(p1, 'guide-box-bottom', return_all = TRUE)
l2 <- cowplot::get_plot_component(temp, 'guide-box-bottom', return_all = TRUE)
legends <- cowplot::plot_grid(legend, l2, nrow = 1)
cowplot::plot_grid(p2, legends, ncol = 1, rel_heights = c(1, .1))

