
library(Rcpp)
library(tidyverse)
library(ambient)
library(flametree)
library(voronoise)
library(paletteer)
library(here)

sourceCpp(here("source", "stepping_stone.cpp")) 
output <- here("images", "vstone_05.png")


# generate stepping stone background --------------------------------------

cat("generating image...\n")

# parameters
seed_ss <- 35
shades <- 1000
scf <- 1
grains_high <- round(525 * scf)
grains_wide <- round(740 * scf)

palette <- paletteer_c(
  palette = "scico::berlin", 
  n = shades
)

# seed for RNG
set.seed(seed_ss)

# create long grid for the raster with appropriate aspect ratio
ar <- grains_high / grains_wide
raster <- long_grid(
  x = seq(0, 1,  length.out = grains_wide), 
  y = seq(0, ar, length.out = grains_high)
)

# initialise raster using worley noise
raster$base <- gen_worley(
  frequency = .001,
  value = "distance2",
  seed = seed_ss, 
  x = raster$x,
  y = raster$y
) 

# convert base raster image to integer index
raster$base <- as.integer(ceiling(normalise(raster$base) * shades))

# convert to matrix and run stepping stone automaton
ss <- matrix(raster$base, grains_wide, grains_high)
ss <- t(timestep(ss, 200))

# read colours off the ss matrix 
raster$shade <- palette[ss]




# generate flametree ------------------------------------------------------

seed_ft <- 15
set.seed(seed_ft)

# the "flametree" itself
ftree <- flametree_grow(
  time = 9,
  seed = seed_ft,
  angle = c(-20, 15, 25),
  scale = c(.5, .85, .95)
)

# compute aspect ratio of generated tree
ar2 <- with(ftree, (max(coord_y) - min(coord_y))/(max(coord_x) - min(coord_x)))

# scale the tree image to fit the postcard
ftree1 <- ftree %>%
  mutate(
    coord_x = normalise(coord_x, to = range(raster$x)),
    coord_y = normalise(coord_y, to = range(raster$y))
  ) %>%
  mutate(
    coord_x = coord_x * min(1, ar/ar2), 
    coord_y = coord_y * min(1, ar2/ar)
  ) %>% 
  mutate(
    coord_x = .5 + coord_x * .8,
    coord_y = coord_y * .8
  )

# "leaf" coordinates are at terminal locations (id_step = 2) 
# on the terminal branches (id_leaf == TRUE) in the tree
vleaf1 <- ftree1 %>% 
  filter(id_leaf == TRUE, id_step == 2) %>%
  sample_frac(1)





# generate flametree ------------------------------------------------------

seed_ft <- 1
set.seed(seed_ft)

# the "flametree" itself
ftree <- flametree_grow(
  time = 11,
  seed = seed_ft,
  angle = c(-20, 15, 25),
  scale = c(.5, .85, .95)
)

# compute aspect ratio of generated tree
ar2 <- with(ftree, (max(coord_y) - min(coord_y))/(max(coord_x) - min(coord_x)))

# scale the tree image to fit the postcard
ftree2 <- ftree %>%
  mutate(
    coord_x = normalise(coord_x, to = range(raster$x)),
    coord_y = normalise(coord_y, to = range(raster$y))
  ) %>%
  mutate(
    coord_x = coord_x * min(1, ar/ar2), 
    coord_y = coord_y * min(1, ar2/ar)
  ) %>% 
  mutate(
    coord_x = .3 + coord_x * .8,
    coord_y = coord_y * .8
  )

# "leaf" coordinates are at terminal locations (id_step = 2) 
# on the terminal branches (id_leaf == TRUE) in the tree
vleaf2 <- ftree2 %>% 
  filter(id_leaf == TRUE, id_step == 2) %>%
  sample_frac(1)


# render the image --------------------------------------------------------

cat("rendering image...\n")

pic <- ggplot(
  data = raster,
  mapping = aes(x, y, fill = shade)
) + 
  
  # the raster object forms the background
  geom_raster() + 
  
  # # tree trunk is drawn using geom_bezier from the 
  # # ggforce package (loaded by voronoise)
  geom_bezier(
    data = ftree1,
    mapping = aes(
      x = coord_x,
      y = coord_y,
      group = id_path,
      size = .2 + seg_wid * 3
    ),
    lineend = "round",
    colour = "white",
    alpha = 1,
    show.legend = FALSE,
    inherit.aes = FALSE
  ) +

  # # tree trunk is drawn using geom_bezier from the 
  # # ggforce package (loaded by voronoise)
  geom_bezier(
    data = ftree2,
    mapping = aes(
      x = coord_x,
      y = coord_y,
      group = id_path,
      size = .2 + seg_wid * 3
    ),
    lineend = "round",
    colour = "white",
    alpha = 1,
    show.legend = FALSE,
    inherit.aes = FALSE
  ) +
  
    
  # leaves generated using the voronoise package (in this instance
  # it's more or less identical to geom_voronoi_tile)
  # geom_voronoise(
  #   data = vleaf,
  #   mapping = aes(
  #     x = coord_x,
  #     y = coord_y
  #   ),
  #   expand = -.0005,
  #   radius = .0001,
  #   max.radius = .03,
  #   size = 2,
  #   alpha = 1,
  #   fill = "white",
  #   perturb = function(data) {
  #     f <- perturb_float(angle = 180);
  #     data <- f(data)
  #     return(data %>% 
  #              filter(x > 0, x < .8, y > 0, y < .8) %>%
  #              mutate(x = x - .1))
  #   },
  #   show.legend = FALSE,
  #   inherit.aes = FALSE
  # ) + 
  # 

geom_point(
  data = vleaf1,
  mapping = aes(
    x = coord_x,
    y = coord_y,
    color = seg_col
  ),
  size = .5,
  alpha = 1,
  show.legend = FALSE,
  inherit.aes = FALSE
) + 

  geom_point(
    data = vleaf2,
    mapping = aes(
      x = coord_x,
      y = coord_y,
      color = seg_col
    ),
    size = .5,
    alpha = 1,
    show.legend = FALSE,
    inherit.aes = FALSE
  ) + 
  
  # bunch of settings...
  scale_color_paletteer_c("viridis::magma") + 
  scale_fill_identity() + 
  scale_size_identity() + 
  coord_equal() + 
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_void() + 
  NULL

# export image
ggsave(
  filename = output,
  plot = pic,
  width = grains_wide / 150,
  height = grains_high / 150,
  dpi = 1200
)
