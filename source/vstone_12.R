
library(Rcpp)
library(tidyverse)
library(ambient)
library(flametree)
library(voronoise)
library(paletteer)
library(here)

sourceCpp(here("source", "stepping_stone.cpp")) 
output <- here("images", "vstone_12.png")


# generate stepping stone background --------------------------------------

cat("generating image...\n")

# parameters
seed_ss <- 2
shades <- 1000
scf <- 1
grains_high <- round(525 * scf)
grains_wide <- round(740 * scf)

palette_name <- "harrypotter::hufflepuff"
palette <- paletteer_c(
  palette = palette_name, 
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
raster$base <- fracture(
  noise = gen_worley,
  fractal = billow,
  octaves = 2, 
  frequency = 2,
  value = "distance2",
  seed = seed_ss, 
  x = raster$x,
  y = raster$y
) 

# convert base raster image to integer index
raster$base <- as.integer(ceiling(normalise(raster$base) * shades))

# convert to matrix and run stepping stone automaton
ss <- matrix(raster$base, grains_wide, grains_high, byrow = TRUE)
ss <- t(timestep(ss, 200))

# read colours off the ss matrix 
raster$shade <- palette[ss]



# generate flametree ------------------------------------------------------

seed_ft <- 3
set.seed(seed_ft)

# the "flametree" itself
ftree <- flametree_grow(
  time = 12,
  seed = seed_ft,
  angle = c(-10, -5, 5, 15, 25),
  scale = c(.5, .9, .975)
)

# compute aspect ratio of generated tree
ar2 <- with(ftree, (max(coord_y) - min(coord_y))/(max(coord_x) - min(coord_x)))

# scale the tree image to fit the postcard
ftree <- ftree %>%
  mutate(
    coord_x = normalise(coord_x, to = range(raster$x)),
    coord_y = normalise(coord_y, to = range(raster$y))
  ) %>%
  mutate(
    coord_x = coord_x * min(1, ar/ar2), 
    coord_y = coord_y * min(1, ar2/ar)
  ) %>% 
  mutate(
    coord_x = .45 + coord_x * .9,
    coord_y = coord_y * .9
  )

# "leaf" coordinates are at terminal locations (id_step = 2) 
# on the terminal branches (id_leaf == TRUE) in the tree
vleaf <- ftree %>% 
  filter(id_leaf == TRUE, id_step == 2) %>%
  sample_frac(.02)


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
    data = ftree,
    mapping = aes(
      x = coord_x,
      y = coord_y,
      color = seg_col,
      group = id_path,
      size = .2 + seg_wid * 3
    ),
    lineend = "round",
    #colour = palette[shades/2],
    alpha = 1,
    show.legend = FALSE,
    inherit.aes = FALSE
  ) +
  
  # #leaves generated using the voronoise package 
  # geom_voronoise(
  #   data = vleaf %>% mutate(
  #     leaf_shade = sample(palette, n(), replace = TRUE)),
  #   mapping = aes(
  #     x = coord_x,
  #     y = coord_y,
  #     fill = leaf_shade,
  #     group = 1
  #   ),
  #   expand = -.0001,
  #   radius = 0,
  #   max.radius = .01,
  #   size = .2,
  #   alpha = 1,
  #   perturb = perturb_float(angles = -90, noise = c(20, 1)),
  #   show.legend = FALSE,
  #   inherit.aes = FALSE
  # ) +


  # bunch of settings...
  scale_color_paletteer_c(palette_name, limits = c(-50, 25)) + 
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
