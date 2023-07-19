
library(Rcpp)
library(tidyverse)
library(ambient)
library(scrawl)
library(paletteer)
library(here)
library(scico)

sourceCpp(here("source", "stepping_stone.cpp")) 
output <- here("images", "vstone_18.png")


# generate stepping stone background --------------------------------------

cat("generating image...\n")

# parameters
seed_ss <- 456
shades <- 1000
grains_high <- 525
grains_wide <- 740

palette <- paletteer_c(
  palette = "viridis::viridis", 
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
  noise = gen_simplex,
  fractal = fbm,
  octaves = 5, 
  frequency = 5,
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




# generate scrawl ---------------------------------------------------------

dat <- scrawl_build(
  seed = 180, 
  n_paths = 5000, 
  n_steps = 500, 
  sz_step = 100,  
  sz_slip = 3
) %>%
  mutate(
    x = x - min(x),
    y = y - min(y),
    z = max(c(x,y/ar)),
    x = x / z,
    y = y / z,
    x = x + (1-max(x))/2
  )

# render the image --------------------------------------------------------

cat("rendering image...\n")

pic <- ggplot(
  data = raster,
  mapping = aes(x, y, fill = shade)
) + 
  
  # the raster object forms the background
  geom_raster() + 
  
  # overlay the scrawl
  geom_path(
    data = dat, 
    mapping = aes(x, y, group = path_id),
    color = "white",
    inherit.aes = FALSE,
    show.legend = FALSE,
    size = .2,
    alpha = .1
  ) +
  
  # bunch of settings...
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
