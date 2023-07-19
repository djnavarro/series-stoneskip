
library(Rcpp)
library(tidyverse)
library(ambient)
library(jasmines)
library(paletteer)
library(here)

sourceCpp(here("source", "stepping_stone.cpp")) 
output <- here("images", "vstone_14.png")


# generate stepping stone background --------------------------------------

cat("generating image...\n")

# parameters
seed_ss <- 10
shades <- 1000
scf <- 1
grains_high <- round(525 * scf)
grains_wide <- round(740 * scf)

palette_name <- "scico::oslo"
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
  fractal = ridged,
  octaves = 1, 
  frequency = 1,
  value = "distance",
  seed = seed_ss, 
  x = raster$x,
  y = raster$y
) %>%
  rank()

# convert base raster image to integer index
raster$base <- as.integer(ceiling(normalise(raster$base) * shades))

# convert to matrix and run stepping stone automaton
ss <- matrix(raster$base, grains_wide, grains_high, byrow = TRUE)
ss <- t(timestep(ss, 200))

# read colours off the ss matrix 
raster$shade <- palette[ss]



# jasmines call -----------------------------------------------------------

jas <- use_seed(120) %>%
  entity_heart(grain = 5000, size = 5) %>%
  unfold_tempest(
    iterations = 100,
    scale = .002
  ) %>%
  mutate(
    order = id,
    x = (1-ar)/2 + normalise(x, to = c(0, ar)),
    y = normalise(y, to = c(0, ar))
  )




# render the image --------------------------------------------------------

cat("rendering image...\n")

pic <- ggplot(
  data = raster,
  mapping = aes(x, y, fill = shade)
) + 
  
  # the raster object forms the background
  geom_raster() + 
  
  # the jasmines object
  geom_ppoint(
    data = jas,
    mapping = aes(
      x = x,
      y = y,
      colour = order,
      alpha = .5 * exp(-normalise(time, to = c(0, 5)))
    ),
    size = .2,
    stroke = NA,
    show.legend = FALSE,
    inherit.aes = FALSE
  ) +
  

  # bunch of settings...
  scale_color_paletteer_c(palette_name) + 
  scale_alpha_identity() + 
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
