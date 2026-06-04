# Script containing various tips and tricks in terra and related packages
# Author: Dawn Nekorchuk
# Created: 2026-06-02
# Last Modified: 2026-06-04

if (!require("pacman")) {
  install.packages("pacman")
}
pacman::p_load(
  terra,
  sf,
  #better ggplot with terra
  tidyterra,
  #weird map demos
  ggnewscale,
  #general data, plotting
  tidyverse,
  stringr,
  viridis
)

### Notes ----------------------------------------------------------------------

# Good reference:
# https://rspatial.org/

### Not NA ---------------------------------------------------------------------

#terra::not.na()
#avoids !is.na()

### Data -----------------------------------------------------------------------

f <- system.file("ex/elev.tif", package = "terra")
r <- terra::rast(f)

plot(r)

# Create a bunch of rasters with single values for later use
# All do similar things, just demonstrating multiple ways of accomplishing this
r1 <- terra::classify(r, cbind(-Inf, Inf, 1))
r2 <- terra::ifel(terra::not.na(r), 2, NA)
r3 <- terra::init(r, 3) # all cells, whether NA or not
r3 <- terra::crop(r3, r, mask = TRUE) # only cells where data existed in r

### ifel() ---------------------------------------------------------------------

# if-else logic for spatial data

ie1 <- terra::ifel(r > 300, -r, r) # same raster
ie2 <- terra::ifel(r > 400, r1, r2) # other rasters - extents must match!

# Can nest/chain, but gets slow on large rasters quickly
# If you are working with large rasters, probably best to do one step at a time
ie3 <- terra::ifel(
  r > 400,
  700,
  # values not greater than 400
  terra::ifel(
    r > 300, # but is greater than 300
    r3,
    r # if no condition applies, keep r
  )
)
# logic gets messier to read, so use sparingly

# can also test for NAs
plot(ifel(is.na(r), 999, r))

## ifel()
# Version matters / various major bugs exist is earlier versions !
packageVersion("terra")
sessionInfo()

# e.g. before v 1.9.7 ifel() had a bug that sometimes allowed NAs to pass conditions
# https://github.com/rspatial/terra/issues/2058

# Note: ifel() seems to be thought of as a convenience function, as
#  you can rewrite all ifel()s with some combination of classify, mask, cover,
#      and/or other functions.

### Cropping & Masking ---------------------------------------------------------

# quick data set up, large raster
large_r <- terra::extend(r, c(1000, 1000))
plot(large_r)
values(large_r) <- 1:terra::ncell(large_r)
plot(large_r, col = map.pal("magma", 100))

# crop() cuts one raster by EXTENT of a second raster
#  if you also want to limit to just the pixels where the second extent has data,
#    you can add masking by crop(..., mask = TRUE)

# HOWEVER, if you are working with one very large raster being cropped by a
#  smaller raster, sometimes if you try to do this in one step, you end up with
#  the MASK being returned instead of the cropped raster values.

# The 'safe' approach is to do it in two steps
# Can have same name, only keeping separate to graph below
c1_step1 <- terra::crop(large_r, r)
c1 <- terra::crop(c1_step1, r, mask = TRUE)

(p_cm <- ggplot2::ggplot() +
  tidyterra::geom_spatraster(data = large_r) +
  viridis::scale_fill_viridis(option = "magma", alpha = 0.5) +
  ggplot2::coord_sf(expand = FALSE))

p_cm +
  ggnewscale::new_scale_fill() +
  tidyterra::geom_spatraster(data = c1_step1) +
  viridis::scale_fill_viridis(
    option = "magma",
    na.value = NA,
    limits = c(1, ncell(large_r))
  ) +
  ggplot2::coord_sf(expand = FALSE)

p_cm +
  ggnewscale::new_scale_fill() +
  tidyterra::geom_spatraster(data = c1) +
  viridis::scale_fill_viridis(
    option = "magma",
    na.value = NA,
    limits = c(1, terra::ncell(large_r))
  ) +
  ggplot2::coord_sf(expand = FALSE)

plot(c1, col = map.pal("magma", 100))
plot(r)

### Mosaic options -------------------------------------------------------------

# data prep, make some tiles, WITHOUT buffer
folder_tiles <- file.path("data", "tiles")
dir.create(folder_tiles, recursive = TRUE, showWarnings = FALSE)
# 500 row/cols per tile
terra::makeTiles(
  large_r,
  500,
  filename = file.path(folder_tiles, "tile_.tif"),
  overwrite = TRUE
)

## Merge vs mosaic

# To create a single raster from multiple rasters that cover different
#  geographic areas, we can either use merge() or mosaic().

# merge() is much faster than mosaic(), but can only be used in certain situations
# 1. If your tiles/sub rasters do NOT overlap
# 2. If they do overlap, but you don't need to calculate new values
#    (i.e. you just want the first non-NA value)

# Merge has multiple algorithms available, I use algo = 2 because
#  it does NOT resample.

# Get the tile raster files
(files_to_merge <- list.files(
  path = folder_tiles,
  # files that end with ".tif". Useful to skip over any .xml files from like QGIS
  pattern = "\\.tif$",
  full.names = TRUE
))

#examples to see
t1 <- terra::rast(grep(files_to_merge, pattern = "tile_1\\.", value = TRUE))
t2 <- terra::rast(grep(files_to_merge, pattern = "tile_2\\.", value = TRUE))
t5 <- terra::rast(grep(files_to_merge, pattern = "tile_5\\.", value = TRUE))
(p_tiles <- ggplot2::ggplot() +
  tidyterra::geom_spatraster(data = t1) +
  viridis::scale_fill_viridis(option = "mako", na.value = NA) +
  ggnewscale::new_scale_fill() +
  tidyterra::geom_spatraster(data = t2) +
  viridis::scale_fill_viridis(option = "inferno", na.value = NA, ) +
  ggnewscale::new_scale_fill() +
  tidyterra::geom_spatraster(data = t5) +
  viridis::scale_fill_viridis(option = "turbo", na.value = NA, ) +
  ggplot2::coord_sf(expand = FALSE))
p_tiles +
  ggnewscale::new_scale_fill() +
  tidyterra::geom_spatraster(data = large_r) +
  viridis::scale_fill_viridis(option = "magma", alpha = 0.25) +
  ggplot2::coord_sf(expand = FALSE)

# Sprc is useful here (but annoyingly limited)
# spatial raster collection does NOT need to have the same extent, origin, etc.
#  like is necessary when you stack rasters
tile_sprc <- terra::sprc(files_to_merge)
tile_sprc

# Warning: sprcs are difficult/impossible to manipulate, edit components or add to.
#  read in the files as you want them to be mosaicked.
tile_sprc[1]
plot(tile_sprc[1])
tile_sprc[1] <- tile_sprc[4] # doesn't work!

# mosaic via merge algo 2
# DEFAULT algo is 1 which resamples! Be careful
# first is true by default, use the first non NA value, otherwise last value
whole_again <- terra::merge(tile_sprc, algo = 2, first = TRUE)
plot(whole_again)

# Otherwise, use mosaic with appropriate function to handle the overlap values
#  e.g. fun = "mean"

### selectRange() --------------------------------------------------------------

#make some new data real quick
r11 <- terra::ifel(terra::not.na(r), 11, NA)
r22 <- terra::ifel(terra::not.na(r), 22, NA)
r33 <- terra::ifel(terra::not.na(r), 33, NA)

# Say we have a complicated set of logic to combine data from multiple sources
#  If we pre-compute the logic and create a essentially a look-up table as a raster
#   we can use terra::selectRange() to pick pixels across multiple sources in
#   one step, rather than successive ifel()/cover()/mask() calls

# For example, say we have different result raster from three different treatments,
#  as represented by r11, r22, and r33 above.
# Pretend for a moment that treatment selection was based on elevation:
#  First, set up a quick reclass matrix
elev_rcl <- tibble::tribble(
  ~to , ~from , ~becomes ,
    0 ,   300 ,        1 ,
  300 ,   400 ,        2 ,
  400 , Inf   ,        3
) %>%
  as.matrix()
r_key <- terra::classify(r, rcl = elev_rcl)
plot(r_key)

# And 1 refers to treatment 11, 2 to treatment 22, etc.

# Then stack IN ORDER of key order and use the key raster as a guide:
my_stack <- c(r11, r22, r33)
stitched <- terra::selectRange(my_stack, r_key)
plot(stitched)

### Crosstabs() and 'manual' crosstabs -----------------------------------------

# Sometimes we want to know the frequency of values
#  in pixels across multiple rasters
# (Makes most sense for categorical, but sometimes limited/rounded continuous data)

# terra::freq() gives us the count for a single layer
# terra::crosstab() gives us the count across multiple layers
#   Warning - more than two layers can get very slow if rasters are large...

# quick data set up, with random data
rr_a <- r
values(rr_a) <- sample(1:4, terra::ncell(r), TRUE) #random integers between 1 and 4
rr_a <- terra::crop(rr_a, r, mask = TRUE) # only cells where data existed in r
names(rr_a) <- "rr_a"
rr_b <- r
values(rr_b) <- sample(1:9, terra::ncell(r), TRUE) #random integers between 1 and 9
names(rr_b) <- "rr_b"
# Note: rr_b has data in full extent, so more than just what is present in r
rr_c <- r
values(rr_c) <- sample(1:50, terra::ncell(r), TRUE) #random integers between 1 and 50
rr_c <- terra::crop(rr_c, r, mask = TRUE) # only cells where data existed in r
names(rr_c) <- "rr_c"


# frequency (pixel count) of one raster
terra::freq(rr_a) %>% tibble::as_tibble()
# individual frequencies of each layer
terra::freq(c(rr_a, rr_b)) %>% tibble::as_tibble()

# frequency of combination of values
(ct_ab <- terra::crosstab(c(rr_a, rr_b), long = TRUE)) %>% tibble::as_tibble()
# by default, drops where EITHER has NAs, so if want, must specify
#  warning: gets slow if you have a large raster
(ct_ab2 <- terra::crosstab(c(rr_a, rr_b), long = TRUE, useNA = TRUE) %>%
  tibble::as_tibble())
ct_ab2 %>% dplyr::filter(is.na(rr_a))

## 'Manual' crosstab
# By the time you are adding three or four layers if they are moderately large,
#  crosstab() starts to slow significantly
# You can do a 'manual' crosstab with doing some clever encoding with
#  raster math, freq(), and then decoding.
# You must:
#  - handle NAs by filling in with a different value if you want to preserve them
#  - take note of how many digits is possible in each layer
#  - stay organized for the order of encoding so you can properly decode

# Let's say I want rr_a (1 digit), rr_b (1 digit), rr_c (2 digits)
#  and I want to get the results for ALL pixels, even NAs in rr_a or rr_c
# Prep rr_a and stitched by filling NAs as 0 (or other unique value)
rr_a0 <- terra::ifel(is.na(rr_a), 0, rr_a)
plot(rr_a0)
rr_c99 <- terra::ifel(is.na(rr_c), 99, rr_c)
plot(rr_c99)

# Encoding
# fmt:skip
encoded <-
  rr_a0        * 1000 + 
  rr_b         *  100 + 
  rr_c99       *    1
# rr_a and rr_b are given 1 digit of space, and stitched 2!
plot(encoded)
terra::global(encoded, fun = "range")
# note that there is a different number of digits,
#  depending on value of rr_a0 (0 = NA)

# Get the encoded frequencies
(freq_e <- terra::freq(encoded) %>% tibble::as_tibble())

# Decoding
(ct_m <- freq_e %>%
  dplyr::select(-layer) %>%
  dplyr::mutate(
    # rr_a0 (left most digit may have been 0 and thus looking missing when as a number
    # so pad to full 4 possible encoded digits, padding a 0 on the left as needed)
    encoded_value = stringr::str_pad(value, width = 4, side = "left", pad = 0)
  ) %>%
  tidyr::separate_wider_position(
    cols = encoded_value,
    #number of digits of each, in same order as encoding
    widths = c("a" = 1, "b" = 1, "c" = 2),
    cols_remove = FALSE
  ) %>%
  dplyr::select(count, value, encoded_value, tidyr::everything()))

# convert back to number and/or decode the NAs if wanted
(ct_m <- ct_m %>%
  dplyr::mutate(across(c(a, b, c), ~ as.numeric(.x))) %>%
  dplyr::mutate(
    a = dplyr::if_else(a == 0, NA, a),
    c = dplyr::if_else(c == 99, NA, c)
  ))

# or use look-up tables if you have a categorical raster ...

### Categorical rasters --------------------------------------------------------

# data set up, make a categorical raster
rr_a
a_lookup <- tibble::tribble(
  ~a_value , ~a_desc  ,
         1 , "desc_1" ,
         2 , "desc_2" ,
         3 , "desc_3" ,
         4 , "desc_4"
)
levels(rr_a)
levels(rr_a) <- a_lookup
levels(rr_a)
rr_a
plot(rr_a)

# Note: you can have multiple columns, and therefore multiple potential 'active'
#  categories being displayed/worked with. Be careful! See activeCat()

# So back to our crosstab categories
ct_m %>%
  dplyr::left_join(
    terra::levels(rr_a)[[1]],
    by = dplyr::join_by("a" == "a_value")
  )

# Saving out categorical rasters ....
# It'll put the categories into an .aux.xml which R will read again
# It does NOT create a .vat.dbf
# I have not yet found a way to get QGIS to play with the .aux.xml or
#  forced R to manually create a vat.dbf
#  (which the author of terra discourages for some reason)

# Use foreign package to create .vat.dbf! -Bryan
# color table column signed integers

### writeRaster(), large data, compression, NA  --------------------------------

folder_write <- file.path("data", "writing_demo")
dir.create(folder_write)

# Remember to check names/varnames, you probably want to change it
#  if you've been doing calculations
stitched
names(stitched)
names(stitched) <- "stitched_results"
stitched

# terra has some decent defaults, but sometimes you need to get in the weeds...
# writeRaster by default will save as datatype FLT4S
# https://rdrr.io/cran/terra/man/datatype.html

# FLT4S : -2,147,483,647 to 2,147,483,647

# This is usually sufficient if not overkill
# E.g. If you have a lot of spatially large but integer rasters, you may want to
#  look at saving as INT2U or INT2S for space reasons

# If you need to play with very very very large numbers there's a few things
#  you need to do
# 1. At the top of the script:
terra::terraOptions(datatype = "FLT8S")
#   this tells R to use 64-bit numbers internally, so it doesn't muck with and
#    round numbers during any processing / calculations
#   your limit here is ~15-16 digits long
# 2. Specify data type during saving
terra::writeRaster(
  large_r,
  file.path(folder_write, "large_r_flt8S.tif"),
  datatype = "FLT8S",
  overwrite = TRUE
)

#  terra uses the compression algorithm "LZW" by default
#  I have found better (smaller tifs) using DEFLATE instead for the rasters
#   I generally work with. YMMV.
terra::writeRaster(
  large_r,
  file.path(folder_write, "large_r_flt8S_deflate.tif"),
  datatype = "FLT8S",
  gdal = c("COMPRESS=DEFLATE"),
  overwrite = TRUE
)

file.size(file.path(folder_write, "large_r_flt8S.tif")) %>%
  rlang::as_bytes()
file.size(file.path(folder_write, "large_r_flt8S_deflate.tif")) %>%
  rlang::as_bytes()

# Special NA values & metadata
# "For all integer and byte types the lowest (signed) or
#     highest (unsigned) value is used to store NA.
#     For float types NaN is used (following the IEEE 754 standard)."

# But what if you want -9999 to be the NA value,
#  and you want the file metadata including statistics to handle this?

# Set up
# re-using stitched object since it has NA values.
# just saving out plain for comparison in a minute
terra::writeRaster(
  stitched,
  file.path(folder_write, "stitched.tif"),
  gdal = c("COMPRESS=DEFLATE"),
  overwrite = TRUE,
  statistics = 3
)
terra::describe(file.path(folder_write, "stitched.tif"))

# Manually set a special NA value of -9999
s_9999 <- terra::ifel(is.na(stitched), -9999, stitched)

# If write as is, the metadata statistics are going to include -9999....
# and "NoData Value" will still be Nan
terra::writeRaster(
  s_9999,
  file.path(folder_write, "stitched_9999.tif"),
  gdal = c("COMPRESS=DEFLATE"),
  overwrite = TRUE
)
terra::describe(file.path(folder_write, "stitched_9999.tif"))

# Use NAflag to set the NA value, and can also force recomputation of stats
terra::writeRaster(
  s_9999,
  file.path(folder_write, "stitched_9999_flag.tif"),
  gdal = c("COMPRESS=DEFLATE"),
  #set NA value
  NAflag = -9999,
  statistics = 3,
  overwrite = TRUE
)
terra::describe(file.path(folder_write, "stitched_9999_flag.tif"))

# COG
# Need a COG? just add 'filetype = "COG"'
terra::writeRaster(
  stitched,
  file.path(folder_write, "stitched_cog.tif"),
  gdal = c("COMPRESS=DEFLATE"),
  overwrite = TRUE,
  filetype = "COG",
  statistics = 3 #ignore
)
#note Image Structure info
terra::describe(file.path(folder_write, "stitched_cog.tif"))

### Clean up and recovery ------------------------------------------------------

# If R has crashed/aborted during terra steps,
#  it may not have cleaned up properly afterwards

# Check:
terra::tmpFiles(current = FALSE, orphan = TRUE, old = TRUE, remove = FALSE)

# Remove:
#terra::tmpFiles(current = FALSE, orphan = TRUE, old = TRUE, remove = TRUE)

# Warning. Don't remove current=TRUE while R is running.
#  You will lose anything being processed >.<

### Extact extract -------------------------------------------------------------

# Quick make some polygons
set.seed(40)
pts <- terra::spatSample(
  r,
  size = 6,
  na.rm = TRUE,
  xy = TRUE,
  values = FALSE
) %>%
  as.data.frame() %>%
  sf::st_as_sf(coords = c(1, 2), crs = terra::crs(r))
polys <- sf::st_buffer(pts, dist = 5000)
polys <- polys %>%
  dplyr::mutate(id = dplyr::row_number(), poly_name = paste0("poly_", id)) %>%
  dplyr::select(id, poly_name)
polys

#quick plot to see what we got
ggplot2::ggplot() +
  tidyterra::geom_spatraster(data = r) +
  tidyterra::scale_fill_hypso_c(name = "Elevation") +
  ggplot2::geom_sf(data = polys, color = "blue", fill = NA, size = 2)

# terra has extract() function
#  Can either be centroid (does cell centroid fall into polygon) OR
#   exact (weighted partial pixels).
#  Works fine, but exactextractr is much faster! (Only exact)

exactextractr::exact_extract(
  r,
  polys,
  fun = "mean",
  append_cols = c("id", "poly_name")
) %>%
  tibble::as_tibble()

#multiple extractions at the same time!
exactextractr::exact_extract(
  r,
  polys,
  fun = c("mean", "quantile"),
  quantiles = c(0.25, 0.5, 0.75),
  append_cols = c("id", "poly_name")
) %>%
  tibble::as.tibble()

#no function = list of values with pixel coverage fraction
exactextractr::exact_extract(
  r,
  polys,
  include_cols = c("id", "poly_name")
) %>%
  #returns list, one for each polygon
  # each row is a pixel
  dplyr::bind_rows() %>%
  tibble::as.tibble()

# There is no frequency function directly,
# but can do frac and count & some fun
# more for categorical data, so using random value raster
(ex_fc <- exactextractr::exact_extract(
  rr_a,
  polys,
  fun = c("frac", "count"),
  append_cols = c("id", "poly_name")
) %>%
  tibble::as.tibble())
#Need to calculate frequency
(ex_fq <- ex_fc %>%
  dplyr::mutate(dplyr::across(
    tidyr::starts_with("frac"),
    function(x) x * count,
    .names = "freq_{.col}"
  )) %>%
  #clean up names from freq_frac_n to just freq_n
  dplyr::rename_with(function(n) sub("_frac_", "_", n)))
#Grab just the frequencies and pivot long
ex_fq %>%
  dplyr::select(id, poly_name, dplyr::starts_with("freq_")) %>%
  tidyr::pivot_longer(
    cols = c(dplyr::starts_with("freq_")),
    names_to = "raster_value",
    values_to = "pixels"
  ) %>%
  #convert freq name to actual value
  # (splitting string at "_", taking second piece, which is the original raster value)
  dplyr::mutate(
    raster_value = stringr::str_split_i(raster_value, "_", 2) %>% as.numeric()
  )


# Coverage of polygon with data in the raster
# Count from above gives us
# "the sum of fractions of raster cells with non-NA values covered by the polygon"
# So now we calculate from a raster of total possible pixels
all_possible <- terra::init(rr_a, 1)
#get total possible pixels per polygon
(tot_poss <- exactextractr::exact_extract(
  all_possible,
  polys,
  fun = "sum",
  append_cols = c("id", "poly_name")
))
tot_poss %>%
  dplyr::left_join(
    ex_fc %>%
      dplyr::select(id, poly_name, count),
    by = join_by(id, poly_name)
  ) %>%
  dplyr::mutate(coverage = count / sum * 100)

# I generally have a bunch of rasters I need to extract from, so I throw
#  things into a loop.
# I create a target list of raster filepaths, loop on that
#  throwing results into a collector list which I bind_rows() at the end.

(targets <- list.files(
  #this is a fake example, most tiles will not have a polygon that falls in it
  path = folder_tiles,
  # files that end with ".tif". Useful to skip over any .xml files from like QGIS
  pattern = "\\.tif$",
  full.names = TRUE
) %>%
  tibble::as_tibble() %>%
  dplyr::rename(fullpath = value) %>%
  dplyr::mutate(
    basefile = tools::file_path_sans_ext(basename(fullpath)),
    file_id = stringr::str_split_i(basefile, "_", 2)
  ))

collector <- vector(mode = "list", length = nrow(targets))

for (i in 1:nrow(targets)) {
  print(paste(i, "of", nrow(targets), "at", Sys.time()))

  this_target <- targets[i, ]

  this_rast <- terra::rast(this_target[["fullpath"]])

  this_extract <- exactextractr::exact_extract(
    this_rast,
    polys,
    fun = c("mean", "max"),
    append_cols = c("poly_name")
  ) %>%
    tibble::as.tibble()

  #append file info (mutate or bind cols)
  this_result <- dplyr::bind_cols(this_target, this_extract)

  #put into my collector
  collector[[i]] <- this_result
}

#bind all collected results together into one tibble
all_result <- dplyr::bind_rows(collector)
all_result
