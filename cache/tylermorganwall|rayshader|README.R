## ----setup, include = FALSE----------------------------------------------
library(knitr)
library(rgl)
knit_hooks$set(rgl = hook_rgl)
knitr::opts_chunk$set(
  fig.path = "man/figures/",
  cache = TRUE
)
set.seed(1001)


## ----README_basicmapping-------------------------------------------------
library(rayshader)

#Here, I load a map with the raster package.
loadzip = tempfile() 
download.file("https://tylermw.com/data/dem_01.tif.zip", loadzip)
localtif = raster::raster(unzip(loadzip, "dem_01.tif"))
unlink(loadzip)

#And convert it to a matrix:
elmat = raster_to_matrix(localtif)

#We use another one of rayshader's built-in textures:
elmat %>%
  sphere_shade(texture = "desert") %>%
  plot_map()

#sphere_shade can shift the sun direction:
elmat %>%
  sphere_shade(sunangle = 45, texture = "desert") %>%
  plot_map()

#detect_water and add_water adds a water layer to the map:
elmat %>%
  sphere_shade(texture = "desert") %>%
  add_water(detect_water(elmat), color = "desert") %>%
  plot_map()

#And we can add a raytraced layer from that sun direction as well:
elmat %>%
  sphere_shade(texture = "desert") %>%
  add_water(detect_water(elmat), color = "desert") %>%
  add_shadow(ray_shade(elmat)) %>%
  plot_map()

#And here we add an ambient occlusion shadow layer, which models 
#lighting from atmospheric scattering:

elmat %>%
  sphere_shade(texture = "desert") %>%
  add_water(detect_water(elmat), color = "desert") %>%
  add_shadow(ray_shade(elmat)) %>%
  add_shadow(ambient_shade(elmat)) %>%
  plot_map()



## ----README_three-d------------------------------------------------------

elmat %>%
  sphere_shade(texture = "desert") %>%
  add_water(detect_water(elmat), color = "desert") %>%
  add_shadow(ray_shade(elmat, zscale = 3), 0.5) %>%
  add_shadow(ambient_shade(elmat), 0.5) %>%
  plot_3d(elmat, zscale = 10, fov = 0, theta = 135, zoom = 0.75, phi = 45, windowsize = c(1000, 800))
Sys.sleep(0.2)
render_snapshot(clear=TRUE)



## ----README_three-dhq----------------------------------------------------

elmat %>%
  sphere_shade(texture = "desert") %>%
  add_water(detect_water(elmat), color = "desert") %>%
  plot_3d(elmat, zscale = 10, fov = 0, theta = 135, zoom = 0.75, phi = 45, windowsize = c(1000, 800))
Sys.sleep(0.2)
render_highquality()



## ----include = FALSE-----------------------------------------------------
rgl::rgl.close()
system("rm dem_01.tif")


## ----README_three-d-water------------------------------------------------

montshadow = ray_shade(montereybay, zscale = 50, lambert = FALSE)
montamb = ambient_shade(montereybay, zscale = 50)
montereybay %>%
    sphere_shade(zscale = 10, texture = "imhof1") %>%
    add_shadow(montshadow, 0.5) %>%
    add_shadow(montamb) %>%
    plot_3d(montereybay, zscale = 50, fov = 0, theta = -45, phi = 45, 
            windowsize = c(1000, 800), zoom = 0.75,
            water = TRUE, waterdepth = 0, wateralpha = 0.5, watercolor = "lightblue",
            waterlinecolor = "white", waterlinealpha = 0.5)
Sys.sleep(0.2)
render_snapshot(clear=TRUE)


## ------------------------------------------------------------------------

montereybay %>%
    sphere_shade(zscale = 10, texture = "imhof1") %>%
    plot_3d(montereybay, zscale = 50, fov = 70, theta = -45, phi = 20, 
            windowsize = c(1000, 800), zoom = 0.6, 
            water = TRUE, waterdepth = 0, wateralpha = 0.5, watercolor = "lightblue",
            waterlinecolor = "white", waterlinealpha = 0.5)
Sys.sleep(0.2)
render_highquality(lightdirection = 0, lightaltitude  = 30, clamp_value = 10, 
                   samples=200, clear=TRUE)




## ----README_three-d-shapes, fig.width = 15, fig.height = 6---------------
par(mfrow = c(1, 2))
montereybay %>% 
    sphere_shade(zscale = 10, texture = "imhof1") %>% 
    add_shadow(montshadow, 0.5) %>%
    add_shadow(montamb) %>%
    plot_3d(montereybay, zscale = 50, fov = 0, theta = -45, phi = 45, windowsize = c(1000, 800), zoom = 0.6,
            water = TRUE, waterdepth = 0, wateralpha = 0.5, watercolor = "lightblue",
            waterlinecolor = "white", waterlinealpha = 0.5, baseshape = "circle")

render_snapshot(clear = TRUE)

montereybay %>% 
    sphere_shade(zscale = 10, texture = "imhof1") %>% 
    add_shadow(montshadow, 0.5) %>%
    add_shadow(montamb) %>%
    plot_3d(montereybay, zscale = 50, fov = 0, theta = -45, phi = 45, windowsize = c(1000, 800), zoom = 0.6,
            water = TRUE, waterdepth = 0, wateralpha = 0.5, watercolor = "lightblue",
            waterlinecolor = "white", waterlinealpha = 0.5, baseshape = "hex")

render_snapshot(clear = TRUE)


## ----README_three-d-labels-----------------------------------------------
montereybay %>% 
    sphere_shade(zscale = 10, texture = "imhof1") %>% 
    add_shadow(montshadow, 0.5) %>%
    add_shadow(montamb) %>%
    plot_3d(montereybay, zscale = 50, fov = 0, theta = -100, phi = 30, windowsize = c(1000, 800), zoom = 0.6,
            water = TRUE, waterdepth = 0, waterlinecolor = "white", waterlinealpha = 0.5,
            wateralpha = 0.5, watercolor = "lightblue")
render_label(montereybay, x = 350, y = 240, z = 4000, zscale = 50,
             text = "Moss Landing", textsize = 2, linewidth = 5)
render_label(montereybay, x = 220, y = 330, z = 6000, zscale = 50,
             text = "Santa Cruz", textcolor = "darkred", linecolor = "darkred",
             textsize = 2, linewidth = 5)
render_label(montereybay, x = 300, y = 130, z = 4000, zscale = 50,
             text = "Monterey", dashed = TRUE, textsize = 2, linewidth = 5)
render_label(montereybay, x = 50, y = 130, z = 1000, zscale = 50,
             text = "Monterey Canyon", relativez = FALSE, textsize = 2, linewidth = 5)
Sys.sleep(0.2)
render_snapshot(clear = TRUE)


## ----README_three-d-depth, fig.width = 10, fig.height = 8----------------

elmat %>%
  sphere_shade(texture = "desert") %>%
  add_water(detect_water(elmat), color = "desert") %>%
  add_shadow(ray_shade(elmat, zscale = 3), 0.5) %>%
  add_shadow(ambient_shade(elmat), 0.5) %>%
  plot_3d(elmat, zscale = 10, fov = 30, theta = -225, phi = 25, windowsize = c(1000, 800), zoom = 0.3)
Sys.sleep(0.2)
render_depth(focus = 0.6, focallength = 200, clear = TRUE)



## ----README_ggplots, fig.width = 10, fig.height = 5----------------------
library(ggplot2)
ggdiamonds = ggplot(diamonds) +
  stat_density_2d(aes(x = x, y = depth, fill = stat(nlevel)), 
                  geom = "polygon", n = 100, bins = 10, contour = TRUE) +
  facet_wrap(clarity~.) +
  scale_fill_viridis_c(option = "A")

par(mfrow = c(1, 2))

plot_gg(ggdiamonds, width = 5, height = 5, raytrace = FALSE, preview = TRUE)
plot_gg(ggdiamonds, width = 5, height = 5, multicore = TRUE, scale = 250, 
        zoom = 0.7, theta = 10, phi = 30, windowsize = c(800, 800))
Sys.sleep(0.2)
render_snapshot(clear = TRUE)



## ----README_ggplots_2, fig.width = 10, fig.height = 5--------------------
library(reshape2)
#Contours and other lines will automatically be ignored. Here is the volcano dataset:

ggvolcano = volcano %>% 
  melt() %>%
  ggplot() +
  geom_tile(aes(x = Var1, y = Var2, fill = value)) +
  geom_contour(aes(x = Var1, y = Var2, z = value), color = "black") +
  scale_x_continuous("X", expand = c(0, 0)) +
  scale_y_continuous("Y", expand = c(0, 0)) +
  scale_fill_gradientn("Z", colours = terrain.colors(10)) +
  coord_fixed()

par(mfrow = c(1, 2))
plot_gg(ggvolcano, width = 7, height = 4, raytrace = FALSE, preview = TRUE)
plot_gg(ggvolcano, multicore = TRUE, raytrace = TRUE, width = 7, height = 4, 
        scale = 300, windowsize = c(1400, 866), zoom = 0.6, phi = 30, theta = 30)
Sys.sleep(0.2)

render_snapshot(clear = TRUE)



## ----README_ggplots_3, fig.width = 10, fig.height = 5--------------------
mtplot = ggplot(mtcars) + 
  geom_point(aes(x = mpg, y = disp, color = cyl)) + 
  scale_color_continuous(limits = c(0, 8))

par(mfrow = c(1, 2))
plot_gg(mtplot, width = 3.5, raytrace = FALSE, preview = TRUE)

plot_gg(mtplot, width = 3.5, multicore = TRUE, windowsize = c(800, 800), 
        zoom = 0.85, phi = 35, theta = 30, sunangle = 225, soliddepth = -100)
Sys.sleep(0.2)
render_snapshot(clear = TRUE)



## ----README_ggplots_4, fig.width = 10, fig.height = 5--------------------
a = data.frame(x = rnorm(20000, 10, 1.9), y = rnorm(20000, 10, 1.2))
b = data.frame(x = rnorm(20000, 14.5, 1.9), y = rnorm(20000, 14.5, 1.9))
c = data.frame(x = rnorm(20000, 9.5, 1.9), y = rnorm(20000, 15.5, 1.9))
data = rbind(a, b, c)

#Lines
pp = ggplot(data, aes(x = x, y = y)) +
  geom_hex(bins = 20, size = 0.5, color = "black") +
  scale_fill_viridis_c(option = "C")

par(mfrow = c(1, 2))
plot_gg(pp, width = 5, height = 4, scale = 300, raytrace = FALSE, preview = TRUE)
plot_gg(pp, width = 5, height = 4, scale = 300, multicore = TRUE, windowsize = c(1000, 800))
render_camera(fov = 70, zoom = 0.5, theta = 130, phi = 35)
Sys.sleep(0.2)
render_snapshot(clear = TRUE)


## ----README_ggplots_5, fig.width = 10, fig.height = 8--------------------

par(mfrow = c(1, 1))
plot_gg(pp, width = 5, height = 4, scale = 300, multicore = TRUE, windowsize = c(1200, 960),
        fov = 70, zoom = 0.4, theta = 330, phi = 40)
Sys.sleep(0.2)
render_depth(focus = 0.68, focallength = 200)

