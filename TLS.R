#-------------------------------------------------------------------------------load library
# Packages
# install.packages("devtools")
# install_github("akamoske/canopyLazR")
# install.packages("lidR")
# install.packages("terra")
# install.packages("rayshader")
# install.packages("remotes")
# install.packages("RCSF")
# install.packages("geometry")

# Load the library
library(ggplot2)
library(raster)
library(remotes)
library(devtools)
library(canopyLazR)
#library(rLiDAR)
#library(forestr)
library(lidR)
library(rayshader)
library(RCSF)
library(geometry)
#-------------------------------------------------------------------------------load data
#---------------------------------------------------------------###load polygone
aoi <- shapefile("Area.shp")
e <- extent(12.30894, 12.31031, 51.36606, 51.36733)
#-------------------------------------------------------###load preprocessed TLS
#top
tls_top<-raster("chm_IEP.tif")
#sd
tls_sd<-raster("h_tls_sd.tif")
#--------------------------------------------------------###load preprocessed S2
#top
s2_top <- raster("ETH_GlobalCanopyHeight_DE_CHM_clip.tif")
crs(s2_top) <- ("+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs")
s2_top <- crop(s2_top, e)
#sd
s2_sd <- raster("ETH_GlobalCanopyHeight_DE_STD_clip.tif")
crs(s2_sd) <- ("+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs")
s2_sd <- crop(s2_sd, e)
#-------------------------------------------------------------------------------first plots
#to check if everything is correct
par(mfrow = c(2, 2))
plot(tls_top)
plot(s2_top)
plot(tls_sd)
plot(s2_sd)
#hist
hist(tls_top)
hist(s2_top)
hist(tls_sd)
hist(s2_sd)
#-------------------------------------------------------------------------------resample tls
tls_top<- resample(tls_top, s2_top,method = "bilinear")
tls_sd<- resample(tls_sd, s2_sd,method = "bilinear")
#-------------------------------------------------------------------------------plot again
par(mfrow = c(2, 2))
plot(tls_top)
plot(s2_top)
#hist
hist(tls_top)
hist(s2_top)
#it really doesn't look like the same image
#what is the problem? 
#TLS seems fine but S2 doesn't make any sense:
#no tree is below 25 meter
#-------------------------------------------------------------------------------scatterplot
# Extract raster values at TLS points
vals_s2 <- s2_top@data@values
vals_tls <- tls_top@data@values
df <- data.frame(TLS_Height = vals_tls, Sentinel2_Height = vals_s2)

# Create the scatter plot with regression line and coefficient
ggplot(data = df, aes(x = TLS_Height, y = Sentinel2_Height)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "seagreen") + 
  geom_text(aes(label = paste("Coeff =", 
  round(coef(summary(lm(Sentinel2_Height ~ TLS_Height, data = df)))[2, 1], 2))),
            x = min(df$TLS_Height), 
            y = max(df$Sentinel2_Height), 
            hjust = 0, vjust = 1, 
            color = "seagreen") + 
  labs(x = "TLS Height", y = "Sentinel-2 Height") +
  ggtitle("Correlation: Sentinel-2 vs TLS Height")
