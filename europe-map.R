## ggplot2 Europe map
## Last update: 12/07/2015

## inspired by http://editerna.free.fr/wp/?p=130

## load libraries
library(ggplot2)
library(rworldmap)
library(RColorBrewer)
library(XLConnect)

## read data
w <- loadWorkbook("europe_map.xlsx")
data <- readWorksheet(w, "data")

## create 'rworldmap' world map
worldMap <- getMap()

## identify EU countries
show <- which(worldMap$NAME %in% data$Country)

## WORLD coordinates
## this will be used as the background and will include non EU countries
plotCoords <-
  lapply(seq(worldMap$NAME),
         function(x) {
           ## collect long/lat in dataframe
           df <- lapply(worldMap@polygons[[x]]@Polygons,
                        function(x) x@coords)
           df <- do.call("rbind", as.list(df))
           df <- data.frame(df)

           ## add geographical name
           df$region <- as.character(worldMap$NAME[x])
           if (is.na(worldMap$NAME[x])) df$region <- "NONAME"

           ## add unique polygon identifier
           id <-
             rep(seq_along(worldMap@polygons[[x]]@Polygons),
                 sapply(worldMap@polygons[[x]]@Polygons,
                        function(x) nrow(x@coords)))
           df$group <- paste0(df$region, id)

           ## add column names and return dataframe
           colnames(df) <- list("long", "lat", "region", "group")
           return(df)
         })
plotCoords <- do.call("rbind", plotCoords)

## add EU identifier
plotCoords$EU <- 0
plotCoords$EU[which(plotCoords$region %in% data$Country)] <- 1

## for some reason, this group gives a horizontal segment across Europe
plotCoords <- plotCoords[plotCoords$group != "United States4", ]


## EU coordinates
showCoords <-
  lapply(show,
         function(x) {
           ## collect long/lat in dataframe
           df <- lapply(worldMap@polygons[[x]]@Polygons,
                        function(x) x@coords)
           df <- do.call("rbind", as.list(df))
           df <- data.frame(df)

           ## add geographical name
           df$region <- as.character(worldMap$NAME[x])
           if (is.na(worldMap$NAME[x])) df$region <- "NONAME"

           ## add unique polygon identifier
           id <-
             rep(seq_along(worldMap@polygons[[x]]@Polygons),
                 sapply(worldMap@polygons[[x]]@Polygons,
                        function(x) nrow(x@coords)))
           df$group <- paste0(df$region, id)

           ## add column names and return dataframe
           colnames(df) <- list("long", "lat", "region", "group")
           return(df)
         })
showCoords <- do.call("rbind", showCoords)

## add endemicity category
showCoords$endemic <-
  data$Endemic[match(showCoords$region, data$Country)]
showCoords$endemic <- factor(showCoords$endemic, unique(showCoords$endemic))

## add smallholder pig production category
showCoords$smallholder <-
  as.numeric(
    gsub(",", ".", data$Smallholder[match(showCoords$region, data$Country)]))


## FIGURE 1
tiff("Fig1.tiff", 6, 6, units = "in", res = 300)
#png("Fig1.png", 6, 6, units = "in", res = 300)
ggplot() +
  geom_polygon(
    data = plotCoords,
    aes(x = long, y = lat, group = group),
    fill = "lightgrey", colour = "darkgrey", size = 0.1) +
  geom_polygon(
    data = showCoords,
    aes(x = long, y = lat, group = group, fill = endemic),
    colour = "black", size = 0.1) +
  scale_fill_manual(
    expression(bolditalic("Taenia solium")~bold("endemicity")),
    values = brewer.pal(3, "Spectral")[c(2,3,1)],
    labels = levels(showCoords$endemic)) +
  scale_x_continuous(element_blank(), breaks = NULL) +
  scale_y_continuous(element_blank(), breaks = NULL) +
  coord_map(xlim = c(-26, 47),  ylim = c(32.5, 73)) + 
  theme_bw() +
  theme(legend.justification = c(0, 1), legend.position = c(0, 1)) +
  theme(legend.background = element_rect(colour = "black"))
graphics.off()


## FIGURE 2
showCoords$smallholder_cat <-
  cut(showCoords$smallholder,
      c(0, 0.01, 0.10, 0.50, 1.00),
      right = FALSE)
levels(showCoords$smallholder_cat) <-
  c(levels(showCoords$smallholder_cat), "NODATA")
showCoords$smallholder_cat[is.na(showCoords$smallholder_cat)] <- "NODATA"

tiff("Fig2.tiff", 6, 6, units = "in", res = 300)
#png("Fig2.png", 6, 6, units = "in", res = 300)
ggplot() +
  geom_polygon(
    data = plotCoords,
    aes(x = long, y = lat, group = group),
    fill = "lightgrey", colour = "darkgrey", size = 0.1) +
  geom_polygon(
    data = showCoords,
    aes(x = long, y = lat, group = group),
    fill = "grey", colour = "black", size = 0.1) +
  geom_polygon(
    data = showCoords,
    aes(x = long, y = lat, group = group, fill = smallholder_cat),
    colour = "black", size = 0.1) +
  scale_fill_manual(
    "% Smallholder pigs",
    values = c(brewer.pal(4, "Reds"), "grey"),
    labels = c("<1%", "1-10%", "10-50%", ">50%", "no data"),
    guide = guide_legend(direction = "horizontal",
                         title.position = "top",
                         nrow = 2)) +
  scale_x_continuous(element_blank(), breaks = NULL) +
  scale_y_continuous(element_blank(), breaks = NULL) +
  coord_map(xlim = c(-26, 47),  ylim = c(32.5, 73)) + 
  theme_bw() +
  theme(legend.justification = c(0, 1), legend.position = c(0, 1)) +
  theme(legend.background = element_rect(colour = "black"))
graphics.off()
