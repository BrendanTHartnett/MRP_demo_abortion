library(albersusa)
library(sf)
library(ggplot2)
library(sf)
library(albersusa)
library(ggrepel)
Census$prediction
Census$predictionP = Census$prediction*100
Census$PredictionP = round(Census$prediction, digits = 2)


usa_sf <-
  st_as_sf(usa_composite("laea")) %>%
  mutate(
    CENTROID = map(geometry, st_centroid),
    COORDS = map(CENTROID, st_coordinates),
    COORDS_X = map_dbl(COORDS, 1),
    COORDS_Y = map_dbl(COORDS, 2)
  ) %>%
  as_tibble() %>%
  st_as_sf()

usa_sf$nudge_x <- 0
usa_sf$nudge_y <- 0



x_range <- abs(Reduce("-", range(usa_sf$COORDS_X)))
y_range <- abs(Reduce("-", range(usa_sf$COORDS_Y)))

ix <- usa_sf$name %in% c("New Hampshire", "Vermont", "Massachusetts")
usa_sf$nudge_x[ix] <- -1 * 0.15 * x_range
usa_sf$nudge_y[ix] <- 1 * 0.15 * y_range

ix <- usa_sf$name %in% c(
  "Massachusetts",
  "Rhode Island", "Connecticut", "New Jersey",
  "Maryland", "Delaware", "New Hampshire"
)
usa_sf$nudge_x[ix] <- 1 * 0.2 * x_range
usa_sf$nudge_y[ix] <- -1 * 0.15 * y_range
head(usa_sf)




usa_sf2 <- subset(usa_sf, name != "District of Columbia")

usa_sf2$fips_state
usa_sf2$fips <- as.numeric(usa_sf2$fips_state)
Census$GEOID
data_to_map <- merge(usa_sf2, Census, by.x="fips", by.y="GEOID")

data_to_map$predictionP <- round(data_to_map$predictionP, digits = 0)


data_to_map$predicted_value <- as.character(data_to_map$predictionP)
data_to_map$predicted_value
data_to_map$predicted_value <- paste(data_to_map$predicted_value, "%", sep="")

data_to_map$predicted_value[data_to_map$name=="Massachusetts"] <- "67%"
data_to_map$predicted_value[data_to_map$name=="Connecticut"] <- "65%"
data_to_map$predicted_value[data_to_map$name=="New Jersey"] <- "62%"
data_to_map$predicted_value[data_to_map$name=="Maryland"] <- "63%"
data_to_map$predicted_value[data_to_map$name=="Delaware"] <- "60%"
data_to_map$predicted_value[data_to_map$name=="New Hampshire"] <- "56%"
data_to_map$predicted_value[data_to_map$name=="Rhode Island"] <- "62%"
data_to_map$predicted_value[data_to_map$name=="Hawaii"] <- "57%"
data_to_map$predicted_value[data_to_map$name=="Vermont"] <- "68%"



data_to_map$predicted_value
data_to_map$nudge_x[data_to_map$name=="Michigan"] <- 45000
data_to_map$nudge_y[data_to_map$name=="Michigan"] <- 45000*(-1)
data_to_map$name

data_to_map$nudge_y[data_to_map$name=="Louisiana"] <- 45000*(-1)
data_to_map$nudge_x[data_to_map$name=="Florida"] <- 90000

data_to_map$nudge_x[data_to_map$name=="Hawaii"] <- 200000*(-1)
data_to_map$nudge_y[data_to_map$name=="Hawaii"] <- 200000*(-1)

table(data_to_map$prediction)
data_to_map$prediction_percent <- 100*data_to_map$prediction
class(data_to_map$COORDS_X)
data_to_map$predicted_value
plot <- ggplot(data = data_to_map) +
  geom_sf(aes(fill = prediction_percent)) +
  geom_text_repel(
    mapping = aes(
      x = COORDS_X,
      y = COORDS_Y,
      label = predicted_value
    ),
    nudge_x = data_to_map$nudge_x,
    nudge_y = data_to_map$nudge_y,
    size = 3,
    min.segment.length = 1,
    point.size = NA,
    segment.color = "grey50"
  ) +
  coord_sf(crs = st_crs(data_to_map), datum = NA) +
  theme_void() +
  xlim(min(usa_sf$COORDS_X) * 1.12, max(usa_sf$COORDS_X) * 1.11) + 
  ylim(min(usa_sf$COORDS_Y) * 1.1, max(usa_sf$COORDS_Y) * 1.2) + 
  labs(title = "     Predicted percentage of eligible voters who support allowing abortion
       in all circumstances as a matter of choice", caption = "Figure by Brendan Hartnett.
       Values predicted derive from multilevel regression and post-stratification model.
       Survey data: CES 2022.
       
       ") +  
  scale_fill_continuous(limits = c(20,75), breaks = c(30, 50, 70), labels = c("30%", "50%", "70%"), low = "white", high = "darkorchid", name = " ") + theme(legend.position = "right")
