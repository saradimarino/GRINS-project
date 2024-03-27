library(sf) # to be installed
library(ggplot2)

# Replace "path_to_your_shapefile" with the path to your shapefile directory
prov_data <- st_read("D:/OneDrive - Politecnico di Milano/Magistrale/mag_2/Applied Statistics/project/ProvCM01012020_g")

head(prov_data)

# Plot the borders
ggplot(data = prov_data) +
  geom_sf()

# Color provinces
ggplot(prov_data) +
  geom_sf(aes(fill = DEN_PROV),
          show.legend = FALSE) +
  theme_void()

# Color provinces
ggplot(prov_data) +
  geom_sf(aes(fill = DEN_PROV),
          show.legend = FALSE) +
  theme_void()

# Add labels to the provinces
ggplot(prov_data) +
  geom_sf() +
  geom_sf_text(aes(label = DEN_PROV),
               size = 2) +
  theme_void()
# Not the best representation


df = ("D:/OneDrive - Politecnico di Milano/Magistrale/mag_2/Applied Statistics/project/Datasets/db_provinciale_21-02")
right_join(prov_data, df, by = "COD_REG") %>% 
  head()