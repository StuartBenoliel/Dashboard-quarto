library(sf)
library(leaflet)
library(dplyr)
library(jsonlite)

set.seed(123)
n <- 2000000

gares_big <- data.frame(
  name = paste0("Gare_", 1:n),
  lon = runif(n, -5, 8),
  lat = runif(n, 42, 51)
) %>%
  mutate(test = "3")

gares_sf <- st_as_sf(gares_big, coords = c("lon", "lat"), crs = 4326)

# Index de recherche
lookup <- gares_big %>%
  select(name, lon, lat) %>%
  distinct(name, .keep_all = TRUE)

lookup_json <- toJSON(lookup, dataframe = "rows")

# Carte
map <- leaflet(gares_sf) %>%
  addProviderTiles(providers$OpenStreetMap, group = "OSM") %>%
  addProviderTiles(providers$CartoDB.Positron, group = "Positron") %>%
  addProviderTiles(providers$CartoDB.DarkMatter, group = "Dark") %>%
  
  addCircleMarkers(
    radius = 4,
    group = "Gares",
    layerId = ~name,
    label = ~name,
    popup = ~paste0(
      "<table>",
      "<tr><th>Nom</th><td>", name, "</td></tr>",
      "<tr><th>Test</th><td>", test, "</td></tr>",
      "</table>"
    ),
    clusterOptions = markerClusterOptions(
      maxClusterRadius = 120,          # clusters plus larges
      spiderfyOnMaxZoom = TRUE,
      showCoverageOnHover = FALSE,
      zoomToBoundsOnClick = TRUE,
      disableClusteringAtZoom = 15
    )
  ) %>%
  
  # --- Contrôle des couches ---
  addLayersControl(
    baseGroups = c("OSM", "Positron", "Dark"),
    overlayGroups = c("Gares"),
    options = layersControlOptions(collapsed = TRUE)
  ) %>%
  
  # --- Barre d’échelle ---
  addScaleBar(position = "bottomleft")

# Contrôle de recherche + autocomplete
map <- map %>%
  addControl(
    html = '
      <div style="padding:6px;">
        <input id="searchGare" list="gareList" placeholder="Rechercher..." style="width:200px;">
        <datalist id="gareList"></datalist>
      </div>
    ',
    position = "topright"
  ) %>% 
  onRender("
    function(el, x) {
      var lc = document.querySelector('.leaflet-control-layers');
      var container = document.querySelector('.leaflet-top.leaflet-left');
      
      if (lc && container) {
        container.appendChild(lc);
      }
    }
  ") %>%
  
  onRender(sprintf("
    function(el, x) {
      var map = this;
      
      // Données (name -> coord)
      var data = %s;
      var index = {};
      
      data.forEach(function(d) {
        index[d.name] = { lat: d.lat, lng: d.lon };
      });
      
      var input = document.getElementById('searchGare');
      var datalist = document.getElementById('gareList');
      
      // Autocomplétion (top 5)
      input.addEventListener('input', function(e) {
        var value = e.target.value.toLowerCase();
        
        datalist.innerHTML = '';
        if (!value) return;
        
        var matches = data
          .filter(d => d.name.toLowerCase().includes(value))
          .slice(0, 5);
        
        matches.forEach(function(d) {
          var option = document.createElement('option');
          option.value = d.name;
          datalist.appendChild(option);
        });
      });
      
      // Recherche + zoom
      input.addEventListener('change', function(e) {
        var value = e.target.value;
        if (!value || !index[value]) return;
        
        var pos = index[value];
        map.setView([pos.lat, pos.lng], 14);
      });
    }
  ", lookup_json))

map