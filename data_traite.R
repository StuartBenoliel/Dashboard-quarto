
#source("utils/connect_duckdb.R")

#bdd_gaia_prod <- get_conn_gaia("prod2")

# 
# resultat <- dbGetQuery(bdd_gaia_prod, "
#   SELECT 
#       *
#   FROM expertise.tableau_de_bord
# ") %>% tail(1)

resultat <- data.frame(
  nombre_adresses_actives = 35000000,
  nombre_voies_actives = 3500000,
  nombre_communes_actives = 35000,
  nombre_parcelles = 89000000
)

france <- st_read('../data/france_geom.json') %>% 
  select(-id)

# Exemple de points France avec coordonnées fictives
france_com <- france %>% 
  st_drop_geometry() %>% 
  st_as_sf(coords = c("xcl2154", "ycl2154"), crs = 2154)

# Ajout d'une variable quantitative aléatoire ressemblant au nombre d'hab
# On crée une variable "pop" avec peu de grandes valeurs
set.seed(42)
france_com <- france_com %>%
  mutate(pop = sample(c(100, 500, 1000, 5000, 10000), n(), replace = TRUE))

# Contour des départements
france_contour <- france %>%
  st_make_valid() %>%
  group_by(dep) %>%
  summarise(geometry = st_union(geometry))

st_crs(france_contour) <- 2154
# 
# # 1. 6 sommets
theta <- seq(0, 2*pi, length.out = 6)
coords <- cbind(cos(theta), sin(theta))

# 2. Fermeture explicite du polygone
coords_closed <- rbind(coords, coords[1, ])

# 3. Objet sf
hex <- st_sf(
  geometry = st_sfc(
    st_polygon(list(coords_closed))
  )
)

######## STAT ADRESSES

resultat_adresses <- data.frame(
  pct_positions = 95,
  pct_positions_gaia = 98,
  pct_positions_ban = 78,
  pct_positions_rca = 28,
  pct_codes_postaux = 53,
  pct_iris = 93,
  nb_qpv = 880000,
  nb_qpv15 = 849999,
  pct_parcelles = 91,
  pct_up = 92,
  nb_ilots = 1000000,
  nb_qrr = 50000
)

resultat_quant <- data.frame(
  date_valeur = Sys.Date(),
  d1  = 10,
  d2  = 20,
  d3  = 35,
  d4  = 55,
  d5  = 80,
  d6  = 110,
  d7  = 150,
  d8  = 200,
  d9  = 260,
  c91 = 320,
  c92 = 390,
  c93 = 470,
  c94 = 560,
  c95 = 660,
  c96 = 770,
  c97 = 890,
  c98 = 1020,
  c99 = 1160,
  d10 = 1300
)

# Ordre voulu des variables
ordre <- c("d1", "d2", "d3", "d4", "d5", "d6", "d7", "d8", "d9",
           "c91", "c92", "c93", "c94", "c95", "c96", "c97", "c98",
           "c99", "d10")

# Convertir en format long
df_long <- resultat_quant %>%
  select(-date_valeur) %>%
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "valeur"
  ) %>%
  mutate(variable = factor(variable, levels = ordre))


########## FLUX 


df_sankey <- data.frame(
  source = rep(c("Majic", "Rca", "Ban"), each = 3),
  final  = rep(c("Identifiées",
                 "Adresses créées (voie existante)",
                 "Adresses créées (nouvelle voie)"), times = 3),
  value  = c(120, 30, 10,
             80, 25, 15,
             60, 40, 20)
)

df_sankey_2 <- data.frame(
  source = rep(c("Majic", "Ban"), each = 3),
  final  = rep(c("Identifiées",
                 "Adresses créées (voie existante)",
                 "Adresses créées (nouvelle voie)"), times = 2),
  value  = c(120, 30, 10,
             60, 40, 20)
)

df_sankey_3 <- data.frame(
  source = rep(c("Ban"), each = 3),
  final  = rep(c("Identifiées",
                 "Adresses créées (voie existante)",
                 "Adresses créées (nouvelle voie)"), times = 1),
  value  = c(120, 30, 10)
)


###### HEATMAP

cats <- LETTERS[1:9]

# Tableau de comptage simulé
df_heatmap <- expand.grid(
  cat_x = cats,
  cat_y = cats
) %>%
  mutate(count = sample(0:50, n(), replace = TRUE))
