################# Table #################

my_table <- function(df, caption = NULL) {
  
  DT::datatable(
    df,
    caption = caption,
    rownames = FALSE,
    class = "stripe compact",
    options = list(
      pageLength = 20,
      paging = TRUE,      # pagination activée
      searching = TRUE,   # barre de recherche
      ordering = TRUE,    # tri colonnes
      #autoWidth = TRUE,
      dom = "ftip"
    )
  )
}

################# Sankey #################

plot_sankey <- function(
    data,
    source,
    final,
    value,
    palette = c(
      "Majic" = "#1b9e77",
      "Rca"   = "#d95f02",
      "Ban"   = "#7570b3"
    ),
    wrap_width = 20,
    flux_offset = 0.18
) {
  
  ggplot(data, aes(axis1 = {{ source }}, axis2 = {{ final }}, y = {{ value }})) +
    
    # Flux
    geom_alluvium(aes(fill = {{ source }}), alpha = 0.7, width = 0.25) +
    
    # Noeuds
    geom_stratum(width = 0.3, fill = "grey90", color = "grey40") +
    
    # Labels noeuds avec totaux
    geom_text(
      stat = "stratum",
      aes(
        label = after_stat(
          paste0(
            stringr::str_wrap(stratum, wrap_width),
            "\n(",
            scales::comma(count),
            ")"
          )
        )
      ),
      size = 4,
      fontface = "bold"
    ) +
    
    # Valeurs flux côté gauche
    geom_text(
      stat = "alluvium",
      aes(
        label = scales::comma(value),
        x = after_stat(x) - flux_offset
      ),
      hjust = 1,
      size = 3.5,
      colour = "black"
    ) +
    
    # Valeurs flux côté droit
    geom_text(
      stat = "alluvium",
      aes(
        label = scales::comma(value),
        x = after_stat(x) + flux_offset
      ),
      hjust = 0,
      size = 3.5,
      colour = "black"
    ) +
    
    scale_fill_manual(values = palette) +
    
    scale_x_discrete(
      limits = c("", ""),
      expand = c(0, 0)
    ) +
  
    theme_minimal() +
    theme(
      legend.position = "none",
      panel.grid = element_blank(),
      axis.title = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank()
    )
}

################# Heatmap #################

plot_heatmap <- function(
    data,
    x,
    y,
    count,
    x_label = "",
    y_label = "",
    legend_label = "",
    low_color = "white",
    high_color = "steelblue",
    show_values = TRUE,
    text_size = 3,
    base_size = 14,
    number_format = scales::comma
) {
  
  p <- ggplot(data, aes(x = {{ x }}, y = {{ y }}, fill = {{ count }})) +
    geom_tile() +
    scale_fill_gradient(low = low_color, high = high_color) +
    theme_minimal(base_size = base_size) +
    theme(panel.grid = element_blank()) +
    labs(
      x = x_label,
      y = y_label,
      fill = legend_label
    )
  
  if (show_values) {
    p <- p +
      geom_text(aes(label = number_format(count)), size = text_size)
  }
  
  return(ggplotly(p))
}

################# Quantile #################

plot_quantile <- function(
    data,
    x,
    y,
    x_label = "Quantile",
    y_label = "Distance en mètres",
    line_color = "steelblue",
    point_size = 3,
    use_log10 = TRUE,
    base_size = 14
) {
  
  p <- ggplot(data, aes(x = {{ x }}, y = {{ y }}, group = 1)) +
    geom_line(color = line_color) +
    geom_point(size = point_size, color = line_color) +
    theme_minimal(base_size = base_size) +
    labs(
      x = x_label,
      y = y_label
    )
  
  if (use_log10) {
    p <- p + scale_y_log10()
  }
  
  return(ggplotly(p))
}

################# Map #################

map_departement <- function(
    data
) {
  
  p <- mapview(data)
  
  return(p)
}

################# Valuebox #################

value_box <- function(
    title,
    valeur,
    color = "red",
    pct = FALSE,
    diff = NA
) {
  
  
  value_formate <- valeur %>% format(big.mark = " ", scientific = FALSE)
  
  value_formate <- ifelse(pct, paste0(value_formate, " %"), value_formate)
  
  if (!is.na(diff)) {
    
    diff_formate <- diff %>% format(big.mark = " ", scientific = FALSE)
    
    title <- ifelse(
      diff >= 0, 
      paste0(title, " (▲ ", diff_formate, ")"), 
      paste0(title, "(▼ ", diff_formate, ")")
    )
    
  }
  
  p <- list(
    title = title,
    color = color,
    value = value_formate
  )
  
  return(p)
}