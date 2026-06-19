library(tidyverse)
library(scales)

data <- readRDS("data/clean/data_merged.rds")

final_data <- data %>%
  filter(source != "SPD") %>%
  mutate(
    team_composition = case_when(
      CAHOOTS == 1 & EPD == 1  ~ "CAHOOTS + EPD",
      CAHOOTS == 1             ~ "CAHOOTS",
      TRUE                     ~ "Other"
    )
  )


# ── 1. Nature lists (dynamic) ──────────────────────────────────────────────────

top_cahoots_natures <- final_data %>%
  filter(CAHOOTS == 1) %>%
  count(nature, sort = TRUE) %>%
  slice_max(n, n = 14) %>%
  pull(nature) %>%
  as.character()

top_mcslc_natures <- final_data %>%
  filter(MCSLC == 1) %>%
  distinct(nature) %>%
  pull(nature) %>%
  as.character()


# ── 2. Helper: build proportion table ─────────────────────────────────────────

prep_outcome_data <- function(df, nature_list, nature_col, outcome_col,
                              agency_flag_col, lump_other = FALSE) {
  df_filtered <- df %>%
    filter(.data[[agency_flag_col]] == 1)
  
  if (lump_other) {
    df_filtered <- df_filtered %>%
      mutate(
        !!nature_col := fct_other(
          as.character(.data[[nature_col]]),
          keep        = nature_list,
          other_level = "Other"
        )
      )
  } else {
    df_filtered <- df_filtered %>%
      filter(.data[[nature_col]] %in% nature_list)
  }
  
  df_filtered %>%
    count(.data[[nature_col]], .data[[outcome_col]], team_composition,  # ← ajout team_composition
          name = "n") %>%
    group_by(.data[[nature_col]], team_composition) %>%                 # ← ajout team_composition
    mutate(
      prop  = n / sum(n),
      total = sum(n)
    ) %>%
    ungroup() %>%
    rename(nature = 1, outcome = 2) %>%
    mutate(
      nature = fct_reorder(nature, total, .fun = max)
    )
}

# ── 3. Subset for each agency group ───────────────────────────────────────────

cahoots_df <- prep_outcome_data(
  df              = final_data,
  nature_list     = top_cahoots_natures,
  nature_col      = "nature",
  outcome_col     = "outcome",
  agency_flag_col = "CAHOOTS",
  lump_other      = TRUE
)

mcslc_df <- prep_outcome_data(
  df              = final_data,
  nature_list     = top_mcslc_natures,
  nature_col      = "nature",
  outcome_col     = "outcome",
  agency_flag_col = "MCSLC",
  lump_other      = FALSE
)


# ── 4. Shared theme ────────────────────────────────────────────────────────────

theme_outcome <- function() {
  theme_minimal(base_size = 11) +
    theme(
      panel.grid.major.y = element_blank(),
      panel.grid.minor   = element_blank(),
      axis.text.y        = element_text(size = 9),
      axis.text.x        = element_text(size = 8),
      legend.position    = "bottom",
      legend.title       = element_text(size = 9, face = "bold"),
      legend.text        = element_text(size = 8),
      legend.key.size    = unit(0.4, "cm"),
      plot.title         = element_text(face = "bold", size = 13),
      plot.subtitle      = element_text(size = 9, color = "grey40"),
      strip.text         = element_blank(),
      plot.margin        = margin(t = 5, r = 60, b = 5, l = 5, unit = "pt")
    )
}

# Thème adapté pour les histogrammes individuels (axe x vertical)
theme_outcome_hist <- function() {
  theme_minimal(base_size = 11) +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor   = element_blank(),
      axis.text.x        = element_text(size = 8, angle = 35, hjust = 1),
      axis.text.y        = element_text(size = 8),
      legend.position    = "none",
      plot.title         = element_text(face = "bold", size = 12),
      plot.subtitle      = element_text(size = 9, color = "grey40"),
      plot.margin        = margin(t = 5, r = 10, b = 5, l = 5, unit = "pt")
    )
}


# 5 proportions =======================================================

plot_outcome_composition <- function(data, title, subtitle = NULL,
                                     n_outcomes_collapse = 8) {
  top_outcomes <- data %>%
    group_by(outcome) %>%
    summarise(total_prop = sum(prop * total) / sum(data$total), .groups = "drop") %>%
    slice_max(total_prop, n = n_outcomes_collapse) %>%
    pull(outcome)
  
  plot_data <- data %>%
    mutate(
      outcome_lumped = fct_other(outcome, keep = as.character(top_outcomes),
                                 other_level = "Other / Rare"),
      outcome_lumped = fct_infreq(outcome_lumped)
    ) %>%
    group_by(nature, outcome_lumped, team_composition) %>%             
    summarise(prop = sum(prop), total = first(total), .groups = "drop")
  
  n_cols <- n_distinct(plot_data$outcome_lumped)
  pal    <- colorRampPalette(RColorBrewer::brewer.pal(8, "Set2"))(n_cols)
  
  ggplot(plot_data,
         aes(x = prop, y = nature, fill = outcome_lumped)) +
    geom_col(width = 0.75, colour = "white", linewidth = 0.3) +
    scale_x_continuous(
      labels = percent_format(accuracy = 1),
      expand = expansion(mult = c(0, 0.18))
    ) +
    facet_wrap(~ team_composition, ncol = 2) +
    scale_fill_manual(values = pal, name = "Outcome") +
    geom_text(
      data = plot_data %>% distinct(nature, team_composition, total),  # ← ajout team_composition
      aes(x = 1.01, y = nature, label = paste0("n=", scales::comma(total))),
      inherit.aes = FALSE,
      hjust = 0, size = 2.8, colour = "grey40"
    ) +
    coord_cartesian(clip = "off") +
    labs(title = title, subtitle = subtitle, x = "Share of calls", y = NULL) +
    theme_outcome() +
    theme(strip.text = element_text(size = 9, face = "bold")) +        # ← rendre les facet labels visibles
    guides(fill = guide_legend(nrow = 3, byrow = TRUE))
}

# ── 6. Plot factory overview (absolus) ────────────────────────────────────────

plot_outcome_composition_abs <- function(data, title, subtitle = NULL,
                                         n_outcomes_collapse = 8) {
  top_outcomes <- data %>%
    group_by(outcome) %>%
    summarise(total_n = sum(n), .groups = "drop") %>%
    slice_max(total_n, n = n_outcomes_collapse) %>%
    pull(outcome)
  
  plot_data <- data %>%
    mutate(
      outcome_lumped = fct_other(outcome, keep = as.character(top_outcomes),
                                 other_level = "Other / Rare"),
      outcome_lumped = fct_infreq(outcome_lumped)
    ) %>%
    group_by(nature, outcome_lumped) %>%
    summarise(n = sum(n), total = first(total), .groups = "drop")
  
  n_cols <- n_distinct(plot_data$outcome_lumped)
  pal    <- colorRampPalette(RColorBrewer::brewer.pal(8, "Set2"))(n_cols)
  
  ggplot(plot_data,
         aes(x = n, y = nature, fill = outcome_lumped)) +
    geom_col(width = 0.75, colour = "white", linewidth = 0.3) +
    scale_x_continuous(
      labels = scales::comma,
      expand = expansion(mult = c(0, 0.18))
    ) +
    scale_fill_manual(values = pal, name = "Outcome") +
    geom_text(
      data = plot_data %>% distinct(nature, total),
      aes(x = total + max(plot_data$total) * 0.02, y = nature,
          label = paste0("n=", scales::comma(total))),
      inherit.aes = FALSE,
      hjust = 0, size = 2.8, colour = "grey40"
    ) +
    coord_cartesian(clip = "off") +
    labs(title = title, subtitle = subtitle, x = "Number of calls", y = NULL) +
    theme_outcome() +
    guides(fill = guide_legend(nrow = 3, byrow = TRUE))
}


# ── 7. Per-nature histogram factory ───────────────────────────────────────────
# Vertical bar chart (histogram) for a single call nature.
# Outcomes are shown individually on the x-axis, ordered by frequency.
# Both proportion and absolute count variants share the same helper.

plot_single_nature_hist <- function(data_one_nature, nature_label,
                                    agency_label, mode = c("prop", "abs"),
                                    n_outcomes_collapse = 8) {
  mode <- match.arg(mode)
  
  # Collapse rare outcomes into "Other / Rare"
  top_outcomes <- data_one_nature %>%
    slice_max(if (mode == "prop") prop else n, n = n_outcomes_collapse) %>%
    pull(outcome) %>%
    as.character()
  
  plot_data <- data_one_nature %>%
    mutate(
      outcome_lumped = fct_other(as.character(outcome),
                                 keep        = top_outcomes,
                                 other_level = "Other / Rare")
    ) %>%
    group_by(outcome_lumped) %>%
    summarise(
      prop  = sum(prop),
      n     = sum(n),
      total = first(total),
      .groups = "drop"
    ) %>%
    # Order bars: most frequent first, "Other / Rare" always last
    mutate(
      is_other      = outcome_lumped == "Other / Rare",
      outcome_lumped = fct_reorder(outcome_lumped,
                                   if (mode == "prop") prop else n,
                                   .desc = TRUE) %>%
        fct_relevel("Other / Rare", after = Inf)
    )
  
  n_cols <- n_distinct(plot_data$outcome_lumped)
  pal    <- colorRampPalette(RColorBrewer::brewer.pal(8, "Set2"))(n_cols)
  
  total_n <- unique(plot_data$total)
  
  if (mode == "prop") {
    p <- ggplot(plot_data, aes(x = outcome_lumped, y = prop, fill = outcome_lumped)) +
      geom_col(width = 0.7, colour = "white", linewidth = 0.3) +
      geom_text(aes(label = percent(prop, accuracy = 0.1)),
                vjust = -0.4, size = 2.8, colour = "grey30") +
      scale_y_continuous(
        labels = percent_format(accuracy = 1),
        expand = expansion(mult = c(0, 0.12))
      ) +
      labs(
        title    = nature_label,
        subtitle = paste0(agency_label, " · n = ", scales::comma(total_n),
                          " · proportion of calls"),
        x        = NULL,
        y        = "Share of calls"
      )
  } else {
    p <- ggplot(plot_data, aes(x = outcome_lumped, y = n, fill = outcome_lumped)) +
      geom_col(width = 0.7, colour = "white", linewidth = 0.3) +
      geom_text(aes(label = scales::comma(n)),
                vjust = -0.4, size = 2.8, colour = "grey30") +
      scale_y_continuous(
        labels = scales::comma,
        expand = expansion(mult = c(0, 0.12))
      ) +
      labs(
        title    = nature_label,
        subtitle = paste0(agency_label, " · n = ", scales::comma(total_n),
                          " · absolute counts"),
        x        = NULL,
        y        = "Number of calls"
      )
  }
  
  p +
    scale_fill_manual(values = pal) +
    theme_outcome_hist()
}


# ── 8. Save per-nature histograms ─────────────────────────────────────────────

save_per_nature_plots <- function(data, agency_label, out_dir,
                                  mode = c("prop", "abs")) {
  mode <- match.arg(mode)
  if (!file.exists(out_dir)) base::dir.create(out_dir, recursive = TRUE)
  
  natures <- as.character(unique(data$nature))
  
  for (nat in natures) {
    data_nat <- data %>% filter(as.character(nature) == nat)
    
    p <- plot_single_nature_hist(
      data_one_nature = data_nat,
      nature_label    = nat,
      agency_label    = agency_label,
      mode            = mode
    )
    
    safe_name <- nat %>%
      str_to_lower() %>%
      str_replace_all("[^a-z0-9]+", "_") %>%
      str_remove("_+$")
    
    ggsave(
      filename = file.path(out_dir, paste0(safe_name, ".png")),
      plot     = p,
      width    = 7, height = 4, dpi = 180
    )
  }
  
  message("Saved ", length(natures), " plots to: ", out_dir)
}


# ── 9. Render overview plots ──────────────────────────────────────────────────

p_cahoots <- plot_outcome_composition(
  data     = cahoots_df,
  title    = "CAHOOTS call outcome by nature",
  subtitle = "Top 14 call natures + Other · proportion of calls per outcome category"
)

p_mcslc <- plot_outcome_composition(
  data     = mcslc_df,
  title    = "MCS LC call outcome by nature",
  subtitle = "All 14 call natures · proportion of calls per outcome category"
)

p_cahoots_abs <- plot_outcome_composition_abs(
  data     = cahoots_df,
  title    = "CAHOOTS call outcome by nature (absolute)",
  subtitle = "Top 14 call natures + Other · count of calls per outcome category"
)

p_mcslc_abs <- plot_outcome_composition_abs(
  data     = mcslc_df,
  title    = "MCS LC call outcome by nature (absolute)",
  subtitle = "All 14 call natures · count of calls per outcome category"
)

print(p_cahoots)
print(p_mcslc)
print(p_cahoots_abs)
print(p_mcslc_abs)

ggsave("figures/cahoots_outcome_composition.png",     p_cahoots,     width = 10, height = 5, dpi = 180)
ggsave("figures/mcslc_outcome_composition.png",       p_mcslc,       width = 10, height = 5, dpi = 180)
ggsave("figures/cahoots_outcome_composition_abs.png", p_cahoots_abs, width = 10, height = 5, dpi = 180)
ggsave("figures/mcslc_outcome_composition_abs.png",   p_mcslc_abs,   width = 10, height = 5, dpi = 180)


# ── 10. Save per-nature histograms ────────────────────────────────────────────

# Proportions
save_per_nature_plots(cahoots_df, agency_label = "CAHOOTS",
                      out_dir = "figures/per_nature/cahoots/prop", mode = "prop")

save_per_nature_plots(mcslc_df,  agency_label = "MCS LC",
                      out_dir = "figures/per_nature/mcslc/prop",   mode = "prop")

# Absolute counts
save_per_nature_plots(cahoots_df, agency_label = "CAHOOTS",
                      out_dir = "figures/per_nature/cahoots/abs", mode = "abs")

save_per_nature_plots(mcslc_df,  agency_label = "MCS LC",
                      out_dir = "figures/per_nature/mcslc/abs",   mode = "abs")




# ── 11. Per-nature histogram: décomposition complète de Other/Rare ─────────────
# Pour chaque nature, trace un histogramme de TOUS les outcomes exclus du top N,
# en valeur absolue. Ne génère un fichier que si Other/Rare existe réellement.

plot_single_nature_other <- function(data_one_nature, nature_label,
                                     agency_label, n_outcomes_collapse = 8) {
  top_outcomes <- data_one_nature %>%
    slice_max(n, n = n_outcomes_collapse) %>%
    pull(outcome) %>%
    as.character()
  
  plot_data <- data_one_nature %>%
    filter(!as.character(outcome) %in% top_outcomes) %>%
    mutate(outcome = fct_reorder(as.character(outcome), n, .desc = TRUE))
  
  if (nrow(plot_data) == 0) return(invisible(NULL))
  
  n_cols <- n_distinct(plot_data$outcome)
  pal    <- colorRampPalette(RColorBrewer::brewer.pal(8, "Pastel2"))(n_cols)
  
  ggplot(plot_data, aes(x = outcome, y = n, fill = outcome)) +
    geom_col(width = 0.7, colour = "white", linewidth = 0.3) +
    geom_text(aes(label = scales::comma(n)),
              vjust = -0.4, size = 2.8, colour = "grey30") +
    scale_y_continuous(
      labels = scales::comma,
      expand = expansion(mult = c(0, 0.14))
    ) +
    scale_fill_manual(values = pal) +
    labs(
      title    = paste0(nature_label, ", Other / Rare breakdown"),
      subtitle = paste0(agency_label, " · outcomes outside top ",
                        n_outcomes_collapse, " · absolute counts"),
      x        = NULL,
      y        = "Number of calls"
    ) +
    theme_outcome_hist() +
    theme(plot.margin = margin(t = 5, r = 10, b = 40, l = 5, unit = "pt"))  # ← marge basse augmentée
}

save_per_nature_other_plots <- function(data, agency_label, out_dir,
                                        n_outcomes_collapse = 8) {
  if (!file.exists(out_dir)) base::dir.create(out_dir, recursive = TRUE)
  
  natures <- as.character(unique(data$nature))
  n_saved <- 0
  
  for (nat in natures) {
    data_nat <- data %>% filter(as.character(nature) == nat)
    
    p <- plot_single_nature_other(
      data_one_nature     = data_nat,
      nature_label        = nat,
      agency_label        = agency_label,
      n_outcomes_collapse = n_outcomes_collapse
    )
    
    if (is.null(p)) next
    
    safe_name <- nat %>%
      str_to_lower() %>%
      str_replace_all("[^a-z0-9]+", "_") %>%
      str_remove("_+$")
    
    ggsave(
      filename = file.path(out_dir, paste0(safe_name, "_other_breakdown.png")),
      plot     = p,
      width    = 8, height = 4.5, dpi = 180  # ← légèrement plus large et plus haut
    )
    n_saved <- n_saved + 1
  }
  
  message("Saved ", n_saved, " breakdown plots to: ", out_dir)
}

# ── 12. Appels ────────────────────────────────────────────────────────────────

save_per_nature_other_plots(cahoots_df, agency_label = "CAHOOTS",
                            out_dir = "figures/per_nature/cahoots/other_breakdown")

save_per_nature_other_plots(mcslc_df,  agency_label = "MCS LC",
                            out_dir = "figures/per_nature/mcslc/other_breakdown")




glimpse(data)

















