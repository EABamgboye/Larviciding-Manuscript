# ============================================================
# Kernel Dispersal Scale Visualization — Stratified by site & season
# Panels: (A) AIC, (B) OR + CI ribbon, (C) p-value trace
# FIX 1: colour = study_source (not site) throughout
# FIX 2: no trailing comma in season_linetypes
# ============================================================
#loadpath
user <- Sys.getenv("USERNAME")
Drive <- file.path(gsub("[\\]", "/", gsub("Documents", "", Sys.getenv("HOME"))))
shapefileDir <- "C:/Users/ebamg/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/kano_ibadan"
Entodir <- "C:/Users/ebamg/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/kano_ibadan/kano_ibadan_ento/LManuscript"
Lavplotsdir <- "C:/Users/ebamg/Urban Malaria Proj Dropbox/urban_malaria/projects/Manuscripts/ongoing/Larviciding Manuscript/New Manuscript Sections"

##Office
user <- Sys.getenv("USERNAME")
Drive <- file.path(gsub("[\\]", "/", gsub("Documents", "", Sys.getenv("HOME"))))
shapefileDir <- "C:/Users/ebamgboye/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/kano_ibadan"
Entodir <- "C:/Users/ebamgboye/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/kano_ibadan/kano_ibadan_ento/LManuscript"
Lavplotsdir <- "C:/Users/ebamgboye/Urban Malaria Proj Dropbox/urban_malaria/projects/Manuscripts/ongoing/Larviciding Manuscript/New Manuscript Sections"

##load packages and themes
source("functions.R")

##load extra packages
library(readxl)
library(corrplot)
library(ggspatial)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggrepel)
library(ggspatial)
library(sf)
library(scales)
library(broom)
library(tibble)
library(rnaturalearth)
library(rnaturalearthdata)
library(patchwork)
library(readr)
library(stringr)

#kernel_data
##Agugu
kernel_results_a <- read_csv(file.path(Lavplotsdir, "kernel_results_agugu.csv"))

##Challenge
kernel_results_c <- read_csv(file.path(Lavplotsdir, "kernel_results_challenge.csv"))
kernel_results_c_d <- read_csv(file.path(Lavplotsdir, "kernel_results_drychallenge.csv"))


kernel_final <- bind_rows(kernel_results_a, kernel_results_c, kernel_results_c_d)

# ── Drop Agugu_combined (wide estimates) ─────────────────────
kernel_final <- kernel_final %>%
  dplyr::filter(study_source != "Agugu_combined") %>%
  mutate(
    study_source = trimws(study_source),
    season       = trimws(season)
  )

##Subset data into bins
##First 50m
kernel_final_50 <- kernel_final %>% 
  dplyr::filter(lambda %in% 2:40)

## 50 - 100m

##100-200m


## 200- 300m


##300-500m
# ── Colour and linetype schemes ───────────────────────────────
# colour keys must exactly match unique(kernel_final$study_source)
# linetype keys must exactly match unique(kernel_final$season)

site_colours <- c(
  "Agugu_dry"     = "#facba8",
  "Agugu_wet"     = "#facba8",
  "Challenge_dry" = "#f37363",
  "Challenge_wet" = "#f37363"
)

season_linetypes <- c(
  "Wet" = "solid",
  "Dry" = "dashed"
)
kernel_final_50 <- kernel_final_50 %>%
  mutate(season = str_to_title(season))  # converts "dry" → "Dry"

# Verify
unique(kernel_final_50$season)
# Should now show only: "Wet" "Dry"

# ── Quick check — will error if keys don't match ──────────────
stopifnot(all(unique(kernel_final_50$study_source) %in% names(site_colours)))
stopifnot(all(unique(kernel_final_50$season)        %in% names(season_linetypes)))

# ── Best-fitting scale per stratum (minimum AIC) ─────────────
best_fits <- kernel_final_50 %>%
  group_by(study_source, season) %>%
  slice_min(AIC, n = 1) %>%
  ungroup()

print(best_fits %>%
        dplyr::select(study_source, lambda, AIC, OR, OR_low95, OR_high95, p_value))

# ── Shared theme ──────────────────────────────────────────────
theme_dispersal <- function() {
  theme_classic(base_size = 12) +
    theme(
      plot.title         = element_text(face = "bold", size = 12),
      plot.subtitle      = element_text(size = 9.5, color = "grey40"),
      axis.title         = element_text(size = 11),
      panel.grid.major.y = element_line(color = "grey92", linewidth = 0.4),
      legend.position    = "bottom",
      legend.title       = element_blank(),
      legend.key.width   = unit(1.8, "cm")
    )
}

# ── Shared scales ─────────────────────────────────────────────
shared_colour   <- scale_colour_manual(values = site_colours,
                                       breaks = names(site_colours))
shared_fill     <- scale_fill_manual(values = site_colours,
                                     breaks = names(site_colours))
shared_linetype <- scale_linetype_manual(values = season_linetypes,
                                         breaks = names(season_linetypes))

# ══════════════════════════════════════════════════════════════
# PANEL A — AIC vs dispersal scale
# ══════════════════════════════════════════════════════════════
p_aic <- ggplot(kernel_final_50,
                aes(x = lambda, y = AIC,
                    colour   = study_source,   # ← study_source not site
                    linetype = season,
                    group    = study_source)) +
  geom_line(linewidth = 0.9) +
  geom_point(data = best_fits,
             aes(x = lambda, y = AIC, colour = study_source),
             size = 3, shape = 21, fill = "white", stroke = 1.5,
             show.legend = FALSE) +
  geom_vline(data = best_fits,
             aes(xintercept = lambda, colour = study_source, linetype = season),
             linewidth = 0.5, alpha = 0.5, show.legend = FALSE) +
  # ── Add lambda value labels at the top of each vline ──────────
  geom_text(data = best_fits,
            aes(x     = lambda,
                y     = max(kernel_final$AIC) * 0.8,  # just above the highest AIC line
                label = paste0("λ=", lambda, "m"),
                colour = study_source),
            angle         = 90,        # rotate to align with vline
            hjust         = 0.2,         # left-align text
            vjust         = 0.5,
            size          = 3,
            show.legend   = FALSE,
            inherit.aes   = FALSE) +
  shared_colour +
  shared_linetype +
  guides(
    colour   = guide_legend(override.aes = list(linewidth = 1.2)),
    linetype = guide_legend(override.aes = list(linewidth = 1.2))
  ) +
  labs(
    title    = "A. Model fit (AIC) across dispersal scales",
    subtitle = "Lower AIC = better fit | Points = best-fitting scale per stratum",
    x = NULL,
    y = "AIC"
  ) +
  theme_dispersal()

print(p_aic)
# ══════════════════════════════════════════════════════════════
# PANEL B — OR + 95% CI ribbon vs dispersal scale
# ══════════════════════════════════════════════════════════════
p_or <- ggplot(kernel_final,
               aes(x = lambda,
                   colour   = study_source,   # ← study_source not site
                   linetype = season,
                   group    = study_source)) +
  geom_ribbon(aes(ymin = OR_low95, ymax = OR_high95, fill = study_source),
              alpha = 0.12, colour = NA, show.legend = FALSE) +
  geom_hline(yintercept = 1, linetype = "dashed",
             colour = "#4DA6E8", linewidth = 0.8) +
  geom_line(aes(y = OR), linewidth = 0.9) +
  geom_point(data = best_fits,
             aes(x = lambda, y = OR, colour = study_source),
             size = 3, shape = 21, fill = "white", stroke = 1.5,
             show.legend = FALSE) +
  coord_cartesian(ylim = c(0.3, 3)) +   # ← clips without removing data
  geom_text(data = best_fits,
            aes(x     = lambda,
                y     = max(kernel_final$OR) * 0.8,  # just above the highest AIC line
                label = paste0("OR=", sprintf("%.2f", OR)),
                colour = study_source),
            angle         = 90,        # rotate to align with vline
            hjust         = 0.2,         # left-align text
            vjust         = 0.9,
            size          = 3,
            show.legend   = FALSE,
            inherit.aes   = FALSE) +
  shared_colour +
  shared_fill +
  shared_linetype +
  guides(
    colour   = guide_legend(override.aes = list(linewidth = 1.2)),
    linetype = guide_legend(override.aes = list(linewidth = 1.2))
  ) +
  labs(
    title    = "B. Odds ratio across dispersal scales",
    subtitle = "Shaded band = 95% CI | Blue dashed = OR 1 | Points = best-fitting scale",
    x = NULL,
    y = "Odds ratio for malaria"
  ) +
  theme_dispersal()

# ══════════════════════════════════════════════════════════════
# PANEL C — p-value trace
# ══════════════════════════════════════════════════════════════
p_pval <- ggplot(kernel_final,
                 aes(x = lambda, y = p_value,
                     colour   = study_source,   # ← study_source not site
                     linetype = season,
                     group    = study_source)) +
  geom_hline(yintercept = 0.05, linetype = "dotted",
             colour = "grey50", linewidth = 0.8) +
  annotate("text", x = 480, y = 0.07,
           label = "p = 0.05", size = 3.2, colour = "grey40") +
  geom_line(linewidth = 0.9) +
  geom_point(data = best_fits,
             aes(x = lambda, y = p_value, colour = study_source),
             size = 3, shape = 21, fill = "white", stroke = 1.5,
             show.legend = FALSE) +
  shared_colour +
  shared_linetype +
  guides(
    colour   = guide_legend(override.aes = list(linewidth = 1.2)),
    linetype = guide_legend(override.aes = list(linewidth = 1.2))
  ) +
  scale_y_continuous(limits = c(0, 1),
                     breaks = c(0, 0.05, 0.25, 0.5, 0.75, 1)) +
  labs(
    title    = "C. Statistical significance across dispersal scales",
    subtitle = "Dotted line = p 0.05 threshold",
    x = "Assumed mosquito dispersal scale (metres)",
    y = "p-value"
  ) +
  theme_dispersal()

# ══════════════════════════════════════════════════════════════
# COMBINED FIGURE
# ══════════════════════════════════════════════════════════════
combined <- (p_aic / p_or) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

combined <- combined +
  plot_annotation(
    title    = "Kernel dispersal scale fitting — all sites and seasons",
    subtitle = "Colour = study stratum | Line type = season (Wet / Dry)",
    theme = theme(
      plot.title    = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(size = 10.5, colour = "grey30")
    )
  )

print(combined)
# ── Save ──────────────────────────────────────────────────────
ggsave("kernel_dispersal_stratified.pdf", combined,
       width = 9, height = 12, dpi = 300, bg = "white")

message("Saved: kernel_dispersal_stratified.png")

# ── Best-fit summary table ────────────────────────────────────
cat("\n── Best-fitting dispersal scales ──\n")
best_fits %>%
  dplyr::select(study_source, lambda, OR, OR_low95, OR_high95, p_value, AIC) %>%
  mutate(across(where(is.numeric), \(x) round(x, 3))) %>%
  print()



###------------------------------------------------------------###
# ── Bin definitions ───────────────────────────────────────────
bins <- list(
  "A. 2–40m"    = 2:50,
  "B. 40–100m"  = 40:100,
  "C. 100–200m" = 100:200,
  "D. 200–500m" = 200:500
)

# ── Function to build one AIC panel ──────────────────────────
make_aic_panel <- function(lambda_range, panel_title) {
  
  # Subset and clean
  df <- kernel_final %>%
    dplyr::filter(lambda %in% lambda_range) %>%
    mutate(season = str_to_title(season))
  
  # Best fit per stratum
  best <- df %>%
    group_by(study_source, season) %>%
    slice_min(AIC, n = 1) %>%
    ungroup()
  
  # Build plot
  ggplot(df,
         aes(x        = lambda,
             y        = AIC,
             colour   = study_source,
             linetype = season,
             group    = study_source)) +
    geom_line(linewidth = 0.9) +
    geom_point(data = best,
               aes(x = lambda, y = AIC, colour = study_source),
               size = 3, shape = 21, fill = "white", stroke = 1.5,
               show.legend = FALSE) +
    geom_vline(data = best,
               aes(xintercept = lambda, colour = study_source, linetype = season),
               linewidth = 0.5, alpha = 0.5, show.legend = FALSE) +
    geom_text(data = best,
              aes(x      = lambda,
                  y      = max(df$AIC) * 0.95,
                  label  = paste0("λ=", lambda, "m"),
                  colour = study_source),
              angle       = 90,
              hjust       = 0.2,
              vjust       = 0.5,
              size        = 3,
              show.legend = FALSE,
              inherit.aes = FALSE) +
    shared_colour +
    shared_linetype +
    guides(
      colour   = guide_legend(override.aes = list(linewidth = 1.2)),
      linetype = guide_legend(override.aes = list(linewidth = 1.2))
    ) +
    labs(
      title    = panel_title,
      subtitle = "Lower AIC = better fit | Points = best-fitting scale per stratum",
      x        = "Dispersal scale (λ, metres)",
      y        = "AIC"
    ) +
    theme_dispersal()
}

# ── Generate all four panels ──────────────────────────────────
panels <- purrr::imap(bins, ~ make_aic_panel(.x, .y))

# ── Combine into one figure using patchwork ───────────────────
library(patchwork)

combined_plot <- (panels[[1]] | panels[[2]]) /
  (panels[[3]] | panels[[4]]) +
  plot_layout(guides = "collect") &        # single shared legend
  theme(legend.position = "bottom")

# ── Print and save ────────────────────────────────────────────
print(combined_plot)

ggsave("AIC_dispersal_panels.pdf",
       plot   = combined_plot,
       width  = 14,
       height = 10,
       dpi    = 300)

kernel_final %>%
  dplyr::filter(lambda %in% 2:500) %>%
  group_by(study_source) %>%
  summarise(
    AIC_min   = min(AIC),
    AIC_max   = max(AIC),
    delta_AIC = max(AIC) - min(AIC)
  )
