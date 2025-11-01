# ============================
# Police-pursuit analysis (FARS via rfars)
# Clean, end-to-end script
# ============================

suppressPackageStartupMessages({
  library(rfars)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(ggrepel)
  library(purrr)
  library(scales)
})

# ---- 1) Parameters ----
yrs <- 2014:2023
metrics <- c("crashes", "injuries", "fatalities")
metric_labels <- c(crashes = "Crashes", injuries = "Injuries", fatalities = "Deaths")

# ---- 2) Aesthetics (global) ----
theme_ian <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title        = element_text(face = "bold", size = rel(1.2)),
      plot.subtitle     = element_text(margin = margin(b = 6)),
      plot.caption      = element_text(color = "grey40"),
      legend.position   = "top",
      legend.title      = element_blank(),
      axis.title.y      = element_text(margin = margin(r = 8)),
      panel.grid.minor  = element_blank(),
      panel.grid.major.x= element_blank()
    )
}
pal <- c(Crashes = "#0072B2", Injuries = "#E69F00", Deaths = "#D55E00")

# ---- 3) LIVE Data acquisition ----
fars <- get_fars(
  years   = yrs,
  source  = "zenodo",
  proceed = TRUE
)

# ---- 4) Small helpers ----
# one_count(): wrapper around rfars::counts that returns tidy long format
one_count <- function(df, what, interval = c("year","month")[1], states = "all") {
  counts(
    df       = df,
    interval = interval,
    what     = what,
    where    = list(states = states, region = "all", urb = "all"),
    who      = "all",
    involved = "police pursuit"
  ) %>%
    transmute(
      !!interval := .data[[interval]],
      Metric = metric_labels[[what]],
      Count  = .data$n
    )
}

# combine_counts(): vectorized over metrics
combine_counts <- function(df, metrics, interval, states = "all") {
  map_dfr(metrics, ~one_count(df, what = .x, interval = interval, states = states)) %>%
    mutate(
      Metric = factor(Metric, levels = c("Crashes","Injuries","Deaths"))
    )
}

one_count <- function(df, what_arg, interval = c("year","month")[1], states = "all") {
  rfars::counts(df, interval, what = what_arg,
                where = list(states = states, region = "all", urb = "all"),
                who = "all", involved = "police pursuit") %>%
    transmute(
      !!interval := .data[[interval]],
      Metric     = metric_labels[[what_arg]],
      Count      = n
    )
}

# ---- 5) Build national panels ----
nat_annual_long  <- combine_counts(fars, metrics, interval = "year",  states = "all") %>%
  filter(year %in% yrs) %>%
  mutate(lbl = comma(Count)) %>%
  arrange(Metric, year)

nat_annual <- nat_annual_long |>
  pivot_wider(names_from = Metric, values_from = Count) |>
  rename(crashes = Crashes, injuries = Injuries, fatalities = Deaths)

nat_monthly_long <- combine_counts(fars, metrics, interval = "month", states = "all") %>%
  # If multiple years are present in the source, this is already aggregated by month
  mutate(month = factor(month, levels = month.abb, ordered = TRUE)) %>%
  group_by(month, Metric) %>%
  summarise(Count = sum(Count), .groups = "drop") %>%
  mutate(lbl = comma(Count)) %>%
  arrange(Metric, month)

# ---- 6) National plots ----
plot_nat_annual <- function(dat_long = nat_annual_long, years = yrs) {
  ggplot(dat_long, aes(x = year, y = Count, color = Metric, group = Metric)) +
    geom_line(linewidth = 1.1) +
    geom_point(size = 2) +
    geom_text_repel(
      aes(label = lbl),
      size = 3.3, show.legend = FALSE, seed = 123,
      box.padding = 0.3, point.padding = 0.2,
      max.overlaps = Inf, min.segment.length = 0.05
    ) +
    scale_color_manual(values = pal) +
    scale_x_continuous(breaks = years, expand = expansion(mult = c(0.01, 0.06))) +
    scale_y_continuous(labels = label_comma(), expand = expansion(mult = c(0.05, 0.10))) +
    labs(
      title    = "Police-pursuit incidents and harms in the United States",
      subtitle = paste0("Annual counts, ", min(years), "–", max(years),
                        " (FARS via rfars; police pursuit-involved)"),
      x = NULL, y = "Count",
      caption  = "Source: NHTSA FARS, tabulated with the rfars package"
    ) +
    theme_ian()
}

plot_nat_monthly <- function(dat_long = nat_monthly_long) {
  end_labels <- dat_long %>% filter(month == factor("Dec", levels = month.abb, ordered = TRUE))
  ggplot(dat_long, aes(x = month, y = Count, color = Metric, group = Metric)) +
    geom_line(linewidth = 1.1) +
    geom_point(size = 2) +
    geom_text(
      data = end_labels,
      aes(label = lbl),
      hjust = 0, nudge_x = 0.25, size = 3.4, show.legend = FALSE
    ) +
    scale_color_manual(values = pal) +
    scale_x_discrete(expand = expansion(mult = c(0.01, 0.08))) +
    scale_y_continuous(labels = label_comma()) +
    labs(
      title    = "Police-pursuit seasonality in the United States",
      subtitle = "Monthly counts aggregated over 2014–2023 (FARS via rfars; pursuit-involved)",
      x = NULL, y = "Count",
      caption  = "Source: NHTSA FARS, tabulated with the rfars package"
    ) +
    theme_ian()
}

# ---- 7) State-year panel & plot ----
.state_name_check <- function(fars, state) {
  valid <- sort(unique(fars$multi_acc$state))
  if (!state %in% valid)
    stop("`state` not recognized. Try one of: ", paste(valid, collapse = ", "))
  invisible(TRUE)
}

state_annual_counts <- function(fars, state, years = yrs) {
  .state_name_check(fars, state)
  combine_counts(fars, metrics, interval = "year", states = state) %>%
    filter(year %in% years) %>%
    mutate(lbl = comma(Count)) %>%
    arrange(Metric, year)
}

plot_pursuit_state <- function(fars, state, years = yrs, overlay_national = FALSE) {
  dat_state <- state_annual_counts(fars, state, years)
  
  p_nat <- NULL
  if (isTRUE(overlay_national)) {
    nat_ref <- nat_annual_long %>%
      filter(year %in% years)
    p_nat <- geom_line(
      data = nat_ref,
      aes(x = year, y = Count, group = Metric),
      color = "grey70", linewidth = 0.7, linetype = "11"
    )
  }
  
  ggplot(dat_state, aes(x = year, y = Count, color = Metric, group = Metric)) +
    p_nat +
    geom_line(linewidth = 1.1) +
    geom_point(size = 2) +
    geom_text_repel(
      aes(label = lbl),
      size = 3.3, show.legend = FALSE, seed = 123,
      box.padding = 0.3, point.padding = 0.2,
      max.overlaps = Inf, min.segment.length = 0.05
    ) +
    scale_color_manual(values = pal) +
    scale_x_continuous(breaks = years, expand = expansion(mult = c(0.01, 0.06))) +
    scale_y_continuous(labels = label_comma(), expand = expansion(mult = c(0.05, 0.10))) +
    labs(
      title    = paste0("Police-pursuit incidents and harms — ", state),
      subtitle = paste0("Annual counts, ", min(years), "–", max(years),
                        " (FARS via rfars; police pursuit-involved)"),
      x = NULL, y = "Count",
      caption  = "Source: NHTSA FARS, tabulated with the rfars package"
    ) +
    theme_ian()
}

# ---- 8) Outputs (objects & examples) ----
print(nat_annual)          # wide annual table
p_annual  <- plot_nat_annual(nat_annual_long, years = yrs)
p_monthly <- plot_nat_monthly(nat_monthly_long)

# Example state uses:
p_sc  <- plot_pursuit_state(fars, "South Carolina")
p_ca  <- plot_pursuit_state(fars, "California", overlay_national = TRUE)
p_fl  <- plot_pursuit_state(fars, "Florida", years = 2018:2023)

# ---- 9) Save (high-res) ----
# ggsave("output/pursuit_nat_annual.png",  p_annual,  width = 8, height = 4.8, dpi = 600)
# ggsave("output/pursuit_nat_monthly.png", p_monthly, width = 8, height = 4.8, dpi = 600)
# ggsave("output/pursuit_SC.png",          p_sc,      width = 8, height = 4.8, dpi = 600)
# ggsave("output/pursuit_CA_overlay.png",  p_ca,      width = 8, height = 4.8, dpi = 600)
# ggsave("output/pursuit_FL_2018_2023.png",p_fl,      width = 8, height = 4.8, dpi = 600)

# ============================
# Batch export: national + every state
# ============================
# ---- Safer slug: always coerce to character ----
.slug <- function(x) {
  x <- as.character(x)
  x <- enc2utf8(x)
  x <- gsub("[^A-Za-z0-9]+", "_", x, perl = TRUE)
  x <- gsub("^_+|_+$", "", x, perl = TRUE)
  tolower(x)
}

.save_plot_dual <- function(plot, file_base, width = 8, height = 5, dpi = 600) {
  ggsave(paste0(file_base, ".png"), plot, width = width, height = height, dpi = dpi)
  ggsave(paste0(file_base, ".pdf"), plot, width = width, height = height, useDingbats = FALSE)
}

export_all_state_plots <- function(
    fars,
    years            = yrs,
    overlay_national = FALSE,
    out_root         = "output",
    width            = 8,
    height           = 5,
    dpi              = 600
) {
  states_dir   <- file.path(out_root, "states")
  national_dir <- file.path(out_root, "national")
  dir.create(states_dir,   recursive = TRUE, showWarnings = FALSE)
  dir.create(national_dir, recursive = TRUE, showWarnings = FALSE)
  
  # National
  p_nat <- plot_nat_annual(nat_annual_long, years = years)
  .save_plot_dual(p_nat, file.path(national_dir, "pursuit_national"), width, height, dpi)
  
  # States present (force character, drop NA/empty, sorted)
  states <- fars$multi_acc$state
  states <- sort(unique(as.character(states)))
  states <- states[!is.na(states) & nzchar(states)]
  
  # Save each state; continue on error
  purrr::walk(states, function(st) {
    base <- file.path(states_dir, paste0("pursuit_", .slug(st)))
    # try to render; skip if anything goes wrong
    try({
      p_st <- plot_pursuit_state(fars, state = st, years = years, overlay_national = overlay_national)
      .save_plot_dual(p_st, base, width, height, dpi)
    }, silent = TRUE)
  })
  
  message("Export complete: ",
          length(states), " state plots to ", normalizePath(states_dir),
          " and national plot to ", normalizePath(national_dir))
}


# ===== Example call =====
# Creates ./output/national/pursuit_national.(png|pdf)
# and ./output/states/pursuit_<STATE>.(png|pdf) for every state in FARS.
export_all_state_plots(fars, years = yrs, overlay_national = FALSE, out_root = "output")

