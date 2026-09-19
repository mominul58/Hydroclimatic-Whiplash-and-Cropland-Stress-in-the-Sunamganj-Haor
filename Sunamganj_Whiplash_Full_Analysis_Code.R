# ==============================================================================
# SUNAMGANJ HYDROCLIMATIC WHIPLASH — FULL Q1-JOURNAL ANALYSIS PIPELINE
# Version 6: complete 42-grid static maps + exact 10-km scale bar + 0.20-degree map ticks + panel headings
# ==============================================================================
# Main scientific question:
#   Do antecedent dry conditions and rapid dry-to-wet transitions amplify
#   post-event cropland vegetation shock during extreme wet events in the
#   Sunamganj Haor region?
#
# IMPORTANT DATA-QUALITY LOGIC
#   * dry_* / WTS are structurally undefined for non-whiplash rows. DO NOT impute.
#   * Sentinel-1 flood_frac is only available for a later-period subset. It is
#     used as secondary physical validation, not required for the primary model.
#   * If the same regional event/grid occurs more than once, the row with the
#     highest rain7_z is retained as the event-grid peak. An audit CSV is saved.
#   * Models and expensive sensitivity calculations are cached as RDS. Re-runs
#     load cached objects unless FORCE_REFIT <- TRUE.
#
# MAIN OUTPUTS
#   8 manuscript figures (PNG/TIFF 600 dpi + vector PDF)
#   manuscript tables (CSV + XLSX)
#   model RDS cache
#   cleaned analysis dataset
#   QA reports, diagnostics, and reproducibility log
# ==============================================================================

# ==============================================================================
# 0. USER SETTINGS
# ==============================================================================
ROOT_DIR <- "D:/Draft/08_Sunamganj_Hydroclimatic_Whiplash"
STUDY_SHP <- "D:/Draft/08_Sunamganj_Hydroclimatic_Whiplash/study_area/Sunamganj.shp"
GRID_GPKG <- file.path(ROOT_DIR, "01_grid", "Sunamganj_10km_grid.gpkg")
MASTER_PARQUET <- file.path(ROOT_DIR, "07_master_dataset", "Sunamganj_whiplash_master.parquet")
MASTER_CSV <- file.path(ROOT_DIR, "07_master_dataset", "Sunamganj_whiplash_master.csv")
DAILY_CLIMATE_PARQUET <- file.path(ROOT_DIR, "02_daily_climate",
                                   "daily_hydroclimate_metrics_with_WMO1991_2020_climatology.parquet")
STATIC_GRID_CSV <- file.path(ROOT_DIR, "06_static", "grid_static_variables.csv")
EVI_RESPONSE_PARQUET <- file.path(ROOT_DIR, "04_modis_crop", "event_EVI_response.parquet")
EVI_RESPONSE_CSV <- file.path(ROOT_DIR, "04_modis_crop", "event_EVI_response.csv")

OUT_DIR <- file.path(ROOT_DIR, "08_analysis")
FIG_DIR <- file.path(OUT_DIR, "Figures")
TAB_DIR <- file.path(OUT_DIR, "Tables")
CACHE_DIR <- file.path(OUT_DIR, "Model_RDS_Cache")
DERIVED_DIR <- file.path(OUT_DIR, "Derived_Data")
DIAG_DIR <- file.path(OUT_DIR, "Diagnostics")
LOG_DIR <- file.path(OUT_DIR, "Logs")
for (d in c(OUT_DIR, FIG_DIR, TAB_DIR, CACHE_DIR, DERIVED_DIR, DIAG_DIR, LOG_DIR)) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

FORCE_REFIT <- FALSE
SEED <- 20260918
set.seed(SEED)

BASE_FAMILY <- "Arial"
BASE_SIZE <- 15  # final manuscript figures: large, bold typography
MAP_CRS <- 4326
PROJECTED_CRS <- 32646

# ==============================================================================
# 1. PACKAGES
# ==============================================================================
required_pkgs <- c(
  "data.table", "dplyr", "tidyr", "purrr", "stringr", "forcats",
  "ggplot2", "patchwork", "scales", "viridis", "sf", "ggspatial",
  "lme4", "lmerTest", "broom", "broom.mixed", "performance",
  "clubSandwich", "fixest", "MatchIt", "cobalt", "mgcv", "ggeffects",
  "Kendall", "trend", "spdep", "openxlsx"
)

install_missing <- function(pkgs) {
  miss <- pkgs[!vapply(pkgs, requireNamespace, FUN.VALUE = logical(1), quietly = TRUE)]
  if (length(miss) > 0) {
    message("Installing missing packages: ", paste(miss, collapse = ", "))
    install.packages(miss, dependencies = TRUE)
  }
}
install_missing(required_pkgs)

suppressPackageStartupMessages({
  library(data.table); library(dplyr); library(tidyr); library(purrr)
  library(stringr); library(forcats); library(ggplot2); library(patchwork)
  library(scales); library(viridis); library(sf); library(ggspatial)
  library(lme4); library(lmerTest); library(broom); library(broom.mixed)
  library(performance); library(clubSandwich); library(fixest)
  library(MatchIt); library(cobalt); library(mgcv); library(ggeffects)
  library(Kendall); library(trend); library(spdep); library(openxlsx)
})

HAS_ARROW <- requireNamespace("arrow", quietly = TRUE)

# ==============================================================================
# 2. GLOBAL HELPERS
# ==============================================================================
`%||%` <- function(x, y) if (!is.null(x)) x else y

cache_get <- function(name, fit_fun, force = FORCE_REFIT) {
  p <- file.path(CACHE_DIR, paste0(name, ".rds"))
  if (file.exists(p) && !force) {
    message("[CACHE] Loading: ", basename(p))
    return(readRDS(p))
  }
  message("[FIT] ", name)
  obj <- fit_fun()
  saveRDS(obj, p, compress = TRUE)
  message("[CACHE] Saved: ", basename(p))
  invisible(gc())
  obj
}

z_safe <- function(x) {
  s <- sd(x, na.rm = TRUE)
  if (!is.finite(s) || s == 0) return(rep(0, length(x)))
  as.numeric((x - mean(x, na.rm = TRUE)) / s)
}

mean_or_na <- function(x) if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
median_or_na <- function(x) if (all(is.na(x))) NA_real_ else median(x, na.rm = TRUE)

fmt_num <- function(x, d = 3) ifelse(is.na(x), NA_character_, formatC(x, digits = d, format = "f"))

save_figure <- function(plot_obj, filename, width = 13.5, height = 9.5) {
  p_png <- file.path(FIG_DIR, paste0(filename, ".png"))
  p_tif <- file.path(FIG_DIR, paste0(filename, ".tiff"))
  p_pdf <- file.path(FIG_DIR, paste0(filename, ".pdf"))
  ggsave(p_png, plot_obj, width = width, height = height, units = "in",
         dpi = 600, bg = "white", limitsize = FALSE)
  ggsave(p_tif, plot_obj, width = width, height = height, units = "in",
         dpi = 600, compression = "lzw", bg = "white", limitsize = FALSE)
  tryCatch(
    ggsave(p_pdf, plot_obj, width = width, height = height, units = "in",
           device = cairo_pdf, bg = "white", limitsize = FALSE),
    error = function(e) ggsave(p_pdf, plot_obj, width = width, height = height,
                               units = "in", bg = "white", limitsize = FALSE)
  )
}

journal_theme <- function(base_size = BASE_SIZE) {
  # Enforce one consistently large typography system even when individual
  # figure blocks request a smaller legacy base_size.
  base_size <- max(base_size, BASE_SIZE)
  theme_bw(base_size = base_size, base_family = BASE_FAMILY) +
    theme(
      text = element_text(face = "bold", size = base_size, family = BASE_FAMILY, colour = "black"),
      plot.title = element_text(face = "bold", size = base_size + 2, hjust = 0, colour = "black"),
      plot.subtitle = element_text(face = "bold", size = base_size, hjust = 0, colour = "black"),
      axis.title = element_text(face = "bold", size = base_size, colour = "black"),
      axis.text = element_text(face = "bold", size = base_size, colour = "black"),
      legend.title = element_text(face = "bold", size = base_size, colour = "black"),
      legend.text = element_text(face = "bold", size = base_size, colour = "black"),
      strip.text = element_text(face = "bold", size = base_size, colour = "black"),
      plot.caption = element_text(face = "bold", size = base_size, colour = "black"),
      panel.grid.minor = element_blank(),
      plot.tag = element_text(face = "bold", size = base_size + 2, colour = "black"),
      plot.margin = margin(8, 8, 8, 8)
    )
}

degE <- function(x) paste0(formatC(x, format = "f", digits = 2), "°E")
degN <- function(x) paste0(formatC(x, format = "f", digits = 2), "°N")

map_theme <- function() {
  journal_theme(BASE_SIZE) +
    theme(
      axis.title = element_text(face = "bold", size = BASE_SIZE, colour = "black"),
      axis.text = element_text(face = "bold", size = BASE_SIZE, colour = "black"),
      legend.title = element_text(face = "bold", size = BASE_SIZE, colour = "black"),
      legend.text = element_text(face = "bold", size = BASE_SIZE, colour = "black"),
      panel.grid.major = element_line(linewidth = 0.30, linetype = "dashed"),
      legend.position = "right"
    )
}

map_coord <- function(study_ll) {
  bb <- st_bbox(study_ll)
  coord_sf(
    crs = st_crs(MAP_CRS),
    xlim = c(bb[["xmin"]], bb[["xmax"]]),
    ylim = c(bb[["ymin"]], bb[["ymax"]]),
    expand = FALSE
  )
}

# Map axes are fixed at 0.20-degree intervals on every manuscript map.
map_breaks_020 <- function(lims) {
  lo <- floor(min(lims, na.rm = TRUE) / 0.20) * 0.20
  hi <- ceiling(max(lims, na.rm = TRUE) / 0.20) * 0.20
  seq(lo, hi, by = 0.20)
}

add_map_axes <- function(p) {
  p +
    scale_x_continuous(breaks = map_breaks_020, labels = degE) +
    scale_y_continuous(breaks = map_breaks_020, labels = degN) +
    labs(x = "Longitude", y = "Latitude")
}

# Add a true 10-km scale bar constructed in UTM 46N, then transform it to
# geographic coordinates for plotting. This avoids an approximate width_hint
# scale. The north arrow and scale are used only on panel (a) of a combined map.
add_north_scale_bl <- function(p, study_ll = study) {
  su <- st_transform(study_ll, PROJECTED_CRS)
  bb <- st_bbox(su)
  w <- as.numeric(bb[["xmax"]] - bb[["xmin"]])
  h <- as.numeric(bb[["ymax"]] - bb[["ymin"]])
  x0 <- as.numeric(bb[["xmin"]] + 0.055 * w)
  y0 <- as.numeric(bb[["ymin"]] + 0.045 * h)
  scale_len <- 10000  # exactly 10 km
  if (x0 + scale_len > bb[["xmax"]] - 0.03 * w) {
    x0 <- as.numeric(bb[["xmax"]] - scale_len - 0.03 * w)
  }
  tick_h <- max(650, 0.018 * h)

  line_sf <- st_sf(geometry = st_sfc(st_linestring(matrix(c(
    x0, y0, x0 + scale_len, y0
  ), ncol = 2, byrow = TRUE)), crs = PROJECTED_CRS)) |> st_transform(MAP_CRS)

  tick1_sf <- st_sf(geometry = st_sfc(st_linestring(matrix(c(
    x0, y0 - tick_h/2, x0, y0 + tick_h/2
  ), ncol = 2, byrow = TRUE)), crs = PROJECTED_CRS)) |> st_transform(MAP_CRS)
  tick2_sf <- st_sf(geometry = st_sfc(st_linestring(matrix(c(
    x0 + scale_len, y0 - tick_h/2, x0 + scale_len, y0 + tick_h/2
  ), ncol = 2, byrow = TRUE)), crs = PROJECTED_CRS)) |> st_transform(MAP_CRS)

  lab_sf <- st_sf(label = "10 km", geometry = st_sfc(
    st_point(c(x0 + scale_len/2, y0 + 1.5 * tick_h)), crs = PROJECTED_CRS
  )) |> st_transform(MAP_CRS)

  p +
    geom_sf(data = line_sf, inherit.aes = FALSE, linewidth = 0.9, color = "black") +
    geom_sf(data = tick1_sf, inherit.aes = FALSE, linewidth = 0.9, color = "black") +
    geom_sf(data = tick2_sf, inherit.aes = FALSE, linewidth = 0.9, color = "black") +
    geom_sf_text(data = lab_sf, aes(label = label), inherit.aes = FALSE,
                 family = BASE_FAMILY, fontface = "bold", size = 5.0) +
    ggspatial::annotation_north_arrow(
      location = "bl", which_north = "true",
      pad_x = grid::unit(0.18, "in"), pad_y = grid::unit(0.76, "in"),
      height = grid::unit(0.52, "in"), width = grid::unit(0.52, "in"),
      style = ggspatial::north_arrow_fancy_orienteering(
        text_family = BASE_FAMILY, text_face = "bold"
      )
    )
}

# Put the panel letter inside the panel heading, e.g.
# "a) Analytical 10-km grid", rather than as a detached patchwork tag.
panel_heading <- function(p, letter) {
  ttl <- p$labels$title %||% ""
  p + labs(title = paste0(letter, ") ", ttl)) +
    theme(plot.title = element_text(face = "bold", size = BASE_SIZE + 2, hjust = 0, colour = "black"))
}

extract_fixed <- function(model, term) {
  tt <- broom.mixed::tidy(model, effects = "fixed", conf.int = TRUE)
  out <- tt[tt$term == term, , drop = FALSE]
  if (nrow(out) == 0) return(NULL)
  out
}

safe_lmer <- function(formula, data) {
  lmerTest::lmer(
    formula, data = data, REML = FALSE,
    control = lme4::lmerControl(
      optimizer = "bobyqa",
      optCtrl = list(maxfun = 200000),
      check.conv.singular = "ignore"
    )
  )
}

# Safely extract a scalar from performance objects that may be a data frame,
# list, or named atomic vector. performance::r2_nakagawa() can return different
# object shapes for singular mixed models, so never use $ directly without checks.
extract_metric_scalar <- function(x, candidates, fallback_single = FALSE) {
  if (is.null(x)) return(NA_real_)

  if (is.data.frame(x) || is.list(x)) {
    for (nm in candidates) {
      val <- tryCatch(x[[nm]], error = function(e) NULL)
      if (!is.null(val) && length(val) > 0 && is.finite(suppressWarnings(as.numeric(val[1])))) {
        return(as.numeric(val[1]))
      }
    }
  }

  if (is.atomic(x)) {
    nms <- names(x)
    if (!is.null(nms)) {
      for (nm in candidates) {
        idx <- which(nms == nm)
        if (length(idx) > 0) {
          val <- suppressWarnings(as.numeric(x[idx[1]]))
          if (is.finite(val)) return(val)
        }
      }
    }
    if (fallback_single && length(x) == 1) {
      val <- suppressWarnings(as.numeric(x[1]))
      if (is.finite(val)) return(val)
    }
  }
  NA_real_
}

safe_model_metrics <- function(mod) {
  singular <- tryCatch(lme4::isSingular(mod, tol = 1e-4), error = function(e) NA)

  # Singular models may only return a marginal R2. Suppress the package warning
  # here because singularity is explicitly retained as a QA field in the table.
  r2_obj <- suppressWarnings(tryCatch(
    performance::r2_nakagawa(mod),
    error = function(e) NULL
  ))
  icc_obj <- suppressWarnings(tryCatch(
    performance::icc(mod),
    error = function(e) NULL
  ))

  r2_m <- extract_metric_scalar(
    r2_obj, c("R2_marginal", "Marginal R2", "R2 Marginal"),
    fallback_single = TRUE
  )
  r2_c <- extract_metric_scalar(
    r2_obj, c("R2_conditional", "Conditional R2", "R2 Conditional"),
    fallback_single = FALSE
  )
  icc_v <- extract_metric_scalar(
    icc_obj, c("ICC_adjusted", "ICC", "ICC_unadjusted"),
    fallback_single = TRUE
  )

  tibble(
    AIC = tryCatch(AIC(mod), error = function(e) NA_real_),
    BIC = tryCatch(BIC(mod), error = function(e) NA_real_),
    logLik = tryCatch(as.numeric(logLik(mod)), error = function(e) NA_real_),
    R2_marginal = r2_m,
    R2_conditional = r2_c,
    ICC = icc_v,
    singular = singular
  )
}

# Tidy helper for fixest robustness models.
tidy_fixest_safe <- function(model) {
  out <- tryCatch(
    broom::tidy(model, conf.int = TRUE),
    error = function(e) NULL
  )
  if (!is.null(out)) return(out)

  sm <- summary(model)
  ct <- as.data.frame(sm$coeftable)
  if (nrow(ct) == 0) return(tibble())
  est <- ct[[1]]
  se_col <- grep("Std", names(ct), value = TRUE)[1]
  stat_col <- grep("t value|z value", names(ct), value = TRUE)[1]
  p_col <- grep("^Pr", names(ct), value = TRUE)[1]
  se <- if (!is.na(se_col)) ct[[se_col]] else rep(NA_real_, length(est))
  tibble(
    term = rownames(ct),
    estimate = est,
    std.error = se,
    statistic = if (!is.na(stat_col)) ct[[stat_col]] else NA_real_,
    p.value = if (!is.na(p_col)) ct[[p_col]] else NA_real_,
    conf.low = est - 1.96 * se,
    conf.high = est + 1.96 * se
  )
}

# ==============================================================================
# 3. READ MASTER DATA + QA
# ==============================================================================
message("\n=== Reading final dataset ===")
if (file.exists(MASTER_PARQUET) && HAS_ARROW) {
  raw <- as.data.frame(arrow::read_parquet(MASTER_PARQUET))
} else if (file.exists(MASTER_CSV)) {
  raw <- fread(MASTER_CSV, data.table = FALSE)
} else {
  stop("Neither master parquet nor master CSV was found.")
}

raw$wet_onset <- as.Date(raw$wet_onset)
if ("dry_date" %in% names(raw)) raw$dry_date <- as.Date(raw$dry_date)
raw$grid_id <- as.character(raw$grid_id)
raw$event_id <- as.character(raw$event_id)
raw$whiplash_num <- as.integer(raw$whiplash)

required_core <- c(
  "grid_id", "event_id", "wet_onset", "year", "month", "whiplash_num",
  "ante_P30_pct", "ante_SM30_pct", "ante_P30_z", "ante_SM30_z",
  "ante_dryness_z", "rain7", "rain7_pct", "rain7_z", "sm30", "temp7",
  "pet30", "runoff7", "crop_frac_strict", "evi_pre", "evi_post", "evi_shock",
  "longitude", "latitude", "elevation_mean", "slope_mean"
)
missing_required <- setdiff(required_core, names(raw))
if (length(missing_required) > 0) {
  stop("Missing required columns: ", paste(missing_required, collapse = ", "))
}

# Full missingness audit
missingness <- data.frame(
  variable = names(raw),
  missing_n = vapply(raw, function(x) sum(is.na(x)), integer(1)),
  missing_pct = round(vapply(raw, function(x) mean(is.na(x)) * 100, numeric(1)), 2)
) %>% arrange(desc(missing_pct), variable)
fwrite(missingness, file.path(TAB_DIR, "Table_S1_Data_Missingness_Audit.csv"))

# Structural missingness checks
qa_structural <- tibble(
  check = c(
    "Non-whiplash rows with missing dry_date",
    "Whiplash rows with missing dry_date",
    "Non-whiplash rows with missing WTS",
    "Whiplash rows with missing WTS",
    "Rows with missing evi_shock",
    "Rows with missing core hydroclimate predictors",
    "Rows with missing flood_frac"
  ),
  n = c(
    sum(raw$whiplash_num == 0 & is.na(raw$dry_date)),
    sum(raw$whiplash_num == 1 & is.na(raw$dry_date)),
    sum(raw$whiplash_num == 0 & is.na(raw$WTS)),
    sum(raw$whiplash_num == 1 & is.na(raw$WTS)),
    sum(is.na(raw$evi_shock)),
    sum(!complete.cases(raw[, c("ante_dryness_z", "rain7_z", "temp7", "pet30", "runoff7")])),
    sum(is.na(raw$flood_frac))
  )
)
fwrite(qa_structural, file.path(TAB_DIR, "Table_S2_Structural_Missingness_Checks.csv"))

# Duplicate event-grid audit and deterministic collapse to peak wet severity
raw <- raw %>% arrange(event_id, grid_id, desc(rain7_z), desc(rain7))
dup_audit <- raw %>%
  group_by(event_id, grid_id) %>%
  filter(n() > 1) %>%
  ungroup()
if (nrow(dup_audit) > 0) {
  fwrite(dup_audit, file.path(TAB_DIR, "Table_S3_Duplicate_EventGrid_Audit.csv"))
}

dat <- raw %>%
  group_by(event_id, grid_id) %>%
  slice_max(order_by = rain7_z, n = 1, with_ties = FALSE) %>%
  ungroup()

# Analysis inclusion flag if present
if ("analysis_include" %in% names(dat)) dat <- dat %>% filter(analysis_include == 1)

# Standardized nuisance covariates; retain climatological z-scores as-is.
dat <- dat %>%
  mutate(
    whiplash = factor(whiplash_num, levels = c(0, 1), labels = c("Non-whiplash", "Whiplash")),
    month_f = factor(month, levels = 2:5, labels = c("Feb", "Mar", "Apr", "May")),
    grid_f = factor(grid_id),
    event_f = factor(event_id),
    evi_pre_s = z_safe(evi_pre),
    crop_frac_s = z_safe(crop_frac_strict),
    temp7_s = z_safe(temp7),
    pet30_s = z_safe(pet30),
    runoff7_s = z_safe(log1p(runoff7)),
    WTS_s = ifelse(!is.na(WTS), z_safe(WTS), NA_real_),
    dry_severity_s = ifelse(!is.na(dry_severity), z_safe(dry_severity), NA_real_),
    year_decade = (year - mean(year, na.rm = TRUE)) / 10
  )

if (HAS_ARROW) {
  arrow::write_parquet(dat, file.path(DERIVED_DIR, "Sunamganj_whiplash_analysis_clean.parquet"))
}
fwrite(dat, file.path(DERIVED_DIR, "Sunamganj_whiplash_analysis_clean.csv"))

# Data-quality summary based on actual analysis dataset
sample_overview <- tibble(
  metric = c(
    "Raw rows", "Clean event-grid rows", "Variables", "Regional events",
    "Active 10-km grid cells", "Whiplash rows", "Non-whiplash wet-event rows",
    "Whiplash regional events", "Years represented", "Sentinel-1 flood observations",
    "Sentinel-1 regional events with flood data", "Duplicate event-grid rows removed"
  ),
  value = c(
    nrow(raw), nrow(dat), ncol(raw), n_distinct(dat$event_id), n_distinct(dat$grid_id),
    sum(dat$whiplash_num == 1), sum(dat$whiplash_num == 0),
    dat %>% group_by(event_id) %>% summarise(w = any(whiplash_num == 1), .groups = "drop") %>%
      summarise(n = sum(w)) %>% pull(n),
    n_distinct(dat$year), sum(!is.na(dat$flood_frac)),
    n_distinct(dat$event_id[!is.na(dat$flood_frac)]),
    nrow(raw) - nrow(dat)
  )
)
fwrite(sample_overview, file.path(TAB_DIR, "Table_1_Study_Sample_and_QA_Overview.csv"))

# ==============================================================================
# 4. READ STUDY AREA + GRID FOR JOURNAL MAPS
# ==============================================================================
if (!file.exists(STUDY_SHP)) stop("Study-area shapefile not found: ", STUDY_SHP)
if (!file.exists(GRID_GPKG)) stop("10-km grid GPKG not found: ", GRID_GPKG)

# Robust polygon cleaning. The clipped 10-km GPKG may contain one or more
# zero-area/EMPTY edge geometries. Those are harmless for tabular analysis but
# must be removed before poly2nb(), st_area(), and map operations.
clean_polygon_sf <- function(x, label = "sf layer") {
  x <- st_make_valid(x)
  empty0 <- st_is_empty(x)
  if (any(empty0, na.rm = TRUE)) {
    message("[GEOMETRY] Removing ", sum(empty0, na.rm = TRUE),
            " EMPTY feature(s) from ", label, ".")
    x <- x[!empty0, , drop = FALSE]
  }
  if (nrow(x) == 0) stop(label, " has no non-empty geometries after cleaning.")

  xp <- suppressWarnings(st_transform(x, PROJECTED_CRS))
  xp <- st_make_valid(xp)
  empty1 <- st_is_empty(xp)
  if (any(empty1, na.rm = TRUE)) xp <- xp[!empty1, , drop = FALSE]
  if (nrow(xp) == 0) stop(label, " has no valid geometries after projection.")

  a_m2 <- suppressWarnings(as.numeric(st_area(xp)))
  keep <- is.finite(a_m2) & a_m2 > 1
  if (any(!keep)) {
    message("[GEOMETRY] Removing ", sum(!keep),
            " zero/near-zero-area feature(s) from ", label, ".")
    xp <- xp[keep, , drop = FALSE]
  }
  if (nrow(xp) == 0) stop(label, " has no positive-area geometries after cleaning.")
  st_transform(xp, MAP_CRS)
}

study <- st_read(STUDY_SHP, quiet = TRUE)
study <- clean_polygon_sf(study, "study-area shapefile")

# ------------------------------------------------------------------------------
# IMPORTANT: the Python builder exported the Earth Engine grid with a selectors
# list. In some Earth Engine/GeoJSON download paths that preserves attributes but
# drops polygon geometry. The resulting GPKG can therefore contain 42 valid grid
# records whose geometries are all EMPTY. This is NOT a data problem.
#
# Earth Engine coveringGrid() encoded the exact UTM grid indices in grid_id, e.g.
#   G_29_276  ->  x index = 29, y index = 276
# for a 10,000-m grid in EPSG:32646. Therefore the exact original cell is:
#   xmin = 29*10000, xmax = 30*10000
#   ymin = 276*10000, ymax = 277*10000
# We reconstruct those cells exactly, then clip them to the Sunamganj polygon.
# This is preferable to buffering lon/lat centroids because it preserves exact
# shared borders required for queen-contiguity Moran's-I analysis.
# ------------------------------------------------------------------------------
rebuild_ee_grid_from_ids <- function(grid_obj, study_ll, cell_size_m = 10000) {
  attrs <- st_drop_geometry(grid_obj)
  if (!"grid_id" %in% names(attrs)) stop("GRID_GPKG must contain grid_id.")
  attrs$grid_id <- as.character(attrs$grid_id)

  mm <- stringr::str_match(attrs$grid_id, "^G_(-?[0-9]+)_(-?[0-9]+)$")
  if (any(is.na(mm[, 2])) || any(is.na(mm[, 3]))) {
    stop("Could not reconstruct grid: one or more grid_id values do not match G_<x>_<y>.")
  }
  ix <- as.numeric(mm[, 2])
  iy <- as.numeric(mm[, 3])

  polys <- lapply(seq_along(ix), function(i) {
    xmin <- ix[i] * cell_size_m
    xmax <- (ix[i] + 1) * cell_size_m
    ymin <- iy[i] * cell_size_m
    ymax <- (iy[i] + 1) * cell_size_m
    st_polygon(list(matrix(
      c(xmin, ymin,
        xmax, ymin,
        xmax, ymax,
        xmin, ymax,
        xmin, ymin),
      ncol = 2, byrow = TRUE
    )))
  })

  full_cells <- st_sf(
    attrs,
    geometry = st_sfc(polys, crs = st_crs(PROJECTED_CRS))
  )

  # Clip to the official Sunamganj polygon, matching the Python builder logic.
  study_p <- st_transform(study_ll, PROJECTED_CRS) %>% st_make_valid()
  study_union <- st_union(st_geometry(study_p))
  clip_sf <- st_sf(.clip_id = 1L, geometry = study_union)

  clipped <- suppressWarnings(st_intersection(full_cells, clip_sf))
  if (".clip_id" %in% names(clipped)) clipped$.clip_id <- NULL

  # study_union is a single dissolved geometry, so each source cell remains
  # one sf feature (possibly MULTIPOLYGON) after intersection.
  clipped <- clean_polygon_sf(clipped, "reconstructed 10-km analysis grid")
  message("[GEOMETRY] Reconstructed ", nrow(clipped),
          " non-empty 10-km grid polygons from grid_id indices.")
  clipped
}

grid_raw <- st_read(GRID_GPKG, quiet = TRUE)
if (!"grid_id" %in% names(grid_raw)) stop("GRID_GPKG must contain grid_id.")
grid_raw$grid_id <- as.character(grid_raw$grid_id)

empty_grid <- st_is_empty(grid_raw)
valid_geom_n <- sum(!empty_grid, na.rm = TRUE)

if (valid_geom_n == 0) {
  message("[GEOMETRY] Grid GPKG contains attributes but no polygon geometry. ",
          "Reconstructing exact 10-km cells from Earth Engine grid_id indices.")
  grid_sf <- rebuild_ee_grid_from_ids(grid_raw, study, cell_size_m = 10000)
} else {
  if (any(empty_grid, na.rm = TRUE)) {
    message("[GEOMETRY] Grid GPKG contains ", sum(empty_grid, na.rm = TRUE),
            " EMPTY feature(s); valid geometries will be retained.")
  }
  grid_sf <- clean_polygon_sf(grid_raw, "10-km analysis grid")
}

if (anyDuplicated(grid_sf$grid_id)) {
  message("[GEOMETRY] Duplicate grid_id values found; retaining the first geometry per grid_id.")
  grid_sf <- grid_sf[!duplicated(grid_sf$grid_id), , drop = FALSE]
}

# Save the repaired/reconstructed grid so it can be inspected in QGIS/ArcGIS.
try(
  st_write(grid_sf, file.path(DERIVED_DIR, "Sunamganj_10km_grid_repaired.gpkg"),
           layer = "sunamganj_10km_grid_repaired", delete_layer = TRUE, quiet = TRUE),
  silent = TRUE
)

# ------------------------------------------------------------------------------
# FULL-GRID STATIC SUPPORT
# The event-level master dataset contains only grids that participate in at least
# one qualifying extreme-wet event. Static variables must therefore NOT be
# summarized only from that table, otherwise inactive retained grids appear
# falsely blank. We read terrain from the dedicated 42-grid static table and
# cropland support from the MODIS event-response table (which contains all 42
# retained cells per regional event). Event-dependent variables remain NA where
# no qualifying event occurred, while event counts are explicitly zero.
# ------------------------------------------------------------------------------

# Event-dependent summaries (27 active grids in the current dataset).
grid_event_summary <- dat %>%
  group_by(grid_id) %>%
  summarise(
    n_extreme_wet = n(),
    n_whiplash = sum(whiplash_num == 1),
    whiplash_rate = mean(whiplash_num == 1),
    evi_shock_mean = mean(evi_shock, na.rm = TRUE),
    ante_dryness_mean = mean(ante_dryness_z, na.rm = TRUE),
    WTS_mean = mean_or_na(WTS),
    flood_frac_mean = mean_or_na(flood_frac),
    .groups = "drop"
  )

# Static terrain for all retained grids.
if (!file.exists(STATIC_GRID_CSV)) {
  stop("Full-grid static file not found: ", STATIC_GRID_CSV,
       "\nRun the Python dataset builder through the static-terrain stage first.")
}
static_full <- fread(STATIC_GRID_CSV) |> as.data.frame() |>
  mutate(grid_id = as.character(grid_id)) |>
  select(grid_id, any_of(c(
    "inside_frac", "area_km2", "longitude", "latitude",
    "elevation_mean", "elevation_stdDev", "elevation_p90",
    "slope_mean", "slope_stdDev", "slope_p90"
  ))) |>
  distinct(grid_id, .keep_all = TRUE)

# Cropland fraction for all retained grids. The event EVI table contains 42 rows
# per regional event; averaging crop_frac_strict over events gives a stable
# grid-level cropland-support measure independent of event occurrence.
if (file.exists(EVI_RESPONSE_PARQUET) && HAS_ARROW) {
  crop_src <- as.data.frame(arrow::read_parquet(EVI_RESPONSE_PARQUET))
} else if (file.exists(EVI_RESPONSE_CSV)) {
  crop_src <- fread(EVI_RESPONSE_CSV) |> as.data.frame()
} else {
  stop("Full-grid MODIS EVI response table not found. Expected: ",
       EVI_RESPONSE_PARQUET, " or ", EVI_RESPONSE_CSV)
}
if (!all(c("grid_id", "crop_frac_strict") %in% names(crop_src))) {
  stop("MODIS EVI response table lacks grid_id and/or crop_frac_strict.")
}
crop_full <- crop_src |>
  mutate(grid_id = as.character(grid_id)) |>
  group_by(grid_id) |>
  summarise(crop_frac_mean = mean_or_na(crop_frac_strict), .groups = "drop")

# Assemble all 42 retained grids. Static variables should now be complete;
# event-dependent variables remain NA where scientifically undefined.
grid_map <- grid_sf %>%
  left_join(static_full, by = "grid_id") %>%
  left_join(crop_full, by = "grid_id") %>%
  left_join(grid_event_summary, by = "grid_id") %>%
  mutate(
    n_extreme_wet = replace_na(n_extreme_wet, 0L),
    n_whiplash = replace_na(n_whiplash, 0L),
    active = factor(
      ifelse(n_extreme_wet > 0, "Qualifying event observed", "No qualifying event observed"),
      levels = c("Qualifying event observed", "No qualifying event observed")
    )
  )

# QA: static-map variables must be available for every retained grid.
static_map_qa <- tibble(
  variable = c("crop_frac_mean", "elevation_mean", "slope_mean", "n_extreme_wet"),
  missing_n = c(
    sum(!is.finite(grid_map$crop_frac_mean)),
    sum(!is.finite(grid_map$elevation_mean)),
    sum(!is.finite(grid_map$slope_mean)),
    sum(is.na(grid_map$n_extreme_wet))
  ),
  total_grids = nrow(grid_map)
)
fwrite(static_map_qa, file.path(TAB_DIR, "Table_S0_FullGrid_Static_Map_QA.csv"))
if (any(static_map_qa$missing_n[static_map_qa$variable %in% c("crop_frac_mean", "elevation_mean")] > 0)) {
  warning("One or more retained grids still lack static cropland/elevation support. See Table_S0_FullGrid_Static_Map_QA.csv")
}

st_write(grid_map, file.path(DERIVED_DIR, "Sunamganj_grid_analysis_summary.gpkg"),
         delete_dsn = TRUE, quiet = TRUE)

# ==============================================================================
# 5. DESCRIPTIVE STATISTICS + EVENT CLIMATOLOGY
# ==============================================================================
desc_vars <- c(
  "ante_P30_pct", "ante_SM30_pct", "ante_dryness_z", "rain7", "rain7_z",
  "sm30", "temp7", "pet30", "runoff7", "crop_frac_strict", "evi_pre",
  "evi_post", "evi_shock", "evi_shock_rel", "flood_frac"
)

descriptive <- dat %>%
  select(whiplash, all_of(desc_vars)) %>%
  pivot_longer(-whiplash, names_to = "variable", values_to = "value") %>%
  group_by(whiplash, variable) %>%
  summarise(
    n = sum(!is.na(value)),
    mean = mean_or_na(value), sd = ifelse(n > 1, sd(value, na.rm = TRUE), NA_real_),
    median = median_or_na(value),
    q25 = ifelse(n > 0, quantile(value, 0.25, na.rm = TRUE), NA_real_),
    q75 = ifelse(n > 0, quantile(value, 0.75, na.rm = TRUE), NA_real_),
    .groups = "drop"
  )
fwrite(descriptive, file.path(TAB_DIR, "Table_2_Descriptive_Statistics_by_Whiplash.csv"))

# Event-level status and annual counts
event_status <- dat %>%
  group_by(event_id, year) %>%
  summarise(
    event_whiplash = any(whiplash_num == 1),
    n_grid = n(), n_whip_grid = sum(whiplash_num == 1),
    mean_rain7 = mean(rain7, na.rm = TRUE),
    .groups = "drop"
  )

annual <- event_status %>%
  group_by(year) %>%
  summarise(
    total_regional_events = n(),
    whiplash_regional_events = sum(event_whiplash),
    .groups = "drop"
  ) %>%
  complete(year = 2001:2025,
           fill = list(total_regional_events = 0, whiplash_regional_events = 0))

annual_rows <- dat %>%
  group_by(year) %>%
  summarise(
    total_grid_events = n(), whiplash_grid_events = sum(whiplash_num == 1),
    whiplash_grid_fraction = mean(whiplash_num == 1), .groups = "drop"
  ) %>%
  complete(year = 2001:2025,
           fill = list(total_grid_events = 0, whiplash_grid_events = 0,
                       whiplash_grid_fraction = NA_real_))

fwrite(annual, file.path(TAB_DIR, "Table_S4_Annual_Regional_Event_Counts.csv"))

# Trend tests
mk_obj <- Kendall::MannKendall(annual$whiplash_regional_events)
sen_obj <- trend::sens.slope(annual$whiplash_regional_events)
qbin_dat <- annual_rows %>% filter(total_grid_events > 0)
trend_glm <- glm(
  cbind(whiplash_grid_events, total_grid_events - whiplash_grid_events) ~ I((year - 2001) / 10),
  family = quasibinomial(), data = qbin_dat
)
trend_table <- tibble(
  method = c("Mann-Kendall regional-event count", "Sen slope regional-event count",
             "Quasibinomial grid-event proportion per decade"),
  estimate = c(
    unname(mk_obj$tau),
    suppressWarnings(as.numeric(sen_obj$estimates)[1]),
    unname(coef(trend_glm)[2])
  ),
  statistic = c(
    NA_real_,
    suppressWarnings(as.numeric(sen_obj$statistic)[1]),
    summary(trend_glm)$coefficients[2, "t value"]
  ),
  p_value = c(
    unname(mk_obj$sl),
    suppressWarnings(as.numeric(sen_obj$p.value)[1]),
    summary(trend_glm)$coefficients[2, "Pr(>|t|)"]
  )
)
fwrite(trend_table, file.path(TAB_DIR, "Table_3_Temporal_Trend_Tests.csv"))

# ==============================================================================
# 6. PRIMARY INFERENCE MODELS — CACHED
# ==============================================================================
primary_formula <- evi_shock ~ whiplash_num + rain7_z + evi_pre_s + crop_frac_s +
  temp7_s + pet30_s + year_decade + month_f + (1 | event_f) + (1 | grid_f)

M1 <- cache_get("M1_Primary_Whiplash_EVI_CrossClassified_LMM", function() {
  safe_lmer(primary_formula, dat)
})

M2 <- cache_get("M2_Continuous_Hydroclimatic_Memory_Interaction_LMM", function() {
  safe_lmer(
    evi_shock ~ ante_dryness_z * rain7_z + evi_pre_s + crop_frac_s +
      temp7_s + pet30_s + year_decade + month_f +
      (1 | event_f) + (1 | grid_f),
    dat
  )
})

# Runoff is a plausible hydrologic mediator. Treat this as mediator-adjusted sensitivity,
# not the primary total-effect model.
M3 <- cache_get("M3_Runoff_Adjusted_Sensitivity_LMM", function() {
  safe_lmer(
    evi_shock ~ whiplash_num + rain7_z + runoff7_s + evi_pre_s + crop_frac_s +
      temp7_s + pet30_s + year_decade + month_f +
      (1 | event_f) + (1 | grid_f),
    dat
  )
})

whip_dat <- dat %>% filter(whiplash_num == 1, !is.na(WTS), !is.na(dry_severity))
M4 <- cache_get("M4_Whiplash_Transition_Speed_LMM", function() {
  safe_lmer(
    evi_shock ~ WTS_s + dry_severity_s + rain7_z + evi_pre_s + crop_frac_s +
      year_decade + month_f + (1 | event_f) + (1 | grid_f),
    whip_dat
  )
})

# Nonlinear robustness: continuous hydroclimatic-memory surface.
M5_GAM <- cache_get("M5_Nonlinear_Hydroclimatic_Memory_GAMM", function() {
  mgcv::gam(
    evi_shock ~
      s(ante_dryness_z, k = 5) +
      s(rain7_z, k = 5) +
      ti(ante_dryness_z, rain7_z, k = c(4, 4)) +
      s(evi_pre_s, k = 4) + crop_frac_s + temp7_s + pet30_s +
      year_decade + month_f +
      s(grid_f, bs = "re") + s(event_f, bs = "re"),
    data = dat, method = "REML"
  )
})

# Sentinel-1 physical validation model: does more inundation correspond to stronger EVI shock?
s1_dat <- dat %>% filter(!is.na(flood_frac))
M6_S1 <- NULL
if (nrow(s1_dat) >= 50 && n_distinct(s1_dat$event_id) >= 5) {
  M6_S1 <- cache_get("M6_Sentinel1_FloodFraction_EVI_Validation_LMM", function() {
    safe_lmer(
      evi_shock ~ flood_frac + rain7_z + evi_pre_s + crop_frac_s +
        year_decade + month_f + (1 | event_f) + (1 | grid_f),
      s1_dat
    )
  })
}

# Crossed event + grid random effects are non-nested, so clubSandwich CR2 is not
# generally available for this LMM structure. Use a two-way fixed-effects model
# with two-way clustered standard errors as the robust inferential sensitivity.
M1_FE2W <- cache_get("M1_Robustness_TwoWayFE_TwoWayCluster", function() {
  fixest::feols(
    evi_shock ~ whiplash_num + rain7_z + evi_pre_s + crop_frac_s + temp7_s + pet30_s |
      event_f + grid_f,
    data = dat,
    vcov = ~ event_f + grid_f,
    warn = FALSE,
    notes = FALSE
  )
})
M1_FE2W_tab <- tidy_fixest_safe(M1_FE2W)
fwrite(M1_FE2W_tab, file.path(TAB_DIR,
                              "Table_S5_Primary_Robustness_TwoWayFE_TwoWayCluster.csv"))

# Standard model tables
model_table <- bind_rows(
  broom.mixed::tidy(M1, effects = "fixed", conf.int = TRUE) %>% mutate(model = "M1 Primary binary whiplash"),
  broom.mixed::tidy(M2, effects = "fixed", conf.int = TRUE) %>% mutate(model = "M2 Continuous memory interaction"),
  broom.mixed::tidy(M3, effects = "fixed", conf.int = TRUE) %>% mutate(model = "M3 Runoff-adjusted sensitivity"),
  broom.mixed::tidy(M4, effects = "fixed", conf.int = TRUE) %>% mutate(model = "M4 Whiplash transition speed")
) %>% select(model, everything())
fwrite(model_table, file.path(TAB_DIR, "Table_4_Mixed_Model_Coefficients.csv"))

# Model performance. This block is deliberately tolerant of singular random
# effects: marginal R2 may still be estimable while conditional R2/ICC are NA.
perf_rows <- list()
for (nm in c("M1", "M2", "M3", "M4")) {
  mod <- get(nm)
  perf_rows[[nm]] <- safe_model_metrics(mod) %>% mutate(model = nm, .before = 1)
}
model_performance <- bind_rows(perf_rows)
fwrite(model_performance, file.path(TAB_DIR, "Table_5_Model_Performance.csv"))

# ==============================================================================
# 7. MATCHED-EVENT ANALYSIS — CACHED
# ==============================================================================
match_obj <- cache_get("M7_MatchIt_Whiplash_Nearest_MonthExact", function() {
  MatchIt::matchit(
    whiplash_num ~ rain7_z + evi_pre_s + crop_frac_s + temp7_s + pet30_s + year_decade,
    data = dat,
    method = "nearest", distance = "glm", ratio = 1,
    exact = ~ month_f,
    caliper = 0.20, std.caliper = TRUE,
    replace = FALSE
  )
})
matched <- MatchIt::match.data(match_obj)
if (HAS_ARROW) arrow::write_parquet(matched, file.path(DERIVED_DIR, "matched_whiplash_nonwhiplash.parquet"))
fwrite(matched, file.path(DERIVED_DIR, "matched_whiplash_nonwhiplash.csv"))

M7_matched <- cache_get("M7_Matched_Pair_FixedEffect_EVI", function() {
  fixest::feols(
    evi_shock ~ whiplash_num + rain7_z | subclass,
    data = matched, weights = ~ weights, cluster = ~ subclass
  )
})

bal <- cobalt::bal.tab(match_obj, un = TRUE, m.threshold = 0.10, disp.v.ratio = TRUE)
bal_df <- tryCatch({
  as.data.frame(bal$Balance) %>% tibble::rownames_to_column("covariate")
}, error = function(e) data.frame())
if (nrow(bal_df) > 0) fwrite(bal_df, file.path(TAB_DIR, "Table_6_Matching_Balance.csv"))

matched_effect <- broom::tidy(M7_matched, conf.int = TRUE) %>% filter(term == "whiplash_num")
fwrite(matched_effect, file.path(TAB_DIR, "Table_7_Matched_Whiplash_Effect.csv"))

# ==============================================================================
# 8. LEAVE-ONE-REGIONAL-EVENT-OUT ROBUSTNESS — CACHED
# ==============================================================================
loo <- cache_get("R1_LeaveOneRegionalEventOut_PrimaryEffect", function() {
  out <- vector("list", length(unique(dat$event_id)))
  evs <- unique(dat$event_id)
  for (i in seq_along(evs)) {
    ev <- evs[i]
    dd <- dat %>% filter(event_id != ev)
    fit <- tryCatch(safe_lmer(primary_formula, dd), error = function(e) NULL)
    if (is.null(fit)) next
    tt <- broom.mixed::tidy(fit, effects = "fixed", conf.int = TRUE) %>%
      filter(term == "whiplash_num")
    if (nrow(tt) == 1) {
      tt$excluded_event <- ev
      out[[i]] <- tt
    }
  }
  bind_rows(out)
})
fwrite(loo, file.path(TAB_DIR, "Table_S6_LeaveOneEventOut_Robustness.csv"))

# ==============================================================================
# 9. MODEL-SPECIFICATION ROBUSTNESS — CACHED
# ==============================================================================
M0_unadj <- cache_get("R2_Unadjusted_Whiplash_LMM", function() {
  safe_lmer(evi_shock ~ whiplash_num + (1 | event_f) + (1 | grid_f), dat)
})
M0_rain <- cache_get("R3_RainAdjusted_Whiplash_LMM", function() {
  safe_lmer(evi_shock ~ whiplash_num + rain7_z + (1 | event_f) + (1 | grid_f), dat)
})

spec_models <- list(
  "Unadjusted" = M0_unadj,
  "+ wet severity" = M0_rain,
  "Primary adjusted" = M1,
  "+ runoff mediator" = M3
)
spec_effects <- imap_dfr(spec_models, function(m, nm) {
  broom.mixed::tidy(m, effects = "fixed", conf.int = TRUE) %>%
    filter(term == "whiplash_num") %>% mutate(specification = nm)
})
fe2_eff <- tidy_fixest_safe(M1_FE2W) %>%
  filter(term == "whiplash_num") %>%
  mutate(specification = "Two-way FE + two-way clustered SE")
spec_effects <- bind_rows(spec_effects, fe2_eff)
fwrite(spec_effects, file.path(TAB_DIR, "Table_S7_Model_Specification_Robustness.csv"))

# ==============================================================================
# 10. OPTIONAL EXACT THRESHOLD SENSITIVITY USING DAILY CLIMATE
# ==============================================================================
threshold_sens <- NULL
if (file.exists(DAILY_CLIMATE_PARQUET) && HAS_ARROW) {
  threshold_sens <- cache_get("R4_Threshold_Sensitivity_DailyClimate", function() {
    message("Reading daily climate for threshold sensitivity...")
    daily <- as.data.table(arrow::read_parquet(DAILY_CLIMATE_PARQUET))
    daily[, date := as.Date(date)]
    setkey(daily, grid_id, date)

    p_thrs <- c(10, 20, 30)
    sm_thrs <- c(20, 30, 40)
    windows <- c(7, 14, 21)
    wet_thrs <- c(95, 97.5)
    combos <- expand.grid(
      p_thr = p_thrs, sm_thr = sm_thrs, window = windows, wet_thr = wet_thrs,
      KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE
    )

    base_rows <- dat %>%
      select(grid_id, event_id, wet_onset, rain7_pct, evi_shock, rain7_z,
             evi_pre_s, crop_frac_s, temp7_s, pet30_s, year_decade, month_f,
             grid_f, event_f)

    res <- vector("list", nrow(combos))
    for (k in seq_len(nrow(combos))) {
      cc <- combos[k, ]
      dd <- base_rows %>% filter(rain7_pct >= cc$wet_thr)
      if (nrow(dd) < 80) next

      exp_new <- logical(nrow(dd))
      for (i in seq_len(nrow(dd))) {
        g <- dd$grid_id[i]
        d <- dd$wet_onset[i]
        pre <- daily[.(g)][date >= (d - cc$window) & date < d]
        exp_new[i] <- if (nrow(pre) == 0) FALSE else any(
          pre$P30_pct < cc$p_thr & pre$SM30_pct < cc$sm_thr,
          na.rm = TRUE
        )
      }
      dd$whip_sens <- as.integer(exp_new)
      n1 <- sum(dd$whip_sens == 1); n0 <- sum(dd$whip_sens == 0)
      if (n1 < 12 || n0 < 25) next

      fit <- tryCatch(
        safe_lmer(
          evi_shock ~ whip_sens + rain7_z + evi_pre_s + crop_frac_s +
            temp7_s + pet30_s + year_decade + month_f +
            (1 | event_f) + (1 | grid_f), dd
        ),
        error = function(e) NULL
      )
      if (is.null(fit)) next
      tt <- broom.mixed::tidy(fit, effects = "fixed", conf.int = TRUE) %>%
        filter(term == "whip_sens")
      if (nrow(tt) == 1) {
        res[[k]] <- tt %>% mutate(
          p_thr = cc$p_thr, sm_thr = cc$sm_thr,
          window = cc$window, wet_thr = cc$wet_thr,
          n = nrow(dd), n_whiplash = n1, n_control = n0
        )
      }
    }
    bind_rows(res)
  })
  if (!is.null(threshold_sens) && nrow(threshold_sens) > 0) {
    fwrite(threshold_sens, file.path(TAB_DIR, "Table_S8_Threshold_Sensitivity.csv"))
  }
} else {
  message("[INFO] Exact threshold sensitivity skipped: daily climate parquet and/or R 'arrow' package unavailable.")
}

# ==============================================================================
# 11. SPATIAL AUTOCORRELATION
# ==============================================================================
# Primary model residuals attached back to rows used by M1
m1_frame <- model.frame(M1)
# M1 uses complete rows and current dat has complete primary covariates, so row order is retained.
dat$resid_M1 <- NA_real_
if (length(residuals(M1)) == nrow(dat)) dat$resid_M1 <- residuals(M1)

resid_grid <- dat %>%
  group_by(grid_id) %>%
  summarise(resid_mean = mean_or_na(resid_M1), .groups = "drop")
grid_map <- grid_map %>% left_join(resid_grid, by = "grid_id")

# Keep only active, non-empty, positive-area polygons before neighborhood construction.
spatial_active <- grid_map %>% filter(n_extreme_wet > 0)
spatial_active <- clean_polygon_sf(spatial_active, "active grids for spatial autocorrelation")

if (nrow(spatial_active) < 5) {
  warning("Fewer than five valid active polygons remain; Moran's I will be reported as NA.")
  nb <- NULL
  lw <- NULL
  isolated_n <- NA_integer_
} else {
  nb <- spdep::poly2nb(spatial_active, queen = TRUE)
  isolated_n <- sum(spdep::card(nb) == 0)
  if (isolated_n > 0) {
    message("[SPATIAL] ", isolated_n,
            " active grid(s) have no queen-contiguous neighbor; zero.policy=TRUE will be used.")
  }
  lw <- spdep::nb2listw(nb, style = "W", zero.policy = TRUE)
}

safe_moran <- function(x, label) {
  ok <- is.finite(x)
  if (is.null(lw) || sum(ok) < 5) {
    return(tibble(variable = label, I = NA_real_, expectation = NA_real_,
                  variance = NA_real_, p_value = NA_real_, n = sum(ok)))
  }
  sf_sub <- spatial_active[ok, , drop = FALSE]
  xx <- x[ok]
  if (nrow(sf_sub) < 5 || !is.finite(sd(xx, na.rm = TRUE)) || sd(xx, na.rm = TRUE) == 0) {
    return(tibble(variable = label, I = NA_real_, expectation = NA_real_,
                  variance = NA_real_, p_value = NA_real_, n = length(xx)))
  }
  nb_sub <- spdep::poly2nb(sf_sub, queen = TRUE)
  lw_sub <- spdep::nb2listw(nb_sub, style = "W", zero.policy = TRUE)
  mt <- tryCatch(
    spdep::moran.test(xx, lw_sub, zero.policy = TRUE, na.action = na.exclude),
    error = function(e) NULL
  )
  if (is.null(mt)) {
    return(tibble(variable = label, I = NA_real_, expectation = NA_real_,
                  variance = NA_real_, p_value = NA_real_, n = length(xx)))
  }
  tibble(
    variable = label,
    I = unname(mt$estimate[["Moran I statistic"]]),
    expectation = unname(mt$estimate[["Expectation"]]),
    variance = unname(mt$estimate[["Variance"]]),
    p_value = mt$p.value,
    n = length(xx)
  )
}

moran_table <- bind_rows(
  safe_moran(spatial_active$whiplash_rate, "Whiplash proportion"),
  safe_moran(spatial_active$evi_shock_mean, "Mean EVI shock"),
  safe_moran(spatial_active$resid_mean, "Primary-model mean residual")
)
fwrite(moran_table, file.path(TAB_DIR, "Table_8_Spatial_Autocorrelation_MoransI.csv"))

# Moran scatter data: use only grids with observed residual means; no imputation.
res_ok <- is.finite(spatial_active$resid_mean)
if (sum(res_ok) >= 5) {
  res_sf <- spatial_active[res_ok, , drop = FALSE]
  xres <- res_sf$resid_mean
  if (is.finite(sd(xres, na.rm = TRUE)) && sd(xres, na.rm = TRUE) > 0) {
    nb_res <- spdep::poly2nb(res_sf, queen = TRUE)
    lw_res <- spdep::nb2listw(nb_res, style = "W", zero.policy = TRUE)
    xres_z <- as.numeric(scale(xres))
    lag_z <- spdep::lag.listw(lw_res, xres_z, zero.policy = TRUE)
    moran_scatter <- data.frame(x = xres_z, lag = lag_z, grid_id = res_sf$grid_id)
  } else {
    moran_scatter <- data.frame(x = numeric(0), lag = numeric(0), grid_id = character(0))
  }
} else {
  moran_scatter <- data.frame(x = numeric(0), lag = numeric(0), grid_id = character(0))
}

# ==============================================================================
# 12. SENTINEL-1 VALIDATION SUMMARY
# ==============================================================================
s1_summary <- tibble(
  metric = c(
    "Grid-event observations with flood_frac", "Regional events with flood_frac",
    "Whiplash grid-events with flood_frac", "Whiplash regional events with flood_frac",
    "Non-whiplash grid-events with flood_frac", "Spearman rho: flood_frac vs EVI shock",
    "Spearman p: flood_frac vs EVI shock"
  ),
  value = c(
    nrow(s1_dat), n_distinct(s1_dat$event_id),
    sum(s1_dat$whiplash_num == 1),
    n_distinct(s1_dat$event_id[s1_dat$whiplash_num == 1]),
    sum(s1_dat$whiplash_num == 0),
    if (nrow(s1_dat) > 5) cor(s1_dat$flood_frac, s1_dat$evi_shock,
                             method = "spearman", use = "complete.obs") else NA_real_,
    if (nrow(s1_dat) > 5) cor.test(s1_dat$flood_frac, s1_dat$evi_shock,
                                  method = "spearman", exact = FALSE)$p.value else NA_real_
  )
)
fwrite(s1_summary, file.path(TAB_DIR, "Table_9_Sentinel1_Validation_Summary.csv"))

if (!is.null(M6_S1)) {
  s1_model_tab <- broom.mixed::tidy(M6_S1, effects = "fixed", conf.int = TRUE)
  fwrite(s1_model_tab, file.path(TAB_DIR, "Table_S9_Sentinel1_EVI_Validation_Model.csv"))
}

# ==============================================================================
# 13. FIGURE 1 — STUDY DESIGN + DATA SUPPORT (4 MAPS) — CORRECTED
# ==============================================================================

message("[FIG1] Building Figure 1...")


# ==============================================================================
# FIGURE 1-SPECIFIC HELPERS
# ==============================================================================

# Shorter coordinate labels to avoid overlap
fig1_degE <- function(x) {
  paste0(
    formatC(
      x,
      format = "f",
      digits = 1
    ),
    "°E"
  )
}

fig1_degN <- function(x) {
  paste0(
    formatC(
      x,
      format = "f",
      digits = 1
    ),
    "°N"
  )
}


# ------------------------------------------------------------------------------
# Keep exact 0.20-degree spacing, but use shorter labels
# ------------------------------------------------------------------------------

fig1_add_axes <- function(p) {

  p +

    scale_x_continuous(
      breaks = map_breaks_020,
      labels = fig1_degE
    ) +

    scale_y_continuous(
      breaks = map_breaks_020,
      labels = fig1_degN
    ) +

    labs(
      x = "Longitude",
      y = "Latitude"
    )
}


# ------------------------------------------------------------------------------
# Figure-1 map theme
#
# Main changes:
#   * legend moved below map
#   * smaller coordinate text
#   * larger map drawing area
# ------------------------------------------------------------------------------

fig1_map_theme <- function() {

  map_theme() +

    theme(

      legend.position = "bottom",

      legend.box = "horizontal",

      legend.box.just = "center",

      legend.margin = margin(
        t = 3,
        r = 0,
        b = 0,
        l = 0
      ),

      legend.title = element_text(
        family = BASE_FAMILY,
        face = "bold",
        size = BASE_SIZE - 1,
        colour = "black",
        hjust = 0.5
      ),

      legend.text = element_text(
        family = BASE_FAMILY,
        face = "bold",
        size = BASE_SIZE - 2,
        colour = "black",
        lineheight = 0.90
      ),

      axis.text.x = element_text(
        family = BASE_FAMILY,
        face = "bold",
        size = BASE_SIZE - 2,
        colour = "black",
        margin = margin(t = 4)
      ),

      axis.text.y = element_text(
        family = BASE_FAMILY,
        face = "bold",
        size = BASE_SIZE - 2,
        colour = "black",
        margin = margin(r = 4)
      ),

      axis.title = element_text(
        family = BASE_FAMILY,
        face = "bold",
        size = BASE_SIZE,
        colour = "black"
      ),

      panel.grid.major = element_line(
        linewidth = 0.30,
        linetype = "dashed",
        colour = "grey88"
      ),

      panel.grid.minor = element_blank(),

      plot.margin = margin(
        7,
        7,
        5,
        7
      )
    )
}


# ==============================================================================
# CUSTOM EXACT 0–5–10 km SCALE BAR + NORTH ARROW
# PANEL a ONLY
# ==============================================================================

add_fig1_north_scale <- function(
    p,
    study_ll = study) {

  # ---------------------------------------------------------------------------
  # Project study boundary to UTM 46N
  # ---------------------------------------------------------------------------

  study_utm_fig1 <- st_transform(
    study_ll,
    PROJECTED_CRS
  )

  bb_fig1 <- st_bbox(
    study_utm_fig1
  )

  map_w <- as.numeric(
    bb_fig1[["xmax"]] -
      bb_fig1[["xmin"]]
  )

  map_h <- as.numeric(
    bb_fig1[["ymax"]] -
      bb_fig1[["ymin"]]
  )


  # ---------------------------------------------------------------------------
  # Scale-bar position
  # ---------------------------------------------------------------------------

  scale_x0 <-
    as.numeric(
      bb_fig1[["xmin"]]
    ) +
    0.055 * map_w

  scale_y0 <-
    as.numeric(
      bb_fig1[["ymin"]]
    ) +
    0.045 * map_h


  # Two blocks × 5 km = exact 10 km
  seg_len <- 5000

  bar_h <- max(
    550,
    0.014 * map_h
  )


  # ---------------------------------------------------------------------------
  # Rectangle helper
  # ---------------------------------------------------------------------------

  make_scale_rect <- function(
      xmin,
      xmax,
      ymin,
      ymax) {

    st_polygon(

      list(

        matrix(

          c(
            xmin, ymin,
            xmax, ymin,
            xmax, ymax,
            xmin, ymax,
            xmin, ymin
          ),

          ncol = 2,
          byrow = TRUE
        )
      )
    )
  }


  # ---------------------------------------------------------------------------
  # Two alternating blocks
  # ---------------------------------------------------------------------------

  scale_blocks <- st_sf(

    block = c(
      "black",
      "white"
    ),

    geometry = st_sfc(

      make_scale_rect(
        scale_x0,
        scale_x0 + seg_len,
        scale_y0,
        scale_y0 + bar_h
      ),

      make_scale_rect(
        scale_x0 + seg_len,
        scale_x0 + 2 * seg_len,
        scale_y0,
        scale_y0 + bar_h
      ),

      crs = PROJECTED_CRS
    )
  )


  # ---------------------------------------------------------------------------
  # Scale labels
  # ---------------------------------------------------------------------------

  scale_labels <- st_sf(

    label = c(
      "0",
      "5",
      "10 km"
    ),

    geometry = st_sfc(

      st_point(
        c(
          scale_x0,
          scale_y0 - 1100
        )
      ),

      st_point(
        c(
          scale_x0 + seg_len,
          scale_y0 - 1100
        )
      ),

      st_point(
        c(
          scale_x0 + 2 * seg_len,
          scale_y0 - 1100
        )
      ),

      crs = PROJECTED_CRS
    )
  )


  # Convert scale to lon/lat
  scale_blocks <-
    st_transform(
      scale_blocks,
      MAP_CRS
    )

  scale_labels <-
    st_transform(
      scale_labels,
      MAP_CRS
    )


  # ---------------------------------------------------------------------------
  # Add new scale + north arrow
  # ---------------------------------------------------------------------------

  p +

    # black 0–5 km block
    geom_sf(
      data =
        scale_blocks[
          scale_blocks$block ==
            "black",
        ],
      inherit.aes = FALSE,
      fill = "black",
      colour = "black",
      linewidth = 0.45
    ) +

    # white 5–10 km block
    geom_sf(
      data =
        scale_blocks[
          scale_blocks$block ==
            "white",
        ],
      inherit.aes = FALSE,
      fill = "white",
      colour = "black",
      linewidth = 0.45
    ) +

    # 0 / 5 / 10 km labels
    geom_sf_text(
      data = scale_labels,
      aes(
        label = label
      ),
      inherit.aes = FALSE,
      family = BASE_FAMILY,
      fontface = "bold",
      size = 4.1,
      colour = "black"
    ) +

    # north arrow
    ggspatial::annotation_north_arrow(
      location = "bl",
      which_north = "true",

      pad_x =
        grid::unit(
          0.20,
          "in"
        ),

      pad_y =
        grid::unit(
          0.82,
          "in"
        ),

      height =
        grid::unit(
          0.44,
          "in"
        ),

      width =
        grid::unit(
          0.44,
          "in"
        ),

      style =
        ggspatial::north_arrow_fancy_orienteering(
          text_family = BASE_FAMILY,
          text_face = "bold"
        )
    )
}


# ==============================================================================
# a) ANALYTICAL 10-km GRID
# ==============================================================================

p1a <-

  ggplot() +

  geom_sf(
    data = study,
    fill = "grey97",
    colour = "grey45",
    linewidth = 0.85
  ) +

  geom_sf(
    data = grid_map,
    aes(
      fill = active
    ),
    colour = "grey40",
    linewidth = 0.28
  ) +

  geom_sf(
    data = study,
    fill = NA,
    colour = "grey40",
    linewidth = 0.85
  ) +

  scale_fill_manual(

    values = c(

      "Qualifying event observed" =
        "#2C7FB8",

      "No qualifying event observed" =
        "grey90"
    ),

    labels = c(

      "Qualifying event observed" =
        "Qualifying event\nobserved",

      "No qualifying event observed" =
        "No qualifying event\nobserved"
    ),

    name = NULL,

    drop = FALSE,

    guide = guide_legend(

      nrow = 1,

      byrow = TRUE,

      keywidth =
        unit(
          0.50,
          "cm"
        ),

      keyheight =
        unit(
          0.50,
          "cm"
        )
    )
  ) +

  map_coord(
    study
  ) +

  fig1_map_theme() +

  labs(
    title =
      "Analytical 10-km grid"
  )


p1a <- fig1_add_axes(
  p1a
)

p1a <- add_fig1_north_scale(
  p1a
)


# ==============================================================================
# b) CROPLAND SUPPORT
# ==============================================================================

p1b <-

  ggplot() +

  geom_sf(
    data = study,
    fill = "white",
    colour = "grey45",
    linewidth = 0.80
  ) +

  geom_sf(
    data = grid_map,
    aes(
      fill = crop_frac_mean
    ),
    colour = "grey40",
    linewidth = 0.25
  ) +

  geom_sf(
    data = study,
    fill = NA,
    colour = "grey40",
    linewidth = 0.80
  ) +

  scale_fill_viridis_c(

    option = "C",

    limits = c(
      0,
      1
    ),

    breaks = c(
      0,
      0.25,
      0.50,
      0.75,
      1.00
    ),

    labels = c(
      "0.00",
      "0.25",
      "0.50",
      "0.75",
      "1.00"
    ),

    na.value =
      "grey92",

    name =
      "Mean cropland fraction",

    guide =
      guide_colorbar(

        direction =
          "horizontal",

        title.position =
          "top",

        title.hjust =
          0.5,

        barwidth =
          unit(
            6.2,
            "cm"
          ),

        barheight =
          unit(
            0.36,
            "cm"
          ),

        ticks =
          TRUE
      )
  ) +

  map_coord(
    study
  ) +

  fig1_map_theme() +

  labs(
    title =
      "Cropland support"
  )


p1b <- fig1_add_axes(
  p1b
)


# ==============================================================================
# c) MEAN ELEVATION
# ==============================================================================

p1c <-

  ggplot() +

  geom_sf(
    data = study,
    fill = "white",
    colour = "grey45",
    linewidth = 0.80
  ) +

  geom_sf(
    data = grid_map,
    aes(
      fill =
        elevation_mean
    ),
    colour = "grey40",
    linewidth = 0.25
  ) +

  geom_sf(
    data = study,
    fill = NA,
    colour = "grey40",
    linewidth = 0.80
  ) +

  scale_fill_viridis_c(

    option = "D",

    na.value =
      "grey92",

    name =
      "Elevation (m)",

    guide =
      guide_colorbar(

        direction =
          "horizontal",

        title.position =
          "top",

        title.hjust =
          0.5,

        barwidth =
          unit(
            6.2,
            "cm"
          ),

        barheight =
          unit(
            0.36,
            "cm"
          ),

        ticks =
          TRUE
      )
  ) +

  map_coord(
    study
  ) +

  fig1_map_theme() +

  labs(
    title =
      "Mean elevation"
  )


p1c <- fig1_add_axes(
  p1c
)


# ==============================================================================
# d) EVENT-OBSERVATION DENSITY
# ==============================================================================

max_events_fig1 <-
  max(
    grid_map$n_extreme_wet,
    na.rm = TRUE
  )


event_breaks_fig1 <-
  unique(
    round(
      seq(
        0,
        max_events_fig1,
        length.out = 6
      )
    )
  )


p1d <-

  ggplot() +

  geom_sf(
    data = study,
    fill = "white",
    colour = "grey45",
    linewidth = 0.80
  ) +

  geom_sf(
    data = grid_map,
    aes(
      fill =
        n_extreme_wet
    ),
    colour = "grey40",
    linewidth = 0.25
  ) +

  geom_sf(
    data = study,
    fill = NA,
    colour = "grey40",
    linewidth = 0.80
  ) +

  scale_fill_gradient(

    low =
      "grey95",

    high =
      "#D95F0E",

    limits = c(
      0,
      max_events_fig1
    ),

    breaks =
      event_breaks_fig1,

    na.value =
      "grey92",

    name =
      "Extreme-wet observations",

    guide =
      guide_colorbar(

        direction =
          "horizontal",

        title.position =
          "top",

        title.hjust =
          0.5,

        barwidth =
          unit(
            6.2,
            "cm"
          ),

        barheight =
          unit(
            0.36,
            "cm"
          ),

        ticks =
          TRUE
      )
  ) +

  map_coord(
    study
  ) +

  fig1_map_theme() +

  labs(
    title =
      "Event-observation density"
  )


p1d <- fig1_add_axes(
  p1d
)


# ==============================================================================
# PANEL HEADINGS
# ==============================================================================

p1a <- panel_heading(
  p1a,
  "a"
)

p1b <- panel_heading(
  p1b,
  "b"
)

p1c <- panel_heading(
  p1c,
  "c"
)

p1d <- panel_heading(
  p1d,
  "d"
)


# ==============================================================================
# COMBINE FIGURE
# ==============================================================================

fig1 <-

  (

    p1a |
      p1b

  ) /

  (

    p1c |
      p1d

  ) +

  patchwork::plot_layout(

    widths = c(
      1,
      1
    ),

    heights = c(
      1,
      1
    )
  )


# ==============================================================================
# SAVE
# ==============================================================================

save_figure(

  fig1,

  "Figure_1_Study_Design_and_Data_Support",

  width = 15.8,

  height = 11.6
)

message("[FIG1] Figure 1 completed.")

# ==============================================================================
# 14. FIGURE 2 — COLOURFUL EVENT CLIMATOLOGY
# Replace the COMPLETE old Section 14 with this block.
# Uses the objects already created earlier in the full script:
#   annual, dat, whip_dat, journal_theme(), save_figure(), FIG_DIR
# ==============================================================================

# Colour palette used only for Figure 2
fig2_cols <- c(
  navy   = "#264653",
  blue   = "#2A9D8F",
  cyan   = "#4CC9F0",
  green  = "#43AA8B",
  yellow = "#F9C74F",
  orange = "#F8961E",
  red    = "#E76F51",
  purple = "#7B2CBF",
  pink   = "#E76FAD"
)

# a) Annual regional whiplash-event occurrence
p2a <- ggplot(annual, aes(x = year, y = whiplash_regional_events)) +
  geom_col(
    aes(fill = whiplash_regional_events),
    width = 0.72,
    colour = "white",
    linewidth = 0.25
  ) +
  geom_smooth(
    method = "lm", se = TRUE,
    colour = fig2_cols["red"],
    fill = scales::alpha(fig2_cols["red"], 0.18),
    linewidth = 1.15
  ) +
  geom_point(
    colour = fig2_cols["navy"],
    size = 2.0
  ) +
  scale_fill_gradientn(
    colours = c(fig2_cols["cyan"], fig2_cols["green"],
                fig2_cols["yellow"], fig2_cols["orange"]),
    guide = "none"
  ) +
  scale_x_continuous(breaks = seq(2001, 2025, 4)) +
  scale_y_continuous(
    breaks = scales::pretty_breaks(n = 6),
    expand = expansion(mult = c(0, 0.08))
  ) +
  labs(
    x = "Year",
    y = "Regional whiplash events",
    title = "a) Annual whiplash-event occurrence"
  ) +
  journal_theme(13) +
  theme(
    plot.title = element_text(face = "bold", size = BASE_SIZE + 2),
    axis.title = element_text(face = "bold", size = BASE_SIZE),
    axis.text = element_text(face = "bold", size = BASE_SIZE, colour = "black")
  )

# b) February-May seasonality
monthly_events <- dat %>%
  filter(whiplash_num == 1) %>%
  distinct(event_id, month_f) %>%
  count(month_f, name = "n") %>%
  tidyr::complete(
    month_f = factor(c("Feb", "Mar", "Apr", "May"),
                     levels = c("Feb", "Mar", "Apr", "May")),
    fill = list(n = 0)
  ) %>%
  mutate(month_f = factor(as.character(month_f),
                          levels = c("Feb", "Mar", "Apr", "May")))

month_pal <- c(
  "Feb" = fig2_cols["cyan"],
  "Mar" = fig2_cols["green"],
  "Apr" = fig2_cols["orange"],
  "May" = fig2_cols["purple"]
)

p2b <- ggplot(monthly_events, aes(x = month_f, y = n, fill = month_f)) +
  geom_col(width = 0.70, colour = "white", linewidth = 0.4) +
  geom_text(
    aes(label = n),
    vjust = -0.45,
    fontface = "bold",
    size = 5.0,
    colour = "black"
  ) +
  scale_fill_manual(values = month_pal, guide = "none") +
  scale_y_continuous(
    breaks = scales::pretty_breaks(n = 6),
    expand = expansion(mult = c(0, 0.13))
  ) +
  labs(
    x = NULL,
    y = "Unique regional events",
    title = "b) Pre-monsoon seasonality"
  ) +
  journal_theme(13) +
  theme(
    plot.title = element_text(face = "bold", size = BASE_SIZE + 2),
    axis.title = element_text(face = "bold", size = BASE_SIZE),
    axis.text = element_text(face = "bold", size = BASE_SIZE, colour = "black")
  )

# c) Dry-to-wet transition duration
transition_med <- median(whip_dat$transition_days, na.rm = TRUE)

p2c <- ggplot(whip_dat, aes(x = transition_days)) +
  geom_histogram(
    bins = 10,
    boundary = 0,
    closed = "left",
    fill = fig2_cols["orange"],
    colour = "white",
    linewidth = 0.5,
    alpha = 0.95
  ) +
  geom_vline(
    xintercept = transition_med,
    linetype = "dashed",
    linewidth = 1.05,
    colour = fig2_cols["navy"]
  ) +
  annotate(
    "label",
    x = transition_med,
    y = Inf,
    label = paste0("Median = ", round(transition_med, 1), " days"),
    hjust = -0.08,
    vjust = 1.35,
    fontface = "bold",
    size = 4.8,
    fill = "white",
    label.size = 0.2
  ) +
  labs(
    x = "Dry-to-wet transition (days)",
    y = "Grid-event count",
    title = "c) Transition duration"
  ) +
  journal_theme(13) +
  theme(
    plot.title = element_text(face = "bold", size = BASE_SIZE + 2),
    axis.title = element_text(face = "bold", size = BASE_SIZE),
    axis.text = element_text(face = "bold", size = BASE_SIZE, colour = "black")
  )

# d) Whiplash Transition Speed distribution
wts_med <- median(whip_dat$WTS, na.rm = TRUE)

p2d <- ggplot(whip_dat, aes(x = WTS)) +
  geom_density(
    fill = scales::alpha(fig2_cols["purple"], 0.58),
    colour = fig2_cols["purple"],
    linewidth = 1.2
  ) +
  geom_rug(
    colour = scales::alpha(fig2_cols["pink"], 0.55),
    linewidth = 0.35,
    sides = "b"
  ) +
  geom_vline(
    xintercept = wts_med,
    linetype = "dashed",
    linewidth = 1.05,
    colour = fig2_cols["blue"]
  ) +
  annotate(
    "label",
    x = wts_med,
    y = Inf,
    label = paste0("Median = ", round(wts_med, 2)),
    hjust = -0.08,
    vjust = 1.35,
    fontface = "bold",
    size = 4.8,
    fill = "white",
    label.size = 0.2
  ) +
  labs(
    x = "Whiplash Transition Speed (WTS)",
    y = "Density",
    title = "d) Whiplash intensity distribution"
  ) +
  journal_theme(13) +
  theme(
    plot.title = element_text(face = "bold", size = BASE_SIZE + 2),
    axis.title = element_text(face = "bold", size = BASE_SIZE),
    axis.text = element_text(face = "bold", size = BASE_SIZE, colour = "black")
  )

fig2 <- (p2a | p2b) / (p2c | p2d)

save_figure(
  fig2,
  "Figure_2_Whiplash_Event_Climatology",
  14.5,
  10.2
)


# ==============================================================================

# ==============================================================================
# 15. FIGURE 3 — SPATIAL WHIPLASH PATTERNS (4 MAPS)
# ==============================================================================
p3a <- ggplot() + geom_sf(data = study, fill = "white", linewidth = 0.8) +
  geom_sf(data = grid_map, aes(fill = n_whiplash), linewidth = 0.2) +
  scale_fill_viridis_c(option = "A", name = "Whiplash\ncount", na.value = "grey92") +
  map_coord(study) + map_theme() + labs(title = "Whiplash frequency")
p3a <- add_map_axes(p3a) |> add_north_scale_bl()

p3b <- ggplot() + geom_sf(data = study, fill = "white", linewidth = 0.8) +
  geom_sf(data = grid_map, aes(fill = whiplash_rate), linewidth = 0.2) +
  scale_fill_viridis_c(option = "C", limits = c(0, 1), labels = percent,
                       name = "Whiplash\nproportion", na.value = "grey92") +
  map_coord(study) + map_theme() + labs(title = "Whiplash proportion")
p3b <- add_map_axes(p3b)

p3c <- ggplot() + geom_sf(data = study, fill = "white", linewidth = 0.8) +
  geom_sf(data = grid_map, aes(fill = ante_dryness_mean), linewidth = 0.2) +
  scale_fill_viridis_c(option = "B", name = "Mean antecedent\ndryness (z)", na.value = "grey92") +
  map_coord(study) + map_theme() + labs(title = "Antecedent dryness")
p3c <- add_map_axes(p3c)

p3d <- ggplot() + geom_sf(data = study, fill = "white", linewidth = 0.8) +
  geom_sf(data = grid_map, aes(fill = WTS_mean), linewidth = 0.2) +
  scale_fill_viridis_c(option = "D", name = "Mean WTS", na.value = "grey92") +
  map_coord(study) + map_theme() + labs(title = "Transition speed")
p3d <- add_map_axes(p3d)

fig3 <- (panel_heading(p3a, "a") | panel_heading(p3b, "b")) /
  (panel_heading(p3c, "c") | panel_heading(p3d, "d"))
save_figure(fig3, "Figure_3_Spatial_Whiplash_Patterns", 14.5, 10.5)

# ==============================================================================
# 16. FIGURE 4 — VEGETATION RESPONSE + HYDROCLIMATIC MEMORY
# ==============================================================================
p4a <- ggplot(dat, aes(whiplash, evi_shock, fill = whiplash)) +
  geom_violin(trim = FALSE, alpha = 0.50, linewidth = 0.4) +
  geom_boxplot(width = 0.18, outlier.shape = NA, alpha = 0.90) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_fill_viridis_d(option = "D", end = 0.85, guide = "none") +
  labs(x = NULL, y = expression(Delta*"EVI (post - pre)"),
       title = "Observed vegetation response") + journal_theme()

p4b <- ggplot(dat, aes(rain7_z, evi_shock, colour = whiplash)) +
  geom_point(alpha = 0.55, size = 1.8) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 1.0) +
  scale_colour_viridis_d(option = "D", end = 0.85, name = NULL) +
  labs(x = "7-day rainfall anomaly (z)", y = expression(Delta*"EVI"),
       title = "Wet severity and vegetation shock") + journal_theme()

p4c <- ggplot(dat, aes(ante_dryness_z, evi_shock)) +
  geom_point(alpha = 0.45, size = 1.7) +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 5), se = TRUE, linewidth = 1.1) +
  labs(x = "Antecedent dryness (z; higher = drier)", y = expression(Delta*"EVI"),
       title = "Hydroclimatic memory") + journal_theme()

p4d <- ggplot(whip_dat, aes(WTS, evi_shock)) +
  geom_point(alpha = 0.55, size = 1.8) +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 4), se = TRUE, linewidth = 1.1) +
  labs(x = "Whiplash Transition Speed", y = expression(Delta*"EVI"),
       title = "Transition-speed dose response") + journal_theme()

fig4 <- (panel_heading(p4a, "a") | panel_heading(p4b, "b")) /
  (panel_heading(p4c, "c") | panel_heading(p4d, "d"))
save_figure(fig4, "Figure_4_Vegetation_Response_and_Hydroclimatic_Memory", 14, 10)

# ==============================================================================
# 17. FIGURE 5 — ADJUSTED MODEL EFFECTS + NONLINEARITY
# ==============================================================================
# Panel a: selected adjusted coefficients
coef_select <- bind_rows(
  broom.mixed::tidy(M1, effects = "fixed", conf.int = TRUE) %>%
    filter(term %in% c("whiplash_num", "rain7_z")) %>% mutate(model = "Primary"),
  broom.mixed::tidy(M2, effects = "fixed", conf.int = TRUE) %>%
    filter(term %in% c("ante_dryness_z", "rain7_z", "ante_dryness_z:rain7_z")) %>%
    mutate(model = "Continuous memory"),
  broom.mixed::tidy(M3, effects = "fixed", conf.int = TRUE) %>%
    filter(term %in% c("whiplash_num", "rain7_z", "runoff7_s")) %>%
    mutate(model = "Runoff adjusted"),
  broom.mixed::tidy(M4, effects = "fixed", conf.int = TRUE) %>%
    filter(term %in% c("WTS_s", "dry_severity_s", "rain7_z")) %>%
    mutate(model = "Whiplash-only")
) %>%
  mutate(label = recode(term,
                        whiplash_num = "Whiplash",
                        rain7_z = "Wet severity",
                        ante_dryness_z = "Antecedent dryness",
                        `ante_dryness_z:rain7_z` = "Dryness × wet severity",
                        runoff7_s = "Runoff",
                        WTS_s = "Transition speed",
                        dry_severity_s = "Dry-state severity"))

p5a <- ggplot(coef_select, aes(estimate, fct_reorder(label, estimate), colour = model)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.15, linewidth = 0.8) +
  geom_point(size = 2.7) +
  scale_colour_viridis_d(option = "D", end = 0.85) +
  labs(x = "Adjusted coefficient (95% CI)", y = NULL, colour = "Model",
       title = "Adjusted hydroclimatic effects") + journal_theme(12)

# Panel b: interaction predictions from M2
pred_int <- as.data.frame(ggeffects::ggpredict(
  M2,
  terms = c("ante_dryness_z [all]", "rain7_z [-1,0,1]")
))
p5b <- ggplot(pred_int, aes(x, predicted, colour = group, fill = group)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.16, colour = NA) +
  geom_line(linewidth = 1.1) +
  scale_colour_viridis_d(option = "D", end = 0.85, name = "Rainfall z") +
  scale_fill_viridis_d(option = "D", end = 0.85, guide = "none") +
  labs(x = "Antecedent dryness (z)", y = "Adjusted EVI shock",
       title = "Adjusted dryness × wet-severity response") + journal_theme(12)

# Panel c: nonlinear GAM response surface
nd <- expand.grid(
  ante_dryness_z = seq(quantile(dat$ante_dryness_z, 0.02), quantile(dat$ante_dryness_z, 0.98), length.out = 60),
  rain7_z = seq(quantile(dat$rain7_z, 0.02), quantile(dat$rain7_z, 0.98), length.out = 60),
  evi_pre_s = 0, crop_frac_s = 0, temp7_s = 0, pet30_s = 0,
  year_decade = 0, month_f = factor("Apr", levels = levels(dat$month_f)),
  grid_f = dat$grid_f[1], event_f = dat$event_f[1]
)
nd$pred <- as.numeric(predict(M5_GAM, newdata = nd, type = "response",
                              exclude = c("s(grid_f)", "s(event_f)")))
p5c <- ggplot(nd, aes(ante_dryness_z, rain7_z, fill = pred)) +
  geom_raster(interpolate = TRUE) +
  geom_contour(aes(z = pred), colour = "white", alpha = 0.70, linewidth = 0.35) +
  scale_fill_viridis_c(option = "C", name = "Predicted\nEVI shock") +
  labs(x = "Antecedent dryness (z)", y = "7-day rainfall anomaly (z)",
       title = "Nonlinear memory surface") + journal_theme(12)

# Panel d: primary residual diagnostic
m1_diag <- data.frame(fitted = fitted(M1), resid = residuals(M1))
p5d <- ggplot(m1_diag, aes(fitted, resid)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(alpha = 0.5, size = 1.6) +
  geom_smooth(method = "loess", se = TRUE, linewidth = 1.0) +
  labs(x = "Fitted EVI shock", y = "Residual",
       title = "Primary-model residual structure") + journal_theme(12)

fig5 <- (panel_heading(p5a, "a") | panel_heading(p5b, "b")) /
  (panel_heading(p5c, "c") | panel_heading(p5d, "d"))
save_figure(fig5, "Figure_5_Adjusted_Effects_and_Nonlinear_Response", 15, 10.5)

# ==============================================================================
# 18. FIGURE 6 — MATCHING + ROBUSTNESS (REDESIGNED)
# Replace the COMPLETE existing Section 18 with this block.
# ==============================================================================

F6_BASE <- if (exists("BASE_SIZE")) max(BASE_SIZE, 15) else 15

fig6_cols <- c(
  coral  = "#E76F51",
  teal   = "#2A9D8F",
  blue   = "#277DA1",
  navy   = "#264653",
  purple = "#7B2CBF",
  orange = "#F8961E",
  green  = "#43AA8B",
  grey   = "#707070"
)

fig6_theme <- function() {
  theme_minimal(base_size = F6_BASE) +
    theme(
      text = element_text(face = "bold", colour = "black"),
      plot.title = element_text(
        face = "bold",
        size = F6_BASE + 2,
        colour = "black",
        margin = margin(b = 7)
      ),
      plot.subtitle = element_text(
        face = "bold",
        size = F6_BASE - 1,
        colour = "grey25",
        margin = margin(b = 8)
      ),
      axis.title = element_text(
        face = "bold",
        size = F6_BASE,
        colour = "black"
      ),
      axis.text = element_text(
        face = "bold",
        size = F6_BASE - 1,
        colour = "black"
      ),
      legend.title = element_text(
        face = "bold",
        size = F6_BASE - 1
      ),
      legend.text = element_text(
        face = "bold",
        size = F6_BASE - 1
      ),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(
        colour = "grey90",
        linewidth = 0.35
      ),
      plot.margin = margin(9, 12, 9, 10)
    )
}

# ==============================================================================
# a) COVARIATE BALANCE
# ==============================================================================

balance_plot <- bal_df %>%
  transmute(
    covariate = as.character(covariate),
    before = abs(as.numeric(Diff.Un)),
    after  = abs(as.numeric(Diff.Adj))
  ) %>%
  filter(is.finite(before) | is.finite(after)) %>%
  mutate(
    covariate_clean = dplyr::recode(
      covariate,
      "distance" = "Propensity score",
      "rain7_z" = "7-day rainfall anomaly",
      "temp7_s" = "Temperature",
      "evi_pre_s" = "Pre-event EVI",
      "pet30_s" = "Potential evaporation",
      "crop_frac_s" = "Cropland fraction",
      "year_decade" = "Year",
      "month_f_Feb" = "February",
      "month_f_Mar" = "March",
      "month_f_Apr" = "April",
      "month_f_May" = "May",
      .default = covariate
    ),
    order_val = pmax(before, after, na.rm = TRUE)
  ) %>%
  arrange(order_val) %>%
  mutate(
    covariate_clean =
      factor(covariate_clean, levels = covariate_clean)
  )

p6a <- ggplot(balance_plot, aes(y = covariate_clean)) +

  annotate(
    "rect",
    xmin = 0,
    xmax = 0.10,
    ymin = -Inf,
    ymax = Inf,
    fill = scales::alpha(fig6_cols["green"], 0.10)
  ) +

  annotate(
    "rect",
    xmin = 0.10,
    xmax = 0.20,
    ymin = -Inf,
    ymax = Inf,
    fill = scales::alpha(fig6_cols["orange"], 0.08)
  ) +

  geom_segment(
    aes(
      x = after,
      xend = before,
      yend = covariate_clean
    ),
    linewidth = 1.15,
    colour = "grey75",
    lineend = "round"
  ) +

  geom_point(
    aes(x = before, colour = "Before matching"),
    size = 4
  ) +

  geom_point(
    aes(x = after, colour = "After matching"),
    size = 4
  ) +

  geom_vline(
    xintercept = 0.10,
    linetype = "dashed",
    linewidth = 1,
    colour = fig6_cols["navy"]
  ) +

  scale_colour_manual(
    values = c(
      "Before matching" = fig6_cols["coral"],
      "After matching" = fig6_cols["teal"]
    ),
    name = NULL
  ) +

  labs(
    title = "a) Covariate balance after matching",
    x = "Absolute standardized mean difference",
    y = NULL
  ) +

  fig6_theme() +

  theme(
    legend.position = "top",
    panel.grid.major.y = element_blank()
  )


# ==============================================================================
# b) MATCHED-EVENT EFFECT ROBUSTNESS
# ==============================================================================

m1_eff <- extract_fixed(M1, "whiplash_num") %>%
  mutate(
    analysis = "Primary mixed model"
  )

meff <- matched_effect %>%
  transmute(
    term,
    estimate,
    std.error,
    statistic,
    p.value,
    conf.low,
    conf.high,
    analysis = "Matched-pair fixed effect"
  )

eff_compare <- bind_rows(
  m1_eff %>% select(any_of(names(meff))),
  meff
) %>%
  mutate(
    analysis = factor(
      analysis,
      levels = c(
        "Matched-pair fixed effect",
        "Primary mixed model"
      )
    ),

    analysis_type = if_else(
      analysis == "Primary mixed model",
      "Primary model",
      "Matched analysis"
    )
  )

p6b <- ggplot(
  eff_compare,
  aes(x = estimate, y = analysis)
) +

  annotate(
    "rect",
    xmin = -0.0025,
    xmax = 0.0025,
    ymin = -Inf,
    ymax = Inf,
    fill = "grey95"
  ) +

  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    linewidth = 1,
    colour = "grey30"
  ) +

  geom_segment(
    aes(
      x = conf.low,
      xend = conf.high,
      yend = analysis,
      colour = analysis_type
    ),
    linewidth = 1.5,
    lineend = "round"
  ) +

  geom_point(
    aes(fill = analysis_type),
    shape = 21,
    size = 5,
    stroke = 0.55,
    colour = "black"
  ) +

  scale_colour_manual(
    values = c(
      "Primary model" = fig6_cols["blue"],
      "Matched analysis" = fig6_cols["purple"]
    )
  ) +

  scale_fill_manual(
    values = c(
      "Primary model" = fig6_cols["blue"],
      "Matched analysis" = fig6_cols["purple"]
    )
  ) +

  labs(
    title = "b) Matched-event effect robustness",
    x = "Estimated whiplash effect on EVI shock (95% CI)",
    y = NULL
  ) +

  fig6_theme() +

  guides(
    colour = "none",
    fill = "none"
  ) +

  theme(
    panel.grid.major.y = element_blank()
  )


# ==============================================================================
# c) MOST INFLUENTIAL EVENT EXCLUSIONS
# ==============================================================================

primary_beta <-
  unname(lme4::fixef(M1)["whiplash_num"])

loo_influence <- loo %>%
  filter(is.finite(estimate)) %>%
  mutate(

    delta_beta =
      estimate - primary_beta,

    abs_delta =
      abs(delta_beta),

    direction =
      if_else(
        delta_beta >= 0,
        "Higher after exclusion",
        "Lower after exclusion"
      ),

    event_date =
      suppressWarnings(
        as.Date(
          sub("^EV_", "", excluded_event),
          format = "%Y%m%d"
        )
      ),

    event_label =
      if_else(
        !is.na(event_date),
        format(event_date, "%d %b %Y"),
        as.character(excluded_event)
      )
  )

# Show only the 10 strongest influences
n_show <-
  min(10L, nrow(loo_influence))

loo_top <- loo_influence %>%
  slice_max(
    order_by = abs_delta,
    n = n_show,
    with_ties = FALSE
  ) %>%
  arrange(delta_beta) %>%
  mutate(
    event_label =
      factor(
        event_label,
        levels = event_label
      )
  )

max_delta <-
  max(
    abs(loo_influence$delta_beta),
    na.rm = TRUE
  )

if (!is.finite(max_delta) ||
    max_delta == 0) {
  max_delta <- 0.01
}

loo_subtitle <- sprintf(
  "Top %d of %d exclusions by |Δβ|; rug shows all leave-one-event-out refits",
  n_show,
  nrow(loo_influence)
)

p6c <- ggplot(
  loo_top,
  aes(y = event_label)
) +

  # very small-change zone
  annotate(
    "rect",
    xmin = -0.002,
    xmax = 0.002,
    ymin = -Inf,
    ymax = Inf,
    fill = scales::alpha(
      fig6_cols["green"],
      0.08
    )
  ) +

  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    linewidth = 1,
    colour = "grey30"
  ) +

  # lollipop stems
  geom_segment(
    aes(
      x = 0,
      xend = delta_beta,
      yend = event_label,
      colour = direction
    ),
    linewidth = 1.35,
    lineend = "round"
  ) +

  # lollipop heads
  geom_point(
    aes(
      x = delta_beta,
      fill = direction
    ),
    shape = 21,
    size = 4.6,
    stroke = 0.45,
    colour = "black"
  ) +

  # show ALL leave-one-event-out estimates
  geom_rug(
    data = loo_influence,
    aes(x = delta_beta),
    inherit.aes = FALSE,
    sides = "b",
    alpha = 0.35,
    linewidth = 0.55,
    colour = fig6_cols["grey"]
  ) +

  scale_colour_manual(
    values = c(
      "Higher after exclusion" =
        fig6_cols["orange"],

      "Lower after exclusion" =
        fig6_cols["blue"]
    )
  ) +

  scale_fill_manual(
    values = c(
      "Higher after exclusion" =
        fig6_cols["orange"],

      "Lower after exclusion" =
        fig6_cols["blue"]
    )
  ) +

  coord_cartesian(
    xlim = c(
      -1.15 * max_delta,
       1.15 * max_delta
    )
  ) +

  labs(
    title = "c) Most influential event exclusions",
    subtitle = loo_subtitle,
    x = paste0(
      "Change in whiplash coefficient ",
      "(leave-one-event-out − full model)"
    ),
    y = NULL
  ) +

  fig6_theme() +

  guides(
    colour = "none",
    fill = "none"
  ) +

  theme(
    panel.grid.major.y =
      element_blank(),

    axis.text.y =
      element_text(
        face = "bold",
        size = F6_BASE - 1
      )
  )


# ==============================================================================
# d) MODEL-SPECIFICATION SENSITIVITY
# ==============================================================================

spec_plot <- spec_effects %>%
  mutate(

    specification = factor(
      specification,
      levels = rev(c(
        "Unadjusted",
        "+ wet severity",
        "Primary adjusted",
        "+ runoff mediator",
        "Two-way FE + two-way clustered SE"
      ))
    ),

    model_class = case_when(

      specification ==
        "Unadjusted" ~
        "Baseline",

      specification ==
        "+ wet severity" ~
        "Wet-severity adjusted",

      specification ==
        "Primary adjusted" ~
        "Primary",

      specification ==
        "+ runoff mediator" ~
        "Hydrologic sensitivity",

      TRUE ~
        "Two-way FE robustness"
    )
  )

spec_pal <- c(
  "Baseline" = "#E76F51",
  "Wet-severity adjusted" = "#F8961E",
  "Primary" = "#277DA1",
  "Hydrologic sensitivity" = "#2A9D8F",
  "Two-way FE robustness" = "#7B2CBF"
)

primary_row <-
  spec_plot %>%
  filter(
    as.character(specification) ==
      "Primary adjusted"
  )

primary_ci_low <- dplyr::first(
  primary_row$conf.low,
  default = NA_real_
)

primary_ci_high <- dplyr::first(
  primary_row$conf.high,
  default = NA_real_
)

p6d <-
  ggplot(
    spec_plot,
    aes(
      x = estimate,
      y = specification
    )
  )

# shaded reference band = primary adjusted model CI
if (
  is.finite(primary_ci_low) &&
  is.finite(primary_ci_high)
) {

  p6d <- p6d +

    annotate(
      "rect",
      xmin = primary_ci_low,
      xmax = primary_ci_high,
      ymin = -Inf,
      ymax = Inf,
      fill =
        scales::alpha(
          fig6_cols["blue"],
          0.07
        )
    )
}

p6d <- p6d +

  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    linewidth = 1,
    colour = "grey30"
  ) +

  geom_segment(
    aes(
      x = conf.low,
      xend = conf.high,
      yend = specification,
      colour = model_class
    ),
    linewidth = 1.45,
    lineend = "round"
  ) +

  geom_point(
    aes(fill = model_class),
    shape = 21,
    size = 4.8,
    stroke = 0.45,
    colour = "black"
  ) +

  scale_colour_manual(
    values = spec_pal
  ) +

  scale_fill_manual(
    values = spec_pal
  ) +

  labs(
    title =
      "d) Model-specification sensitivity",

    x =
      "Whiplash coefficient (95% CI)",

    y = NULL
  ) +

  fig6_theme() +

  guides(
    colour = "none",
    fill = "none"
  ) +

  theme(
    panel.grid.major.y =
      element_blank()
  )


# ==============================================================================
# COMBINE FIGURE 6
# ==============================================================================

fig6 <-
  (p6a | p6b) /
  (p6c | p6d) +

  patchwork::plot_layout(
    heights = c(1, 1.08)
  )

save_figure(
  fig6,
  "Figure_6_Matching_and_Robustness",
  16.5,
  11.5
)
# ==============================================================================
# 19. FIGURE 7 — SENTINEL-1 EVENT VALIDATION MAPS
# ==============================================================================
if (nrow(s1_dat) > 0) {
  s1_event_stats <- s1_dat %>%
    group_by(event_id, year) %>%
    summarise(
      n = n(), whip_share = mean(whiplash_num),
      mean_flood = mean(flood_frac, na.rm = TRUE),
      .groups = "drop"
    )

  # Prefer two whiplash-dominant events; complement with two high-flood non-whiplash events.
  ev_wh <- s1_event_stats %>% filter(whip_share > 0.5) %>% arrange(desc(mean_flood)) %>% slice_head(n = 2)
  ev_non <- s1_event_stats %>% filter(whip_share == 0) %>% arrange(desc(mean_flood)) %>% slice_head(n = 2)
  selected_events <- bind_rows(ev_wh, ev_non) %>% distinct(event_id, .keep_all = TRUE) %>% slice_head(n = 4)

  flood_max <- max(s1_dat$flood_frac[s1_dat$event_id %in% selected_events$event_id], na.rm = TRUE)
  if (!is.finite(flood_max) || flood_max <= 0) flood_max <- 1

  s1_maps <- list()
  for (i in seq_len(nrow(selected_events))) {
    ev <- selected_events$event_id[i]
    dd <- dat %>% filter(event_id == ev) %>% select(grid_id, flood_frac, whiplash_num)
    mm <- grid_sf %>% left_join(dd, by = "grid_id")
    typ <- ifelse(selected_events$whip_share[i] > 0.5, "Whiplash-dominant", "Non-whiplash")
    pp <- ggplot() +
      geom_sf(data = study, fill = "white", linewidth = 0.8) +
      geom_sf(data = mm, aes(fill = flood_frac), linewidth = 0.20) +
      scale_fill_viridis_c(option = "C", limits = c(0, flood_max), labels = percent,
                           na.value = "grey92", name = "Flooded\ncropland") +
      map_coord(study) + map_theme() +
      labs(title = paste0(ev, "\n", typ))
    pp <- add_map_axes(pp)
    if (i == 1) pp <- add_north_scale_bl(pp)
    s1_maps[[i]] <- pp
  }
  while (length(s1_maps) < 4) s1_maps[[length(s1_maps) + 1]] <- ggplot() + theme_void()
  fig7 <- (panel_heading(s1_maps[[1]], "a") | panel_heading(s1_maps[[2]], "b")) /
    (panel_heading(s1_maps[[3]], "c") | panel_heading(s1_maps[[4]], "d"))
  save_figure(fig7, "Figure_7_Sentinel1_Event_Flood_Validation", 14.5, 10.5)
}

# ==============================================================================
# 20. FIGURE 8 — SPATIAL IMPACT + RESIDUAL DIAGNOSTICS
# ==============================================================================
p8a <- ggplot() + geom_sf(data = study, fill = "white", linewidth = 0.8) +
  geom_sf(data = grid_map, aes(fill = evi_shock_mean), linewidth = 0.2) +
  scale_fill_viridis_c(option = "D", direction = -1, name = "Mean\nEVI shock", na.value = "grey92") +
  map_coord(study) + map_theme() + labs(title = "Mean vegetation response")
p8a <- add_map_axes(p8a) |> add_north_scale_bl()

p8b <- ggplot() + geom_sf(data = study, fill = "white", linewidth = 0.8) +
  geom_sf(data = grid_map, aes(fill = flood_frac_mean), linewidth = 0.2) +
  scale_fill_viridis_c(option = "C", labels = percent, name = "Mean S1\nflood fraction",
                       na.value = "grey92") +
  map_coord(study) + map_theme() + labs(title = "Sentinel-1 inundation support")
p8b <- add_map_axes(p8b)

p8c <- ggplot() + geom_sf(data = study, fill = "white", linewidth = 0.8) +
  geom_sf(data = grid_map, aes(fill = resid_mean), linewidth = 0.2) +
  scale_fill_gradient2(low = "#2166AC", mid = "white", high = "#B2182B", midpoint = 0,
                       name = "Mean model\nresidual", na.value = "grey92") +
  map_coord(study) + map_theme() + labs(title = "Residual spatial structure")
p8c <- add_map_axes(p8c)

if (nrow(moran_scatter) >= 3) {
  p8d <- ggplot(moran_scatter, aes(x, lag)) +
    geom_hline(yintercept = 0, linewidth = 0.5) + geom_vline(xintercept = 0, linewidth = 0.5) +
    geom_point(size = 2.3, alpha = 0.75) +
    geom_smooth(method = "lm", se = TRUE, linewidth = 1.0) +
    labs(x = "Standardized mean residual", y = "Spatial lag",
         title = "Moran residual scatter") + journal_theme(12)
} else {
  p8d <- ggplot() +
    annotate("text", x = 0, y = 0,
             label = "Insufficient valid grids\nfor Moran residual scatter",
             fontface = "bold", size = 5.3) +
    xlim(-1, 1) + ylim(-1, 1) +
    labs(x = NULL, y = NULL, title = "Moran residual scatter") + journal_theme(12)
}

fig8 <- (panel_heading(p8a, "a") | panel_heading(p8b, "b")) /
  (panel_heading(p8c, "c") | panel_heading(p8d, "d"))
save_figure(fig8, "Figure_8_Spatial_Impact_and_Model_Diagnostics", 14.5, 10.5)

# ==============================================================================
# 21. SUPPLEMENTARY DIAGNOSTICS
# ==============================================================================
# QQ + residual-vs-fitted primary model
pqq <- ggplot(m1_diag, aes(sample = resid)) +
  stat_qq(size = 1.5, alpha = 0.6) + stat_qq_line(linewidth = 1) +
  labs(x = "Theoretical quantiles", y = "Residual quantiles", title = "Residual Q-Q plot") +
  journal_theme()
prf <- ggplot(m1_diag, aes(fitted, sqrt(abs(resid)))) +
  geom_point(alpha = 0.5) + geom_smooth(method = "loess", se = TRUE) +
  labs(x = "Fitted values", y = expression(sqrt("|residual|")), title = "Scale-location") +
  journal_theme()
save_figure(pqq | prf, "Figure_S2_Primary_Model_Diagnostics", 12, 5.5)

# Correlation matrix table for continuous predictors
corr_vars <- c("ante_dryness_z", "rain7_z", "temp7", "pet30", "runoff7",
               "crop_frac_strict", "evi_pre", "elevation_mean", "slope_mean")
corr_mat <- cor(dat[, corr_vars], use = "pairwise.complete.obs", method = "spearman")
fwrite(as.data.frame(corr_mat) %>% tibble::rownames_to_column("variable"),
       file.path(TAB_DIR, "Table_S10_Spearman_Predictor_Correlation.csv"))

# ==============================================================================
# 22. MANUSCRIPT-READY SUMMARY TABLE
# ==============================================================================
key_results <- list()
add_key <- function(label, model, term) {
  tt <- extract_fixed(model, term)
  if (is.null(tt)) return(NULL)
  tibble(
    analysis = label,
    term = term,
    estimate = tt$estimate,
    conf_low = tt$conf.low,
    conf_high = tt$conf.high,
    p_value = tt$p.value
  )
}
key_results[[1]] <- add_key("Primary binary whiplash model", M1, "whiplash_num")
key_results[[2]] <- add_key("Continuous antecedent dryness", M2, "ante_dryness_z")
key_results[[3]] <- add_key("Dryness × wet-severity interaction", M2, "ante_dryness_z:rain7_z")
key_results[[4]] <- add_key("Runoff-adjusted whiplash", M3, "whiplash_num")
key_results[[5]] <- add_key("Transition-speed model", M4, "WTS_s")
fe_key <- tidy_fixest_safe(M1_FE2W) %>% filter(term == "whiplash_num")
if (nrow(fe_key) == 1) {
  key_results[[6]] <- tibble(
    analysis = "Two-way FE robustness (event + grid FE; two-way clustered SE)",
    term = "whiplash_num",
    estimate = fe_key$estimate,
    conf_low = fe_key$conf.low,
    conf_high = fe_key$conf.high,
    p_value = fe_key$p.value
  )
}
if (!is.null(M6_S1)) key_results[[7]] <- add_key("S1 flood fraction → EVI response", M6_S1, "flood_frac")
key_results_table <- bind_rows(key_results)
fwrite(key_results_table, file.path(TAB_DIR, "Table_10_Key_Inferential_Results.csv"))

# ==============================================================================
# 23. EXCEL WORKBOOK WITH CORE MANUSCRIPT TABLES
# ==============================================================================
wb <- createWorkbook()
write_sheet <- function(name, obj) {
  addWorksheet(wb, substr(name, 1, 31))
  writeData(wb, substr(name, 1, 31), obj)
  freezePane(wb, substr(name, 1, 31), firstRow = TRUE)
  setColWidths(wb, substr(name, 1, 31), cols = 1:ncol(obj), widths = "auto")
}
write_sheet("T1_Sample_QA", sample_overview)
write_sheet("T2_Descriptive", descriptive)
write_sheet("T3_Trend", trend_table)
write_sheet("T4_Model_Coefficients", model_table)
write_sheet("T5_Model_Performance", model_performance)
if (nrow(bal_df) > 0) write_sheet("T6_Matching_Balance", bal_df)
write_sheet("T7_Matched_Effect", matched_effect)
write_sheet("T8_MoransI", moran_table)
write_sheet("T9_Sentinel1", s1_summary)
write_sheet("T10_Key_Results", key_results_table)
write_sheet("S1_Missingness", missingness)
write_sheet("S2_Structural_NA", qa_structural)
if (nrow(dup_audit) > 0) write_sheet("S3_Duplicates", as.data.frame(dup_audit))
write_sheet("S4_LOO", loo)
write_sheet("S5_Spec_Robust", spec_effects)
write_sheet("S6_TwoWayFE_Robust", M1_FE2W_tab)
if (!is.null(threshold_sens) && nrow(threshold_sens) > 0) write_sheet("S7_Thresholds", threshold_sens)
saveWorkbook(wb, file.path(TAB_DIR, "All_Manuscript_Tables.xlsx"), overwrite = TRUE)

# ==============================================================================
# 24. AUTOMATED RESULTS / QA LOG
# ==============================================================================
primary_eff <- extract_fixed(M1, "whiplash_num")
interaction_eff <- extract_fixed(M2, "ante_dryness_z:rain7_z")
wts_eff <- extract_fixed(M4, "WTS_s")

log_lines <- c(
  "SUNAMGANJ HYDROCLIMATIC WHIPLASH — ANALYSIS SUMMARY",
  paste0("Generated: ", Sys.time()),
  "",
  "DATASET",
  paste0("Raw rows: ", nrow(raw)),
  paste0("Clean unique event-grid rows: ", nrow(dat)),
  paste0("Regional events: ", n_distinct(dat$event_id)),
  paste0("Active grids: ", n_distinct(dat$grid_id)),
  paste0("Whiplash rows: ", sum(dat$whiplash_num == 1)),
  paste0("Non-whiplash rows: ", sum(dat$whiplash_num == 0)),
  paste0("Duplicate event-grid rows removed: ", nrow(raw) - nrow(dat)),
  paste0("Missing flood_frac: ", sum(is.na(dat$flood_frac)), " / ", nrow(dat)),
  "",
  "IMPORTANT MISSINGNESS INTERPRETATION",
  "dry_* and WTS are structurally undefined for non-whiplash rows and were not imputed.",
  "Sentinel-1 flood_frac is treated as secondary validation and analyzed only where observed.",
  "",
  "PRIMARY MODEL",
  if (!is.null(primary_eff)) paste0("Whiplash coefficient = ", round(primary_eff$estimate, 4),
                                    "; 95% CI [", round(primary_eff$conf.low, 4), ", ",
                                    round(primary_eff$conf.high, 4), "]; p = ",
                                    signif(primary_eff$p.value, 4)) else "Primary coefficient unavailable",
  "",
  "CONTINUOUS MEMORY INTERACTION",
  if (!is.null(interaction_eff)) paste0("Dryness × wet-severity coefficient = ",
                                        round(interaction_eff$estimate, 4),
                                        "; 95% CI [", round(interaction_eff$conf.low, 4), ", ",
                                        round(interaction_eff$conf.high, 4), "]; p = ",
                                        signif(interaction_eff$p.value, 4)) else "Interaction unavailable",
  "",
  "TRANSITION SPEED",
  if (!is.null(wts_eff)) paste0("WTS standardized coefficient = ", round(wts_eff$estimate, 4),
                                "; 95% CI [", round(wts_eff$conf.low, 4), ", ",
                                round(wts_eff$conf.high, 4), "]; p = ",
                                signif(wts_eff$p.value, 4)) else "WTS coefficient unavailable",
  "",
  "MIXED-MODEL QA",
  paste0("M1 singular fit: ", tryCatch(lme4::isSingular(M1, tol = 1e-4), error = function(e) NA)),
  "If a mixed model is singular, conditional R2/ICC may be unavailable; the two-way fixed-effects model with two-way clustered SE is retained as a robustness check.",
  "",
  "SENTINEL-1 CAUTION",
  paste0("S1 observations: ", nrow(s1_dat), "; S1 events: ", n_distinct(s1_dat$event_id),
         "; whiplash S1 events: ", n_distinct(s1_dat$event_id[s1_dat$whiplash_num == 1])),
  "Because the S1-era whiplash-event count may be small, S1 is secondary physical validation, not the primary causal comparison.",
  "",
  "OUTPUT",
  paste0("Figures: ", FIG_DIR),
  paste0("Tables: ", TAB_DIR),
  paste0("RDS cache: ", CACHE_DIR)
)
writeLines(log_lines, file.path(LOG_DIR, "Automated_Results_and_QA_Summary.txt"))
writeLines(capture.output(sessionInfo()), file.path(LOG_DIR, "R_sessionInfo.txt"))

message("\n============================================================")
message("ANALYSIS COMPLETE")
message("Figures: ", FIG_DIR)
message("Tables : ", TAB_DIR)
message("Cache  : ", CACHE_DIR)
message("============================================================\n")
