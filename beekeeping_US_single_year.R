# Interactive US Beekeeping Model Shiny App - Live Parameter Exploration
# Self-contained app with real-time simulation updates
# Author Antoine Champetier with help from Claude AI.
# November 2025

library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(tidyr)
library(plotly)
library(scales)

# =============================================================================
# PARAMETER HANDLING
# =============================================================================

# Function to load and parse parameter CSV
load_parameter_config <- function(filepath, silent = FALSE) {
  tryCatch({
    params_df <- read.csv(filepath, stringsAsFactors = FALSE)
    params_list <- list()
    for (i in 1:nrow(params_df)) {
      row <- params_df[i, ]
      params_list[[row$Parameter]] <- list(
        value = as.numeric(row$Value),
        min = as.numeric(row$Min),
        max = as.numeric(row$Max),
        step = as.numeric(row$Step)
      )
    }
    return(params_list)
  }, error = function(e) {
    if (!silent) {
      message("Warning: Could not read parameter file: ", e$message)
    }
    return(NULL)
  })
}

# Load default parameters from the CSV file at startup
# Try multiple possible paths to find the parameter file
param_file_paths <- unique(c(
  "beekeeping_settings_2026-03-18.csv",
  file.path("..", "beekeeping_settings_2026-03-18.csv"),
  file.path(getwd(), "beekeeping_settings_2026-03-18.csv"),
  file.path(getwd(), "..", "beekeeping_settings_2026-03-18.csv"),
  "app_parameters.csv",
  "implementation_multi_location_model_v0/app_parameters.csv",
  file.path(getwd(), "app_parameters.csv"),
  file.path(getwd(), "implementation_multi_location_model_v0", "app_parameters.csv")
))

default_params <- NULL
for (path in param_file_paths) {
  if (file.exists(path)) {
    default_params <- load_parameter_config(path, silent = TRUE)
    if (!is.null(default_params)) {
      message("Loaded default parameters from: ", normalizePath(path))
      break
    }
  }
}

if (is.null(default_params)) {
  message("No default parameter file found. Using hardcoded defaults.")
}

csv_default_value <- function(param_name, fallback) {
  if (!is.null(default_params) && !is.null(default_params[[param_name]])) {
    v <- suppressWarnings(as.numeric(default_params[[param_name]]$value))
    if (length(v) > 0 && is.finite(v[1])) {
      return(v[1])
    }
  }
  fallback
}

# =============================================================================
# CORE MODEL FUNCTIONS (embedded for self-contained app)
# =============================================================================

# Calculate forager share based on colony strength
calculate_forager_share <- function(frames_per_colony, omega = 0.5, theta = 0.3) {
  1 / (1 + exp(omega - theta * frames_per_colony))
}

# Forage collection function - simple step function
# A: max marginal forag10e, B: saturation point, I: intercept (not used for forage)
calculate_forage_collection <- function(foragers, A, B, I) {
  # Simple step function: A before saturation, 0 after
  result <- ifelse(foragers < B, A, 0)
  return(result)
}

# Forage collection integral function - simple ramp then plateau
# A: max marginal forage, B: saturation point, I: intercept (not used for forage)
calculate_forage_integral <- function(foragers, A, B, I) {
  # Simple ramp then plateau: A*x until B, then constant A*B
  result <- ifelse(foragers < B, A * foragers, A * B)
  return(result)
}

# Derivative of forage collection function - zero everywhere
# Since marginal production is constant (step function), derivative is 0
calculate_forage_derivative <- function(foragers, A, B, I) {
  # Derivative is 0 everywhere (step function has zero derivative except at discontinuity)
  result <- rep(0, length(foragers))
  return(result)
}

# Crop yield function - simple step function
# D: max marginal crop, E: saturation point, G: intercept (not used for crop)
calculate_crop_yield <- function(foragers, D, E, G) {
  # Simple step function: D before saturation, 0 after
  result <- ifelse(foragers < E, D, 0)
  return(result)
}

# Crop yield integral function - simple ramp then plateau
# D: max marginal crop, E: saturation point, G: intercept (not used for crop)
calculate_crop_integral <- function(foragers, D, E, G) {
  # Simple ramp then plateau: D*x until E, then constant D*E
  result <- ifelse(foragers < E, D * foragers, D * E)
  return(result)
}

# Derivative of crop yield function - zero everywhere
# Since marginal production is constant (step function), derivative is 0
calculate_crop_derivative <- function(foragers, D, E, G) {
  # Derivative is 0 everywhere (step function has zero derivative except at discontinuity)
  result <- rep(0, length(foragers))
  return(result)
}

# Calculate marginal product of foragers (derivative of crop yield) - using new function
calculate_marginal_crop_yield <- function(foragers, D, E, G) {
  return(calculate_crop_derivative(foragers, D, E, G))
}

# Calculate bee stock dynamics at end of period
calculate_bee_dynamics <- function(colonies_start, frames_start, alpha, delta, t_dur) {
  # Debug: Check for problematic inputs
  if (any(is.na(c(colonies_start, frames_start, alpha, delta, t_dur)))) {
    cat(
      "ERROR in calculate_bee_dynamics - NA inputs:",
      "colonies_start=", colonies_start, "frames_start=", frames_start,
      "alpha=", alpha, "delta=", delta, "t_dur=", t_dur, "\n"
    )
    return(list(frames_end = NA, colonies_end = NA))
  }
  
  # If colonies or frames are zero or negative, the operation is over - return zeros
  if (colonies_start <= 0 || frames_start <= 0) {
    return(list(frames_end = 0, colonies_end = 0))
  }
  
  beta <- alpha * t_dur
  frames_end <- pmax(0, frames_start + beta * colonies_start - delta * frames_start)
  colonies_end <- colonies_start * (1 - delta)
  
  # Debug: Check for problematic outputs
  if (any(is.na(c(frames_end, colonies_end))) || any(!is.finite(c(frames_end, colonies_end)))) {
    cat(
      "ERROR in calculate_bee_dynamics - problematic outputs:",
      "frames_end=", frames_end, "colonies_end=", colonies_end, "\n"
    )
  }
  
  return(list(frames_end = frames_end, colonies_end = colonies_end))
}

# Create a simple location schedule
create_simple_schedule <- function(n_years = 1, start_season = "Spring") {
  # Define season order starting from the selected season
  all_seasons <- c("Spring", "Summer", "Fall", "Winter")
  start_idx <- which(all_seasons == start_season)
  season_order <- c(all_seasons[start_idx:4], if(start_idx > 1) all_seasons[1:(start_idx-1)] else NULL)
  
  periods <- data.frame(
    period = 1:(n_years * 4),
    year = rep(1:n_years, each = 4),
    season = rep(season_order, n_years),
    t_start = seq(0, (n_years * 4 - 1) * 13, by = 13),
    t_dur = 13,
    stringsAsFactors = FALSE
  )
  periods$t_end <- periods$t_start + periods$t_dur
  return(periods)
}

# Resolve seasonal parameter names like alpha_spring or alpha_spring_loc2
get_seasonal_param <- function(params, base_name, season, default_value, location_suffix = "") {
  season_key <- tolower(season)
  seasonal_name <- paste0(base_name, "_", season_key, location_suffix)

  if (!is.null(params[[seasonal_name]])) {
    return(params[[seasonal_name]])
  }

  if (!is.null(params[[base_name]])) {
    return(params[[base_name]])
  }

  default_value
}

get_seasonal_vector <- function(schedule_seasons, params, base_name, default_value, location_suffix = "") {
  vapply(
    schedule_seasons,
    function(season) get_seasonal_param(params, base_name, season, default_value, location_suffix),
    numeric(1)
  )
}

SEASON_KEYS <- c("spring", "summer", "fall", "winter")
SEASON_LABELS <- c(spring = "Spring", summer = "Summer", fall = "Fall", winter = "Winter")
SEASON_ACTIVE_LOCATIONS <- list(
  winter = c("loc1", "loc2"),
  spring = c("loc1", "loc2", "loc3"),
  summer = c("loc1", "loc2", "loc3"),
  fall = c("loc1", "loc2")
)
US_LOCATION_LABELS <- list(
  winter = c(loc1 = "Almond pol", loc2 = "Yards"),
  spring = c(loc1 = "Tallow Forage", loc2 = "Veg/Berries pol", loc3 = "Yards"),
  summer = c(loc1 = "Dakotas Forage", loc2 = "Other forage", loc3 = "Yards"),
  fall = c(loc1 = "Yards (High Feed)", loc2 = "Yards (Low Feed)")
)

SEASONAL_DEFAULTS <- list(
  alpha = c(
    spring = csv_default_value("alpha_spring", 0.50),
    summer = csv_default_value("alpha_summer", 0.40),
    fall = csv_default_value("alpha_fall", -0.40),
    winter = csv_default_value("alpha_winter", 0.30)
  ),
  delta = c(
    spring = csv_default_value("delta_spring", 0.10),
    summer = csv_default_value("delta_summer", 0.12),
    fall = csv_default_value("delta_fall", 0.13),
    winter = csv_default_value("delta_winter", 0.14)
  ),
  gamma = c(
    spring = csv_default_value("gamma_spring", 2.0),
    summer = csv_default_value("gamma_summer", 1.5),
    fall = csv_default_value("gamma_fall", 3.0),
    winter = csv_default_value("gamma_winter", 0.8)
  ),
  omega = c(
    spring = csv_default_value("omega_spring", 0.2),
    summer = csv_default_value("omega_summer", 0.4),
    fall = csv_default_value("omega_fall", 0.1),
    winter = csv_default_value("omega_winter", 0.0)
  ),
  theta = c(
    spring = csv_default_value("theta_spring", 0.25),
    summer = csv_default_value("theta_summer", 0.35),
    fall = csv_default_value("theta_fall", 0.2),
    winter = csv_default_value("theta_winter", 0.15)
  ),
  A = c(
    spring = csv_default_value("A_spring", 15),
    summer = csv_default_value("A_summer", 40),
    fall = csv_default_value("A_fall", 5),
    winter = csv_default_value("A_winter", 0)
  ),
  B = c(
    spring = csv_default_value("B_spring", 8000),
    summer = csv_default_value("B_summer", 15000),
    fall = csv_default_value("B_fall", 20000),
    winter = csv_default_value("B_winter", 100000)
  ),
  Ph = c(
    spring = csv_default_value("Ph_spring", 5.0),
    summer = csv_default_value("Ph_summer", 4.5),
    fall = csv_default_value("Ph_fall", 5.5),
    winter = csv_default_value("Ph_winter", 6.0)
  ),
  D = c(
    spring = csv_default_value("D_spring", 8),
    summer = csv_default_value("D_summer", 25),
    fall = csv_default_value("D_fall", 3),
    winter = csv_default_value("D_winter", 0)
  ),
  E = c(
    spring = csv_default_value("E_spring", 10000),
    summer = csv_default_value("E_summer", 12000),
    fall = csv_default_value("E_fall", 25000),
    winter = csv_default_value("E_winter", 100000)
  ),
  Pc = c(
    spring = csv_default_value("Pc_spring", 40),
    summer = csv_default_value("Pc_summer", 45),
    fall = csv_default_value("Pc_fall", 35),
    winter = csv_default_value("Pc_winter", 0)
  )
)

season_value <- function(param_name, season_key) {
  as.numeric(SEASONAL_DEFAULTS[[param_name]][season_key])
}

season_param_id <- function(base_name, season_key, location_tag = "loc1") {
  suffix <- if (identical(location_tag, "loc1")) "" else paste0("_", location_tag)
  paste0(base_name, "_", season_key, suffix)
}

build_season_columns <- function(season_key, location_tag = "loc1") {
  omega_min <- if (identical(season_key, "winter")) 0.0 else 0.1

  tagList(
    column(
      3, h6("Growth & Loss"),
      sliderInput(season_param_id("alpha", season_key, location_tag), "Growth (frames/col/week):", min = -1.0, max = 2.0, value = season_value("alpha", season_key), step = 0.01),
      tags$div(
        textOutput(paste0("alpha_full_season_", season_key, "_", location_tag)),
        style = "font-size: 10px; color: #666; margin-top: -6px; margin-bottom: 4px;"
      ),
      sliderInput(season_param_id("delta", season_key, location_tag), "Loss Rate (%/season):", min = 0.01, max = 0.2, value = season_value("delta", season_key), step = 0.01)
    ),
    column(
      3, h6("Feed & Foraging"),
      sliderInput(season_param_id("gamma", season_key, location_tag), "Feed Consumpt.(lbs/frame/week):", min = 0, max = 5.0, value = season_value("gamma", season_key), step = 0.1),
      sliderInput(season_param_id("omega", season_key, location_tag), "Omega (base foraging):", min = omega_min, max = 1.0, value = season_value("omega", season_key), step = 0.05),
      sliderInput(season_param_id("theta", season_key, location_tag), "Theta (col. strength-foragers):", min = 0.1, max = 0.5, value = season_value("theta", season_key), step = 0.05),
      plotOutput(paste0(season_key, "_", location_tag, "_forager_plot"), height = "250px")
    ),
    column(
      3, h6("Forage Collection & Honey"),
      sliderInput(season_param_id("A", season_key, location_tag), "Forage Max Value:", value = season_value("A", season_key), min = 0, max = 1000, step = 1),
      sliderInput(season_param_id("B", season_key, location_tag), "Forage Midpoint:", value = season_value("B", season_key), min = 0, max = 100000, step = 1000),
      sliderInput(season_param_id("Ph", season_key, location_tag), "Honey Price ($/lbs):", value = season_value("Ph", season_key), min = 0, max = 20.0, step = 0.1),
      plotOutput(paste0(season_key, "_", location_tag, "_forage_plot"), height = "250px")
    ),
    column(
      3, h6("Crop Pollination"),
      sliderInput(season_param_id("D", season_key, location_tag), "Crop Max Value:", value = season_value("D", season_key), min = 0, max = 100, step = 1),
      sliderInput(season_param_id("E", season_key, location_tag), "Crop Midpoint:", value = season_value("E", season_key), min = 0, max = 100000, step = 1000),
      sliderInput(season_param_id("Pc", season_key, location_tag), "Crop Price ($/lbs):", value = season_value("Pc", season_key), min = 0, max = 100, step = 1),
      plotOutput(paste0(season_key, "_", location_tag, "_crop_plot"), height = "250px")
    )
  )
}

build_season_panel <- function(season_key) {
  season_label <- SEASON_LABELS[[season_key]]
  toggle_id <- paste0(season_key, "_loc_toggle")
  active_locations <- SEASON_ACTIVE_LOCATIONS[[season_key]]
  location_choices <- setNames(active_locations, US_LOCATION_LABELS[[season_key]][active_locations])

  conditionalPanel(
    condition = sprintf("input.season_selector == '%s'", season_key),
    div(
      style = "height: 665px; width: 95%; overflow-y: auto;",
      radioButtons(toggle_id, NULL,
                   choices = location_choices,
                   selected = active_locations[[1]], inline = TRUE
      ),
      tagList(lapply(active_locations, function(loc_tag) {
        conditionalPanel(
          condition = sprintf("input.%s == '%s'", toggle_id, loc_tag),
          div(
            style = "padding: 4px; background-color: #fff8e1; border-radius: 4px; margin-bottom: 6px;",
            em(
              sprintf("%s parameters for %s. Allocation share set in the Management panel.", US_LOCATION_LABELS[[season_key]][[loc_tag]], season_label),
              style = "font-size:11px; color:#666;"
            )
          ),
          fluidRow(build_season_columns(season_key, loc_tag))
        )
      }))
    )
  )
}

# Create location parameters for each period
create_location_parameters_generic <- function(schedule, params, location_id = 1, location_tag = "loc1") {
  season_vec <- schedule$season
  location_suffix <- if (identical(location_tag, "loc1")) "" else paste0("_", location_tag)
  cost_key <- if (identical(location_tag, "loc1")) "Cost" else paste0("Cost_", location_tag)
  cost_value <- params[[cost_key]] %||% params$Cost

  locations <- data.frame(
    period = schedule$period,
    location = location_id,
    season = season_vec,
    t_start = schedule$t_start,
    t_dur = schedule$t_dur,
    t_end = schedule$t_end,
    
    # Location-specific parameters (season/location lookup based)
    Cost = cost_value,
    Ph = get_seasonal_vector(season_vec, params, "Ph", params$Ph %||% 0, location_suffix = location_suffix),
    Pc = get_seasonal_vector(season_vec, params, "Pc", params$Pc %||% 0, location_suffix = location_suffix),
    
    # Biological parameters (seasonal variation)
    gamma = get_seasonal_vector(season_vec, params, "gamma", 0.5, location_suffix = location_suffix),
    alpha = get_seasonal_vector(season_vec, params, "alpha", 0.1, location_suffix = location_suffix),
    delta = get_seasonal_vector(season_vec, params, "delta", 0.13, location_suffix = location_suffix),
    omega = get_seasonal_vector(season_vec, params, "omega", 0.5, location_suffix = location_suffix),
    theta = get_seasonal_vector(season_vec, params, "theta", 0.3, location_suffix = location_suffix),
    
    # Forage production parameters (seasonal variation)
    # A = max value, B = midpoint, I = slope at midpoint (negative)
    A = get_seasonal_vector(season_vec, params, "A", params$A %||% 0, location_suffix = location_suffix),
    B = get_seasonal_vector(season_vec, params, "B", params$B %||% 0, location_suffix = location_suffix),
    I = get_seasonal_vector(season_vec, params, "I", params$I %||% 0, location_suffix = location_suffix),
    
    # Crop production parameters (seasonal variation)
    # D = max value, E = midpoint, G = slope at midpoint (negative)
    D = get_seasonal_vector(season_vec, params, "D", params$D %||% 0, location_suffix = location_suffix),
    E = get_seasonal_vector(season_vec, params, "E", params$E %||% 0, location_suffix = location_suffix),
    G = get_seasonal_vector(season_vec, params, "G", params$G %||% 0, location_suffix = location_suffix),
    stringsAsFactors = FALSE
  )
  
  return(locations)
}

create_location_parameters <- function(schedule, params) {
  create_location_parameters_generic(schedule, params, location_id = 1, location_tag = "loc1")
}

# Create location parameters for Location 2 (uses _loc2 suffixed params)
create_location_parameters_loc2 <- function(schedule, params) {
  create_location_parameters_generic(schedule, params, location_id = 2, location_tag = "loc2")
}

create_location_parameters_loc3 <- function(schedule, params) {
  create_location_parameters_generic(schedule, params, location_id = 3, location_tag = "loc3")
}

# Run single period simulation
simulate_period <- function(period_data, colonies_allocated, frames_allocated, P_feed) {
  # If colonies or frames are zero, return zero results immediately
  if (colonies_allocated <= 0 || frames_allocated <= 0) {
    return(list(
      frames_per_colony = 0, forager_share = 0, total_foragers = 0,
      forage_collected = 0, forage_consumed = 0, net_forage = 0,
      feed_required = 0, honey_harvested = 0, crop_yield = 0,
      colonies_end = 0, frames_end = 0,
      revenue_honey = 0, revenue_crop = 0, cost_maintenance = 0, cost_feed = 0, profit = 0
    ))
  }
  
  frames_per_colony <- frames_allocated / colonies_allocated
  forager_share <- calculate_forager_share(frames_per_colony, period_data$omega, period_data$theta)
  total_foragers <- forager_share * frames_allocated
  
  forage_collected <- calculate_forage_integral(
    total_foragers, period_data$A, period_data$B, period_data$I
  )
  
  crop_yield <- calculate_crop_yield(
    total_foragers, period_data$D, period_data$E, period_data$G
  )
  
  # Calculate marginal value product for pollination revenue
  marginal_crop_yield <- calculate_crop_yield(
    total_foragers, period_data$D, period_data$E, period_data$G
  )
  
  forage_consumed <- frames_allocated * period_data$gamma * period_data$t_dur
  net_forage <- forage_collected - forage_consumed
  feed_required <- pmax(0, -net_forage)
  honey_harvested <- pmax(0, net_forage)
  
  bee_dynamics <- calculate_bee_dynamics(
    colonies_allocated, frames_allocated, period_data$alpha, period_data$delta, period_data$t_dur
  )
  
  revenue_honey <- honey_harvested * period_data$Ph
  # Pollination revenue based on marginal value product
  revenue_crop <- marginal_crop_yield * period_data$Pc * total_foragers
  cost_maintenance <- colonies_allocated * period_data$Cost / 4 # Convert annual to per-period cost
  cost_feed <- feed_required * P_feed
  
  profit <- revenue_honey + revenue_crop - cost_maintenance - cost_feed
  
  # Ensure profit is not NA
  profit <- ifelse(is.na(profit), 0, profit)
  
  return(list(
    colonies_start = colonies_allocated,
    frames_start = frames_allocated,
    frames_per_colony = frames_per_colony,
    forager_share = forager_share,
    total_foragers = total_foragers,
    forage_collected = forage_collected,
    forage_consumed = forage_consumed,
    net_forage = net_forage,
    feed_required = feed_required,
    honey_harvested = honey_harvested,
    crop_yield = crop_yield,
    colonies_end = bee_dynamics$colonies_end,
    frames_end = bee_dynamics$frames_end,
    revenue_honey = revenue_honey,
    revenue_crop = revenue_crop,
    cost_maintenance = cost_maintenance,
    cost_feed = cost_feed,
    profit = profit
  ))
}

# Main simulation function
# locations_loc2 and allocation_params are optional; if NULL, single-location mode
run_beekeeping_simulation <- function(locations, initial_colonies, initial_frames, P_feed, management_params,
                                      locations_loc2 = NULL, allocation_params = NULL,
                                      location_tables = NULL) {
  safe_num <- function(x, default = 0) {
    val <- suppressWarnings(as.numeric(x))
    if (length(val) == 0 || !is.finite(val[1])) {
      return(default)
    }
    val[1]
  }
  
  # Defensive defaults so simulation remains stable even if caller omits cost fields.
  management_params$P_split <- safe_num(management_params$P_split, 0)
  management_params$P_merge <- safe_num(management_params$P_merge, 0)
  management_params$P_cull <- safe_num(management_params$P_cull, 0)
  
  build_location_tables <- function(locations, locations_loc2, location_tables) {
    if (!is.null(location_tables)) {
      return(location_tables)
    }
    if (!is.null(locations_loc2)) {
      return(list(loc1 = locations, loc2 = locations_loc2))
    }
    list(loc1 = locations)
  }

  get_share_vector <- function(allocation_params, n_locations, season_label, prefix) {
    if (is.null(allocation_params) || n_locations <= 1) {
      return(rep(1 / n_locations, n_locations))
    }

    season_key <- tolower(season_label)
    shares_pct <- rep(NA_real_, n_locations)
    explicit_count <- 0

    for (i in seq_len(n_locations)) {
      key <- paste0(prefix, "_loc", i, "_", season_key)
      if (!is.null(allocation_params[[key]])) {
        shares_pct[i] <- as.numeric(allocation_params[[key]])
        explicit_count <- explicit_count + 1
      }
    }

    if (explicit_count == 0) {
      return(rep(1 / n_locations, n_locations))
    }

    remaining <- 100 - sum(shares_pct, na.rm = TRUE)
    missing_idx <- which(is.na(shares_pct))
    if (length(missing_idx) > 0) {
      shares_pct[missing_idx] <- remaining / length(missing_idx)
    }

    shares <- pmax(0, shares_pct / 100)
    total <- sum(shares)
    if (!is.finite(total) || total <= 0) {
      return(rep(1 / n_locations, n_locations))
    }
    shares / total
  }
  
  get_fpc_targets <- function(allocation_params, n_locations, season_label, season_avg_fpc, cols_by_loc) {
    season_key <- tolower(season_label)
    targets <- rep(season_avg_fpc, n_locations)
    if (is.null(allocation_params)) {
      return(targets)
    }
    equal_key <- paste0("fpc_equal_", season_key)
    if (isTRUE(allocation_params[[equal_key]])) {
      # Enforce identical frames/colony across locations for this season.
      return(rep(season_avg_fpc, n_locations))
    }

    target_key_loc1 <- paste0("fpc_target_loc1_", season_key)
    target_loc1 <- suppressWarnings(as.numeric(allocation_params[[target_key_loc1]]))
    if (!is.finite(target_loc1) || target_loc1 < 0) {
      target_loc1 <- season_avg_fpc
    }

    safe_cols <- pmax(0, as.numeric(cols_by_loc))
    total_cols <- sum(safe_cols)
    total_frames <- season_avg_fpc * total_cols
    if (!is.finite(total_frames) || total_cols <= 0) {
      return(rep(0, n_locations))
    }

    c1 <- safe_cols[1]
    other_idx <- if (n_locations > 1) 2:n_locations else integer(0)
    other_cols <- if (length(other_idx) > 0) sum(safe_cols[other_idx]) else 0

    if (c1 <= 0) {
      if (other_cols > 0 && length(other_idx) > 0) {
        other_target <- total_frames / other_cols
        targets[other_idx] <- other_target
      } else {
        targets[] <- 0
      }
      return(targets)
    }

    if (other_cols <= 0 || length(other_idx) == 0) {
      targets[1] <- total_frames / c1
      return(targets)
    }

    other_target <- (total_frames - c1 * target_loc1) / other_cols
    if (is.finite(other_target) && other_target >= 0) {
      targets[1] <- target_loc1
      targets[other_idx] <- other_target
    } else {
      # Infeasible target: cap others at zero and allocate all frames to location 1.
      targets[1] <- total_frames / c1
      targets[other_idx] <- 0
    }
    targets
  }
  
  allocate_frames_by_target <- function(total_frames, cols_by_loc, fpc_targets) {
    n_locations <- length(cols_by_loc)
    if (!is.finite(total_frames) || total_frames <= 0 || n_locations == 0) {
      return(rep(0, n_locations))
    }
    
    safe_cols <- pmax(0, as.numeric(cols_by_loc))
    if (sum(safe_cols) <= 0) {
      return(rep(0, n_locations))
    }
    
    safe_targets <- pmax(0, as.numeric(fpc_targets))
    raw_frames <- safe_cols * safe_targets
    raw_total <- sum(raw_frames, na.rm = TRUE)
    
    if (!is.finite(raw_total) || raw_total <= 0) {
      return(total_frames * safe_cols / sum(safe_cols))
    }
    
    # Rescale to enforce strict frame conservation across locations.
    raw_frames * (total_frames / raw_total)
  }

  location_tables <- build_location_tables(locations, locations_loc2, location_tables)
  n_locations <- length(location_tables)
  n_periods <- nrow(location_tables[[1]])
  results <- vector("list", n_periods)
  current_colonies <- initial_colonies
  current_frames <- initial_frames

  for (t in seq_len(n_periods)) {
    operation_over <- (current_colonies <= 0 || current_frames <= 0)
    if (operation_over) {
      current_colonies <- 0
      current_frames <- 0
    }

    season_t <- location_tables[[1]][t, "season"]
    col_shares <- get_share_vector(allocation_params, n_locations, season_t, "col_share")
    cols_by_loc <- current_colonies * col_shares
    season_avg_fpc <- ifelse(current_colonies > 0, current_frames / current_colonies, 0)
    fpc_targets <- get_fpc_targets(allocation_params, n_locations, season_t, season_avg_fpc, cols_by_loc)
    frms_by_loc <- allocate_frames_by_target(current_frames, cols_by_loc, fpc_targets)
    frm_shares <- ifelse(current_frames > 0, frms_by_loc / current_frames, 0)
    res_by_loc <- vector("list", n_locations)

    for (i in seq_len(n_locations)) {
      res_by_loc[[i]] <- simulate_period(location_tables[[i]][t, ], cols_by_loc[i], frms_by_loc[i], P_feed)
    }

    total_cols_end <- sum(vapply(res_by_loc, function(x) x$colonies_end, numeric(1)))
    total_frms_end <- sum(vapply(res_by_loc, function(x) x$frames_end, numeric(1)))
    total_frms_start <- sum(frms_by_loc)

    weighted_forager_share <- ifelse(
      total_frms_start > 0,
      sum(vapply(seq_len(n_locations), function(i) res_by_loc[[i]]$forager_share * frms_by_loc[i], numeric(1))) / total_frms_start,
      0
    )

    period_result <- list(
      colonies_end = total_cols_end,
      frames_end = total_frms_end,
      frames_per_colony = ifelse(total_cols_end > 0, total_frms_end / total_cols_end, 0),
      forager_share = weighted_forager_share,
      total_foragers = sum(vapply(res_by_loc, function(x) x$total_foragers, numeric(1))),
      forage_collected = sum(vapply(res_by_loc, function(x) x$forage_collected, numeric(1))),
      forage_consumed = sum(vapply(res_by_loc, function(x) x$forage_consumed, numeric(1))),
      net_forage = sum(vapply(res_by_loc, function(x) x$net_forage, numeric(1))),
      feed_required = sum(vapply(res_by_loc, function(x) x$feed_required, numeric(1))),
      honey_harvested = sum(vapply(res_by_loc, function(x) x$honey_harvested, numeric(1))),
      crop_yield = sum(vapply(res_by_loc, function(x) x$crop_yield, numeric(1))),
      revenue_honey = sum(vapply(res_by_loc, function(x) x$revenue_honey, numeric(1))),
      revenue_crop = sum(vapply(res_by_loc, function(x) x$revenue_crop, numeric(1))),
      cost_maintenance = sum(vapply(res_by_loc, function(x) x$cost_maintenance, numeric(1))),
      cost_feed = sum(vapply(res_by_loc, function(x) x$cost_feed, numeric(1))),
      profit = sum(vapply(res_by_loc, function(x) x$profit, numeric(1))),
      # Preserve backwards-compatible fields while introducing general structure
      location_results = res_by_loc,
      location_colony_shares = col_shares,
      location_frame_shares = frm_shares,
      location_fpc_targets = fpc_targets,
      location_colonies_start = cols_by_loc,
      location_frames_start = frms_by_loc
    )

    # Backward-compatible loc1/loc2 columns for existing plots/tables
    get_loc_val <- function(index, extractor, default = NA_real_) {
      if (length(res_by_loc) >= index) {
        extractor(res_by_loc[[index]])
      } else {
        default
      }
    }

    loc_start_val <- function(index, vec, default = NA_real_) {
      if (length(vec) >= index) vec[index] else default
    }
    get_loc_fpc_start <- function(index) {
      c_start <- loc_start_val(index, cols_by_loc, 0)
      f_start <- loc_start_val(index, frms_by_loc, 0)
      ifelse(c_start > 0, f_start / c_start, 0)
    }
    set_loc_metrics <- function(index, loc_label) {
      period_result[[paste0("frames_per_colony_", loc_label)]] <<- get_loc_fpc_start(index)
      period_result[[paste0("frames_per_colony_end_", loc_label)]] <<- get_loc_val(index, function(x) ifelse(x$colonies_end > 0, x$frames_end / x$colonies_end, 0))
      period_result[[paste0("total_foragers_", loc_label)]] <<- get_loc_val(index, function(x) x$total_foragers)
      period_result[[paste0("colonies_start_", loc_label)]] <<- loc_start_val(index, cols_by_loc)
      period_result[[paste0("frames_start_", loc_label)]] <<- loc_start_val(index, frms_by_loc)
      period_result[[paste0("colonies_end_", loc_label)]] <<- get_loc_val(index, function(x) x$colonies_end)
      period_result[[paste0("frames_end_", loc_label)]] <<- get_loc_val(index, function(x) x$frames_end)
      period_result[[paste0("forage_collected_", loc_label)]] <<- get_loc_val(index, function(x) x$forage_collected)
      period_result[[paste0("forage_consumed_", loc_label)]] <<- get_loc_val(index, function(x) x$forage_consumed)
      period_result[[paste0("feed_required_", loc_label)]] <<- get_loc_val(index, function(x) x$feed_required)
      period_result[[paste0("honey_harvested_", loc_label)]] <<- get_loc_val(index, function(x) x$honey_harvested)
      period_result[[paste0("crop_yield_", loc_label)]] <<- get_loc_val(index, function(x) x$crop_yield)
      period_result[[paste0("revenue_honey_", loc_label)]] <<- get_loc_val(index, function(x) x$revenue_honey)
      period_result[[paste0("revenue_crop_", loc_label)]] <<- get_loc_val(index, function(x) x$revenue_crop)
      period_result[[paste0("cost_feed_", loc_label)]] <<- get_loc_val(index, function(x) x$cost_feed)
    }

    set_loc_metrics(1, "loc1")
    set_loc_metrics(2, "loc2")
    set_loc_metrics(3, "loc3")
    
    # Add metadata fields
    period_result$period <- t
    period_result$season <- season_t
    period_result$year <- ceiling(t / 4)
    period_result$colonies_start <- current_colonies
    period_result$frames_start <- current_frames
    
    # Apply management at end of period
    season <- season_t
    
    # Get management rates for this season
    cull_rate <- switch(season,
                        "Spring" = management_params$cull_spring / 100,
                        "Summer" = management_params$cull_summer / 100,
                        "Fall" = management_params$cull_fall / 100,
                        "Winter" = management_params$cull_winter / 100,
                        0
    )
    
    adjust_rate <- switch(season,
                          "Spring" = management_params$adjust_spring / 100,
                          "Summer" = management_params$adjust_summer / 100,
                          "Fall" = management_params$adjust_fall / 100,
                          "Winter" = management_params$adjust_winter / 100,
                          0
    )
    cull_rate <- safe_num(cull_rate, 0)
    adjust_rate <- safe_num(adjust_rate, 0)
    
    # Calculate management costs
    management_cost <- 0
    if (cull_rate > 0) {
      management_cost <- management_cost + (period_result$frames_end * cull_rate * management_params$P_cull)
    }
    if (adjust_rate != 0) {
      # Use split cost for positive adjustment (splitting), merge cost for negative (merging)
      adjustment_cost_per_unit <- ifelse(adjust_rate > 0, management_params$P_split, management_params$P_merge)
      management_cost <- management_cost + (period_result$colonies_end * abs(adjust_rate) * adjustment_cost_per_unit)
    }
    
    # Apply management actions
    current_colonies <- period_result$colonies_end
    current_frames <- period_result$frames_end
    
    # Apply culling (removes entire colonies - both frames and colonies)
    current_colonies <- current_colonies * (1 - cull_rate)
    current_frames <- current_frames * (1 - cull_rate)
    
    # Apply colony adjustment (positive = split, negative = merge)
    # Positive adjust_rate: increase colonies (splitting)
    # Negative adjust_rate: decrease colonies (merging)
    if (!operation_over) {
      current_colonies <- current_colonies * (1 + adjust_rate)
      # Ensure colonies stay non-negative (minimum 0) only when operation is ongoing
      current_colonies <- max(0, current_colonies)
    }
    # Frames remain the same total (redistributed among adjusted number of colonies)
    
    # Store management info
    period_result$cull_rate <- cull_rate
    period_result$adjust_rate <- adjust_rate
    period_result$management_cost <- management_cost
    period_result$profit <- period_result$profit - management_cost # Subtract management costs from profit
    
    # Store final states after management
    period_result$colonies_end_mgmt <- current_colonies
    period_result$frames_end_mgmt <- current_frames
    
    results[[t]] <- period_result
    
    # Only enforce minimums when operation is ongoing
    if (!operation_over) {
      current_colonies <- max(0, current_colonies)
      current_frames <- max(0, current_frames)
    }
  }
  
  return(results)
}

# Convert simulation results to data frame
results_to_dataframe <- function(simulation_results) {
  df <- do.call(rbind, lapply(simulation_results, function(x) {
    data.frame(
      period = x$period,
      season = x$season,
      year = x$year,
      colonies_start = x$colonies_start,
      frames_start = x$frames_start,
      frames_per_colony = x$frames_per_colony,
      frames_per_colony_loc1 = ifelse(is.null(x$frames_per_colony_loc1), NA, x$frames_per_colony_loc1),
      frames_per_colony_loc2 = ifelse(is.null(x$frames_per_colony_loc2), NA, x$frames_per_colony_loc2),
      frames_per_colony_loc3 = ifelse(is.null(x$frames_per_colony_loc3), NA, x$frames_per_colony_loc3),
      frames_per_colony_end_loc1 = ifelse(is.null(x$frames_per_colony_end_loc1), NA, x$frames_per_colony_end_loc1),
      frames_per_colony_end_loc2 = ifelse(is.null(x$frames_per_colony_end_loc2), NA, x$frames_per_colony_end_loc2),
      frames_per_colony_end_loc3 = ifelse(is.null(x$frames_per_colony_end_loc3), NA, x$frames_per_colony_end_loc3),
      colonies_start_loc1 = ifelse(is.null(x$colonies_start_loc1), NA, x$colonies_start_loc1),
      colonies_start_loc2 = ifelse(is.null(x$colonies_start_loc2), NA, x$colonies_start_loc2),
      colonies_start_loc3 = ifelse(is.null(x$colonies_start_loc3), NA, x$colonies_start_loc3),
      frames_start_loc1 = ifelse(is.null(x$frames_start_loc1), NA, x$frames_start_loc1),
      frames_start_loc2 = ifelse(is.null(x$frames_start_loc2), NA, x$frames_start_loc2),
      frames_start_loc3 = ifelse(is.null(x$frames_start_loc3), NA, x$frames_start_loc3),
      colonies_end_loc1 = ifelse(is.null(x$colonies_end_loc1), NA, x$colonies_end_loc1),
      colonies_end_loc2 = ifelse(is.null(x$colonies_end_loc2), NA, x$colonies_end_loc2),
      colonies_end_loc3 = ifelse(is.null(x$colonies_end_loc3), NA, x$colonies_end_loc3),
      frames_end_loc1 = ifelse(is.null(x$frames_end_loc1), NA, x$frames_end_loc1),
      frames_end_loc2 = ifelse(is.null(x$frames_end_loc2), NA, x$frames_end_loc2),
      frames_end_loc3 = ifelse(is.null(x$frames_end_loc3), NA, x$frames_end_loc3),
      forager_share = x$forager_share,
      total_foragers = x$total_foragers,
      total_foragers_loc1 = ifelse(is.null(x$total_foragers_loc1), NA, x$total_foragers_loc1),
      total_foragers_loc2 = ifelse(is.null(x$total_foragers_loc2), NA, x$total_foragers_loc2),
      total_foragers_loc3 = ifelse(is.null(x$total_foragers_loc3), NA, x$total_foragers_loc3),
      forage_collected = x$forage_collected,
      forage_collected_loc1 = ifelse(is.null(x$forage_collected_loc1), NA, x$forage_collected_loc1),
      forage_collected_loc2 = ifelse(is.null(x$forage_collected_loc2), NA, x$forage_collected_loc2),
      forage_collected_loc3 = ifelse(is.null(x$forage_collected_loc3), NA, x$forage_collected_loc3),
      forage_consumed = x$forage_consumed,
      forage_consumed_loc1 = ifelse(is.null(x$forage_consumed_loc1), NA, x$forage_consumed_loc1),
      forage_consumed_loc2 = ifelse(is.null(x$forage_consumed_loc2), NA, x$forage_consumed_loc2),
      forage_consumed_loc3 = ifelse(is.null(x$forage_consumed_loc3), NA, x$forage_consumed_loc3),
      net_forage = x$net_forage,
      feed_required = x$feed_required,
      feed_required_loc1 = ifelse(is.null(x$feed_required_loc1), NA, x$feed_required_loc1),
      feed_required_loc2 = ifelse(is.null(x$feed_required_loc2), NA, x$feed_required_loc2),
      feed_required_loc3 = ifelse(is.null(x$feed_required_loc3), NA, x$feed_required_loc3),
      honey_harvested = x$honey_harvested,
      honey_harvested_loc1 = ifelse(is.null(x$honey_harvested_loc1), NA, x$honey_harvested_loc1),
      honey_harvested_loc2 = ifelse(is.null(x$honey_harvested_loc2), NA, x$honey_harvested_loc2),
      honey_harvested_loc3 = ifelse(is.null(x$honey_harvested_loc3), NA, x$honey_harvested_loc3),
      crop_yield = x$crop_yield,
      crop_yield_loc1 = ifelse(is.null(x$crop_yield_loc1), NA, x$crop_yield_loc1),
      crop_yield_loc2 = ifelse(is.null(x$crop_yield_loc2), NA, x$crop_yield_loc2),
      crop_yield_loc3 = ifelse(is.null(x$crop_yield_loc3), NA, x$crop_yield_loc3),
      colonies_end = x$colonies_end,
      frames_end = x$frames_end,
      revenue_honey = x$revenue_honey,
      revenue_honey_loc1 = ifelse(is.null(x$revenue_honey_loc1), NA, x$revenue_honey_loc1),
      revenue_honey_loc2 = ifelse(is.null(x$revenue_honey_loc2), NA, x$revenue_honey_loc2),
      revenue_honey_loc3 = ifelse(is.null(x$revenue_honey_loc3), NA, x$revenue_honey_loc3),
      revenue_crop = x$revenue_crop,
      revenue_crop_loc1 = ifelse(is.null(x$revenue_crop_loc1), NA, x$revenue_crop_loc1),
      revenue_crop_loc2 = ifelse(is.null(x$revenue_crop_loc2), NA, x$revenue_crop_loc2),
      revenue_crop_loc3 = ifelse(is.null(x$revenue_crop_loc3), NA, x$revenue_crop_loc3),
      cost_maintenance = x$cost_maintenance,
      cost_feed = x$cost_feed,
      cost_feed_loc1 = ifelse(is.null(x$cost_feed_loc1), NA, x$cost_feed_loc1),
      cost_feed_loc2 = ifelse(is.null(x$cost_feed_loc2), NA, x$cost_feed_loc2),
      cost_feed_loc3 = ifelse(is.null(x$cost_feed_loc3), NA, x$cost_feed_loc3),
      cull_rate = ifelse(is.null(x$cull_rate), 0, x$cull_rate),
      adjust_rate = ifelse(is.null(x$adjust_rate), 0, x$adjust_rate),
      management_cost = ifelse(is.null(x$management_cost), 0, x$management_cost),
      colonies_end_mgmt = ifelse(is.null(x$colonies_end_mgmt), x$colonies_end, x$colonies_end_mgmt),
      frames_end_mgmt = ifelse(is.null(x$frames_end_mgmt), x$frames_end, x$frames_end_mgmt),
      profit = x$profit,
      stringsAsFactors = FALSE
    )
  }))
  return(df)
}

# Create stock dynamics plot
plot_stock_dynamics <- function(results_df, panels_to_show = c("colonies", "frames", "frames_per_colony", "forage_balance"), forage_per_colony = TRUE) {
  # Calculate average colony count for per-colony forage metrics
  avg_max_colonies <- max(results_df$colonies_end, na.rm = TRUE)
  
  # Prepare candlestick data for colonies, frames, and frames_per_colony
  candlestick_data <- bind_rows(
    # Colonies and frames data
    results_df %>%
      select(period, season, year, colonies_start, colonies_end, frames_start, frames_end) %>%
      mutate(
        colonies_open = colonies_start,
        colonies_close = colonies_end,
        colonies_high = pmax(colonies_start, colonies_end),
        colonies_low = pmin(colonies_start, colonies_end),
        frames_open = frames_start,
        frames_close = frames_end,
        frames_high = pmax(frames_start, frames_end),
        frames_low = pmin(frames_start, frames_end)
      ) %>%
      select(
        period, season, year,
        colonies_open, colonies_high, colonies_low, colonies_close,
        frames_open, frames_high, frames_low, frames_close
      ) %>%
      gather(key = "metric_type", value = "value", -period, -season, -year) %>%
      separate(metric_type, into = c("metric", "candlestick_part"), sep = "_"),
    
    # Frames per colony data - calculate start and end values to show biological growth
    results_df %>%
      mutate(
        frames_per_colony_start = ifelse((frames_start == 0) | (colonies_start == 0), 0, frames_start / colonies_start),
        frames_per_colony_end = ifelse((frames_end == 0) | (colonies_end == 0), 0, frames_end / colonies_end)
      ) %>%
      select(period, season, year, frames_per_colony_start, frames_per_colony_end) %>%
      mutate(
        metric = "frames_per_colony",
        open = frames_per_colony_start,
        close = frames_per_colony_end,
        high = pmax(frames_per_colony_start, frames_per_colony_end),
        low = pmin(frames_per_colony_start, frames_per_colony_end)
      ) %>%
      select(-frames_per_colony_start, -frames_per_colony_end) %>%
      gather(key = "candlestick_part", value = "value", open, close, high, low)
  ) %>%
    filter(!is.na(value)) %>%
    filter(metric %in% panels_to_show)  # Filter by selected panels
  
  # Prepare forage balance data
  forage_data <- results_df %>%
    mutate(
      forage_collected_pc = if (forage_per_colony) forage_collected / avg_max_colonies else forage_collected,
      feed_required_pc = if (forage_per_colony) feed_required / avg_max_colonies else feed_required,
      forage_consumed_pc = if (forage_per_colony) -forage_consumed / avg_max_colonies else -forage_consumed,
      honey_harvested_pc = if (forage_per_colony) -honey_harvested / avg_max_colonies else -honey_harvested
    ) %>%
    select(period, season, forage_collected_pc, feed_required_pc, forage_consumed_pc, honey_harvested_pc) %>%
    gather(key = "component", value = "value", -period, -season) %>%
    mutate(
      metric = "forage_balance",
      component = factor(
        component,
        levels = c("forage_collected_pc", "feed_required_pc", "honey_harvested_pc", "forage_consumed_pc"),
        labels = c("Forage Collected", "Feed Added", "Honey Harvested", "Forage Consumed")
      )
    )
  
  # Filter forage data if not selected
  if (!"forage_balance" %in% panels_to_show) {
    forage_data <- forage_data %>% filter(FALSE)  # Empty dataframe
  }
  
  # Prepare management data separately
  mgmt_data <- results_df %>%
    mutate(
      total_mgmt = abs(cull_rate) + abs(adjust_rate),
      dominant_mgmt = case_when(
        abs(cull_rate) > abs(adjust_rate) ~ "Cull",
        adjust_rate > 0 ~ "Split",
        adjust_rate < 0 ~ "Merge",
        TRUE ~ "None"
      ),
      # Calculate frames per colony after management for arrows
      frames_per_colony_mgmt = ifelse(colonies_end_mgmt > 0, frames_end_mgmt / colonies_end_mgmt, frames_per_colony)
    ) %>%
    filter(total_mgmt > 0) %>%
    select(period, season, colonies_end_mgmt, frames_end_mgmt, frames_per_colony_mgmt, dominant_mgmt, total_mgmt) %>%
    gather(key = "metric_mgmt", value = "value_mgmt", colonies_end_mgmt, frames_end_mgmt, frames_per_colony_mgmt) %>%
    mutate(metric = case_when(
      metric_mgmt == "colonies_end_mgmt" ~ "colonies",
      metric_mgmt == "frames_end_mgmt" ~ "frames",
      metric_mgmt == "frames_per_colony_mgmt" ~ "frames_per_colony",
      TRUE ~ gsub("_end_mgmt|_mgmt", "", metric_mgmt)
    )) %>%
    filter(!is.na(value_mgmt)) %>%
    filter(metric %in% panels_to_show)  # Filter by selected panels
  
  # Set proper season factor order
  candlestick_data$season <- factor(candlestick_data$season, levels = c("Spring", "Summer", "Fall", "Winter"))
  candlestick_data$metric <- factor(candlestick_data$metric, levels = c("colonies", "frames", "frames_per_colony", "forage_balance"))
  forage_data$season <- factor(forage_data$season, levels = c("Spring", "Summer", "Fall", "Winter"))
  
  if (nrow(mgmt_data) > 0) {
    mgmt_data$season <- factor(mgmt_data$season, levels = c("Spring", "Summer", "Fall", "Winter"))
    mgmt_data$metric <- factor(mgmt_data$metric, levels = c("colonies", "frames", "frames_per_colony", "forage_balance"))
  }
  
  # Prepare arrow data for displaying period transitions
  arrow_bodies <- candlestick_data %>%
    select(period, metric, season, candlestick_part, value) %>%
    spread(candlestick_part, value) %>%
    mutate(
      growing = close > open,
      x_start = period - 0.4,
      x_end = period + 0.4,
      y_start = open,
      y_end = close,
      arrow_length = abs(close - open),
      location = "aggregate"
    )
  
  # Prepare location-specific arrow data for frames per colony
  arrow_bodies_locations <- bind_rows(
    # Location 1 arrows
    results_df %>%
      filter(!is.na(frames_per_colony_loc1), !is.na(frames_per_colony_end_loc1)) %>%
      select(period, season, frames_per_colony_loc1, frames_per_colony_end_loc1) %>%
      mutate(
        metric = "frames_per_colony",
        open = frames_per_colony_loc1,
        close = frames_per_colony_end_loc1,
        high = pmax(frames_per_colony_loc1, frames_per_colony_end_loc1),
        low = pmin(frames_per_colony_loc1, frames_per_colony_end_loc1),
        growing = close > open,
        x_start = period - 0.4,
        x_end = period + 0.4,
        y_start = open,
        y_end = close,
        arrow_length = abs(close - open),
        location = "location1"
      ),
    # Location 2 arrows
    results_df %>%
      filter(!is.na(frames_per_colony_loc2), !is.na(frames_per_colony_end_loc2)) %>%
      select(period, season, frames_per_colony_loc2, frames_per_colony_end_loc2) %>%
      mutate(
        metric = "frames_per_colony",
        open = frames_per_colony_loc2,
        close = frames_per_colony_end_loc2,
        high = pmax(frames_per_colony_loc2, frames_per_colony_end_loc2),
        low = pmin(frames_per_colony_loc2, frames_per_colony_end_loc2),
        growing = close > open,
        x_start = period - 0.4,
        x_end = period + 0.4,
        y_start = open,
        y_end = close,
        arrow_length = abs(close - open),
        location = "location2"
      )
  ) %>%
    filter(metric %in% panels_to_show)
  
  # Set season factor for location arrows
  if (nrow(arrow_bodies_locations) > 0) {
    arrow_bodies_locations$season <- factor(arrow_bodies_locations$season, levels = c("Spring", "Summer", "Fall", "Winter"))
  }
  
  # Add dummy data points at y=0 for candlestick panels to force y-axis to start at 0
  candlestick_panels <- intersect(c("colonies", "frames", "frames_per_colony"), panels_to_show)
  if (length(candlestick_panels) > 0) {
    zero_points <- data.frame(
      period = 1,
      metric = candlestick_panels,
      value = 0,
      stringsAsFactors = FALSE
    )
  } else {
    zero_points <- data.frame(
      period = numeric(0),
      metric = character(0),
      value = numeric(0),
      stringsAsFactors = FALSE
    )
  }
  
  # Create dynamic facet labels based on selected panels
  forage_label <- if (forage_per_colony) {
    "Forage/Feed/Honey Flows (lbs/colony)"
  } else {
    "Forage/Feed/Honey Flows (lbs total)"
  }
  
  facet_labels <- c(
    "colonies" = "Colonies",
    "frames" = "Frames", 
    "frames_per_colony" = "Frames per Colony",
    "forage_balance" = forage_label
  )
  
  # Only keep labels for selected panels - must maintain order
  selected_labels <- setNames(
    facet_labels[panels_to_show],
    panels_to_show
  )
  
  # Return empty plot if no panels selected
  if (length(panels_to_show) == 0) {
    return(ggplot() + 
             annotate("text", x = 0.5, y = 0.5, label = "No panels selected", size = 6) +
             theme_void())
  }
  
  # Create plot with four facets: three arrow charts + one forage balance
  p <- ggplot()
  
  # Add invisible points at y=0 for candlestick panels to force axis to include 0
  if (nrow(zero_points) > 0) {
    p <- p + geom_point(
      data = zero_points,
      aes(x = period, y = value),
      alpha = 0
    )
  }
  
  # Draw diagonal arrows instead of candlesticks
  if (nrow(arrow_bodies) > 0) {
    # Determine if we have location-specific arrows for frames_per_colony
    have_location_arrows <- exists("arrow_bodies_locations") && nrow(arrow_bodies_locations) > 0
    
    # For aggregate arrows, exclude frames_per_colony if we have location arrows
    arrows_to_plot <- arrow_bodies
    if (have_location_arrows) {
      arrows_to_plot <- filter(arrow_bodies, metric != "frames_per_colony")
    }
    
    # Filter out arrows with no change (zero length)
    arrows_with_change <- filter(arrows_to_plot, arrow_length > 0)
    
    if (nrow(arrows_with_change) > 0) {
      p <- p + geom_segment(
        data = arrows_with_change,
        aes(
          x = x_start, y = y_start,
          xend = x_end, yend = y_end,
          color = season
        ),
        arrow = arrow(length = unit(0.3, "cm"), type = "closed"),
        linewidth = 2, alpha = 0.8
      )
    }
    
    # For periods with no change, show a horizontal line at the period center
    arrows_no_change <- filter(arrows_to_plot, arrow_length == 0)
    if (nrow(arrows_no_change) > 0) {
      p <- p + geom_segment(
        data = arrows_no_change,
        aes(x = x_start, y = y_start, xend = x_end, yend = y_start, color = season),
        linewidth = 2, alpha = 0.8
      )
    }
  }
  
  # Draw location-specific arrows (for frames_per_colony only)
  if (exists("arrow_bodies_locations") && nrow(arrow_bodies_locations) > 0) {
    # Location 1 arrows with change (season colors)
    arrows_loc1_change <- filter(arrow_bodies_locations, arrow_length > 0, location == "location1")
    if (nrow(arrows_loc1_change) > 0) {
      p <- p + geom_segment(
        data = arrows_loc1_change,
        aes(
          x = x_start, y = y_start,
          xend = x_end, yend = y_end,
          color = season
        ),
        arrow = arrow(length = unit(0.3, "cm"), type = "closed"),
        linewidth = 2, alpha = 0.8
      )
    }
    
    # Location 2 arrows with change (gray)
    arrows_loc2_change <- filter(arrow_bodies_locations, arrow_length > 0, location == "location2")
    if (nrow(arrows_loc2_change) > 0) {
      p <- p + geom_segment(
        data = arrows_loc2_change,
        aes(
          x = x_start, y = y_start,
          xend = x_end, yend = y_end
        ),
        color = "#808080",
        arrow = arrow(length = unit(0.3, "cm"), type = "closed"),
        linewidth = 2, alpha = 0.8
      )
    }
    
    # Location 1 arrows with no change (season colors)
    arrows_loc1_no_change <- filter(arrow_bodies_locations, arrow_length == 0, location == "location1")
    if (nrow(arrows_loc1_no_change) > 0) {
      p <- p + geom_segment(
        data = arrows_loc1_no_change,
        aes(x = x_start, y = y_start, xend = x_end, yend = y_start, color = season),
        linewidth = 2, alpha = 0.8
      )
    }
    
    # Location 2 arrows with no change (gray)
    arrows_loc2_no_change <- filter(arrow_bodies_locations, arrow_length == 0, location == "location2")
    if (nrow(arrows_loc2_no_change) > 0) {
      p <- p + geom_segment(
        data = arrows_loc2_no_change,
        aes(x = x_start, y = y_start, xend = x_end, yend = y_start),
        color = "#808080",
        linewidth = 2, alpha = 0.8
      )
    }
  }
  
  # Forage balance bars (only if forage data exists)
  if (nrow(forage_data) > 0) {
    p <- p + geom_col(
      data = forage_data,
      aes(x = period, y = value, fill = component),
      position = "stack", width = 0.7
    )
  }
  
  # Zero line for forage balance panel
  if ("forage_balance" %in% panels_to_show) {
    p <- p + geom_hline(
      data = data.frame(metric = "forage_balance"),
      aes(yintercept = 0),
      color = "black", linewidth = 0.8
    )
  }
  
  # Add faceting and themes
  p <- p +
    facet_wrap(
      ~ factor(metric,
               levels = panels_to_show,
               labels = selected_labels
      ),
      scales = "free_y", ncol = 1
    ) +
    labs(x = "Period", y = "") +
    theme_minimal() +
    theme(
      strip.text = element_text(size = 16, face = "bold"),
      axis.title = element_text(size = 14, face = "bold"),
      axis.text = element_text(size = 12),
      legend.position = "none",
      panel.grid.major = element_line(size = 0.8, color = "#e0e0e0"),
      panel.grid.minor = element_line(size = 0.4, color = "#f0f0f0")
    ) +
    scale_color_manual(
      values = c(
        "Spring" = "#2E8B57", "Summer" = "#FFD700",
        "Fall" = "#FF8C00", "Winter" = "#4682B4",
        "Cull" = "#DC143C", "Split" = "#555555",
        "Merge" = "#8B008B", "None" = "#808080"
      ),
      guide = "none"
    ) +
    scale_fill_manual(
      values = c(
        "Forage Collected" = "#1a6b1a",
        "Feed Added" = "#8B9D83",
        "Honey Harvested" = "#FFC30B",
        "Forage Consumed" = "#FF6B6B"
      ),
      guide = "none"
    ) +
    scale_y_continuous(
      expand = expansion(mult = c(0.05, 0.05)),
      labels = function(x) {
        ifelse(abs(x) >= 1000,
               paste0(round(x / 1000, 1), "k"),
               as.character(round(x, 0))
        )
      }
    )
  
  # Add red dots for starting point (period 1) - at the arrow start position
  if (nrow(candlestick_data) > 0) {
    starting_points <- candlestick_data %>%
      filter(period == 1, candlestick_part == "open") %>%
      select(period, metric, value) %>%
      mutate(x_pos = period - 0.4)
    
    if (nrow(starting_points) > 0) {
      p <- p + geom_point(
        data = starting_points,
        aes(x = x_pos, y = value),
        color = "red", size = 4, shape = 16
      )
    }
  }
  
  # Add management arrows if any exist
  if (nrow(mgmt_data) > 0 && nrow(candlestick_data) > 0) {
    # Prepare vertical arrow data - from biological end to management result
    arrow_data <- mgmt_data %>%
      left_join(
        candlestick_data %>%
          filter(candlestick_part == "close") %>%
          select(period, metric, close_value = value),
        by = c("period", "metric")
      ) %>%
      mutate(
        x_pos = period + 0.5, # Vertical position between candlesticks
        y_start = close_value, # Start at biological end value
        y_end = value_mgmt # End at post-management value
      ) %>%
      filter(!is.na(close_value))
    
    if (nrow(arrow_data) > 0) {
      p <- p + geom_segment(
        data = arrow_data,
        aes(
          x = x_pos, y = y_start,
          xend = x_pos, yend = y_end,
          color = dominant_mgmt
        ),
        arrow = arrow(length = unit(0.15, "cm"), type = "closed"),
        linewidth = 1.2, alpha = 0.8
      )
    }
  }
  
  return(p)
}

# =============================================================================
# SERVER HELPER FUNCTIONS
# =============================================================================

calculate_avg_max_colonies <- function(selected_data) {
  if (nrow(selected_data) == 0 || all(is.na(selected_data$colonies_end))) {
    return(1)
  }
  max(selected_data$colonies_end, na.rm = TRUE)
}

# =============================================================================
# SHINY UI
# =============================================================================

ui <- dashboardPage(
  dashboardHeader(title = "US Beekeeping Industry Simulation"),
  dashboardSidebar(
    sidebarMenu(
      id = "main_tab",
      menuItem("Main simulation", tabName = "simulation", icon = icon("chart-line")),
      menuItem("Seasonal parameters", tabName = "parameters", icon = icon("sliders-h")),
      menuItem("Economic parameters", tabName = "economics", icon = icon("dollar-sign")),
      
      # Simulation Parameters
      conditionalPanel(
        condition = "input.main_tab == 'simulation'",
        h4("Simulation Setup", style = "color: white; margin-left: 15px;"),
        tags$div(
          style = "color: #ccc; font-size: 10px; margin-left: 15px; margin-top: -10px; margin-bottom: 10px;",
          textOutput("sim_time_display")
        ),
        selectInput("start_season", "Starting Season:",
                    choices = c("Spring", "Summer", "Fall", "Winter"),
                    selected = "Winter"
        ),
        sliderInput("annual_discount_rate", "Annual Discount Rate (%):",
                    min = 0, max = 15, value = 3, step = 0.1
        ),
        numericInput("initial_colonies", "Initial Colonies:", value = 3000, min = 100, max = 5000, step = 100),
        numericInput("initial_frames", "Initial Frames:", value = 18000, min = 500, max = 30000, step = 500),
        br(),
        div(
          style = "text-align: center;",
          actionButton("reset_params", "Reset All Parameters", class = "btn-secondary"),
          br(), br(),
          downloadButton("save_settings", "Export Parameters to CSV", class = "btn-primary"),
          br(), br(),
          fileInput("load_settings_csv", "Import Parameters from CSV", accept = ".csv"),
          br(),
          downloadButton("export_results", "Export Simulation to CSV",
                         class = "btn-success", icon = icon("download"))
        ),
        hr(style = "border-color: white;")
      )
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #f4f4f4;
        }
        /* Hide slider min/max labels but show tick marks for management variables */
        .management-box .irs-min,
        .management-box .irs-max {
          display: none !important;
        }
        .management-season-panel {
          font-size: 11px;
          font-weight: 400;
        }
        .management-season-panel label,
        .management-season-panel .control-label,
        .management-season-panel .checkbox label {
          font-size: 11px !important;
          font-weight: 400 !important;
        }
        .management-season-panel .shiny-text-output {
          font-size: 11px !important;
          font-weight: 400 !important;
        }
        .management-season-panel.season-spring .irs-bar,
        .management-season-panel.season-spring .irs-bar-edge {
          background: #2E8B57 !important;
          border-top-color: #2E8B57 !important;
        }
        .management-season-panel.season-spring .irs-from,
        .management-season-panel.season-spring .irs-to,
        .management-season-panel.season-spring .irs-single {
          background: #2E8B57 !important;
          color: #fff !important;
        }
        .management-season-panel.season-spring .irs-slider {
          border: 2px solid #2E8B57 !important;
          background: #fff !important;
        }
        .management-season-panel.season-summer .irs-bar,
        .management-season-panel.season-summer .irs-bar-edge {
          background: #FFD700 !important;
          border-top-color: #FFD700 !important;
        }
        .management-season-panel.season-summer .irs-from,
        .management-season-panel.season-summer .irs-to,
        .management-season-panel.season-summer .irs-single {
          background: #FFD700 !important;
          color: #000 !important;
        }
        .management-season-panel.season-summer .irs-slider {
          border: 2px solid #FF8C00 !important;
          background: #fff !important;
        }
        .management-season-panel.season-fall .irs-bar,
        .management-season-panel.season-fall .irs-bar-edge {
          background: #FF8C00 !important;
          border-top-color: #FF8C00 !important;
        }
        .management-season-panel.season-fall .irs-from,
        .management-season-panel.season-fall .irs-to,
        .management-season-panel.season-fall .irs-single {
          background: #FF8C00 !important;
          color: #fff !important;
        }
        .management-season-panel.season-fall .irs-slider {
          border: 2px solid #FF8C00 !important;
          background: #fff !important;
        }
        .management-season-panel.season-winter .irs-bar,
        .management-season-panel.season-winter .irs-bar-edge {
          background: #4682B4 !important;
          border-top-color: #4682B4 !important;
        }
        .management-season-panel.season-winter .irs-from,
        .management-season-panel.season-winter .irs-to,
        .management-season-panel.season-winter .irs-single {
          background: #4682B4 !important;
          color: #fff !important;
        }
        .management-season-panel.season-winter .irs-slider {
          border: 2px solid #4682B4 !important;
          background: #fff !important;
        }

        /* Color location allocation sliders */
        .shiny-input-container:has(#col_share_loc1) .irs-bar {
          background: #6666cc !important;
        }
        .shiny-input-container:has(#col_share_loc1) .irs-from,
        .shiny-input-container:has(#col_share_loc1) .irs-to,
        .shiny-input-container:has(#col_share_loc1) .irs-single {
          background: #6666cc !important;
          color: #fff !important;
        }
        
        .shiny-input-container:has(#frame_pull) .irs-bar {
          background: #9999dd !important;
        }
        .shiny-input-container:has(#frame_pull) .irs-from,
        .shiny-input-container:has(#frame_pull) .irs-to,
        .shiny-input-container:has(#frame_pull) .irs-single {
          background: #9999dd !important;
          color: #fff !important;
        }

        /* Color management action sliders by ID for precise targeting */
        /* Spring management sliders */
        .shiny-input-container:has(#adjust_spring) .irs-bar,
        .shiny-input-container:has(#cull_spring) .irs-bar,
        .shiny-input-container:has(#col_share_loc1_spring) .irs-bar,
        .shiny-input-container:has(#col_share_loc2_spring) .irs-bar,
        .shiny-input-container:has(#col_share_loc3_spring) .irs-bar,
        .shiny-input-container:has(#fpc_target_loc1_spring) .irs-bar {
          background: #2E8B57 !important;
        }
        .shiny-input-container:has(#adjust_spring) .irs-from,
        .shiny-input-container:has(#adjust_spring) .irs-to,
        .shiny-input-container:has(#adjust_spring) .irs-single,
        .shiny-input-container:has(#cull_spring) .irs-from,
        .shiny-input-container:has(#cull_spring) .irs-to,
        .shiny-input-container:has(#cull_spring) .irs-single,
        .shiny-input-container:has(#col_share_loc1_spring) .irs-from,
        .shiny-input-container:has(#col_share_loc1_spring) .irs-to,
        .shiny-input-container:has(#col_share_loc1_spring) .irs-single,
        .shiny-input-container:has(#col_share_loc2_spring) .irs-from,
        .shiny-input-container:has(#col_share_loc2_spring) .irs-to,
        .shiny-input-container:has(#col_share_loc2_spring) .irs-single,
        .shiny-input-container:has(#col_share_loc3_spring) .irs-from,
        .shiny-input-container:has(#col_share_loc3_spring) .irs-to,
        .shiny-input-container:has(#col_share_loc3_spring) .irs-single,
        .shiny-input-container:has(#fpc_target_loc1_spring) .irs-from,
        .shiny-input-container:has(#fpc_target_loc1_spring) .irs-to,
        .shiny-input-container:has(#fpc_target_loc1_spring) .irs-single {
          background: #2E8B57 !important;
          color: #fff !important;
        }

        /* Summer management sliders */
        .shiny-input-container:has(#adjust_summer) .irs-bar,
        .shiny-input-container:has(#cull_summer) .irs-bar,
        .shiny-input-container:has(#col_share_loc1_summer) .irs-bar,
        .shiny-input-container:has(#col_share_loc2_summer) .irs-bar,
        .shiny-input-container:has(#col_share_loc3_summer) .irs-bar,
        .shiny-input-container:has(#fpc_target_loc1_summer) .irs-bar {
          background: #FFD700 !important;
        }
        .shiny-input-container:has(#adjust_summer) .irs-from,
        .shiny-input-container:has(#adjust_summer) .irs-to,
        .shiny-input-container:has(#adjust_summer) .irs-single,
        .shiny-input-container:has(#cull_summer) .irs-from,
        .shiny-input-container:has(#cull_summer) .irs-to,
        .shiny-input-container:has(#cull_summer) .irs-single,
        .shiny-input-container:has(#col_share_loc1_summer) .irs-from,
        .shiny-input-container:has(#col_share_loc1_summer) .irs-to,
        .shiny-input-container:has(#col_share_loc1_summer) .irs-single,
        .shiny-input-container:has(#col_share_loc2_summer) .irs-from,
        .shiny-input-container:has(#col_share_loc2_summer) .irs-to,
        .shiny-input-container:has(#col_share_loc2_summer) .irs-single,
        .shiny-input-container:has(#col_share_loc3_summer) .irs-from,
        .shiny-input-container:has(#col_share_loc3_summer) .irs-to,
        .shiny-input-container:has(#col_share_loc3_summer) .irs-single,
        .shiny-input-container:has(#fpc_target_loc1_summer) .irs-from,
        .shiny-input-container:has(#fpc_target_loc1_summer) .irs-to,
        .shiny-input-container:has(#fpc_target_loc1_summer) .irs-single {
          background: #FFD700 !important;
          color: #000 !important;
        }

        /* Fall management sliders */
        .shiny-input-container:has(#adjust_fall) .irs-bar,
        .shiny-input-container:has(#cull_fall) .irs-bar,
        .shiny-input-container:has(#col_share_loc1_fall) .irs-bar,
        .shiny-input-container:has(#col_share_loc2_fall) .irs-bar,
        .shiny-input-container:has(#fpc_target_loc1_fall) .irs-bar {
          background: #FF8C00 !important;
        }
        .shiny-input-container:has(#adjust_fall) .irs-from,
        .shiny-input-container:has(#adjust_fall) .irs-to,
        .shiny-input-container:has(#adjust_fall) .irs-single,
        .shiny-input-container:has(#cull_fall) .irs-from,
        .shiny-input-container:has(#cull_fall) .irs-to,
        .shiny-input-container:has(#cull_fall) .irs-single,
        .shiny-input-container:has(#col_share_loc1_fall) .irs-from,
        .shiny-input-container:has(#col_share_loc1_fall) .irs-to,
        .shiny-input-container:has(#col_share_loc1_fall) .irs-single,
        .shiny-input-container:has(#col_share_loc2_fall) .irs-from,
        .shiny-input-container:has(#col_share_loc2_fall) .irs-to,
        .shiny-input-container:has(#col_share_loc2_fall) .irs-single,
        .shiny-input-container:has(#fpc_target_loc1_fall) .irs-from,
        .shiny-input-container:has(#fpc_target_loc1_fall) .irs-to,
        .shiny-input-container:has(#fpc_target_loc1_fall) .irs-single {
          background: #FF8C00 !important;
          color: #fff !important;
        }

        /* Winter management sliders */
        .shiny-input-container:has(#adjust_winter) .irs-bar,
        .shiny-input-container:has(#cull_winter) .irs-bar,
        .shiny-input-container:has(#col_share_loc1_winter) .irs-bar,
        .shiny-input-container:has(#col_share_loc2_winter) .irs-bar,
        .shiny-input-container:has(#fpc_target_loc1_winter) .irs-bar {
          background: #4682B4 !important;
        }
        .shiny-input-container:has(#adjust_winter) .irs-from,
        .shiny-input-container:has(#adjust_winter) .irs-to,
        .shiny-input-container:has(#adjust_winter) .irs-single,
        .shiny-input-container:has(#cull_winter) .irs-from,
        .shiny-input-container:has(#cull_winter) .irs-to,
        .shiny-input-container:has(#cull_winter) .irs-single,
        .shiny-input-container:has(#col_share_loc1_winter) .irs-from,
        .shiny-input-container:has(#col_share_loc1_winter) .irs-to,
        .shiny-input-container:has(#col_share_loc1_winter) .irs-single,
        .shiny-input-container:has(#col_share_loc2_winter) .irs-from,
        .shiny-input-container:has(#col_share_loc2_winter) .irs-to,
        .shiny-input-container:has(#col_share_loc2_winter) .irs-single,
        .shiny-input-container:has(#fpc_target_loc1_winter) .irs-from,
        .shiny-input-container:has(#fpc_target_loc1_winter) .irs-to,
        .shiny-input-container:has(#fpc_target_loc1_winter) .irs-single {
          background: #4682B4 !important;
          color: #fff !important;
        }

        /* Rotary knob styling - Custom circular range input */
        .rotary-knob {
          position: relative;
          width: 70px;
          height: 70px;
          margin: 5px auto;
          text-align: center;
        }

        .rotary-knob input[type='range'] {
          position: absolute;
          width: 70px;
          height: 70px;
          border-radius: 50%;
          background: none;
          outline: none;
          cursor: pointer;
          -webkit-appearance: none;
          appearance: none;
          transform: rotate(-90deg);
        }

        .rotary-knob input[type='range']::-webkit-slider-track {
          background: none;
          height: 70px;
          border-radius: 50%;
        }

        .rotary-knob input[type='range']::-webkit-slider-thumb {
          -webkit-appearance: none;
          width: 12px;
          height: 12px;
          border-radius: 50%;
          background: #333;
          cursor: pointer;
          box-shadow: 0 0 0 3px rgba(0,0,0,0.1);
          transform: translateY(-25px);
        }

        .rotary-knob input[type='range']::-moz-range-track {
          background: none;
          height: 70px;
          border-radius: 50%;
          border: none;
        }

        .rotary-knob input[type='range']::-moz-range-thumb {
          width: 12px;
          height: 12px;
          border-radius: 50%;
          background: #333;
          cursor: pointer;
          border: none;
          box-shadow: 0 0 0 3px rgba(0,0,0,0.1);
        }

        .knob-background {
          position: absolute;
          width: 70px;
          height: 70px;
          border-radius: 50%;
          background: radial-gradient(circle, #f8f9fa 40%, #e9ecef 100%);
          border: 3px solid #ddd;
          box-shadow:
            0 2px 8px rgba(0,0,0,0.15),
            inset 0 1px 3px rgba(255,255,255,0.8),
            inset 0 -1px 3px rgba(0,0,0,0.1);
          pointer-events: none;
        }

        .knob-value {
          position: absolute;
          top: 50%;
          left: 50%;
          transform: translate(-50%, -50%);
          font-size: 12px;
          font-weight: bold;
          color: #333;
          pointer-events: none;
          z-index: 10;
        }

        .knob-label {
          font-size: 10px;
          color: #666;
          margin-top: 75px;
          font-weight: normal;
        }

        /* Season-specific knob colors */
        .spring-knob .knob-background {
          border-color: #2E8B57;
        }
        .spring-knob input[type='range']::-webkit-slider-thumb {
          background: #2E8B57;
        }
        .spring-knob input[type='range']::-moz-range-thumb {
          background: #2E8B57;
        }

        .summer-knob .knob-background {
          border-color: #FFD700;
        }
        .summer-knob input[type='range']::-webkit-slider-thumb {
          background: #FF8C00;
        }
        .summer-knob input[type='range']::-moz-range-thumb {
          background: #FF8C00;
        }

        .fall-knob .knob-background {
          border-color: #FF8C00;
        }
        .fall-knob input[type='range']::-webkit-slider-thumb {
          background: #FF8C00;
        }
        .fall-knob input[type='range']::-moz-range-thumb {
          background: #FF8C00;
        }

        .winter-knob .knob-background {
          border-color: #4682B4;
        }
        .winter-knob input[type='range']::-webkit-slider-thumb {
          background: #4682B4;
        }
        .winter-knob input[type='range']::-moz-range-thumb {
          background: #4682B4;
        }

        /* Color seasonal parameter labels by season */
        /* Spring labels */
        div[style*='spring'] label {
          color: #2E8B57 !important;
          font-weight: bold;
        }
        /* Summer labels */
        div[style*='summer'] label {
          color: #FFD700 !important;
          font-weight: bold;
        }
        /* Fall labels */
        div[style*='fall'] label {
          color: #FF8C00 !important;
          font-weight: bold;
        }
        /* Winter labels */
        div[style*='winter'] label {
          color: #4682B4 !important;
          font-weight: bold;
        }

        /* Alternative approach - target conditional panels directly */
        div[data-display-if*='spring'] .control-label {
          color: #2E8B57 !important;
          font-weight: bold;
        }
        div[data-display-if*='summer'] .control-label {
          color: #FFD700 !important;
          font-weight: bold;
        }
        div[data-display-if*='fall'] .control-label {
          color: #FF8C00 !important;
          font-weight: bold;
        }
        div[data-display-if*='winter'] .control-label {
          color: #4682B4 !important;
          font-weight: bold;
        }

      "))
    ),
    tabItems(
      tabItem(
        tabName = "simulation",
        fluidRow(
          # Main plot - larger and prominent
          box(
            title = "Bee Stock Dynamics",
            status = "primary", solidHeader = TRUE,
            width = 4, height = "900px",
            plotOutput("stock_plot", height = "460px"),
            div(
              style = "margin-top: -10px; padding: 5px; font-size: 10px; color: #666;",
              em("Arrows show biological growth. For Frames per Colony: location 1 = season color, location 2 = gray")
            ),
            hr(style = "margin: 8px 0;"),
            div(
              style = "max-height: 390px; overflow-y: auto; overflow-x: hidden; padding-right: 4px;",
              # Starting point indicator
              div(
                style = "text-align: left; margin-bottom: 10px;",
                tags$span(style = "display: inline-block; width: 10px; height: 10px; background-color: red; border-radius: 50%; margin-right: 8px; vertical-align: middle;"),
                tags$span("Starting point of simulation", style = "font-size: 11px; vertical-align: middle;")
              ),
              fluidRow(
                column(
                  6,
                  h5("Legend", style = "margin-bottom: 8px; font-weight: bold; font-size: 12px;"),
                  div(
                    style = "margin-bottom: 6px;",
                    span(style = "display: inline-block; width: 20px; height: 20px; background-color: white; border: 2px solid #228B22; margin-right: 8px; vertical-align: middle;"),
                    span("Stock Growth", style = "vertical-align: middle; font-size: 11px;")
                  ),
                  div(
                    style = "margin-bottom: 6px;",
                    span(style = "display: inline-block; width: 0; height: 0; border-left: 10px solid transparent; border-right: 10px solid transparent; border-bottom: 15px solid #555555; margin-right: 8px; vertical-align: middle;"),
                    span("Split", style = "vertical-align: middle; font-size: 11px;")
                  ),
                  div(
                    style = "margin-bottom: 6px;",
                    span(style = "display: inline-block; width: 0; height: 0; border-left: 10px solid transparent; border-right: 10px solid transparent; border-bottom: 15px solid #8B008B; margin-right: 8px; vertical-align: middle;"),
                    span("Merge", style = "vertical-align: middle; font-size: 11px;")
                  ),
                  div(
                    style = "margin-bottom: 8px;",
                    span(style = "display: inline-block; width: 0; height: 0; border-left: 10px solid transparent; border-right: 10px solid transparent; border-top: 15px solid #DC143C; margin-right: 8px; vertical-align: middle;"),
                    span("Cull", style = "vertical-align: middle; font-size: 11px;")
                  ),
                  div(
                    style = "margin-bottom: 6px;",
                    span(style = "display: inline-block; width: 20px; height: 20px; background-color: #1a6b1a; margin-right: 8px; vertical-align: middle;"),
                    span("Forage Collected", style = "vertical-align: middle; font-size: 11px;")
                  ),
                  div(
                    style = "margin-bottom: 6px;",
                    span(style = "display: inline-block; width: 20px; height: 20px; background-color: #8B9D83; margin-right: 8px; vertical-align: middle;"),
                    span("Supplemental Feed", style = "vertical-align: middle; font-size: 11px;")
                  ),
                  div(
                    style = "margin-bottom: 6px;",
                    span(style = "display: inline-block; width: 20px; height: 20px; background-color: #FF6B6B; margin-right: 8px; vertical-align: middle;"),
                    span("Forage Consumed", style = "vertical-align: middle; font-size: 11px;")
                  ),
                  div(
                    style = "margin-bottom: 10px;",
                    span(style = "display: inline-block; width: 20px; height: 20px; background-color: #FFC30B; margin-right: 8px; vertical-align: middle;"),
                    span("Honey Harvested", style = "vertical-align: middle; font-size: 11px;")
                  )
                ),
                column(
                  6,
                  h5("Display Panels", style = "margin-bottom: 8px; font-weight: bold; font-size: 12px;"),
                  checkboxInput("show_colonies", "Colonies", value = TRUE),
                  checkboxInput("show_frames", "Frames", value = TRUE),
                  checkboxInput("show_frames_per_colony", "Frames/Colony", value = TRUE),
                  checkboxInput("show_forage_balance", "Forage/Feed/Honey", value = TRUE),
                  radioButtons("forage_per_colony", "Forage Display:",
                               choices = c("Per Colony" = TRUE, "Total" = FALSE),
                               selected = TRUE,
                               inline = TRUE
                  )
                )
              )
            )
          ),

          # Simulation Results
          box(
            title = "Simulation Results",
            status = "success", solidHeader = TRUE,
            width = 3, height = "900px",
            div(
              style = "height: 850px; overflow-y: auto; overflow-x: hidden;",
              fluidRow(
                column(
                  12,
                  h5("Production & Operations Metrics"),
                  tableOutput("operations_table")
                )
              ),
              fluidRow(
                column(
                  12,
                  h5("Financial Summary (all values are Net Present Values)"),
                  tableOutput("financial_table")
                )
              )
            )
          ),
          
          # Management Variables
          box(
            title = "Management Variables",
            status = "warning", solidHeader = TRUE,
            width = 5, height = "900px",
            class = "management-box",
            div(
              style = "height: 850px; overflow-y: auto; overflow-x: hidden; padding-right: 4px;",
              uiOutput("management_columns"),
              div(
                style = "margin-top: 8px;",
                em("Compact seasonal controls: split/merge, culling, colony shares, and target/effective frames per colony.", style = "font-size: 10px; color: #666;")
              )
            )
          )
        ),
        fluidRow(
          box(
            title = "Management Actions",
            status = "warning", solidHeader = TRUE,
            width = 12,
            fluidRow(
              column(2, actionButton("preset_zero", "Set all to zero", class = "btn-default btn-block")),
              column(2, actionButton("preset_growth", "Maximum growth", class = "btn-default btn-block")),
              column(2, actionButton("preset_average", "US average 2015-2023", class = "btn-default btn-block")),
              column(2, actionButton("optimize_mgmt", "Optimize all controls", class = "btn-primary btn-block")),
              column(2, actionButton("reset_allocation", "Reset alloc/targets", class = "btn-default btn-block"))
            )
          )
        ),
      ),
      tabItem(
        tabName = "parameters",
        fluidRow(
          box(
            title = "Seasonal Parameters",
            status = "info", solidHeader = TRUE,
            width = 12, height = "850px",
            # Season selector at the top, outside conditional panels
            fluidRow(
              column(
                2,
                h5("Season Selector"),
                selectInput("season_selector", NULL,
                            choices = list(
                              "Spring" = "spring", "Summer" = "summer",
                              "Fall" = "fall", "Winter" = "winter"
                            ),
                            selected = "spring", width = "100%"
                )
              )
            ),
            fluidRow(
              column(
                12,
                # Dynamic content based on season selection
                tagList(lapply(SEASON_KEYS, build_season_panel)),
                # Legend explaining vertical lines
                div(
                  style = "margin-top: 10px; padding: 8px; background-color: #f8f9fa; border-radius: 4px; font-size: 11px; color: #555;",
                  em("Vertical lines in the plots represent values of foragers and frames per colony in the simulation year. Solid lines show aggregate values. When colony and frame allocation percentages differ between locations, location 1 (season color) and location 2 (gray) lines appear.")
                )
              )
            )
          )
        )
      ),
      tabItem(
        tabName = "economics",
        fluidRow(
          box(
            title = "Economic Parameters",
            status = "info", solidHeader = TRUE,
            width = 12,
            h4("Operating costs"),
            column(
              12,
              numericInput("Cost", "Maintenance Cost ($/col/year):", value = 200, min = 0, max = 400, step = 5),
              numericInput("P_feed", "Feed Price ($/unit):", value = 0.2, min = 0, max = 10, step = 0.01),
              numericInput("P_split", "Splitting Cost ($/op):", value = 25, min = 0, max = 100, step = 1),
              numericInput("P_merge", "Merging Cost ($/op):", value = 10, min = 0, max = 100, step = 1),
              numericInput("P_cull", "Culling Cost ($/op):", value = 10, min = 0, max = 100, step = 1)
            )
          )
        )
      )
    )
  )
)



# =============================================================================
# SHINY SERVER
# =============================================================================

server <- function(input, output, session) {
  
  # ===========================================================================
  # Parameter Loading and UI Updates
  # ===========================================================================
  
  # Function to update all UI inputs from a parameter list
  update_all_inputs <- function(param_list) {
    if (is.null(param_list)) return()
    
    for (param_name in names(param_list)) {
      params <- param_list[[param_name]]
      # Use a switch to handle different input types if necessary, for now, it's slider/numeric
      # The control is identified by its ID, which is param_name
      tryCatch({
        updateSliderInput(session, param_name,
                          value = params$value,
                          min = params$min,
                          max = params$max,
                          step = params$step)
      }, error = function(e) {
        # Slider doesn't exist, try numeric input
      })
      tryCatch({
        updateNumericInput(session, param_name,
                           value = params$value,
                           min = params$min,
                           max = params$max,
                           step = params$step)
      }, error = function(e) {
        # Numeric input doesn't exist either, skip
      })
      tryCatch({
        checkbox_val <- suppressWarnings(as.numeric(params$value))
        updateCheckboxInput(session, param_name, value = is.finite(checkbox_val) && checkbox_val > 0)
      }, error = function(e) {
        # Checkbox input doesn't exist either, skip
      })
    }
  }
  
  safe_numeric_input <- function(input_id, default_value) {
    val <- suppressWarnings(as.numeric(isolate(input[[input_id]])))
    if (length(val) == 0 || !is.finite(val[1])) {
      return(default_value)
    }
    val[1]
  }
  
  safe_logical_input <- function(input_id, default_value) {
    val <- isolate(input[[input_id]])
    if (is.null(val) || length(val) == 0) {
      return(default_value)
    }
    isTRUE(val)
  }
  
  ordered_management_seasons <- reactive({
    start_key <- tolower(input$start_season %||% "Spring")
    start_idx <- match(start_key, SEASON_KEYS)
    if (is.na(start_idx)) {
      return(SEASON_KEYS)
    }
    c(SEASON_KEYS[start_idx:length(SEASON_KEYS)], SEASON_KEYS[seq_len(start_idx - 1)])
  })
  
  build_management_column <- function(season_key) {
    panel_style <- switch(
      season_key,
      spring = "padding: 6px; background-color: #f8fff9; border: 1px solid #d6eadc; border-radius: 4px;",
      summer = "padding: 6px; background-color: #fffdf2; border: 1px solid #f3e9ba; border-radius: 4px;",
      fall = "padding: 6px; background-color: #fff7f0; border: 1px solid #f0d8bf; border-radius: 4px;",
      winter = "padding: 6px; background-color: #f4f8ff; border: 1px solid #cfe1f7; border-radius: 4px;",
      "padding: 6px; border: 1px solid #ddd; border-radius: 4px;"
    )
    header_color <- switch(
      season_key,
      spring = "#2E8B57",
      summer = "#FF8C00",
      fall = "#8B4513",
      winter = "#4682B4",
      "#333333"
    )
    default_shares <- switch(
      season_key,
      spring = c(50, 30, 20),
      summer = c(50, 30, 20),
      fall = c(65, 35, 0),
      winter = c(65, 35, 0),
      c(50, 30, 20)
    )
    default_adjust <- switch(
      season_key,
      spring = 23,
      summer = 7,
      fall = 6,
      winter = 16,
      0
    )
    default_cull <- switch(
      season_key,
      spring = 5,
      summer = 0,
      fall = 15,
      winter = 10,
      0
    )
    default_equal <- if (season_key == "winter") FALSE else TRUE
    has_loc3 <- "loc3" %in% SEASON_ACTIVE_LOCATIONS[[season_key]]
    loc_labels <- US_LOCATION_LABELS[[season_key]]
    bold_loc_label <- function(x) HTML(paste0("<span style='font-weight:700;'>", x, "</span>"))
    
    build_share_control <- function(loc_tag, default_share) {
      slider_id <- paste0("col_share_", loc_tag, "_", season_key)
      tagList(
        sliderInput(
          slider_id,
          bold_loc_label(loc_labels[[loc_tag]] %||% loc_tag),
          min = 0, max = 100,
          value = safe_numeric_input(slider_id, default_share),
          step = 5, width = "100%", ticks = FALSE, post = "%"
        ),
        tags$div(
          textOutput(paste0("colonies_start_", loc_tag, "_", season_key)),
          style = "font-size: 10px; color: #666; margin-top: -6px; margin-bottom: 2px;"
        )
      )
    }
    
    share_inputs <- list(
      build_share_control("loc1", default_shares[1]),
      build_share_control("loc2", default_shares[2])
    )
    if (has_loc3) {
      share_inputs <- c(share_inputs, list(
        build_share_control("loc3", default_shares[3])
      ))
    } else {
      share_inputs <- c(share_inputs, list(tags$div(style = "height: 58px;")))
    }
    
    column(
      3,
      div(
        class = paste("management-season-panel", paste0("season-", season_key)),
        style = panel_style,
        h5(SEASON_LABELS[[season_key]], style = paste0("margin-top: 0; margin-bottom: 6px; color: ", header_color, ";")),
        tags$hr(style = "margin-top: 2px; margin-bottom: 6px; border-top: 1px solid #d9d9d9;"),
        tags$div("Colonies split/merge:", style = "font-size: 11px; margin-top: 0;"),
        sliderInput(
          paste0("adjust_", season_key), NULL,
          min = -100, max = 100,
          value = safe_numeric_input(paste0("adjust_", season_key), default_adjust),
          step = 5, width = "100%", ticks = FALSE, post = "%"
        ),
        tags$div(
          textOutput(paste0("adjust_colonies_", season_key)),
          style = "font-size: 10px; color: #666; margin-top: -6px; margin-bottom: 2px;"
        ),
        tags$hr(style = "margin-top: 4px; margin-bottom: 6px; border-top: 1px solid #d9d9d9;"),
        tags$div("Colonies culled:", style = "font-size: 11px; margin-top: -4px;"),
        sliderInput(
          paste0("cull_", season_key), NULL,
          min = 0, max = 100,
          value = safe_numeric_input(paste0("cull_", season_key), default_cull),
          step = 1, width = "100%", ticks = FALSE, post = "%"
        ),
        tags$div(
          textOutput(paste0("cull_colonies_", season_key)),
          style = "font-size: 10px; color: #666; margin-top: -6px; margin-bottom: 2px;"
        ),
        tags$hr(style = "margin-top: 4px; margin-bottom: 6px; border-top: 1px solid #d9d9d9;"),
        tags$div(
          style = "min-height: 185px;",
          tags$div("Colonies in:", style = "font-size: 11px; margin-top: -4px;"),
          share_inputs
        ),
        tags$hr(style = "margin-top: 4px; margin-bottom: 6px; border-top: 1px solid #d9d9d9;"),
        tags$div("Targetted frames/colony", style = "font-size: 11px; margin-top: -4px;"),
        conditionalPanel(
          condition = paste0("!input.fpc_equal_", season_key),
          sliderInput(
            paste0("fpc_target_loc1_", season_key),
            bold_loc_label(loc_labels[["loc1"]] %||% "Location 1"),
            min = 0, max = 20,
            value = safe_numeric_input(paste0("fpc_target_loc1_", season_key), 6),
            step = 0.1, width = "100%", ticks = FALSE
          )
        ),
        checkboxInput(
          paste0("fpc_equal_", season_key),
          "Equalize frames/colony",
          value = safe_logical_input(paste0("fpc_equal_", season_key), default_equal)
        ),
        tags$div(textOutput(paste0("eff_fpc_loc1_", season_key)), style = "font-size: 11px;"),
        tags$div(textOutput(paste0("eff_fpc_loc2_", season_key)), style = "font-size: 11px;"),
        if (has_loc3) tags$div(textOutput(paste0("eff_fpc_loc3_", season_key)), style = "font-size: 11px;")
      )
    )
  }
  
  output$management_columns <- renderUI({
    season_cols <- lapply(ordered_management_seasons(), build_management_column)
    do.call(fluidRow, season_cols)
  })

  season_defaults_for_collection <- function(base_ph) {
    list(
      alpha = c(spring = season_value("alpha", "spring"), summer = season_value("alpha", "summer"), fall = season_value("alpha", "fall"), winter = season_value("alpha", "winter")),
      delta = c(spring = season_value("delta", "spring"), summer = season_value("delta", "summer"), fall = season_value("delta", "fall"), winter = season_value("delta", "winter")),
      gamma = c(spring = season_value("gamma", "spring"), summer = season_value("gamma", "summer"), fall = season_value("gamma", "fall"), winter = season_value("gamma", "winter")),
      omega = c(spring = season_value("omega", "spring"), summer = season_value("omega", "summer"), fall = season_value("omega", "fall"), winter = season_value("omega", "winter")),
      theta = c(spring = season_value("theta", "spring"), summer = season_value("theta", "summer"), fall = season_value("theta", "fall"), winter = season_value("theta", "winter")),
      Ph = c(spring = season_value("Ph", "spring"), summer = season_value("Ph", "summer"), fall = season_value("Ph", "fall"), winter = season_value("Ph", "winter")),
      Pc = c(spring = season_value("Pc", "spring"), summer = season_value("Pc", "summer"), fall = season_value("Pc", "fall"), winter = season_value("Pc", "winter")),
      A = c(spring = season_value("A", "spring"), summer = season_value("A", "summer"), fall = season_value("A", "fall"), winter = season_value("A", "winter")),
      I = c(spring = -2000, summer = -2000, fall = -2000, winter = -2000),
      B = c(spring = season_value("B", "spring"), summer = season_value("B", "summer"), fall = season_value("B", "fall"), winter = season_value("B", "winter")),
      D = c(spring = season_value("D", "spring"), summer = season_value("D", "summer"), fall = season_value("D", "fall"), winter = season_value("D", "winter")),
      G = c(spring = -1500, summer = -1500, fall = -1500, winter = -1500),
      E = c(spring = season_value("E", "spring"), summer = season_value("E", "summer"), fall = season_value("E", "fall"), winter = season_value("E", "winter"))
    )
  }

  collect_primary_params <- function(input) {
    base_ph <- input$Ph %||% 3.0
    params <- list(
      Cost = input$Cost %||% 50,
      Ph = base_ph,
      Pc = input$Pc %||% 0.5
    )

    seasonal_defaults <- season_defaults_for_collection(base_ph)
    for (param_name in names(seasonal_defaults)) {
      for (season_key in SEASON_KEYS) {
        field_name <- paste0(param_name, "_", season_key)
        params[[field_name]] <- input[[field_name]] %||% seasonal_defaults[[param_name]][[season_key]]
      }
    }

    params$A <- 100
    params$B <- input$B %||% 0.001
    params$I <- 50
    params$D <- 15
    params$E <- input$E %||% 0.002
    params$G <- 30

    params
  }

  collect_management_params <- function(input) {
    management_params <- list()
    for (action in c("cull", "adjust")) {
      for (season_key in SEASON_KEYS) {
        field_name <- paste0(action, "_", season_key)
        management_params[[field_name]] <- input[[field_name]]
      }
    }
    management_params$P_split <- input$P_split
    management_params$P_merge <- input$P_merge
    management_params$P_cull <- input$P_cull
    management_params
  }

  collect_location_params <- function(input, params, location_tag = "loc2") {
    params_loc <- list()
    seasonal_fields <- c("Ph", "Pc", "gamma", "alpha", "delta", "omega", "theta", "A", "B", "I", "D", "E", "G")
    for (field in seasonal_fields) {
      for (season_key in SEASON_KEYS) {
        key_loc1 <- paste0(field, "_", season_key)
        key_loc <- paste0(field, "_", season_key, "_", location_tag)
        params_loc[[key_loc]] <- input[[key_loc]] %||% params[[key_loc1]]
      }
    }

    cost_key <- paste0("Cost_", location_tag)
    params_loc[[cost_key]] <- input[[cost_key]] %||% params$Cost
    params_loc$Ph <- params$Ph %||% 3.0
    params_loc$Pc <- params$Pc %||% 0.5
    params_loc$A <- params$A %||% 100
    params_loc$B <- params$B %||% 0.002
    params_loc$I <- params$I %||% 50
    params_loc$D <- params$D %||% 15
    params_loc$E <- params$E %||% 0.003
    params_loc$G <- params$G %||% 30
    params_loc
  }

  clamp_percent <- function(x, default_value = 0) {
    val <- suppressWarnings(as.numeric(x))
    if (length(val) == 0 || is.na(val[1]) || !is.finite(val[1])) {
      return(default_value)
    }
    min(100, max(0, val[1]))
  }

  share_input_ids <- function(season_key) {
    c(
      loc1 = paste0("col_share_loc1_", season_key),
      loc2 = paste0("col_share_loc2_", season_key),
      loc3 = paste0("col_share_loc3_", season_key)
    )
  }
  
  season_has_loc3 <- function(season_key) {
    "loc3" %in% (SEASON_ACTIVE_LOCATIONS[[season_key]] %||% c("loc1", "loc2", "loc3"))
  }
  
  constrained_shares <- function(season_key, changed_loc = 1) {
    ids <- share_input_ids(season_key)
    include_loc3 <- season_has_loc3(season_key)
    shares <- c(
      clamp_percent(input[[ids["loc1"]]], 50),
      clamp_percent(input[[ids["loc2"]]], 30),
      if (include_loc3) clamp_percent(input[[ids["loc3"]]], 20) else 0
    )
    names(shares) <- c("loc1", "loc2", "loc3")

    adjust_pair_to_sum <- function(a, b, target_sum) {
      target_sum <- min(100, max(0, target_sum))
      pair <- c(a = min(100, max(0, a)), b = min(100, max(0, b)))
      current_sum <- sum(pair)
      if (current_sum > target_sum) {
        excess <- current_sum - target_sum
        if (pair["a"] >= pair["b"]) {
          drop_a <- min(pair["a"], excess)
          pair["a"] <- pair["a"] - drop_a
          excess <- excess - drop_a
          pair["b"] <- max(0, pair["b"] - excess)
        } else {
          drop_b <- min(pair["b"], excess)
          pair["b"] <- pair["b"] - drop_b
          excess <- excess - drop_b
          pair["a"] <- max(0, pair["a"] - excess)
        }
      } else if (current_sum < target_sum) {
        deficit <- target_sum - current_sum
        if (pair["a"] >= pair["b"]) {
          add_a <- min(100 - pair["a"], deficit)
          pair["a"] <- pair["a"] + add_a
          deficit <- deficit - add_a
          pair["b"] <- min(100, pair["b"] + deficit)
        } else {
          add_b <- min(100 - pair["b"], deficit)
          pair["b"] <- pair["b"] + add_b
          deficit <- deficit - add_b
          pair["a"] <- min(100, pair["a"] + deficit)
        }
      }
      pair
    }

    changed_loc <- as.integer(changed_loc %||% 1)
    changed_loc <- min(if (include_loc3) 3 else 2, max(1, changed_loc))
    
    if (!include_loc3) {
      if (changed_loc == 1) {
        shares[2] <- max(0, 100 - shares[1])
      } else {
        shares[1] <- max(0, 100 - shares[2])
      }
      shares[1] <- min(100, max(0, shares[1]))
      shares[2] <- min(100, max(0, shares[2]))
      snap5 <- function(x) round(x / 5) * 5
      shares[1] <- snap5(shares[1])
      shares[2] <- min(100, max(0, 100 - shares[1]))
      shares[3] <- 0
      return(shares)
    }

    if (changed_loc %in% c(1, 2)) {
      other_idx <- if (changed_loc == 1) 2 else 1
      desired_l3 <- 100 - shares[1] - shares[2]
      if (desired_l3 >= 0) {
        shares[3] <- desired_l3
      } else {
        shares[3] <- 0
        shares[other_idx] <- max(0, 100 - shares[changed_loc])
      }
    } else {
      pair <- adjust_pair_to_sum(shares[1], shares[2], 100 - shares[3])
      shares[1] <- pair["a"]
      shares[2] <- pair["b"]
    }

    shares[1] <- min(100, max(0, shares[1]))
    shares[2] <- min(100, max(0, shares[2]))
    shares[3] <- min(100, max(0, shares[3]))
    shares[3] <- min(100, max(0, 100 - shares[1] - shares[2]))

    # Snap to slider step (5%) to avoid oscillation loops from rounding.
    snap5 <- function(x) round(x / 5) * 5
    shares[1] <- snap5(shares[1])
    shares[2] <- snap5(shares[2])
    shares[3] <- min(100, max(0, 100 - shares[1] - shares[2]))
    shares
  }

  share_update_lock <- reactiveVal(FALSE)

  enforce_share_rule_ui <- function(season_key, changed_loc = 1) {
    if (isTRUE(share_update_lock())) {
      return(invisible(NULL))
    }
    ids <- share_input_ids(season_key)
    include_loc3 <- season_has_loc3(season_key)
    before_vals <- c(
      clamp_percent(input[[ids["loc1"]]], 50),
      clamp_percent(input[[ids["loc2"]]], 30),
      if (include_loc3) clamp_percent(input[[ids["loc3"]]], 20) else 0
    )
    shares <- constrained_shares(season_key, changed_loc = changed_loc)
    share_update_lock(TRUE)
    on.exit(share_update_lock(FALSE), add = TRUE)
    id1 <- unname(ids["loc1"])
    id2 <- unname(ids["loc2"])
    id3 <- unname(ids["loc3"])
    v1 <- as.numeric(shares["loc1"])
    v2 <- as.numeric(shares["loc2"])
    v3 <- as.numeric(shares["loc3"])
    
    # Always push normalized values back to the UI.
    # The lock prevents feedback loops from these programmatic updates.
    updateSliderInput(session, id1, value = v1)
    updateSliderInput(session, id2, value = v2)
    if (include_loc3) {
      updateSliderInput(session, id3, value = v3)
    }
    invisible(NULL)
  }

  last_share_inputs <- reactiveValues()
  detect_changed_loc <- function(prev_vals, curr_vals, include_loc3 = TRUE) {
    if (length(prev_vals) != 3 || any(!is.finite(prev_vals))) {
      return(1L)
    }
    idx <- if (include_loc3) 1:3 else 1:2
    deltas <- abs(curr_vals[idx] - prev_vals[idx])
    if (all(deltas <= 1e-9)) {
      return(1L)
    }
    as.integer(which.max(deltas))
  }
  
  for (season_key in SEASON_KEYS) {
    local({
      sk <- season_key
      ids <- share_input_ids(sk)
      id1 <- unname(ids["loc1"])
      id2 <- unname(ids["loc2"])
      id3 <- unname(ids["loc3"])
      include_loc3 <- season_has_loc3(sk)
      
      if (include_loc3) {
        observeEvent(list(input[[id1]], input[[id2]], input[[id3]]), {
          curr_vals <- c(
            clamp_percent(input[[id1]], 50),
            clamp_percent(input[[id2]], 30),
            clamp_percent(input[[id3]], 20)
          )
          last_key <- paste0("shares_", sk)
          prev_vals <- isolate(last_share_inputs[[last_key]])
          changed_loc <- detect_changed_loc(prev_vals, curr_vals, include_loc3 = TRUE)
          
          enforced_vals <- constrained_shares(sk, changed_loc = changed_loc)
          enforce_share_rule_ui(sk, changed_loc = changed_loc)
          last_share_inputs[[last_key]] <- as.numeric(enforced_vals)
        }, ignoreInit = TRUE)
      } else {
        observeEvent(list(input[[id1]], input[[id2]]), {
          curr_vals <- c(
            clamp_percent(input[[id1]], 50),
            clamp_percent(input[[id2]], 30),
            0
          )
          last_key <- paste0("shares_", sk)
          prev_vals <- isolate(last_share_inputs[[last_key]])
          changed_loc <- detect_changed_loc(prev_vals, curr_vals, include_loc3 = FALSE)
          
          enforced_vals <- constrained_shares(sk, changed_loc = changed_loc)
          enforce_share_rule_ui(sk, changed_loc = changed_loc)
          last_share_inputs[[last_key]] <- as.numeric(enforced_vals)
        }, ignoreInit = TRUE)
      }
    })
  }

  collect_allocation_params <- function(input) {
    allocation_params <- list()
    
    # Colony share controls
    for (season_key in SEASON_KEYS) {
      shares <- constrained_shares(season_key, changed_loc = 1)
      allocation_params[[paste0("col_share_loc1_", season_key)]] <- unname(as.numeric(shares["loc1"]))
      allocation_params[[paste0("col_share_loc2_", season_key)]] <- unname(as.numeric(shares["loc2"]))
      allocation_params[[paste0("col_share_loc3_", season_key)]] <- unname(as.numeric(shares["loc3"]))
    }
    
    # Frames/colony target on location 1 by season.
    default_fpc <- (input$initial_frames %||% 6000) / max(1, (input$initial_colonies %||% 1000))
    for (season_key in SEASON_KEYS) {
      equal_key <- paste0("fpc_equal_", season_key)
      allocation_params[[equal_key]] <- isTRUE(input[[equal_key]])
      target_key <- paste0("fpc_target_loc1_", season_key)
      target_val <- suppressWarnings(as.numeric(input[[target_key]]))
      if (length(target_val) == 0 || !is.finite(target_val[1]) || target_val[1] < 0) {
        target_val <- default_fpc
      } else {
        target_val <- target_val[1]
      }
      allocation_params[[target_key]] <- target_val
    }
    allocation_params
  }

  effective_fpc_from_controls <- function(season_key) {
    shares <- as.numeric(constrained_shares(season_key, changed_loc = 1)) / 100
    total_colonies <- as.numeric(input$initial_colonies %||% 1000)
    total_frames <- as.numeric(input$initial_frames %||% 6000)
    cols <- pmax(0, total_colonies * shares)
    avg_fpc <- ifelse(total_colonies > 0, total_frames / total_colonies, 0)
    equal_mode <- isTRUE(input[[paste0("fpc_equal_", season_key)]])
    target_loc1 <- suppressWarnings(as.numeric(input[[paste0("fpc_target_loc1_", season_key)]]))
    if (length(target_loc1) == 0 || !is.finite(target_loc1[1]) || target_loc1[1] < 0) {
      target_loc1 <- avg_fpc
    } else {
      target_loc1 <- target_loc1[1]
    }

    eff <- rep(0, 3)
    feasible <- TRUE
    if (equal_mode) {
      eff[] <- avg_fpc
      target_used <- avg_fpc
    } else {
      c1 <- cols[1]
      c_other <- sum(cols[2:3])
      if (c1 <= 0) {
        target_used <- 0
        eff[1] <- 0
        eff[2:3] <- if (c_other > 0) total_frames / c_other else 0
      } else if (c_other <= 0) {
        target_used <- total_frames / c1
        eff[1] <- target_used
        eff[2:3] <- 0
      } else {
        other_target <- (total_frames - c1 * target_loc1) / c_other
        if (is.finite(other_target) && other_target >= 0) {
          target_used <- target_loc1
          eff[1] <- target_loc1
          eff[2:3] <- other_target
        } else {
          feasible <- FALSE
          target_used <- total_frames / c1
          eff[1] <- target_used
          eff[2:3] <- 0
        }
      }
    }

    baseline <- list(
      eff_loc1 = eff[1],
      eff_loc2 = eff[2],
      eff_loc3 = eff[3],
      feasible = feasible,
      target_used = target_used,
      equal_mode = equal_mode
    )
    
    # Prefer actual simulated seasonal values when available.
    # Fallback to control-implied values until simulation data exists.
    scalar_num <- function(x, default = NA_real_) {
      val <- suppressWarnings(as.numeric(x))
      if (length(val) == 0 || !is.finite(val[1])) {
        return(default)
      }
      val[1]
    }
    season_label <- SEASON_LABELS[[season_key]] %||% season_key
    sim_df <- tryCatch(simulation_results(), error = function(e) NULL)
    if (is.null(sim_df) || nrow(sim_df) == 0 || !("season" %in% names(sim_df))) {
      return(baseline)
    }
    season_rows <- sim_df[sim_df$season == season_label, , drop = FALSE]
    if (nrow(season_rows) == 0) {
      return(baseline)
    }
    row_idx <- which.min(season_rows$period)
    season_row <- season_rows[row_idx, , drop = FALSE]
    
    eff1_sim <- scalar_num(season_row$frames_per_colony_loc1, baseline$eff_loc1)
    eff2_sim <- scalar_num(season_row$frames_per_colony_loc2, baseline$eff_loc2)
    has_loc3 <- "loc3" %in% (SEASON_ACTIVE_LOCATIONS[[season_key]] %||% c("loc1", "loc2", "loc3"))
    eff3_sim <- if (has_loc3) scalar_num(season_row$frames_per_colony_loc3, baseline$eff_loc3) else 0
    
    baseline$eff_loc1 <- eff1_sim
    baseline$eff_loc2 <- eff2_sim
    baseline$eff_loc3 <- eff3_sim
    baseline
  }
  
  location_name_for <- function(season_key, loc_tag) {
    seasonal_labels <- US_LOCATION_LABELS[[season_key]]
    if (!is.null(seasonal_labels) && !is.null(seasonal_labels[[loc_tag]])) {
      return(as.character(seasonal_labels[[loc_tag]]))
    }
    loc_tag
  }
  
  colonies_start_for_slider <- function(season_key, loc_tag) {
    season_label <- SEASON_LABELS[[season_key]] %||% season_key
    sim_df <- tryCatch(simulation_results(), error = function(e) NULL)
    sim_col_name <- paste0("colonies_start_", loc_tag)
    
    if (!is.null(sim_df) && nrow(sim_df) > 0 && ("season" %in% names(sim_df)) && (sim_col_name %in% names(sim_df))) {
      season_rows <- sim_df[sim_df$season == season_label, , drop = FALSE]
      if (nrow(season_rows) > 0) {
        row_idx <- which.min(season_rows$period)
        val <- suppressWarnings(as.numeric(season_rows[[sim_col_name]][row_idx]))
        if (length(val) > 0 && is.finite(val[1])) {
          return(val[1])
        }
      }
    }
    
    # Fallback before simulation exists: percentage allocation of initial colonies.
    shares <- constrained_shares(season_key, changed_loc = 1)
    share_val <- as.numeric(shares[[loc_tag]]) %||% 0
    init_cols <- suppressWarnings(as.numeric(input$initial_colonies %||% 3000))
    if (!is.finite(init_cols)) {
      init_cols <- 3000
    }
    init_cols * share_val / 100
  }
  
  management_colony_impacts <- function(season_key) {
    safe_scalar <- function(x, default = 0) {
      val <- suppressWarnings(as.numeric(x))
      if (length(val) == 0 || !is.finite(val[1])) return(default)
      val[1]
    }
    season_label <- SEASON_LABELS[[season_key]] %||% season_key
    cull_rate <- safe_scalar(input[[paste0("cull_", season_key)]], 0) / 100
    adjust_rate <- safe_scalar(input[[paste0("adjust_", season_key)]], 0) / 100
    
    sim_df <- tryCatch(simulation_results(), error = function(e) NULL)
    colonies_pre_mgmt <- NA_real_
    if (!is.null(sim_df) && nrow(sim_df) > 0 && "season" %in% names(sim_df) && "colonies_end" %in% names(sim_df)) {
      season_rows <- sim_df[sim_df$season == season_label, , drop = FALSE]
      if (nrow(season_rows) > 0) {
        row_idx <- which.min(season_rows$period)
        colonies_pre_mgmt <- safe_scalar(season_rows$colonies_end[row_idx], NA_real_)
      }
    }
    
    if (!is.finite(colonies_pre_mgmt)) {
      colonies_pre_mgmt <- safe_scalar(input$initial_colonies, 3000)
    }
    
    culled <- pmax(0, colonies_pre_mgmt * pmax(0, cull_rate))
    # For UI clarity, show split/merge impact against pre-management colonies.
    adjusted <- pmax(0, colonies_pre_mgmt * abs(adjust_rate))
    adjust_label <- if (adjust_rate > 0) "Colonies split" else if (adjust_rate < 0) "Colonies merged" else "Colonies split/merged"
    
    list(
      cull_text = paste0("Colonies culled: ", format(round(culled, 0), big.mark = ",", scientific = FALSE)),
      adjust_text = paste0(adjust_label, ": ", format(round(adjusted, 0), big.mark = ",", scientific = FALSE))
    )
  }

  for (season_key in SEASON_KEYS) {
    local({
      sk <- season_key
      output[[paste0("eff_fpc_loc1_", sk)]] <- renderText({
        vals <- effective_fpc_from_controls(sk)
        paste0("Effective ", location_name_for(sk, "loc1"), ": ", sprintf("%.2f", vals$eff_loc1))
      })
      output[[paste0("eff_fpc_loc2_", sk)]] <- renderText({
        vals <- effective_fpc_from_controls(sk)
        paste0("Effective ", location_name_for(sk, "loc2"), ": ", sprintf("%.2f", vals$eff_loc2))
      })
      output[[paste0("eff_fpc_loc3_", sk)]] <- renderText({
        vals <- effective_fpc_from_controls(sk)
        paste0("Effective ", location_name_for(sk, "loc3"), ": ", sprintf("%.2f", vals$eff_loc3))
      })
      output[[paste0("cull_colonies_", sk)]] <- renderText({
        management_colony_impacts(sk)$cull_text
      })
      output[[paste0("adjust_colonies_", sk)]] <- renderText({
        management_colony_impacts(sk)$adjust_text
      })
      
      for (loc_tag in SEASON_ACTIVE_LOCATIONS[[sk]]) {
        local({
          lk <- loc_tag
          output[[paste0("colonies_start_", lk, "_", sk)]] <- renderText({
            val <- colonies_start_for_slider(sk, lk)
            paste0("Start colonies: ", format(round(val, 0), big.mark = ",", scientific = FALSE))
          })
        })
      }
    })
  }

  last_target_labels <- reactiveValues()
  observe({
    for (season_key in SEASON_KEYS) {
      vals <- effective_fpc_from_controls(season_key)
      target_id <- paste0("fpc_target_loc1_", season_key)
      loc1_name <- location_name_for(season_key, "loc1")
      base_label <- loc1_name
      label_txt <- if (vals$equal_mode) {
        paste0("Targetted frames/colony (", loc1_name, ") (equalized)")
      } else if (!vals$feasible) {
        paste0("<span style='color:#DC143C; font-weight:600;'>", base_label, " (infeasible)</span>")
      } else {
        base_label
      }
      label_key <- paste0("target_label_", season_key)
      if (!identical(last_target_labels[[label_key]], label_txt)) {
        updateSliderInput(session, target_id, label = HTML(label_txt))
        last_target_labels[[label_key]] <- label_txt
      }
    }
  })
  
  # Observer for file upload
  observeEvent(input$load_settings_csv, {
    req(input$load_settings_csv)
    
    new_params <- load_parameter_config(input$load_settings_csv$datapath, silent = FALSE)
    if (!is.null(new_params)) {
      update_all_inputs(new_params)
      showNotification("Parameters loaded successfully from file.", type = "message")
    } else {
      showNotification("Error loading parameter file. Check format.", type = "error")
    }
  })
  
  # Simulation status/time text shown in the sidebar.
  sim_status_text <- reactiveVal("Ready")
  sim_time_text <- reactiveVal("Simulation time: not run yet")
  
  output$sim_status <- renderText({
    sim_status_text()
  })
  output$sim_time_display <- renderText({
    sim_time_text()
  })
  
  # Reactive function to check constraint violations
  constraint_violations <- reactive({
    violations <- character(0)
    seasons <- c("spring", "summer", "fall", "winter")
    
    for (season in seasons) {
      split_input <- paste0("split_", season)
      merge_input <- paste0("merge_", season)
      
      split_val <- input[[split_input]] %||% 0
      merge_val <- input[[merge_input]] %||% 0
      
      if (split_val > 0.1 && merge_val > 0.1) {
        violations <- c(violations, paste(
          "Split and merge both active in", season,
          "(split:", split_val, "%, merge:", merge_val, "%)"
        ))
      }
    }
    
    return(violations)
  })
  
  # Simulation trigger control:
  # - Live updates for Main simulation controls (debounced)
  # - Seasonal/Economic edits are deferred until user returns to Main simulation
  simulation_trigger_nonce <- reactiveVal(0L)
  deferred_simulation_update <- reactiveVal(FALSE)
  
  main_sim_live_inputs <- reactive({
    scalar_num <- function(id) suppressWarnings(as.numeric(input[[id]] %||% NA_real_))
    list(
      start_season = input$start_season,
      annual_discount_rate = input$annual_discount_rate,
      initial_colonies = input$initial_colonies,
      initial_frames = input$initial_frames,
      cull = vapply(SEASON_KEYS, function(sk) scalar_num(paste0("cull_", sk)), numeric(1)),
      adjust = vapply(SEASON_KEYS, function(sk) scalar_num(paste0("adjust_", sk)), numeric(1)),
      shares = unlist(lapply(SEASON_KEYS, function(sk) {
        c(
          scalar_num(paste0("col_share_loc1_", sk)),
          scalar_num(paste0("col_share_loc2_", sk)),
          scalar_num(paste0("col_share_loc3_", sk))
        )
      }), use.names = FALSE),
      fpc_equal = vapply(SEASON_KEYS, function(sk) isTRUE(input[[paste0("fpc_equal_", sk)]]), logical(1)),
      fpc_target = vapply(SEASON_KEYS, function(sk) scalar_num(paste0("fpc_target_loc1_", sk)), numeric(1))
    )
  })
  main_sim_live_inputs_debounced <- debounce(main_sim_live_inputs, 300)
  
  deferred_input_ids <- c(
    # Economic tab controls
    "Cost", "Cost_loc2", "Cost_loc3", "P_feed", "P_split", "P_merge", "P_cull",
    # Seasonal parameters tab controls (all locations)
    as.vector(unlist(lapply(c("alpha", "delta", "gamma", "omega", "theta", "A", "B", "I", "D", "E", "G", "Ph", "Pc"), function(field) {
      as.vector(unlist(lapply(SEASON_KEYS, function(sk) {
        c(
          paste0(field, "_", sk),
          paste0(field, "_", sk, "_loc2"),
          paste0(field, "_", sk, "_loc3")
        )
      })))
    })))
  )
  
  deferred_inputs <- reactive({
    vals <- lapply(deferred_input_ids, function(id) input[[id]])
    names(vals) <- deferred_input_ids
    vals
  })
  deferred_inputs_debounced <- debounce(deferred_inputs, 300)
  
  observeEvent(main_sim_live_inputs_debounced(), {
    if (identical(input$main_tab, "simulation")) {
      simulation_trigger_nonce(simulation_trigger_nonce() + 1L)
    }
  }, ignoreInit = TRUE)
  
  observeEvent(deferred_inputs_debounced(), {
    if (identical(input$main_tab, "simulation")) {
      simulation_trigger_nonce(simulation_trigger_nonce() + 1L)
    } else {
      deferred_simulation_update(TRUE)
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$main_tab, {
    if (identical(input$main_tab, "simulation") && isTRUE(deferred_simulation_update())) {
      deferred_simulation_update(FALSE)
      simulation_trigger_nonce(simulation_trigger_nonce() + 1L)
    }
  }, ignoreInit = TRUE)
  
  # Keep update button as explicit manual re-run.
  observeEvent(input$update_simulation, {
    simulation_trigger_nonce(simulation_trigger_nonce() + 1L)
  }, ignoreInit = TRUE)
  
  # ignoreNULL = FALSE keeps one initial run at app startup.
  simulation_results <- eventReactive(simulation_trigger_nonce(), {
    # Start timing
    start_time <- Sys.time()
    
    # Show that simulation is running
    sim_status_text("Running simulation...")
    
    # Collect all parameters (data-driven seasonal/location handling)
    params <- collect_primary_params(input)
    management_params <- collect_management_params(input)
    
    # Create schedule and locations
    schedule <- create_simple_schedule(1, input$start_season %||% "Spring")
    locations <- create_location_parameters(schedule, params)
    
    # Build Location 2 parameters and allocation shares (two-location mode)
    params_loc2 <- collect_location_params(input, params, "loc2")
    params_loc3 <- collect_location_params(input, params, "loc3")
    locations_loc2 <- create_location_parameters_loc2(schedule, params_loc2)
    locations_loc3 <- create_location_parameters_loc3(schedule, params_loc3)
    
    allocation_params <- collect_allocation_params(input)
    
    # Run simulation
    results <- run_beekeeping_simulation(
      locations = locations,
      initial_colonies = input$initial_colonies,
      initial_frames = input$initial_frames,
      P_feed = input$P_feed,
      management_params = management_params,
      locations_loc2 = locations_loc2,
      allocation_params = allocation_params,
      location_tables = list(loc1 = locations, loc2 = locations_loc2, loc3 = locations_loc3)
    )
    
    # Convert to dataframe
    results_df <- results_to_dataframe(results)
    
    # Add present value calculations
    annual_rate <- (input$annual_discount_rate %||% 3) / 100
    quarterly_rate <- (1 + annual_rate)^(1 / 4) - 1
    
    # Calculate discount factors and present values
    results_df$discount_factor <- sapply(1:nrow(results_df), function(i) {
      1 / (1 + quarterly_rate)^(i - 1)
    })
    results_df$pv_profit <- results_df$profit * results_df$discount_factor
    
    # Calculate simulation time
    end_time <- Sys.time()
    sim_duration <- as.numeric(difftime(end_time, start_time, units = "secs"))
    
    # Update status and time
    sim_status_text(paste("Simulation complete! (", nrow(results_df), "periods)"))
    sim_time_text(
      if (sim_duration < 1) {
        paste("Simulation time:", round(sim_duration * 1000, 1), "ms")
      } else {
        paste("Simulation time:", round(sim_duration, 3), "seconds")
      }
    )
    
    return(results_df)
  }, ignoreNULL = FALSE)
  
  # Generate stock dynamics candlestick plot
  output$stock_plot <- renderPlot({
    results_df <- simulation_results()
    
    # Determine which panels to show based on checkboxes
    panels_to_show <- c()
    if (input$show_colonies) panels_to_show <- c(panels_to_show, "colonies")
    if (input$show_frames) panels_to_show <- c(panels_to_show, "frames")
    if (input$show_frames_per_colony) panels_to_show <- c(panels_to_show, "frames_per_colony")
    if (input$show_forage_balance) panels_to_show <- c(panels_to_show, "forage_balance")
    
    plot_stock_dynamics(results_df, panels_to_show, input$forage_per_colony)
  })
  
  
  # Generate operations table
  output$operations_table <- renderTable(
    {
      results_df <- simulation_results()

      selected_data <- results_df
      avg_max_colonies <- calculate_avg_max_colonies(selected_data)
      
      # Calculate operations metrics
      avg_colonies <- mean(selected_data$colonies_end, na.rm = TRUE)
      avg_frames <- mean(selected_data$frames_end, na.rm = TRUE)
      avg_frames_per_colony <- mean(selected_data$frames_per_colony, na.rm = TRUE)
      total_forage <- sum(selected_data$forage_collected, na.rm = TRUE)
      total_honey <- sum(selected_data$honey_harvested, na.rm = TRUE)
      total_feed <- sum(selected_data$feed_required, na.rm = TRUE)
      
      operations_stats <- data.frame(
        Metric = c(
          "Average Colonies",
          "Average Frames",
          "Average Frames/Colony",
          "Forage Collected (lbs)",
          "Honey Harvested (lbs)",
          "Feed Provided (lbs)"
        ),
        Operation = c(
          format(round(avg_colonies, 0), big.mark = ",", scientific = FALSE),
          format(round(avg_frames, 0), big.mark = ",", scientific = FALSE),
          format(round(avg_frames_per_colony, 1), nsmall = 1, scientific = FALSE),
          format(round(total_forage, 0), big.mark = ",", scientific = FALSE),
          format(round(total_honey, 0), big.mark = ",", scientific = FALSE),
          format(round(total_feed, 0), big.mark = ",", scientific = FALSE)
        ),
        `Per Colony` = c(
          "1.0", # Per colony colonies is always 1
          format(round(avg_frames / avg_max_colonies, 0), big.mark = ",", scientific = FALSE),
          format(round(avg_frames_per_colony, 1), nsmall = 1, scientific = FALSE), # This is already per colony
          format(round(total_forage / avg_max_colonies, 0), big.mark = ",", scientific = FALSE),
          format(round(total_honey / avg_max_colonies, 0), big.mark = ",", scientific = FALSE),
          format(round(total_feed / avg_max_colonies, 0), big.mark = ",", scientific = FALSE)
        ),
        check.names = FALSE
      )
      
      return(operations_stats)
    },
    striped = TRUE,
    bordered = TRUE
  )
  
  # Generate total profit display
  output$total_profit_display <- renderText({
    results_df <- simulation_results()
    if (is.null(results_df)) {
      return("$0")
    }
    
    # Calculate present value of total profit over entire simulation
    total_pv_profit <- sum(results_df$pv_profit, na.rm = TRUE)
    
    paste("$", format(round(total_pv_profit), big.mark = ","))
  })
  
  # Generate seasonal balance chart
  output$seasonal_balance_chart <- renderPlot({
    results_df <- simulation_results()

    selected_data <- results_df
    start_period <- min(selected_data$period, na.rm = TRUE)
    end_period <- max(selected_data$period, na.rm = TRUE)
    avg_max_colonies <- calculate_avg_max_colonies(selected_data)
    
    # Calculate per-colony metrics for all periods (not averaged by season)
    period_data <- selected_data %>%
      mutate(
        forage_collected_pc = forage_collected / avg_max_colonies,
        feed_required_pc = feed_required / avg_max_colonies,
        forage_consumed_pc = -forage_consumed / avg_max_colonies,  # Negative for downward bar
        honey_harvested_pc = -honey_harvested / avg_max_colonies   # Negative for downward bar
      ) %>%
      select(period, season, forage_collected_pc, feed_required_pc, forage_consumed_pc, honey_harvested_pc)
    
    # Create data for plotting - all periods with single bar per period
    plot_data <- data.frame(
      Period = rep(period_data$period, 4),
      Season = rep(period_data$season, 4),
      Type = factor(
        rep(c("Forage Collected", "Feed Added", "Honey Harvested", "Forage Consumed"), each = nrow(period_data)),
        levels = c("Forage Collected", "Feed Added", "Honey Harvested", "Forage Consumed")
      ),
      Value = c(
        period_data$forage_collected_pc, period_data$feed_required_pc,
        period_data$honey_harvested_pc, period_data$forage_consumed_pc
      )
    )
    
    # Create the diverging bar plot
    ggplot(plot_data, aes(x = Period, y = Value, fill = Type)) +
      geom_col(position = "stack", width = 0.7) +
      geom_hline(yintercept = 0, color = "black", linewidth = 0.8) +  # Emphasize zero line
      scale_fill_manual(values = c(
        "Forage Collected" = "#1a6b1a",
        "Feed Added" = "#8B9D83",
        "Honey Harvested" = "#FFC30B",
        "Forage Consumed" = "#FF6B6B"
      )) +
      scale_x_continuous(
        breaks = unique(period_data$period),
        labels = unique(period_data$period),
        limits = c(start_period - 0.5, end_period + 0.5)
      ) +
      labs(
        title = "",
        x = "Period",
        y = "Per Colony (lbs)",
        fill = ""
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 13),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 13),
        legend.position = "bottom",
        panel.grid.minor.x = element_blank(),
        panel.grid.major = element_line(size = 0.5, color = "#e0e0e0")
      ) +
      guides(fill = guide_legend(ncol = 2))
  })
  
  # Generate financial table
  output$financial_table <- renderTable(
    {
      results_df <- simulation_results()

      selected_data <- results_df
      
      # Calculate costs (nominal for compatibility)
      maintenance_costs <- sum(selected_data$cost_maintenance, na.rm = TRUE)
      
      stock_operations_costs <- 0
      if ("management_cost" %in% names(selected_data)) {
        stock_operations_costs <- sum(selected_data$management_cost, na.rm = TRUE)
      }
      
      feed_costs <- sum(selected_data$cost_feed, na.rm = TRUE)
      
      # Calculate present value costs
      maintenance_costs_pv <- sum(selected_data$cost_maintenance * selected_data$discount_factor, na.rm = TRUE)
      
      stock_operations_costs_pv <- 0
      if ("management_cost" %in% names(selected_data)) {
        stock_operations_costs_pv <- sum(selected_data$management_cost * selected_data$discount_factor, na.rm = TRUE)
      }
      
      feed_costs_pv <- sum(selected_data$cost_feed * selected_data$discount_factor, na.rm = TRUE)
      total_costs_pv <- maintenance_costs_pv + stock_operations_costs_pv + feed_costs_pv
      
      # Calculate present value totals for the full simulation year
      total_pv_revenue <- sum(selected_data$revenue_honey * selected_data$discount_factor, na.rm = TRUE) +
        sum(selected_data$revenue_crop * selected_data$discount_factor, na.rm = TRUE)
      total_pv_costs <- sum(selected_data$cost_maintenance * selected_data$discount_factor, na.rm = TRUE) +
        sum(selected_data$cost_feed * selected_data$discount_factor, na.rm = TRUE) +
        sum(selected_data$management_cost * selected_data$discount_factor, na.rm = TRUE)
      total_pv_profit <- sum(selected_data$pv_profit, na.rm = TRUE)
      
      # Calculate revenues by season (present value)
      honey_spring <- sum(selected_data$revenue_honey[selected_data$season == "Spring"] *
                            selected_data$discount_factor[selected_data$season == "Spring"], na.rm = TRUE)
      honey_summer <- sum(selected_data$revenue_honey[selected_data$season == "Summer"] *
                            selected_data$discount_factor[selected_data$season == "Summer"], na.rm = TRUE)
      honey_fall <- sum(selected_data$revenue_honey[selected_data$season == "Fall"] *
                          selected_data$discount_factor[selected_data$season == "Fall"], na.rm = TRUE)
      honey_winter <- sum(selected_data$revenue_honey[selected_data$season == "Winter"] *
                            selected_data$discount_factor[selected_data$season == "Winter"], na.rm = TRUE)
      
      poll_spring <- sum(selected_data$revenue_crop[selected_data$season == "Spring"] *
                           selected_data$discount_factor[selected_data$season == "Spring"], na.rm = TRUE)
      poll_summer <- sum(selected_data$revenue_crop[selected_data$season == "Summer"] *
                           selected_data$discount_factor[selected_data$season == "Summer"], na.rm = TRUE)
      poll_fall <- sum(selected_data$revenue_crop[selected_data$season == "Fall"] *
                         selected_data$discount_factor[selected_data$season == "Fall"], na.rm = TRUE)
      poll_winter <- sum(selected_data$revenue_crop[selected_data$season == "Winter"] *
                           selected_data$discount_factor[selected_data$season == "Winter"], na.rm = TRUE)
      
      total_revenue <- total_pv_revenue
      total_profit <- total_pv_profit
      
      # Calculate average colony count for per-colony metrics
      avg_max_colonies <- calculate_avg_max_colonies(selected_data)
      
      # Build financial stats with two columns
      metrics <- c()
      operation_values <- c()
      per_colony_values <- c()
      
      # Add costs section
      metrics <- c(metrics, "--- COSTS ---")
      operation_values <- c(operation_values, "")
      per_colony_values <- c(per_colony_values, "")
      
      metrics <- c(metrics, "Maintenance ($)")
      operation_values <- c(operation_values, paste("$", format(round(maintenance_costs_pv), big.mark = ",")))
      per_colony_values <- c(per_colony_values, paste("$", format(round(maintenance_costs_pv / avg_max_colonies), big.mark = ",")))
      
      metrics <- c(metrics, "Stock Operations ($)")
      operation_values <- c(operation_values, paste("$", format(round(stock_operations_costs_pv), big.mark = ",")))
      per_colony_values <- c(per_colony_values, paste("$", format(round(stock_operations_costs_pv / avg_max_colonies), big.mark = ",")))
      
      metrics <- c(metrics, "Feed ($)")
      operation_values <- c(operation_values, paste("$", format(round(feed_costs_pv), big.mark = ",")))
      per_colony_values <- c(per_colony_values, paste("$", format(round(feed_costs_pv / avg_max_colonies), big.mark = ",")))
      
      metrics <- c(metrics, "Total Costs ($)")
      operation_values <- c(operation_values, paste("$", format(round(total_pv_costs), big.mark = ",")))
      per_colony_values <- c(per_colony_values, paste("$", format(round(total_pv_costs / avg_max_colonies), big.mark = ",")))
      
      # Add revenues section header
      metrics <- c(metrics, "--- REVENUES ---")
      operation_values <- c(operation_values, "")
      per_colony_values <- c(per_colony_values, "")
      
      # Add honey revenues only if non-zero
      if (honey_spring > 0) {
        metrics <- c(metrics, "Honey Rev. (Spring) ($)")
        operation_values <- c(operation_values, paste("$", format(round(honey_spring), big.mark = ",")))
        per_colony_values <- c(per_colony_values, paste("$", format(round(honey_spring / avg_max_colonies), big.mark = ",")))
      }
      if (honey_summer > 0) {
        metrics <- c(metrics, "Honey Rev. (Summer) ($)")
        operation_values <- c(operation_values, paste("$", format(round(honey_summer), big.mark = ",")))
        per_colony_values <- c(per_colony_values, paste("$", format(round(honey_summer / avg_max_colonies), big.mark = ",")))
      }
      if (honey_fall > 0) {
        metrics <- c(metrics, "Honey Rev. (Fall) ($)")
        operation_values <- c(operation_values, paste("$", format(round(honey_fall), big.mark = ",")))
        per_colony_values <- c(per_colony_values, paste("$", format(round(honey_fall / avg_max_colonies), big.mark = ",")))
      }
      if (honey_winter > 0) {
        metrics <- c(metrics, "Honey Rev. (Winter) ($)")
        operation_values <- c(operation_values, paste("$", format(round(honey_winter), big.mark = ",")))
        per_colony_values <- c(per_colony_values, paste("$", format(round(honey_winter / avg_max_colonies), big.mark = ",")))
      }
      
      # Add pollination revenues only if non-zero
      if (poll_spring > 0) {
        metrics <- c(metrics, "Poll Rev (Spring) ($)")
        operation_values <- c(operation_values, paste("$", format(round(poll_spring), big.mark = ",")))
        per_colony_values <- c(per_colony_values, paste("$", format(round(poll_spring / avg_max_colonies), big.mark = ",")))
      }
      if (poll_summer > 0) {
        metrics <- c(metrics, "Poll Rev. (Summer) ($)")
        operation_values <- c(operation_values, paste("$", format(round(poll_summer), big.mark = ",")))
        per_colony_values <- c(per_colony_values, paste("$", format(round(poll_summer / avg_max_colonies), big.mark = ",")))
      }
      if (poll_fall > 0) {
        metrics <- c(metrics, "Poll Rev. (Fall) ($)")
        operation_values <- c(operation_values, paste("$", format(round(poll_fall), big.mark = ",")))
        per_colony_values <- c(per_colony_values, paste("$", format(round(poll_fall / avg_max_colonies), big.mark = ",")))
      }
      if (poll_winter > 0) {
        metrics <- c(metrics, "Poll Rev. (Winter) ($)")
        operation_values <- c(operation_values, paste("$", format(round(poll_winter), big.mark = ",")))
        per_colony_values <- c(per_colony_values, paste("$", format(round(poll_winter / avg_max_colonies), big.mark = ",")))
      }
      
      # Add total revenue
      metrics <- c(metrics, "Total Revenues ($)")
      operation_values <- c(operation_values, paste("$", format(round(total_revenue), big.mark = ",")))
      per_colony_values <- c(per_colony_values, paste("$", format(round(total_revenue / avg_max_colonies), big.mark = ",")))
      
      # Add profits section header
      metrics <- c(metrics, "--- PROFIT ---")
      operation_values <- c(operation_values, "")
      per_colony_values <- c(per_colony_values, "")
      
      # Add present value totals
      metrics <- c(metrics, "Profit ($)")
      operation_values <- c(
        operation_values,
        paste("$", format(round(total_pv_profit), big.mark = ","))
      )
      per_colony_values <- c(
        per_colony_values,
        paste("$", format(round(total_pv_profit / avg_max_colonies), big.mark = ","))
      )
      
      financial_stats <- data.frame(
        Metric = metrics,
        Operation = operation_values,
        `Per Colony` = per_colony_values,
        check.names = FALSE
      )
      
      profit_row <- which(financial_stats$Metric == "Profit ($)")
      if (length(profit_row) > 0) {
        highlight <- function(x) {
          paste0("<span style='color:#8B0000; font-weight:700;'>", x, "</span>")
        }
        financial_stats$Metric[profit_row] <- highlight(financial_stats$Metric[profit_row])
        financial_stats$Operation[profit_row] <- highlight(financial_stats$Operation[profit_row])
        financial_stats$`Per Colony`[profit_row] <- highlight(financial_stats$`Per Colony`[profit_row])
      }
      
      return(financial_stats)
    },
    striped = TRUE,
    bordered = TRUE,
    sanitize.text.function = function(x) x
  )
  
  # Management preset strategies
  observeEvent(input$preset_zero, {
    # Set all to zero: no management actions
    updateSliderInput(session, "cull_spring", value = 0)
    updateSliderInput(session, "cull_summer", value = 0)
    updateSliderInput(session, "cull_fall", value = 0)
    updateSliderInput(session, "cull_winter", value = 0)
    
    updateSliderInput(session, "adjust_spring", value = 0) # No adjustment
    updateSliderInput(session, "adjust_summer", value = 0) # No adjustment
    updateSliderInput(session, "adjust_fall", value = 0) # No adjustment
    updateSliderInput(session, "adjust_winter", value = 0) # No adjustment
  })
  
  observeEvent(input$preset_growth, {
    # Maximum growth: no culling, maximum splitting all seasons
    updateSliderInput(session, "cull_spring", value = 0)
    updateSliderInput(session, "cull_summer", value = 0)
    updateSliderInput(session, "cull_fall", value = 0)
    updateSliderInput(session, "cull_winter", value = 0)
    
    updateSliderInput(session, "adjust_spring", value = 100) # Maximum splitting
    updateSliderInput(session, "adjust_summer", value = 100) # Maximum splitting
    updateSliderInput(session, "adjust_fall", value = 100) # Maximum splitting
    updateSliderInput(session, "adjust_winter", value = 100) # Maximum splitting
  })
  
  observeEvent(input$preset_average, {
    # US average 2015-2023 management guess.
    updateSliderInput(session, "cull_spring", value = 0)
    updateSliderInput(session, "cull_summer", value = 0)
    updateSliderInput(session, "cull_fall", value = 10) # Fall culling of weak colonies
    updateSliderInput(session, "cull_winter", value = 0)
    
    # Split rates from table in Winter -> Spring -> Summer -> Fall order:
    # 16%, 23%, 7%, 6%.
    updateSliderInput(session, "adjust_spring", value = 23)
    updateSliderInput(session, "adjust_summer", value = 7)
    updateSliderInput(session, "adjust_fall", value = 6)
    updateSliderInput(session, "adjust_winter", value = 16)
  })
  
  observeEvent(input$reset_allocation, {
    default_fpc <- (input$initial_frames %||% 6000) / max(1, (input$initial_colonies %||% 1000))
    
    updateSliderInput(session, "col_share_loc1_spring", value = 50)
    updateSliderInput(session, "col_share_loc2_spring", value = 30)
    updateSliderInput(session, "col_share_loc3_spring", value = 20)
    updateSliderInput(session, "col_share_loc1_summer", value = 50)
    updateSliderInput(session, "col_share_loc2_summer", value = 30)
    updateSliderInput(session, "col_share_loc3_summer", value = 20)
    updateSliderInput(session, "col_share_loc1_fall", value = 50)
    updateSliderInput(session, "col_share_loc2_fall", value = 50)
    updateSliderInput(session, "col_share_loc1_winter", value = 50)
    updateSliderInput(session, "col_share_loc2_winter", value = 50)
    
    fpc_ids <- c(
      "fpc_target_loc1_spring", "fpc_target_loc1_summer",
      "fpc_target_loc1_fall", "fpc_target_loc1_winter"
    )
    for (id in fpc_ids) {
      updateSliderInput(session, id, value = round(default_fpc, 2))
    }
    updateCheckboxInput(session, "fpc_equal_spring", value = FALSE)
    updateCheckboxInput(session, "fpc_equal_summer", value = FALSE)
    updateCheckboxInput(session, "fpc_equal_fall", value = FALSE)
    updateCheckboxInput(session, "fpc_equal_winter", value = FALSE)
  })
  
  # Joint optimization for management + allocations:
  # stage 1 global search (DEoptim), stage 2 local refinement (L-BFGS-B).
  observeEvent(input$optimize_mgmt, {
    if (!requireNamespace("DEoptim", quietly = TRUE)) {
      showNotification(
        "Package 'DEoptim' is required for optimization. Please run install.packages('DEoptim').",
        type = "error",
        duration = 8
      )
      return(NULL)
    }
    
    showNotification("Optimizing all controls (global + local)...", type = "message", duration = 3)
    
    # Fixed model inputs reused across objective evaluations
    params <- collect_primary_params(input)
    schedule <- create_simple_schedule(1, input$start_season %||% "Spring")
    locations <- create_location_parameters(schedule, params)
    params_loc2 <- collect_location_params(input, params, "loc2")
    params_loc3 <- collect_location_params(input, params, "loc3")
    locations_loc2 <- create_location_parameters_loc2(schedule, params_loc2)
    locations_loc3 <- create_location_parameters_loc3(schedule, params_loc3)
    location_tables <- list(loc1 = locations, loc2 = locations_loc2, loc3 = locations_loc3)
    
    annual_rate <- (input$annual_discount_rate %||% 3) / 100
    quarterly_rate <- (1 + annual_rate)^(1 / 4) - 1
    
    # Debug counters to trace warnings raised during optimization calls.
    opt_eval_count <- 0L
    opt_warning_count <- 0L
    opt_warning_examples <- character(0)
    
    seasons <- tolower(SEASON_KEYS)
    mgmt_names <- c(
      paste0("cull_", seasons),
      paste0("adjust_", seasons)
    )
    alloc_names <- c(
      # Colony shares by season/location
      "col_share_loc1_spring", "col_share_loc2_spring",
      "col_share_loc3_spring",
      "col_share_loc1_summer", "col_share_loc2_summer",
      "col_share_loc3_summer",
      "col_share_loc1_fall", "col_share_loc2_fall",
      "col_share_loc1_winter", "col_share_loc2_winter",
      # Frames/colony targets on location 1
      "fpc_target_loc1_spring", "fpc_target_loc1_summer",
      "fpc_target_loc1_fall", "fpc_target_loc1_winter"
    )
    decision_names <- c(mgmt_names, alloc_names)
    
    get_current_value <- function(name) {
      value <- input[[name]]
      if (is.null(value) || !is.finite(value)) {
        return(0)
      }
      as.numeric(value)
    }
    
    x0 <- vapply(decision_names, get_current_value, numeric(1))
    
    lower <- vapply(decision_names, function(name) {
      if (startsWith(name, "cull_")) return(0)
      if (startsWith(name, "adjust_")) return(-100)
      if (startsWith(name, "col_share_")) return(0)
      if (startsWith(name, "fpc_target_")) return(0)
      0
    }, numeric(1))
    upper <- vapply(decision_names, function(name) {
      if (startsWith(name, "cull_")) return(100)
      if (startsWith(name, "adjust_")) return(100)
      if (startsWith(name, "col_share_")) return(100)
      if (startsWith(name, "fpc_target_")) return(20)
      100
    }, numeric(1))
    
    clamp <- function(x, lo, hi) pmin(hi, pmax(lo, x))
    
    decode_controls <- function(x) {
      x <- clamp(as.numeric(x), lower, upper)
      names(x) <- decision_names
      
      management <- list(
        cull_spring = x["cull_spring"],
        cull_summer = x["cull_summer"],
        cull_fall = x["cull_fall"],
        cull_winter = x["cull_winter"],
        adjust_spring = x["adjust_spring"],
        adjust_summer = x["adjust_summer"],
        adjust_fall = x["adjust_fall"],
        adjust_winter = x["adjust_winter"],
        P_split = as.numeric(input$P_split %||% 25),
        P_merge = as.numeric(input$P_merge %||% 10),
        P_cull = as.numeric(input$P_cull %||% 10)
      )
      
      allocation <- collect_allocation_params(input)
      for (season_key in SEASON_KEYS) {
        include_loc3 <- season_has_loc3(season_key)
        s1 <- clamp(x[paste0("col_share_loc1_", season_key)], 0, 100)
        s2 <- clamp(x[paste0("col_share_loc2_", season_key)], 0, 100)
        s3 <- if (include_loc3) clamp(x[paste0("col_share_loc3_", season_key)], 0, 100) else 0
        share_sum <- s1 + s2 + s3
        if (!is.finite(share_sum) || share_sum <= 0) {
          if (include_loc3) {
            s1 <- 50
            s2 <- 30
            s3 <- 20
          } else {
            s1 <- 50
            s2 <- 50
            s3 <- 0
          }
        } else {
          if (include_loc3) {
            s1 <- 100 * s1 / share_sum
            s2 <- 100 * s2 / share_sum
            s3 <- 100 - s1 - s2
          } else {
            two_sum <- s1 + s2
            if (!is.finite(two_sum) || two_sum <= 0) {
              s1 <- 50
              s2 <- 50
            } else {
              s1 <- 100 * s1 / two_sum
              s2 <- 100 - s1
            }
            s3 <- 0
          }
        }
        allocation[[paste0("col_share_loc1_", season_key)]] <- s1
        allocation[[paste0("col_share_loc2_", season_key)]] <- s2
        allocation[[paste0("col_share_loc3_", season_key)]] <- s3
      }
      
      fpc_names <- c(
        "fpc_target_loc1_spring", "fpc_target_loc1_summer",
        "fpc_target_loc1_fall", "fpc_target_loc1_winter"
      )
      for (name in fpc_names) {
        allocation[[name]] <- clamp(x[name], 0, 20)
      }
      
      list(management = management, allocation = allocation)
    }
    
    objective_fn <- function(x) {
      opt_eval_count <<- opt_eval_count + 1L
      controls <- decode_controls(x)
      sim <- withCallingHandlers(
        tryCatch(
          run_beekeeping_simulation(
            locations = locations,
            initial_colonies = input$initial_colonies,
            initial_frames = input$initial_frames,
            P_feed = input$P_feed,
            management_params = controls$management,
            locations_loc2 = locations_loc2,
            allocation_params = controls$allocation,
            location_tables = location_tables
          ),
          error = function(e) NULL
        ),
        warning = function(w) {
          msg <- conditionMessage(w)
          opt_warning_count <<- opt_warning_count + 1L
          if (length(opt_warning_examples) < 8) {
            opt_warning_examples <<- c(opt_warning_examples, paste0("eval ", opt_eval_count, ": ", msg))
          }
          message("[OPT DEBUG] warning during simulation at eval ", opt_eval_count, ": ", msg)
          invokeRestart("muffleWarning")
        }
      )
      if (is.null(sim)) {
        return(1e12)
      }
      
      sim_df <- withCallingHandlers(
        tryCatch(results_to_dataframe(sim), error = function(e) NULL),
        warning = function(w) {
          msg <- conditionMessage(w)
          opt_warning_count <<- opt_warning_count + 1L
          if (length(opt_warning_examples) < 8) {
            opt_warning_examples <<- c(opt_warning_examples, paste0("eval ", opt_eval_count, ": ", msg))
          }
          message("[OPT DEBUG] warning during dataframe conversion at eval ", opt_eval_count, ": ", msg)
          invokeRestart("muffleWarning")
        }
      )
      if (is.null(sim_df) || nrow(sim_df) == 0) {
        return(1e12)
      }
      
      sim_df$discount_factor <- sapply(seq_len(nrow(sim_df)), function(i) {
        1 / (1 + quarterly_rate)^(i - 1)
      })
      sim_df$pv_profit <- sim_df$profit * sim_df$discount_factor
      
      total_profit <- sum(sim_df$pv_profit, na.rm = TRUE)
      if (!is.finite(total_profit)) {
        return(1e12)
      }
      
      # DEoptim and optim are minimizers; negate profit.
      -total_profit
    }
    
    de_np <- max(80, 10 * length(decision_names))
    de_result <- DEoptim::DEoptim(
      fn = objective_fn,
      lower = lower,
      upper = upper,
      control = DEoptim::DEoptim.control(
        itermax = 150,
        NP = de_np,
        trace = FALSE
      )
    )
    
    local_result <- optim(
      par = de_result$optim$bestmem,
      fn = objective_fn,
      method = "L-BFGS-B",
      lower = lower,
      upper = upper,
      control = list(maxit = 200)
    )
    
    best_controls <- decode_controls(local_result$par)
    
    # Push optimized values back to UI sliders
    updateSliderInput(session, "cull_spring", value = round(best_controls$management$cull_spring, 1))
    updateSliderInput(session, "cull_summer", value = round(best_controls$management$cull_summer, 1))
    updateSliderInput(session, "cull_fall", value = round(best_controls$management$cull_fall, 1))
    updateSliderInput(session, "cull_winter", value = round(best_controls$management$cull_winter, 1))
    updateSliderInput(session, "adjust_spring", value = round(best_controls$management$adjust_spring, 1))
    updateSliderInput(session, "adjust_summer", value = round(best_controls$management$adjust_summer, 1))
    updateSliderInput(session, "adjust_fall", value = round(best_controls$management$adjust_fall, 1))
    updateSliderInput(session, "adjust_winter", value = round(best_controls$management$adjust_winter, 1))
    
    share_to_update <- c(
      "col_share_loc1_spring", "col_share_loc2_spring",
      "col_share_loc3_spring",
      "col_share_loc1_summer", "col_share_loc2_summer", "col_share_loc3_summer",
      "col_share_loc1_fall", "col_share_loc2_fall",
      "col_share_loc1_winter", "col_share_loc2_winter"
    )
    for (name in share_to_update) {
      updateSliderInput(session, name, value = round(best_controls$allocation[[name]], 1))
    }
    fpc_to_update <- c(
      "fpc_target_loc1_spring", "fpc_target_loc1_summer",
      "fpc_target_loc1_fall", "fpc_target_loc1_winter"
    )
    for (name in fpc_to_update) {
      updateSliderInput(session, name, value = round(best_controls$allocation[[name]], 2))
    }
    
    base_profit <- -objective_fn(x0)
    best_profit <- -objective_fn(local_result$par)
    
    showNotification(
      paste0(
        "Optimization complete. Profit over full year: $",
        format(round(best_profit), big.mark = ","),
        " (from $", format(round(base_profit), big.mark = ","), ")."
      ),
      type = "message",
      duration = 8
    )
    
    message("[OPT DEBUG] Completed ", opt_eval_count, " objective evaluations.")
    if (opt_warning_count > 0) {
      message("[OPT DEBUG] Total warnings captured during optimization: ", opt_warning_count)
      for (example_msg in unique(opt_warning_examples)) {
        message("[OPT DEBUG] Example warning: ", example_msg)
      }
      showNotification(
        paste0(
          "Optimization finished with ", opt_warning_count,
          " captured warnings (see R console logs prefixed with [OPT DEBUG])."
        ),
        type = "warning",
        duration = 8
      )
    } else {
      message("[OPT DEBUG] No warnings captured during optimization objective calls.")
    }
  })



  # Download handler for saving current settings
  # Save settings to CSV
  output$save_settings <- downloadHandler(
    filename = function() {
      paste("beekeeping_settings_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      # Collect all current parameter values
      settings_data <- data.frame(
        Parameter = c(
          # Simulation setup
          "annual_discount_rate", "initial_colonies", "initial_frames",
          # Seasonal biological parameters
          "alpha_spring", "alpha_summer", "alpha_fall", "alpha_winter",
          "delta_spring", "delta_summer", "delta_fall", "delta_winter",
          "gamma_spring", "gamma_summer", "gamma_fall", "gamma_winter",
          "omega_spring", "omega_summer", "omega_fall", "omega_winter",
          "theta_spring", "theta_summer", "theta_fall", "theta_winter",
          # Economic parameters
          "Cost", "P_feed", "P_split", "P_merge", "P_cull",
          # Management parameters
          "cull_spring", "cull_summer", "cull_fall", "cull_winter",
          "adjust_spring", "adjust_summer", "adjust_fall", "adjust_winter",
          # Seasonal forage production parameters
          "A_spring", "A_summer", "A_fall", "A_winter",
          "B_spring", "B_summer", "B_fall", "B_winter",
          # Seasonal crop production parameters
          "D_spring", "D_summer", "D_fall", "D_winter",
          "E_spring", "E_summer", "E_fall", "E_winter",
          # Seasonal prices
          "Ph_spring", "Ph_summer", "Ph_fall", "Ph_winter",
          "Pc_spring", "Pc_summer", "Pc_fall", "Pc_winter",
          # Fixed model parameter
          "t_dur"
        ),
        Value = c(
          # Simulation setup
          input$annual_discount_rate %||% 3, input$initial_colonies %||% 1000, input$initial_frames %||% 6000,
          # Seasonal biological parameters
          input$alpha_spring %||% 0.50, input$alpha_summer %||% 0.40, input$alpha_fall %||% -0.40, input$alpha_winter %||% 0.30,
          input$delta_spring %||% 0.10, input$delta_summer %||% 0.12, input$delta_fall %||% 0.13, input$delta_winter %||% 0.14,
          input$gamma_spring %||% 0.5, input$gamma_summer %||% 0.5, input$gamma_fall %||% 0.5, input$gamma_winter %||% 0.5,
          input$omega_spring %||% 0.5, input$omega_summer %||% 0.5, input$omega_fall %||% 0.5, input$omega_winter %||% 0.5,
          input$theta_spring %||% 0.3, input$theta_summer %||% 0.3, input$theta_fall %||% 0.3, input$theta_winter %||% 0.3,
          # Economic parameters
          input$Cost %||% 50, input$P_feed %||% 0.02, input$P_split %||% 25, input$P_merge %||% 10, input$P_cull %||% 10,
          # Management parameters
          input$cull_spring %||% 0, input$cull_summer %||% 0, input$cull_fall %||% 0, input$cull_winter %||% 5,
          input$adjust_spring %||% 23, input$adjust_summer %||% 7, input$adjust_fall %||% 6, input$adjust_winter %||% 16,
          # Seasonal forage production parameters
          input$A_spring %||% 100, input$A_summer %||% 100, input$A_fall %||% 0, input$A_winter %||% 0,
          input$B_spring %||% 2000, input$B_summer %||% 2000, input$B_fall %||% 2000, input$B_winter %||% 2000,
          # Seasonal crop production parameters
          input$D_spring %||% 3, input$D_summer %||% 15, input$D_fall %||% 0, input$D_winter %||% 0,
          input$E_spring %||% 2000, input$E_summer %||% 2000, input$E_fall %||% 2000, input$E_winter %||% 2000,
          # Seasonal prices
          input$Ph_spring %||% 3.0, input$Ph_summer %||% 3.0, input$Ph_fall %||% 3.0, input$Ph_winter %||% 0,
          input$Pc_spring %||% 100, input$Pc_summer %||% 100, input$Pc_fall %||% 0, input$Pc_winter %||% 0,
          # Fixed
          13
        ),
        stringsAsFactors = FALSE
      )
      
      # Metadata: Units, min, max, default for each parameter
      units <- c(
        # Simulation setup
        "%", "colonies", "frames",
        # Seasonal biological parameters - alpha (growth)
        "Spring growth (frames/col/week)", "Summer growth (frames/col/week)", "Fall growth (frames/col/week)", "Winter growth (frames/col/week)",
        # Seasonal biological parameters - delta (loss)
        "Spring loss rate (%/season)", "Summer loss rate (%/season)", "Fall loss rate (%/season)", "Winter loss rate (%/season)",
        # Seasonal biological parameters - gamma (consumption)
        "Spring feed consumption (lbs/frame/week)", "Summer feed consumption (lbs/frame/week)", "Fall feed consumption (lbs/frame/week)", "Winter feed consumption (lbs/frame/week)",
        # Seasonal biological parameters - omega (base foraging)
        "Spring omega (base foraging)", "Summer omega (base foraging)", "Fall omega (base foraging)", "Winter omega (base foraging)",
        # Seasonal biological parameters - theta (colony strength)
        "Spring theta (colony strength-foragers)", "Summer theta (colony strength-foragers)", "Fall theta (colony strength-foragers)", "Winter theta (colony strength-foragers)",
        # Economic parameters
        "Maintenance cost ($/col/year)", "Feed price ($/unit)", "Splitting cost ($/op)", "Merging cost ($/op)", "Culling cost ($/op)",
        # Management parameters
        "Spring culling rate (%)", "Summer culling rate (%)", "Fall culling rate (%)", "Winter culling rate (%)",
        "Spring adjustment rate (%)", "Summer adjustment rate (%)", "Fall adjustment rate (%)", "Winter adjustment rate (%)",
        # Seasonal forage production parameters - A (max value)
        "Spring forage max value", "Summer forage max value", "Fall forage max value", "Winter forage max value",
        # Seasonal forage production parameters - B (midpoint)
        "Spring forage midpoint", "Summer forage midpoint", "Fall forage midpoint", "Winter forage midpoint",
        # Seasonal crop production parameters - D (max value)
        "Spring crop max value", "Summer crop max value", "Fall crop max value", "Winter crop max value",
        # Seasonal crop production parameters - E (midpoint)
        "Spring crop midpoint", "Summer crop midpoint", "Fall crop midpoint", "Winter crop midpoint",
        # Seasonal prices
        "Spring honey price ($/lbs)", "Summer honey price ($/lbs)", "Fall honey price ($/lbs)", "Winter honey price ($/lbs)",
        "Spring crop price ($/lbs)", "Summer crop price ($/lbs)", "Fall crop price ($/lbs)", "Winter crop price ($/lbs)",
        # Fixed model parameter
        "Season duration (weeks)"
      )
      
      mins <- c(
        # Simulation
        0, 100, 500,
        # Bio
        rep(-0.5, 4), c(0.01, 0.01, 0.01, 0.01), rep(0, 4), c(0.1, 0.1, 0.1, 0.1), c(0.1, 0.1, 0.1, 0.1),
        # Econ
        0, 0, 0, 0, 0,
        # Mgmt
        0, 0, 0, 0, -50, -50, -50, -50,
        # Forage
        0, 0, 0, 0, 0, 0, 0, 0,
        # Crop
        0, 0, 0, 0, 0, 0, 0, 0,
        # Prices
        0, 0, 0, 0, 0, 0, 0, 0,
        # Fixed
        13
      )
      
      maxs <- c(
        # Simulation
        15, 5000, 30000,
        # Bio
        rep(1.0, 4), c(0.2, 0.2, 0.2, 0.2), rep(5.0, 4), c(1.0, 1.0, 1.0, 1.0), c(0.5, 0.5, 0.5, 0.5),
        # Econ
        400, 10, 100, 100, 100,
        # Mgmt
        100, 100, 100, 100, 50, 50, 50, 50,
        # Forage
        1000, 1000, 10000, 10000, 100000, 100000, 100000, 100000,
        # Crop
        100, 100, 20, 20, 100000, 10000, 100000, 100000,
        # Prices
        20.0, 20.0, 10.0, 10.0, 100, 100, 100, 100,
        # Fixed
        13
      )
      
      defaults <- c(
        # Simulation
        3, 3000, 18000,
        # Spring bio
        0.4, 0.1, 0, 0, 0.15, 0.05, 0.15, 0.2, 0.5, 0.5, 0.5, 0.19, 0.5, 0.5, 0.5, 0.5, 0.3, 0.3, 0.3, 0.3,
        # Econ
        200, 0.2, 25, 10, 10,
        # Mgmt (UI defaults inferred from code comments/usage)
        NA, NA, NA, NA, NA, NA, NA, NA,
        # Forage
        70, 20, 0, 0, 2100, 22000, 2000, 2000,
        # Crop
        4, 17, 0, 15.5, 32000, 2700, 2000, 83000,
        # Prices
        3.0, 2.0, 3.0, 0.0, 2, 2, 0, 2,
        # Fixed
        13
      )
      
      # Attach metadata columns
      settings_data$Unit <- units
      settings_data$Min <- mins
      settings_data$Max <- maxs
      settings_data$Default <- defaults
      
      # Add timestamp and description
      settings_data$Timestamp <- Sys.time()
      settings_data$Description <- c(
        # Simulation setup
        "Annual discount rate (%)", "Initial number of colonies", "Initial number of frames",
        # Seasonal biological parameters - alpha (growth)
        "Spring growth (frames/col/week)", "Summer growth (frames/col/week)", "Fall growth (frames/col/week)", "Winter growth (frames/col/week)",
        # Seasonal biological parameters - delta (loss)
        "Spring loss rate (%/season)", "Summer loss rate (%/season)", "Fall loss rate (%/season)", "Winter loss rate (%/season)",
        # Seasonal biological parameters - gamma (consumption)
        "Spring feed consumption (lbs/frame/week)", "Summer feed consumption (lbs/frame/week)", "Fall feed consumption (lbs/frame/week)", "Winter feed consumption (lbs/frame/week)",
        # Seasonal biological parameters - omega (base foraging)
        "Spring omega (base foraging)", "Summer omega (base foraging)", "Fall omega (base foraging)", "Winter omega (base foraging)",
        # Seasonal biological parameters - theta (colony strength)
        "Spring theta (colony strength-foragers)", "Summer theta (colony strength-foragers)", "Fall theta (colony strength-foragers)", "Winter theta (colony strength-foragers)",
        # Economic parameters
        "Maintenance cost ($/col/year)", "Feed price ($/unit)", "Splitting cost ($/op)", "Merging cost ($/op)", "Culling cost ($/op)",
        # Management parameters
        "Spring culling rate (%)", "Summer culling rate (%)", "Fall culling rate (%)", "Winter culling rate (%)",
        "Spring adjustment rate (%)", "Summer adjustment rate (%)", "Fall adjustment rate (%)", "Winter adjustment rate (%)",
        # Seasonal forage production parameters - A (max value)
        "Spring forage max value", "Summer forage max value", "Fall forage max value", "Winter forage max value",
        # Seasonal forage production parameters - B (midpoint)
        "Spring forage midpoint", "Summer forage midpoint", "Fall forage midpoint", "Winter forage midpoint",
        # Seasonal crop production parameters - D (max value)
        "Spring crop max value", "Summer crop max value", "Fall crop max value", "Winter crop max value",
        # Seasonal crop production parameters - E (midpoint)
        "Spring crop midpoint", "Summer crop midpoint", "Fall crop midpoint", "Winter crop midpoint",
        # Seasonal prices
        "Spring honey price ($/lbs)", "Summer honey price ($/lbs)", "Fall honey price ($/lbs)", "Winter honey price ($/lbs)",
        "Spring crop price ($/lbs)", "Summer crop price ($/lbs)", "Fall crop price ($/lbs)", "Winter crop price ($/lbs)",
        # Fixed model parameter
        "Season duration (weeks)"
      )
      
      write.csv(settings_data, file, row.names = FALSE)
    }
  )
  
  # Export simulation results to CSV
  output$export_results <- downloadHandler(
    filename = function() {
      paste("beekeeping_simulation_results_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      results_df <- simulation_results()
      write.csv(results_df, file, row.names = FALSE)
    }
  )
  
  # Production function plots for seasonal parameters
  # Shared function to create forage production plot
  create_forage_plot <- function(A, B, I, season_name, season_color, location_tag = "loc1") {
    # Get current simulation data to determine colony range
    results_df <- simulation_results()
    if (nrow(results_df) > 0) {
      avg_frames_per_colony <- mean(results_df$frames_per_colony, na.rm = TRUE)
      max_colonies <- max(results_df$colonies_end, na.rm = TRUE)
      avg_colonies <- mean(results_df$colonies_end, na.rm = TRUE)
      
      # Get forager counts for this specific season by year
      loc_foragers_col <- paste0("total_foragers_", location_tag)
      if (loc_foragers_col %in% names(results_df)) {
        season_data <- results_df %>%
          filter(season == season_name) %>%
          transmute(year = year, total_foragers = .data[[loc_foragers_col]]) %>%
          distinct()
      } else {
        season_data <- results_df %>%
          filter(season == season_name) %>%
          select(year, total_foragers) %>%
          distinct()
      }
    } else {
      avg_frames_per_colony <- 6
      max_colonies <- 1000
      avg_colonies <- 500
      season_data <- data.frame(year = 1, total_foragers = 500 * 6 * 0.5)
    }
    
    # Create range of forager numbers
    max_foragers <- max_colonies * avg_frames_per_colony * 0.5
    foragers <- seq(0, max_foragers * 1.5, length.out = 200)
    
    # Calculate forage production functions
    forage_marginal <- sapply(foragers, function(f) calculate_forage_collection(f, A, B, I))
    forage_total <- sapply(foragers, function(f) calculate_forage_integral(f, A, B, I))
    
    # Create data frame for plotting
    plot_data <- data.frame(
      Foragers = rep(foragers, 2),
      Production = c(forage_marginal, forage_total),
      Type = rep(c("Marginal", "Total"), each = length(foragers))
    )
    
    # Create plot with facets for Marginal and Total
    p <- ggplot(plot_data, aes(x = Foragers, y = Production)) +
      geom_line(size = 1.2, color = season_color)
    
    # Add vertical lines for each year's forager count with viridis gradient
    if (nrow(season_data) > 0) {
      n_years <- max(season_data$year)
      viridis_colors <- viridis::viridis(n_years)
      for (i in 1:nrow(season_data)) {
        yr <- season_data$year[i]
        forager_count <- season_data$total_foragers[i]
        p <- p + geom_vline(
          xintercept = forager_count,
          color = viridis_colors[yr],
          linetype = "solid",
          size = 0.6,
          alpha = 0.8
        )
      }
    }
    
    p <- p +
      facet_wrap(~Type, scales = "free_y", ncol = 1) +
      scale_x_continuous(labels = scales::comma_format(), n.breaks = 5, expand = c(0.02, 0)) +
      scale_y_continuous(labels = scales::comma_format(), n.breaks = 4, expand = c(0.02, 0)) +
      labs(
        x = "Number of Foragers", y = "Forage Production (lbs)",
        title = paste(season_name, "Forage Collection")
      ) +
      theme_minimal() +
      theme(
        legend.position = "none",
        text = element_text(size = 10, color = "black"),
        plot.title = element_text(size = 10, hjust = 0.5, face = "bold", color = "black"),
        axis.title = element_text(size = 9, face = "bold", color = "black"),
        axis.text = element_text(size = 9, color = "black"),
        axis.text.x = element_text(angle = 0, hjust = 0.5, color = "black", size = 9),
        axis.text.y = element_text(hjust = 1, color = "black", size = 9),
        axis.ticks = element_line(color = "gray30", size = 0.5),
        axis.ticks.length = unit(0.15, "cm"),
        strip.text = element_text(size = 9, face = "bold", color = "black"),
        panel.grid.major = element_line(color = "gray85", size = 0.3),
        panel.grid.minor = element_blank()
      )
    
    return(p)
  }
  
  # Shared function to create crop production plot
  create_crop_plot <- function(D, E, G, season_name, season_color, location_tag = "loc1") {
    # Get current simulation data to determine colony range
    results_df <- simulation_results()
    if (nrow(results_df) > 0) {
      avg_frames_per_colony <- mean(results_df$frames_per_colony, na.rm = TRUE)
      max_colonies <- max(results_df$colonies_end, na.rm = TRUE)
      avg_colonies <- mean(results_df$colonies_end, na.rm = TRUE)
      
      # Get forager counts for this specific season by year
      loc_foragers_col <- paste0("total_foragers_", location_tag)
      if (loc_foragers_col %in% names(results_df)) {
        season_data <- results_df %>%
          filter(season == season_name) %>%
          transmute(year = year, total_foragers = .data[[loc_foragers_col]]) %>%
          distinct()
      } else {
        season_data <- results_df %>%
          filter(season == season_name) %>%
          select(year, total_foragers) %>%
          distinct()
      }
    } else {
      avg_frames_per_colony <- 6
      max_colonies <- 1000
      avg_colonies <- 500
      season_data <- data.frame(year = 1, total_foragers = 500 * 6 * 0.5)
    }
    
    # Create range of forager numbers
    max_foragers <- max_colonies * avg_frames_per_colony * 0.5
    foragers <- seq(0, max_foragers * 1.5, length.out = 200)
    
    # Calculate crop production functions
    crop_marginal <- sapply(foragers, function(f) calculate_crop_yield(f, D, E, G))
    crop_total <- sapply(foragers, function(f) calculate_crop_integral(f, D, E, G))
    
    # Create data frame for plotting
    plot_data <- data.frame(
      Foragers = rep(foragers, 2),
      Production = c(crop_marginal, crop_total),
      Type = rep(c("Marginal", "Total"), each = length(foragers))
    )
    
    # Create plot with facets for Marginal and Total
    p <- ggplot(plot_data, aes(x = Foragers, y = Production)) +
      geom_line(size = 1.2, color = season_color)
    
    # Add vertical lines for each year's forager count with viridis gradient
    if (nrow(season_data) > 0) {
      n_years <- max(season_data$year)
      viridis_colors <- viridis::viridis(n_years)
      for (i in 1:nrow(season_data)) {
        yr <- season_data$year[i]
        forager_count <- season_data$total_foragers[i]
        p <- p + geom_vline(
          xintercept = forager_count,
          color = viridis_colors[yr],
          linetype = "solid",
          size = 0.6,
          alpha = 0.8
        )
      }
    }
    
    p <- p +
      facet_wrap(~Type, scales = "free_y", ncol = 1) +
      scale_x_continuous(labels = scales::comma_format(), n.breaks = 5, expand = c(0.02, 0)) +
      scale_y_continuous(labels = scales::comma_format(), n.breaks = 4, expand = c(0.02, 0)) +
      labs(
        x = "Number of Foragers", y = "Crop Production (lbs)",
        title = paste(season_name, "Crop Pollination")
      ) +
      theme_minimal() +
      theme(
        legend.position = "none",
        text = element_text(size = 10, color = "black"),
        plot.title = element_text(size = 10, hjust = 0.5, face = "bold", color = "black"),
        axis.title = element_text(size = 9, face = "bold", color = "black"),
        axis.text = element_text(size = 9, color = "black"),
        axis.text.x = element_text(angle = 0, hjust = 0.5, color = "black", size = 9),
        axis.text.y = element_text(hjust = 1, color = "black", size = 9),
        axis.ticks = element_line(color = "gray30", size = 0.5),
        axis.ticks.length = unit(0.15, "cm"),
        strip.text = element_text(size = 9, face = "bold", color = "black"),
        panel.grid.major = element_line(color = "gray85", size = 0.3),
        panel.grid.minor = element_blank()
      )
    
    return(p)
  }
  
  season_colors <- c(spring = "#2E8B57", summer = "#FFD700", fall = "#FF8C00", winter = "#4682B4")

  get_loc_input <- function(base_name, season_key, location_tag, default_value) {
    input[[season_param_id(base_name, season_key, location_tag)]] %||% default_value
  }
  
  season_duration_weeks <- 13

  for (season_key in SEASON_KEYS) {
    local({
      sk <- season_key
      season_name <- SEASON_LABELS[[sk]]
      season_color <- season_colors[[sk]]

      for (loc_tag in SEASON_ACTIVE_LOCATIONS[[sk]]) {
        local({
          lk <- loc_tag
          alpha_indicator_id <- paste0("alpha_full_season_", sk, "_", lk)
          forage_plot_id <- paste0(sk, "_", lk, "_forage_plot")
          crop_plot_id <- paste0(sk, "_", lk, "_crop_plot")
          
          output[[alpha_indicator_id]] <- renderText({
            alpha_val <- suppressWarnings(as.numeric(
              get_loc_input("alpha", sk, lk, season_value("alpha", sk))
            ))
            if (!is.finite(alpha_val)) {
              alpha_val <- season_value("alpha", sk)
            }
            full_season_growth <- alpha_val * season_duration_weeks
            paste0(
              "Full season growth: ",
              format(round(full_season_growth, 2), nsmall = 2),
              " frames/col (",
              format(round(alpha_val, 2), nsmall = 2),
              " x ", season_duration_weeks, " weeks)"
            )
          })

          output[[forage_plot_id]] <- renderPlot({
            create_forage_plot(
              get_loc_input("A", sk, lk, 0),
              get_loc_input("B", sk, lk, 2000),
              get_loc_input("I", sk, lk, -1),
              season_name,
              season_color,
              location_tag = lk
            )
          })

          output[[crop_plot_id]] <- renderPlot({
            create_crop_plot(
              get_loc_input("D", sk, lk, 0),
              get_loc_input("E", sk, lk, 2000),
              get_loc_input("G", sk, lk, -1),
              season_name,
              season_color,
              location_tag = lk
            )
          })
        })
      }
    })
  }
  
  # Forager share plots for each season
  create_forager_share_plot <- function(omega, theta, season_name, season_color, location_tag = "loc1") {
    # Get simulation data for this season's frames per colony by year
    results_df <- simulation_results()
    if (nrow(results_df) > 0) {
      loc_fpc_col <- paste0("frames_per_colony_", location_tag)
      if (loc_fpc_col %in% names(results_df)) {
        season_data <- results_df %>%
          filter(season == season_name) %>%
          transmute(year = year, frames_per_colony = .data[[loc_fpc_col]]) %>%
          distinct()
      } else {
        season_data <- results_df %>%
          filter(season == season_name) %>%
          select(year, frames_per_colony) %>%
          distinct()
      }
    } else {
      season_data <- data.frame(year = 1, frames_per_colony = 6)
    }
    
    frames_per_colony <- seq(0, 20, length.out = 100)
    forager_share <- calculate_forager_share(frames_per_colony, omega, theta)
    
    plot_data <- data.frame(
      frames_per_colony = frames_per_colony,
      forager_share = forager_share
    )
    
    p <- ggplot(plot_data, aes(x = frames_per_colony, y = forager_share)) +
      geom_line(size = 1.5, color = season_color) +
      geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray50", alpha = 0.5)
    
    # Add vertical lines for each year's frames per colony with viridis gradient
    if (nrow(season_data) > 0) {
      n_years <- max(season_data$year)
      viridis_colors <- viridis::viridis(n_years)
      for (i in 1:nrow(season_data)) {
        yr <- season_data$year[i]
        fpc <- season_data$frames_per_colony[i]
        p <- p + geom_vline(
          xintercept = fpc,
          color = viridis_colors[yr],
          linetype = "solid",
          size = 0.8,
          alpha = 0.8
        )
      }
    }
    
    p <- p +
      scale_y_continuous(limits = c(0, 1), labels = scales::percent_format()) +
      labs(
        x = "Frames per Colony",
        y = "Forager Share",
        title = paste(season_name, "Forager Share")
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 11, hjust = 0.5, face = "bold"),
        axis.title = element_text(size = 10),
        axis.text = element_text(size = 9)
      )
    
    return(p)
  }
  
  for (season_key in SEASON_KEYS) {
    local({
      sk <- season_key
      season_name <- SEASON_LABELS[[sk]]
      season_color <- season_colors[[sk]]

      for (loc_tag in SEASON_ACTIVE_LOCATIONS[[sk]]) {
        local({
          lk <- loc_tag
          forager_plot_id <- paste0(sk, "_", lk, "_forager_plot")

          output[[forager_plot_id]] <- renderPlot({
            create_forager_share_plot(
              get_loc_input("omega", sk, lk, 0.5),
              get_loc_input("theta", sk, lk, 0.3),
              season_name,
              season_color,
              location_tag = lk
            )
          })
        })
      }
    })
  }
  
  # Phase space plot removed
  
}

# =============================================================================
# RUN THE APP
# =============================================================================

shinyApp(ui = ui, server = server)
