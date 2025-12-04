# Interactive Beekeeping Model Shiny App - Live Parameter Exploration
# Self-contained app with real-time simulation updates
# Author Antoine Champetier with help from Claude.
# November 2025

library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(tidyr)
library(plotly)
library(scales)

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
create_simple_schedule <- function(n_years = 10) {
  periods <- data.frame(
    period = 1:(n_years * 4),
    year = rep(1:n_years, each = 4),
    season = rep(c("Spring", "Summer", "Fall", "Winter"), n_years),
    t_start = seq(0, (n_years * 4 - 1) * 13, by = 13),
    t_dur = 13,
    stringsAsFactors = FALSE
  )
  periods$t_end <- periods$t_start + periods$t_dur
  return(periods)
}

# Create location parameters for each period
create_location_parameters <- function(schedule, params) {
  locations <- data.frame(
    period = schedule$period,
    location = 1,
    season = schedule$season,
    t_start = schedule$t_start,
    t_dur = schedule$t_dur,
    t_end = schedule$t_end,

    # Location-specific parameters (vary by season)
    Cost = params$Cost,
    Ph = case_when(
      schedule$season == "Spring" ~ params$Ph_spring,
      schedule$season == "Summer" ~ params$Ph_summer,
      schedule$season == "Fall" ~ params$Ph_fall,
      schedule$season == "Winter" ~ params$Ph_winter,
      TRUE ~ params$Ph
    ),
    Pc = case_when(
      schedule$season == "Spring" ~ params$Pc_spring,
      schedule$season == "Summer" ~ params$Pc_summer,
      schedule$season == "Fall" ~ params$Pc_fall,
      schedule$season == "Winter" ~ params$Pc_winter,
      TRUE ~ params$Pc
    ),
    
    # Biological parameters (seasonal variation)
    gamma = case_when(
      schedule$season == "Spring" ~ params$gamma_spring,
      schedule$season == "Summer" ~ params$gamma_summer,
      schedule$season == "Fall" ~ params$gamma_fall,
      schedule$season == "Winter" ~ params$gamma_winter,
      TRUE ~ 0.5  # Default fallback
    ),
    alpha = case_when(
      schedule$season == "Spring" ~ params$alpha_spring,
      schedule$season == "Summer" ~ params$alpha_summer,
      schedule$season == "Fall" ~ params$alpha_fall,
      schedule$season == "Winter" ~ params$alpha_winter,
      TRUE ~ 0.1  # Default fallback
    ),
    delta = case_when(
      schedule$season == "Spring" ~ params$delta_spring,
      schedule$season == "Summer" ~ params$delta_summer,
      schedule$season == "Fall" ~ params$delta_fall,
      schedule$season == "Winter" ~ params$delta_winter,
      TRUE ~ 0.05  # Default fallback
    ),
    omega = case_when(
      schedule$season == "Spring" ~ params$omega_spring,
      schedule$season == "Summer" ~ params$omega_summer,
      schedule$season == "Fall" ~ params$omega_fall,
      schedule$season == "Winter" ~ params$omega_winter,
      TRUE ~ 0.5  # Default fallback
    ),
    theta = case_when(
      schedule$season == "Spring" ~ params$theta_spring,
      schedule$season == "Summer" ~ params$theta_summer,
      schedule$season == "Fall" ~ params$theta_fall,
      schedule$season == "Winter" ~ params$theta_winter,
      TRUE ~ 0.3  # Default fallback
    ),

    # Forage production parameters (seasonal variation)
    # A = max value, B = midpoint, I = slope at midpoint (negative)
    A = case_when(
      schedule$season == "Spring" ~ params$A_spring,
      schedule$season == "Summer" ~ params$A_summer,
      schedule$season == "Fall" ~ params$A_fall,
      schedule$season == "Winter" ~ params$A_winter,
      TRUE ~ params$A
    ),
    B = case_when(
      schedule$season == "Spring" ~ params$B_spring,
      schedule$season == "Summer" ~ params$B_summer,
      schedule$season == "Fall" ~ params$B_fall,
      schedule$season == "Winter" ~ params$B_winter,
      TRUE ~ params$B
    ),
    I = case_when(
      schedule$season == "Spring" ~ params$I_spring,
      schedule$season == "Summer" ~ params$I_summer,
      schedule$season == "Fall" ~ params$I_fall,
      schedule$season == "Winter" ~ params$I_winter,
      TRUE ~ params$I
    ),

    # Crop production parameters (seasonal variation)
    # D = max value, E = midpoint, G = slope at midpoint (negative)
    D = case_when(
      schedule$season == "Spring" ~ params$D_spring,
      schedule$season == "Summer" ~ params$D_summer,
      schedule$season == "Fall" ~ params$D_fall,
      schedule$season == "Winter" ~ params$D_winter,
      TRUE ~ params$D
    ),
    E = case_when(
      schedule$season == "Spring" ~ params$E_spring,
      schedule$season == "Summer" ~ params$E_summer,
      schedule$season == "Fall" ~ params$E_fall,
      schedule$season == "Winter" ~ params$E_winter,
      TRUE ~ params$E
    ),
    G = case_when(
      schedule$season == "Spring" ~ params$G_spring,
      schedule$season == "Summer" ~ params$G_summer,
      schedule$season == "Fall" ~ params$G_fall,
      schedule$season == "Winter" ~ params$G_winter,
      TRUE ~ params$G
    ),
    stringsAsFactors = FALSE
  )

  return(locations)
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
run_beekeeping_simulation <- function(locations, initial_colonies, initial_frames, P_feed, management_params) {
  n_periods <- nrow(locations)
  results <- vector("list", n_periods)

  current_colonies <- initial_colonies
  current_frames <- initial_frames

  for (t in 1:n_periods) {
    # If colonies or frames hit zero, the operation is over - set everything to zero
    operation_over <- (current_colonies <= 0 || current_frames <= 0)
    if (operation_over) {
      current_colonies <- 0
      current_frames <- 0
    }

    period_result <- simulate_period(
      locations[t, ], current_colonies, current_frames, P_feed
    )

    # Add missing fields that are expected by results_to_dataframe
    period_result$period <- t
    period_result$season <- locations[t, "season"]
    period_result$year <- ceiling(t / 4)
    period_result$colonies_start <- current_colonies
    period_result$frames_start <- current_frames

    # Apply management at end of period
    season <- locations[t, "season"]

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
      forager_share = x$forager_share,
      total_foragers = x$total_foragers,
      forage_collected = x$forage_collected,
      forage_consumed = x$forage_consumed,
      net_forage = x$net_forage,
      feed_required = x$feed_required,
      honey_harvested = x$honey_harvested,
      crop_yield = x$crop_yield,
      colonies_end = x$colonies_end,
      frames_end = x$frames_end,
      revenue_honey = x$revenue_honey,
      revenue_crop = x$revenue_crop,
      cost_maintenance = x$cost_maintenance,
      cost_feed = x$cost_feed,
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
} # Optimization function for management parameters
optimize_management <- function(params, locations, initial_colonies, initial_frames, P_feed, management_costs) {
  # Extract management parameters from optimization vector
  # Order: cull_spring, cull_summer, cull_fall, cull_winter,
  #        adjust_spring, adjust_summer, adjust_fall, adjust_winter

  # Debug logging for objective function calls
  if (length(params) != 8) {
    cat("WARNING: optimize_management called with", length(params), "parameters instead of 8:\n")
    cat("Params:", paste(params, collapse = ", "), "\n")
  }

  # Validate inputs
  if (any(!is.finite(params))) {
    cat("ERROR: Non-finite parameters:", paste(params, collapse = ", "), "\n")
    return(1e10)
  }
  if (length(params) != 8) {
    cat("ERROR: Wrong parameter length:", length(params), "expected 8\n")
    cat("Received params:", paste(params, collapse = ", "), "\n")
    return(1e10)
  }
  if (any(is.na(c(initial_colonies, initial_frames, P_feed)))) {
    cat("ERROR: NA in simulation inputs\n")
    return(1e10)
  }

  mgmt_params <- list(
    cull_spring = params[1], cull_summer = params[2], cull_fall = params[3], cull_winter = params[4],
    adjust_spring = params[5], adjust_summer = params[6], adjust_fall = params[7], adjust_winter = params[8],
    P_split = management_costs$P_split,
    P_merge = management_costs$P_merge,
    P_cull = management_costs$P_cull
  )

  # Run simulation with these parameters
  tryCatch(
    {
      results <- run_beekeeping_simulation(
        locations = locations,
        initial_colonies = initial_colonies,
        initial_frames = initial_frames,
        P_feed = P_feed,
        management_params = mgmt_params
      )

      # Calculate present value of profits
      # Convert annual discount rate to quarterly rate: (1 + annual_rate)^(1/4) - 1
      annual_rate <- management_costs$annual_discount_rate / 100
      quarterly_rate <- (1 + annual_rate)^(1 / 4) - 1

      # Calculate discounted profits
      total_profit <- sum(sapply(1:length(results), function(i) {
        period_profit <- results[[i]]$profit
        discount_factor <- 1 / (1 + quarterly_rate)^(i - 1)
        return(period_profit * discount_factor)
      }), na.rm = TRUE)

      # Return negative profit (since optim minimizes)
      return(-total_profit)
    },
    error = function(e) {
      # Return large penalty if simulation fails
      return(1e10)
    }
  )
}

# Create stock dynamics plot
plot_stock_dynamics <- function(results_df, panels_to_show = c("colonies", "frames", "frames_per_colony", "forage_balance")) {
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
      forage_collected_pc = forage_collected / avg_max_colonies,
      feed_required_pc = feed_required / avg_max_colonies,
      forage_consumed_pc = -forage_consumed / avg_max_colonies,
      honey_harvested_pc = -honey_harvested / avg_max_colonies
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

  # Prepare proper candlestick body data
  candlestick_bodies <- candlestick_data %>%
    select(period, metric, season, candlestick_part, value) %>%
    spread(candlestick_part, value) %>%
    mutate(
      growing = close > open,
      body_color = ifelse(growing, "green", "black"),
      xmin = period - 0.3,
      xmax = period + 0.3,
      ymin = pmin(open, close),
      ymax = pmax(open, close)
    )

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
  facet_labels <- c(
    "colonies" = "Colonies",
    "frames" = "Frames", 
    "frames_per_colony" = "Frames per Colony",
    "forage_balance" = "Forage Balance"
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

  # Create plot with four facets: three candlesticks + one forage balance
  p <- ggplot()
  
  # Add invisible points at y=0 for candlestick panels to force axis to include 0
  if (nrow(zero_points) > 0) {
    p <- p + geom_point(
      data = zero_points,
      aes(x = period, y = value),
      alpha = 0
    )
  }
  
  # Candlestick bodies with conditional border colors (only if we have candlestick data)
  if (nrow(candlestick_bodies) > 0) {
    # Non-growing bodies
    bodies_not_growing <- filter(candlestick_bodies, !growing)
    if (nrow(bodies_not_growing) > 0) {
      p <- p + geom_rect(
        data = bodies_not_growing,
        aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = season),
        alpha = 0.7, color = "black", size = 0.5
      )
    }
    
    # Growing bodies
    bodies_growing <- filter(candlestick_bodies, growing)
    if (nrow(bodies_growing) > 0) {
      p <- p + geom_rect(
        data = bodies_growing,
        aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = season),
        alpha = 0.7, color = "#228B22", size = 1.2
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
    scale_fill_manual(
      values = c(
        "Spring" = "#2E8B57", "Summer" = "#FFD700",
        "Fall" = "#FF8C00", "Winter" = "#4682B4",
        "Forage Collected" = "#1a6b1a",
        "Feed Added" = "#8B9D83",
        "Honey Harvested" = "#FFA500",
        "Forage Consumed" = "#FF6B6B"
      ),
      guide = "none"
    ) +
    scale_y_continuous(expand = expansion(mult = c(0.05, 0.05)))

  # Add red dots for starting point (period 1) - only for candlestick panels
  if (nrow(candlestick_data) > 0) {
    starting_points <- candlestick_data %>%
      filter(period == 1, candlestick_part == "open") %>%
      select(period, metric, value)
    
    if (nrow(starting_points) > 0) {
      p <- p + geom_point(
        data = starting_points,
        aes(x = period, y = value),
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
        size = 1.2, alpha = 0.8
      ) +
        scale_color_manual(values = c(
          "Cull" = "#DC143C", "Split" = "#555555",
          "Merge" = "#8B008B", "None" = "#808080"
        )) +
        labs(color = "Management")
    }
  }

  return(p)
}

# =============================================================================
# SHINY UI
# =============================================================================

ui <- dashboardPage(
  dashboardHeader(title = "Beekeeping Economics Simulation"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Simulation settings", tabName = "simulation", icon = icon("chart-line")),

      # Simulation Parameters
      h4("Simulation Setup", style = "color: white; margin-left: 15px;"),
      numericInput("n_years", "Years to Simulate:", value = 5, min = 1, max = 30, step = 1),
      tags$div(
        style = "color: #ccc; font-size: 10px; margin-left: 15px; margin-top: -10px; margin-bottom: 10px;",
        textOutput("sim_time_display")
      ),
      sliderInput("annual_discount_rate", "Annual Discount Rate (%):",
        min = 0, max = 15, value = 3, step = 0.1
      ),
      numericInput("initial_colonies", "Initial Colonies:", value = 3000, min = 100, max = 5000, step = 100),
      numericInput("initial_frames", "Initial Frames:", value = 18000, min = 500, max = 30000, step = 500),

      # Reset and Save buttons
      br(),
      div(
        style = "text-align: center;",
        actionButton("reset_params", "Reset All Parameters", class = "btn-secondary"),
        br(), br(),
        downloadButton("save_settings", "Export Parameters to CSV", class = "btn-primary"),
        br(), br(),
        downloadButton("export_results", "Export Simulation to CSV", 
                     class = "btn-success", icon = icon("download"))
      ),
      hr(style = "border-color: white;"),


      # Economic Parameters
      h4("Operating costs", style = "color: white; margin-left: 15px;"),
      numericInput("Cost", "Maintenance Cost ($/col/year):", value = 200, min = 0, max = 400, step = 5),
      numericInput("P_feed", "Feed Price ($/unit):", value = 0.2, min = 0, max = 10, step = 0.01),
      numericInput("P_split", "Splitting Cost ($/op):", value = 25, min = 0, max = 100, step = 1),
      numericInput("P_merge", "Merging Cost ($/op):", value = 10, min = 0, max = 100, step = 1),
      numericInput("P_cull", "Culling Cost ($/op):", value = 10, min = 0, max = 100, step = 1),
      hr(style = "border-color: white;")
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

        /* Color management sliders by season - v2 layout with rows for each season */
        /* Spring sliders (first data row - columns 2 and 3) */
        .management-box .row:nth-child(2) .col-sm-6 .irs-bar,
        .management-box .row:nth-child(2) .col-sm-4 .irs-bar {
          background: #2E8B57 !important;
        }
        .management-box .row:nth-child(2) .col-sm-6 .irs-from,
        .management-box .row:nth-child(2) .col-sm-6 .irs-to,
        .management-box .row:nth-child(2) .col-sm-6 .irs-single,
        .management-box .row:nth-child(2) .col-sm-4 .irs-from,
        .management-box .row:nth-child(2) .col-sm-4 .irs-to,
        .management-box .row:nth-child(2) .col-sm-4 .irs-single {
          background: #2E8B57 !important;
          color: #fff !important;
        }

        /* Summer sliders (second data row) */
        .management-box .row:nth-child(3) .col-sm-6 .irs-bar,
        .management-box .row:nth-child(3) .col-sm-4 .irs-bar {
          background: #FFD700 !important;
        }
        .management-box .row:nth-child(3) .col-sm-6 .irs-from,
        .management-box .row:nth-child(3) .col-sm-6 .irs-to,
        .management-box .row:nth-child(3) .col-sm-6 .irs-single,
        .management-box .row:nth-child(3) .col-sm-4 .irs-from,
        .management-box .row:nth-child(3) .col-sm-4 .irs-to,
        .management-box .row:nth-child(3) .col-sm-4 .irs-single {
          background: #FFD700 !important;
          color: #000 !important;
        }

        /* Fall sliders (third data row) */
        .management-box .row:nth-child(4) .col-sm-6 .irs-bar,
        .management-box .row:nth-child(4) .col-sm-4 .irs-bar {
          background: #FF8C00 !important;
        }
        .management-box .row:nth-child(4) .col-sm-6 .irs-from,
        .management-box .row:nth-child(4) .col-sm-6 .irs-to,
        .management-box .row:nth-child(4) .col-sm-6 .irs-single,
        .management-box .row:nth-child(4) .col-sm-4 .irs-from,
        .management-box .row:nth-child(4) .col-sm-4 .irs-to,
        .management-box .row:nth-child(4) .col-sm-4 .irs-single {
          background: #FF8C00 !important;
          color: #fff !important;
        }

        /* Winter sliders (fourth data row) */
        .management-box .row:nth-child(5) .col-sm-6 .irs-bar,
        .management-box .row:nth-child(5) .col-sm-4 .irs-bar {
          background: #4682B4 !important;
        }
        .management-box .row:nth-child(5) .col-sm-6 .irs-from,
        .management-box .row:nth-child(5) .col-sm-6 .irs-to,
        .management-box .row:nth-child(5) .col-sm-6 .irs-single,
        .management-box .row:nth-child(5) .col-sm-4 .irs-from,
        .management-box .row:nth-child(5) .col-sm-4 .irs-to,
        .management-box .row:nth-child(5) .col-sm-4 .irs-single {
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

        /* Fix period range slider tick marks and styling */
        #period_range .irs-grid {
          display: none !important;
        }

        #period_range .irs-grid-text {
          display: none !important;
        }

        #period_range .irs-grid-pol {
          display: none !important;
        }

        #period_range .irs-min,
        #period_range .irs-max {
          display: none !important;
        }

        /* Style the period range slider bar */
        #period_range .irs-bar {
          background: linear-gradient(to right, #2E8B57, #FFD700, #FF8C00, #4682B4) !important;
          height: 8px !important;
        }

        #period_range .irs-handle {
          width: 18px !important;
          height: 18px !important;
          border: 2px solid #fff !important;
          box-shadow: 0 2px 6px rgba(0,0,0,0.3) !important;
        }

        #period_range .irs-line {
          height: 8px !important;
          background: #e0e0e0 !important;
        }
      ")),

      # No constraint validation needed anymore since we use single adjustment variable
    ),
    tabItems(
      tabItem(
        tabName = "simulation",
        fluidRow(
          # Main plot - larger and prominent
          box(
            title = "Bee Stock Dynamics",
            status = "primary", solidHeader = TRUE,
            width = 9, height = "750px",
            plotOutput("stock_plot", height = "700px")
          ),

          # Management Variables
          box(
            title = "Management Variables",
            status = "warning", solidHeader = TRUE,
            width = 3, height = "750px",
            class = "management-box",

            # Management action column headers
            fluidRow(
              column(2, div()), # Empty column for season labels
              column(6, div(h5(HTML("<span style='color: #2E8B57;'>Colony Adjustment</span> <span style='color: #2E8B57;'>(+Split / -Merge)</span>"),
                style = "text-align: center; margin-bottom: 10px; font-weight: bold;"
              ))),
              column(4, div(h5(HTML("<span style='color: #DC143C;'>Culling</span><br><span style='color: #DC143C;'>(↓ % colonies and frames)</span>"),
                style = "text-align: center; margin-bottom: 10px; font-weight: bold;"
              )))
            ),

            # Spring row
            fluidRow(
              column(2, div(h5("Spring", style = "text-align: center; color: #2E8B57; margin-bottom: 10px; font-weight: bold; margin-top: 15px;"))),
              column(6, div(
                class = "management-box",
                sliderInput("adjust_spring", NULL,
                  min = -100, max = 100, value = 60, step = 5,
                  width = "100%", ticks = TRUE
                ),
                style = "text-align: center;"
              )),
              column(4, div(
                class = "management-box",
                sliderInput("cull_spring", NULL,
                  min = 0, max = 100, value = 0, step = 1,
                  width = "100%", ticks = TRUE
                ),
                style = "text-align: center;"
              ))
            ),

            # Summer row
            fluidRow(
              column(2, div(h5("Summer", style = "text-align: center; color: #FFD700; margin-bottom: 10px; font-weight: bold; margin-top: 15px;"))),
              column(6, div(
                class = "management-box",
                sliderInput("adjust_summer", NULL,
                  min = -100, max = 100, value = 30, step = 5,
                  width = "100%", ticks = TRUE
                ),
                style = "text-align: center;"
              )),
              column(4, div(
                class = "management-box",
                sliderInput("cull_summer", NULL,
                  min = 0, max = 100, value = 0, step = 1,
                  width = "100%", ticks = TRUE
                ),
                style = "text-align: center;"
              ))
            ),

            # Fall row
            fluidRow(
              column(2, div(h5("Fall", style = "text-align: center; color: #FF8C00; margin-bottom: 10px; font-weight: bold; margin-top: 15px;"))),
              column(6, div(
                class = "management-box",
                sliderInput("adjust_fall", NULL,
                  min = -100, max = 100, value = -5, step = 5,
                  width = "100%", ticks = TRUE
                ),
                style = "text-align: center;"
              )),
              column(4, div(
                class = "management-box",
                sliderInput("cull_fall", NULL,
                  min = 0, max = 100, value = 5, step = 1,
                  width = "100%", ticks = TRUE
                ),
                style = "text-align: center;"
              ))
            ),

            # Winter row
            fluidRow(
              column(2, div(h5("Winter", style = "text-align: center; color: #4682B4; margin-bottom: 10px; font-weight: bold; margin-top: 15px;"))),
              column(6, div(
                class = "management-box",
                sliderInput("adjust_winter", NULL,
                  min = -100, max = 100, value = -10, step = 5,
                  width = "100%", ticks = TRUE
                ),
                style = "text-align: center;"
              )),
              column(4, div(
                class = "management-box",
                sliderInput("cull_winter", NULL,
                  min = 0, max = 100, value = 0, step = 1,
                  width = "100%", ticks = TRUE
                ),
                style = "text-align: center;"
              ))
            ),

            # Preset buttons with reduced margins
            div(
              style = "margin-top: 10px;",
              fluidRow(
                column(12, actionButton("preset_zero", "Set all to zero", class = "btn-default btn-block", style = "margin-bottom: 3px;"))
              ),
              fluidRow(
                column(12, actionButton("preset_growth", "Maximum growth", class = "btn-default btn-block", style = "margin-bottom: 3px;"))
              ),
              fluidRow(
                column(12, actionButton("preset_average", "US average 2024 guess", class = "btn-default btn-block", style = "margin-bottom: 3px;"))
              ),
              fluidRow(
                column(12, actionButton("optimize_mgmt", "NPV max (numerical optim.)", class = "btn-default btn-block", style = "margin-bottom: 8px;"))
              ),
              # Total profit display with compact margins
              div(
                style = "margin-top: 5px; padding: 6px; background-color: #f8f9fa; border-radius: 3px; border-left: 3px solid #dc3545;",
                h6("Total Profit:", style = "margin-bottom: 2px; font-weight: bold; font-size: 11px;"),
                h5(textOutput("total_profit_display", inline = TRUE), style = "margin: 0; color: #dc3545; font-weight: bold;")
              )
            )
          )
        ),
        fluidRow(
          # Simulation Results
          box(
            title = "Simulation Results",
            status = "success", solidHeader = TRUE,
            width = 9,

            # Period range selection
            fluidRow(
              column(
                6,
                div(
                  style = "margin-bottom: 15px;",
                  sliderInput("period_range",
                    label = "Select Period Range for Analysis:",
                    min = 1,
                    max = 100, # Will be updated dynamically
                    value = c(1, 20),
                    step = 1,
                    ticks = FALSE,
                    animate = FALSE,
                    width = "100%"
                  )
                )
              ),
              column(
                6,
                # Period selection indicator moved to same row
                div(
                  style = "margin-top: 25px; padding: 8px; background-color: #f8f9fa; border-radius: 4px; border-left: 4px solid #28a745;",
                  strong("Selected: "),
                  textOutput("period_indicator", inline = TRUE),
                  style = "font-size: 13px; color: #495057;"
                )
              )
            ),
            fluidRow(
              column(
                6,
                h5("Production & Operations Metrics"),
                tableOutput("operations_table")
              ),
              column(
                6,
                h5("Financial Summary (all values are Net Present Values)"),
                tableOutput("financial_table")
              )
            )
          ),
          
          # Plot Legend and Options
          box(
            title = "Plot Legend and Options",
            status = "info", solidHeader = TRUE,
            width = 3,
            
            # Starting point indicator
            div(
              style = "text-align: left; margin-bottom: 20px;",
              tags$span(style = "display: inline-block; width: 10px; height: 10px; background-color: red; border-radius: 50%; margin-right: 8px; vertical-align: middle;"),
              tags$span("Starting point of simulation", style = "font-size: 12px; vertical-align: middle;")
            ),
            
            fluidRow(
              column(6,
                h5("Colony and Frames", style = "margin-bottom: 15px; font-weight: bold;"),
                
                # Stock Growth
                div(
                  style = "margin-bottom: 10px;",
                  span(style = "display: inline-block; width: 20px; height: 20px; background-color: white; border: 2px solid #228B22; margin-right: 8px; vertical-align: middle;"),
                  span("Stock Growth", style = "vertical-align: middle; font-size: 11px;")
                ),
                
                # Split (grey arrow)
                div(
                  style = "margin-bottom: 10px;",
                  span(style = "display: inline-block; width: 0; height: 0; border-left: 10px solid transparent; border-right: 10px solid transparent; border-bottom: 15px solid #555555; margin-right: 8px; vertical-align: middle;"),
                  span("Split", style = "vertical-align: middle; font-size: 11px;")
                ),
                
                # Merge (purple arrow)
                div(
                  style = "margin-bottom: 10px;",
                  span(style = "display: inline-block; width: 0; height: 0; border-left: 10px solid transparent; border-right: 10px solid transparent; border-bottom: 15px solid #8B008B; margin-right: 8px; vertical-align: middle;"),
                  span("Merge", style = "vertical-align: middle; font-size: 11px;")
                ),
                
                # Cull (red arrow)
                div(
                  style = "margin-bottom: 20px;",
                  span(style = "display: inline-block; width: 0; height: 0; border-left: 10px solid transparent; border-right: 10px solid transparent; border-top: 15px solid #DC143C; margin-right: 8px; vertical-align: middle;"),
                  span("Cull", style = "vertical-align: middle; font-size: 11px;")
                )
              ),
              
              column(6,
                h5("Forage Balance", style = "margin-bottom: 15px; font-weight: bold;"),
                
                # Forage Collected
                div(
                  style = "margin-bottom: 10px;",
                  span(style = "display: inline-block; width: 20px; height: 20px; background-color: #1a6b1a; margin-right: 8px; vertical-align: middle;"),
                  span("Forage Collected", style = "vertical-align: middle; font-size: 11px;")
                ),
                
                # Feed Added
                div(
                  style = "margin-bottom: 10px;",
                  span(style = "display: inline-block; width: 20px; height: 20px; background-color: #8B9D83; margin-right: 8px; vertical-align: middle;"),
                  span("Supplemental Feed", style = "vertical-align: middle; font-size: 11px;")
                ),
                
                # Forage Consumed
                div(
                  style = "margin-bottom: 10px;",
                  span(style = "display: inline-block; width: 20px; height: 20px; background-color: #FF6B6B; margin-right: 8px; vertical-align: middle;"),
                  span("Forage Consumed", style = "vertical-align: middle; font-size: 11px;")
                ),
                
                # Honey Harvested
                div(
                  style = "margin-bottom: 20px;",
                  span(style = "display: inline-block; width: 20px; height: 20px; background-color: #FFA500; margin-right: 8px; vertical-align: middle;"),
                  span("Honey Harvested", style = "vertical-align: middle; font-size: 11px;")
                )
              )
            ),
            
            hr(),
            
            h5("Display Panels", style = "margin-bottom: 15px; font-weight: bold;"),
            
            fluidRow(
              column(6,
                checkboxInput("show_colonies", "Colonies", value = TRUE),
                checkboxInput("show_frames", "Frames", value = TRUE)
              ),
              column(6,
                checkboxInput("show_frames_per_colony", "Frames/Colony", value = TRUE),
                checkboxInput("show_forage_balance", "Forage Balance", value = TRUE)
              )
            )
          )
        ),
        fluidRow(
          # Seasonal Parameters
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
                conditionalPanel(
                  condition = "input.season_selector == 'spring'",
                  div(
                    style = "height: 665px; width: 95%; overflow-y: auto;",
                    fluidRow(
                      column(
                        3,
                        h6("Growth & Loss"),
                        sliderInput("alpha_spring", "Growth (frames/col/week):",
                          min = 0, max = 1.0, value = 0.4, step = 0.05
                        ),
                        sliderInput("delta_spring", "Loss Rate (%/season):",
                          min = 0.01, max = 0.2, value = 0.15, step = 0.01
                        )
                      ),
                      column(
                        3,
                        h6("Feed & Foraging"),
                        sliderInput("gamma_spring", "Feed Consumpt.(lbs/frame/week):",
                          min = 0, max = 5.0, value = 0.5, step = 0.01
                        ),
                        sliderInput("omega_spring", "Omega (base foraging):",
                          min = 0.1, max = 1.0, value = 0.5, step = 0.05
                        ),
                        sliderInput("theta_spring", "Theta (col. strength-foragers):",
                          min = 0.1, max = 0.5, value = 0.3, step = 0.05
                        ),
                        plotOutput("spring_forager_plot", height = "250px")
                      ),
                      column(
                        3,
                        h6("Forage Collection & Honey"),
                        sliderInput("A_spring", "Forage Max Value:", value = 70, min = 0, max = 1000, step = 10),
                        sliderInput("B_spring", "Forage Midpoint:", value = 2100, min = 0, max = 100000, step = 100),
                        sliderInput("Ph_spring", "Honey Price ($/lbs):", value = 3.0, min = 0, max = 20.0, step = 0.1),
                        plotOutput("spring_forage_plot", height = "250px")
                      ),
                      column(
                        3,
                        h6("Crop Pollination"),
                        sliderInput("D_spring", "Crop Max Value:", value = 25, min = 0, max = 100, step = 0.5),
                        sliderInput("E_spring", "Crop Midpoint:", value = 32000, min = 0, max = 100000, step = 1000),
                        sliderInput("Pc_spring", "Crop Price ($/lbs):", value = 2, min = 0, max = 100, step = 1),
                        plotOutput("spring_crop_plot", height = "250px")
                      )
                    )
                  )
                ),
                conditionalPanel(
                  condition = "input.season_selector == 'summer'",
                  div(
                    style = "height: 665px; width: 95%; overflow-y: auto;",
                    fluidRow(
                      column(
                        3,
                        h6("Growth & Loss"),
                        sliderInput("alpha_summer", "Growth (frames/col/week):",
                          min = 0, max = 1.0, value = 0.1, step = 0.05
                        ),
                        sliderInput("delta_summer", "Loss Rate (%/season):",
                          min = 0.01, max = 0.2, value = 0.05, step = 0.01
                        )
                      ),
                      column(
                        3,
                        h6("Feed & Foraging"),
                        sliderInput("gamma_summer", "Feed Consumpt.(lbs/frame/week):",
                          min = 0, max = 5.0, value = 0.5, step = 0.01
                        ),
                        sliderInput("omega_summer", "Omega (base foraging):",
                          min = 0.1, max = 1.0, value = 0.5, step = 0.05
                        ),
                        sliderInput("theta_summer", "Theta (col. strength-foragers):",
                          min = 0.1, max = 0.5, value = 0.3, step = 0.05
                        ),
                        plotOutput("summer_forager_plot", height = "250px")
                      ),
                      column(
                        3,
                        h6("Forage Collection & Honey"),
                        sliderInput("A_summer", "Forage Max Value:", value = 20, min = 0, max = 1000, step = 10),
                        sliderInput("B_summer", "Forage Midpoint:", value = 60000, min = 0, max = 100000, step = 1000),
                        sliderInput("Ph_summer", "Honey Price ($/lbs):", value = 2.0, min = 0, max = 20.0, step = 0.1),
                        plotOutput("summer_forage_plot", height = "250px")
                      ),
                      column(
                        3,
                        h6("Crop Pollination"),
                        sliderInput("D_summer", "Crop Max Value:", value = 17, min = 0, max = 100, step = 0.5),
                        sliderInput("E_summer", "Crop Midpoint:", value = 2700, min = 0, max = 10000, step = 100),
                        sliderInput("Pc_summer", "Crop Price ($/lbs):", value = 2, min = 0, max = 100, step = 1),
                        plotOutput("summer_crop_plot", height = "250px")
                      )
                    )
                  )
                ),
                conditionalPanel(
                  condition = "input.season_selector == 'fall'",
                  div(
                    style = "height: 665px; width: 95%; overflow-y: auto;",
                    fluidRow(
                      column(
                        3,
                        h6("Growth & Loss"),
                        sliderInput("alpha_fall", "Growth (frames/col/week):",
                          min = 0, max = 1.0, value = 0, step = 0.05
                        ),
                        sliderInput("delta_fall", "Loss Rate (%/season):",
                          min = 0.01, max = 0.2, value = 0.15, step = 0.01
                        )
                      ),
                      column(
                        3,
                        h6("Feed & Foraging"),
                        sliderInput("gamma_fall", "Feed Consumpt.(lbs/frame/week):",
                          min = 0, max = 5.0, value = 0.5, step = 0.01
                        ),
                        sliderInput("omega_fall", "Omega (base foraging):",
                          min = 0.1, max = 1.0, value = 0.5, step = 0.05
                        ),
                        sliderInput("theta_fall", "Theta (col. strength-foragers):",
                          min = 0.1, max = 0.5, value = 0.3, step = 0.05
                        ),
                        plotOutput("fall_forager_plot", height = "250px")
                      ),
                      column(
                        3,
                        h6("Forage Collection & Honey"),
                        sliderInput("A_fall", "Forage Max Value:", value = 0, min = 0, max = 10000, step = 10),
                        sliderInput("B_fall", "Forage Midpoint:", value = 2000, min = 0, max = 100000, step = 1000),
                        sliderInput("Ph_fall", "Honey Price ($/lbs):", value = 3.0, min = 0, max = 10.0, step = 0.1),
                        plotOutput("fall_forage_plot", height = "250px")
                      ),
                      column(
                        3,
                        h6("Crop Pollination"),
                        sliderInput("D_fall", "Crop Max Value:", value = 0, min = 0, max = 20, step = 0.5),
                        sliderInput("E_fall", "Crop Midpoint:", value = 2000, min = 0, max = 100000, step = 1000),
                        sliderInput("Pc_fall", "Crop Price ($/lbs):", value = 0, min = 0, max = 100, step = 1),
                        plotOutput("fall_crop_plot", height = "250px")
                      )
                    )
                  )
                ),
                conditionalPanel(
                  condition = "input.season_selector == 'winter'",
                  div(
                    style = "height: 665px; width: 95%; overflow-y: auto;",
                    fluidRow(
                      column(
                        3,
                        h6("Growth & Loss"),
                        sliderInput("alpha_winter", "Growth (frames/col/week):",
                          min = 0, max = 1.0, value = 0, step = 0.05
                        ),
                        sliderInput("delta_winter", "Loss Rate (%/season):",
                          min = 0.01, max = 0.2, value = 0.2, step = 0.01
                        )
                      ),
                      column(
                        3,
                        h6("Feed & Foraging"),
                        sliderInput("gamma_winter", "Feed Consumpt.(lbs/frame/week):",
                          min = 0, max = 5.0, value = 0.19, step = 0.01
                        ),
                        sliderInput("omega_winter", "Omega (base foraging):",
                          min = 0.1, max = 1.0, value = 0.5, step = 0.05
                        ),
                        sliderInput("theta_winter", "Theta (col. strength-foragers):",
                          min = 0.1, max = 0.5, value = 0.3, step = 0.05
                        ),
                        plotOutput("winter_forager_plot", height = "250px")
                      ),
                      column(
                        3,
                        h6("Forage Collection & Honey"),
                        sliderInput("A_winter", "Forage Max Value:", value = 0, min = 0, max = 10000, step = 10),
                        sliderInput("B_winter", "Forage Midpoint:", value = 2000, min = 0, max = 100000, step = 1000),
                        sliderInput("Ph_winter", "Honey Price ($/lbs):", value = 0, min = 0, max = 10.0, step = 0.1),
                        plotOutput("winter_forage_plot", height = "250px")
                      ),
                      column(
                        3,
                        h6("Crop Pollination"),
                        sliderInput("D_winter", "Crop Max Value:", value = 0, min = 0, max = 20, step = 0.5),
                        sliderInput("E_winter", "Crop Midpoint:", value = 2000, min = 0, max = 100000, step = 1000),
                        sliderInput("Pc_winter", "Crop Price ($/lbs):", value = 0, min = 0, max = 100, step = 1),
                        plotOutput("winter_crop_plot", height = "250px")
                      )
                    )
                  )
                ),
                # Legend explaining vertical lines
                div(
                  style = "margin-top: 10px; padding: 8px; background-color: #f8f9fa; border-radius: 4px; font-size: 11px; color: #555;",
                  em("Vertical lines in the plots represent values of foragers and frames per colony in the simulation. Earlier years in dark purple and last year in yellow.")
                )
              )
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
# =============================================================================
# SHINY SERVER
# =============================================================================

server <- function(input, output, session) {
  # Display simulation time
  output$sim_time_display <- renderText({
    paste("Last simulation:", format(Sys.time(), "%H:%M:%S"))
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

  # Reactive function to run simulation (runs automatically when inputs change)
  simulation_results <- reactive({
    # Start timing
    start_time <- Sys.time()

    # Show that simulation is running
    output$sim_status <- renderText("Running simulation...")

    # Collect all parameters
    params <- list(
      Cost = ifelse(is.null(input$Cost), 50, input$Cost),
      Ph = ifelse(is.null(input$Ph), 3.0, input$Ph),
      Pc = ifelse(is.null(input$Pc), 0.5, input$Pc),
      # Seasonal biological parameters
      alpha_spring = ifelse(is.null(input$alpha_spring), 0.1, input$alpha_spring),
      alpha_summer = ifelse(is.null(input$alpha_summer), 0.1, input$alpha_summer),
      alpha_fall = ifelse(is.null(input$alpha_fall), 0.1, input$alpha_fall),
      alpha_winter = ifelse(is.null(input$alpha_winter), 0.1, input$alpha_winter),
      delta_spring = ifelse(is.null(input$delta_spring), 0.05, input$delta_spring),
      delta_summer = ifelse(is.null(input$delta_summer), 0.05, input$delta_summer),
      delta_fall = ifelse(is.null(input$delta_fall), 0.05, input$delta_fall),
      delta_winter = ifelse(is.null(input$delta_winter), 0.05, input$delta_winter),
      gamma_spring = ifelse(is.null(input$gamma_spring), 0.5, input$gamma_spring),
      gamma_summer = ifelse(is.null(input$gamma_summer), 0.5, input$gamma_summer),
      gamma_fall = ifelse(is.null(input$gamma_fall), 0.5, input$gamma_fall),
      gamma_winter = ifelse(is.null(input$gamma_winter), 0.5, input$gamma_winter),
      omega_spring = ifelse(is.null(input$omega_spring), 0.5, input$omega_spring),
      omega_summer = ifelse(is.null(input$omega_summer), 0.5, input$omega_summer),
      omega_fall = ifelse(is.null(input$omega_fall), 0.5, input$omega_fall),
      omega_winter = ifelse(is.null(input$omega_winter), 0.5, input$omega_winter),
      theta_spring = ifelse(is.null(input$theta_spring), 0.3, input$theta_spring),
      theta_summer = ifelse(is.null(input$theta_summer), 0.3, input$theta_summer),
      theta_fall = ifelse(is.null(input$theta_fall), 0.3, input$theta_fall),
      theta_winter = ifelse(is.null(input$theta_winter), 0.3, input$theta_winter),
      # Seasonal honey prices
      Ph_spring = ifelse(is.null(input$Ph_spring), input$Ph, input$Ph_spring),
      Ph_summer = ifelse(is.null(input$Ph_summer), input$Ph, input$Ph_summer),
      Ph_fall = ifelse(is.null(input$Ph_fall), input$Ph, input$Ph_fall),
      Ph_winter = ifelse(is.null(input$Ph_winter), input$Ph, input$Ph_winter),
      # Seasonal crop prices
      Pc_spring = ifelse(is.null(input$Pc_spring), 22713, input$Pc_spring),
      Pc_summer = ifelse(is.null(input$Pc_summer), 5678, input$Pc_summer),
      Pc_fall = ifelse(is.null(input$Pc_fall), 0, input$Pc_fall),
      Pc_winter = ifelse(is.null(input$Pc_winter), 0, input$Pc_winter),
      # Seasonal production capacities
      A_spring = ifelse(is.null(input$A_spring), 3, input$A_spring),
      A_summer = ifelse(is.null(input$A_summer), 11, input$A_summer),
      A_fall = ifelse(is.null(input$A_fall), 2, input$A_fall),
      A_winter = ifelse(is.null(input$A_winter), 0, input$A_winter),
      I_spring = ifelse(is.null(input$I_spring), -2000, input$I_spring),
      I_summer = ifelse(is.null(input$I_summer), -2000, input$I_summer),
      I_fall = ifelse(is.null(input$I_fall), -2000, input$I_fall),
      I_winter = ifelse(is.null(input$I_winter), -2000, input$I_winter),
      B_spring = ifelse(is.null(input$B_spring), 0.002, input$B_spring),
      B_summer = ifelse(is.null(input$B_summer), 0.002, input$B_summer),
      B_fall = ifelse(is.null(input$B_fall), 0.002, input$B_fall),
      B_winter = ifelse(is.null(input$B_winter), 0.002, input$B_winter),
      D_spring = ifelse(is.null(input$D_spring), 15, input$D_spring),
      D_summer = ifelse(is.null(input$D_summer), 15, input$D_summer),
      D_fall = ifelse(is.null(input$D_fall), 0, input$D_fall),
      D_winter = ifelse(is.null(input$D_winter), 0, input$D_winter),
      G_spring = ifelse(is.null(input$G_spring), -1500, input$G_spring),
      G_summer = ifelse(is.null(input$G_summer), -1500, input$G_summer),
      G_fall = ifelse(is.null(input$G_fall), -1500, input$G_fall),
      G_winter = ifelse(is.null(input$G_winter), -1500, input$G_winter),
      E_spring = ifelse(is.null(input$E_spring), 0.003, input$E_spring),
      E_summer = ifelse(is.null(input$E_summer), 0.003, input$E_summer),
      E_fall = ifelse(is.null(input$E_fall), 0.003, input$E_fall),
      E_winter = ifelse(is.null(input$E_winter), 0.003, input$E_winter),
      # Global production parameters
      A = 100, # Fallback value
      B = ifelse(is.null(input$B), 0.001, input$B),
      I = 50, # Fixed for simplicity
      D = 15, # Fallback value - reduced for realistic pollination fees
      E = ifelse(is.null(input$E), 0.002, input$E),
      G = 30 # Fixed for simplicity
    )

    # Collect management parameters
    management_params <- list(
      cull_spring = input$cull_spring,
      cull_summer = input$cull_summer,
      cull_fall = input$cull_fall,
      cull_winter = input$cull_winter,
      adjust_spring = input$adjust_spring,
      adjust_summer = input$adjust_summer,
      adjust_fall = input$adjust_fall,
      adjust_winter = input$adjust_winter,
      P_split = input$P_split,
      P_merge = input$P_merge,
      P_cull = input$P_cull
    )

    # Create schedule and locations
    schedule <- create_simple_schedule(input$n_years)
    locations <- create_location_parameters(schedule, params)

    # Run simulation
    results <- run_beekeeping_simulation(
      locations = locations,
      initial_colonies = input$initial_colonies,
      initial_frames = input$initial_frames,
      P_feed = input$P_feed,
      management_params = management_params
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
    output$sim_status <- renderText(paste("Simulation complete! (", nrow(results_df), "periods)"))
    output$sim_time_display <- renderText({
      if (sim_duration < 1) {
        paste("Simulation time:", round(sim_duration * 1000, 1), "ms")
      } else {
        paste("Simulation time:", round(sim_duration, 3), "seconds")
      }
    })

    return(results_df)
  })

  # Generate stock dynamics candlestick plot
  output$stock_plot <- renderPlot({
    results_df <- simulation_results()
    
    # Determine which panels to show based on checkboxes
    panels_to_show <- c()
    if (input$show_colonies) panels_to_show <- c(panels_to_show, "colonies")
    if (input$show_frames) panels_to_show <- c(panels_to_show, "frames")
    if (input$show_frames_per_colony) panels_to_show <- c(panels_to_show, "frames_per_colony")
    if (input$show_forage_balance) panels_to_show <- c(panels_to_show, "forage_balance")
    
    plot_stock_dynamics(results_df, panels_to_show)
  })


  # Generate operations table
  output$operations_table <- renderTable(
    {
      results_df <- simulation_results()

      # Use period range slider
      if (is.null(input$period_range)) {
        return(NULL)
      }
      start_period <- input$period_range[1]
      end_period <- input$period_range[2]

      # Filter data for selected period
      selected_data <- results_df[start_period:end_period, ]

      # Calculate average colony count for per-colony metrics (same as financial table)
      years_in_period <- ceiling((end_period - start_period + 1) / 4)
      yearly_max_colonies <- numeric()

      for (year in 1:years_in_period) {
        year_start <- start_period + (year - 1) * 4
        year_end <- min(end_period, year_start + 3)
        if (year_start <= end_period) {
          year_data <- selected_data[selected_data$period >= year_start & selected_data$period <= year_end, ]
          max_colonies <- if (all(is.na(year_data$colonies_end))) {
            cat("WARNING: All colonies_end values are NA for year", year, "\n")
            1 # Use 1 as fallback
          } else {
            max(year_data$colonies_end, na.rm = TRUE)
          }
          yearly_max_colonies <- c(yearly_max_colonies, max_colonies)
        }
      }
      avg_max_colonies <- mean(yearly_max_colonies, na.rm = TRUE)

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
          "Total Forage Collected",
          "Total Honey Harvested",
          "Total Feed Provided"
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

  # Reset period range ONLY when n_years changes (preserve user selection for other changes)
  observeEvent(input$n_years,
    {
      new_max <- input$n_years * 4
      updateSliderInput(session, "period_range",
        max = new_max,
        value = c(1, new_max)
      )
    },
    ignoreInit = TRUE
  )

  # Update slider range based on simulation results (for other parameter changes)
  observe({
    results_df <- simulation_results()
    new_max <- nrow(results_df)

    # Only update max, preserve user's current selection
    # If current selection is beyond new range, adjust it
    current_range <- input$period_range
    if (!is.null(current_range)) {
      # Preserve current selection if it's still valid
      if (current_range[2] <= new_max) {
        updateSliderInput(session, "period_range", max = new_max)
      } else {
        # Only adjust if user's selection exceeds new range
        new_end <- min(current_range[2], new_max)
        new_start <- min(current_range[1], new_end)
        updateSliderInput(session, "period_range",
          max = new_max,
          value = c(new_start, new_end)
        )
      }
    } else {
      # First time initialization only
      updateSliderInput(session, "period_range",
        max = new_max,
        value = c(1, min(20, new_max))
      )
    }
  })

  # Generate period indicator text
  output$period_indicator <- renderText({
    if (is.null(input$period_range)) {
      return("No range selected")
    }

    start_period <- input$period_range[1]
    end_period <- input$period_range[2]

    # Function to get season name from period
    get_season <- function(period) {
      season_in_year <- ((period - 1) %% 4) + 1
      switch(season_in_year,
        "1" = "Winter",
        "2" = "Spring",
        "3" = "Summer",
        "4" = "Fall"
      )
    }

    # Calculate years and seasons
    start_year <- ceiling(start_period / 4)
    end_year <- ceiling(end_period / 4)
    start_season <- get_season(start_period)
    end_season <- get_season(end_period)

    # Calculate total periods selected
    total_periods <- end_period - start_period + 1

    # Create readable period range description
    if (start_period == end_period) {
      paste(
        total_periods, "period selected",
        "(", start_season, "of year", start_year, ")"
      )
    } else {
      paste(
        total_periods, "periods selected",
        "(Start in", tolower(start_season), "of year", start_year,
        ", end in", tolower(end_season), "of year", end_year, ")"
      )
    }
  })

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

    # Use period range slider
    if (is.null(input$period_range)) {
      return(NULL)
    }
    start_period <- input$period_range[1]
    end_period <- input$period_range[2]

    # Filter data for selected period
    selected_data <- results_df[start_period:end_period, ]

    # Calculate average colony count for per-colony metrics
    years_in_period <- ceiling((end_period - start_period + 1) / 4)
    yearly_max_colonies <- numeric()

    for (year in 1:years_in_period) {
      year_start <- start_period + (year - 1) * 4
      year_end <- min(end_period, year_start + 3)
      if (year_start <= end_period) {
        year_data <- selected_data[selected_data$period >= year_start & selected_data$period <= year_end, ]
        max_colonies <- if (all(is.na(year_data$colonies_end))) {
          cat("WARNING: All colonies_end values are NA for year", year, "\n")
          1 # Use 1 as fallback
        } else {
          max(year_data$colonies_end, na.rm = TRUE)
        }
        yearly_max_colonies <- c(yearly_max_colonies, max_colonies)
      }
    }
    avg_max_colonies <- mean(yearly_max_colonies, na.rm = TRUE)

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
        "Honey Harvested" = "#FFA500",
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

      # Use period range slider
      if (is.null(input$period_range)) {
        return(NULL)
      }
      start_period <- input$period_range[1]
      end_period <- input$period_range[2]

      # Filter data for selected period
      selected_data <- results_df[start_period:end_period, ]

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

      # Calculate present value totals for selected period range
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
      years_in_period <- ceiling((end_period - start_period + 1) / 4)
      yearly_max_colonies <- numeric()

      for (year in 1:years_in_period) {
        year_start <- start_period + (year - 1) * 4
        year_end <- min(end_period, year_start + 3)
        if (year_start <= end_period) {
          year_data <- selected_data[selected_data$period >= year_start & selected_data$period <= year_end, ]
          max_colonies <- if (all(is.na(year_data$colonies_end))) {
            cat("WARNING: All colonies_end values are NA for year", year, "\n")
            1 # Use 1 as fallback
          } else {
            max(year_data$colonies_end, na.rm = TRUE)
          }
          yearly_max_colonies <- c(yearly_max_colonies, max_colonies)
        }
      }
      avg_max_colonies <- mean(yearly_max_colonies, na.rm = TRUE)

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

      return(financial_stats)
    },
    striped = TRUE,
    bordered = TRUE
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
    # US average 2024 guess: realistic US beekeeper management patterns
    updateSliderInput(session, "cull_spring", value = 0)
    updateSliderInput(session, "cull_summer", value = 0)
    updateSliderInput(session, "cull_fall", value = 10) # Fall culling of weak colonies
    updateSliderInput(session, "cull_winter", value = 0)

    updateSliderInput(session, "adjust_spring", value = 60) # Strong spring splitting
    updateSliderInput(session, "adjust_summer", value = 30) # Moderate summer splitting
    updateSliderInput(session, "adjust_fall", value = -5) # Light fall merging
    updateSliderInput(session, "adjust_winter", value = -10) # Winter merging for survival
  })

  # Management optimization
  observeEvent(input$optimize_mgmt, {
    # Show progress
    showNotification("Starting optimization... This may take a few minutes.", type = "message", duration = 3)

    # Collect current parameters (excluding management parameters)
    params <- list(
      Cost = input$Cost %||% 50,
      Ph = input$Ph %||% 3.0,
      Pc = input$Pc %||% 0.5,
      # Seasonal biological parameters
      alpha_spring = input$alpha_spring %||% 0.1,
      alpha_summer = input$alpha_summer %||% 0.1,
      alpha_fall = input$alpha_fall %||% 0.1,
      alpha_winter = input$alpha_winter %||% 0.1,
      delta_spring = input$delta_spring %||% 0.05,
      delta_summer = input$delta_summer %||% 0.05,
      delta_fall = input$delta_fall %||% 0.05,
      delta_winter = input$delta_winter %||% 0.05,
      gamma_spring = input$gamma_spring %||% 0.5,
      gamma_summer = input$gamma_summer %||% 0.5,
      gamma_fall = input$gamma_fall %||% 0.5,
      gamma_winter = input$gamma_winter %||% 0.5,
      omega_spring = input$omega_spring %||% 0.5,
      omega_summer = input$omega_summer %||% 0.5,
      omega_fall = input$omega_fall %||% 0.5,
      omega_winter = input$omega_winter %||% 0.5,
      theta_spring = input$theta_spring %||% 0.3,
      theta_summer = input$theta_summer %||% 0.3,
      theta_fall = input$theta_fall %||% 0.3,
      theta_winter = input$theta_winter %||% 0.3,
      # Seasonal production
      A_spring = input$A_spring %||% 3, A_summer = input$A_summer %||% 11, A_fall = input$A_fall %||% 2, A_winter = input$A_winter %||% 0,
      I_spring = input$I_spring %||% -2000, I_summer = input$I_summer %||% -2000, I_fall = input$I_fall %||% -2000, I_winter = input$I_winter %||% -2000,
      B_spring = input$B_spring %||% 0.002, B_summer = input$B_summer %||% 0.002, B_fall = input$B_fall %||% 0.002, B_winter = input$B_winter %||% 0.002,
      D_spring = input$D_spring %||% 15, D_summer = input$D_summer %||% 15, D_fall = input$D_fall %||% 0, D_winter = input$D_winter %||% 0,
      G_spring = input$G_spring %||% -1500, G_summer = input$G_summer %||% -1500, G_fall = input$G_fall %||% -1500, G_winter = input$G_winter %||% -1500,
      E_spring = input$E_spring %||% 0.003, E_summer = input$E_summer %||% 0.003, E_fall = input$E_fall %||% 0.003, E_winter = input$E_winter %||% 0.003,
      # Seasonal prices
      Ph_spring = input$Ph_spring %||% 3.0, Ph_summer = input$Ph_summer %||% 3.0, Ph_fall = input$Ph_fall %||% 3.0, Ph_winter = input$Ph_winter %||% 3.0,
      Pc_spring = input$Pc_spring %||% 22713, Pc_summer = input$Pc_summer %||% 5678, Pc_fall = input$Pc_fall %||% 0, Pc_winter = input$Pc_winter %||% 0,
      # Global parameters
      A = 100, B = input$B %||% 0.001, I = 50, D = 15, E = input$E %||% 0.002, G = 30
    )

    # Create schedule and locations
    schedule <- create_simple_schedule(input$n_years %||% 3)
    locations <- create_location_parameters(schedule, params)

    # Management costs and financial parameters
    mgmt_costs <- list(
      P_split = input$P_split %||% 25,
      P_merge = input$P_merge %||% 15,
      P_cull = input$P_cull %||% 10,
      annual_discount_rate = input$annual_discount_rate %||% 3
    )

    # Current management as starting point
    start_params <- c(
      input$cull_spring %||% 0, input$cull_summer %||% 0, input$cull_fall %||% 0, input$cull_winter %||% 5,
      input$adjust_spring %||% 10, input$adjust_summer %||% 5, input$adjust_fall %||% -5, input$adjust_winter %||% -10
    )

    # Run optimization with detailed error logging
    tryCatch(
      {
        cat("\n=== PRE-OPTIMIZATION DEBUG ===\n")
        cat("Starting parameters:", paste(start_params, collapse = ", "), "\n")
        cat("Lower bounds:", paste(c(rep(0, 4), rep(-100, 4)), collapse = ", "), "\n")
        cat("Upper bounds:", paste(c(rep(100, 4), rep(100, 4)), collapse = ", "), "\n")

        # Test the objective function once before optimization
        cat("Testing objective function with start params...\n")
        test_obj <- tryCatch(
          {
            optimize_management(
              start_params, locations, input$initial_colonies %||% 1000,
              input$initial_frames %||% 6000, input$P_feed %||% 2.0,
              mgmt_costs
            )
          },
          error = function(e) {
            cat("ERROR in objective function test:", e$message, "\n")
            traceback()
            return(NA)
          }
        )
        cat("Test objective value:", test_obj, "\n")

        if (is.na(test_obj) || !is.finite(test_obj)) {
          stop("Objective function fails with starting parameters")
        }

        opt_result <- optim(
          par = start_params,
          fn = function(params) {
            # Debug: Check parameter vector length right at wrapper entry
            if (length(params) != 8) {
              cat("ERROR: Wrapper function received", length(params), "parameters instead of 8\n")
              cat("Parameters:", paste(params, collapse = ", "), "\n")
              return(1e10)
            }
            tryCatch(
              {
                result <- optimize_management(
                  params, locations, input$initial_colonies %||% 1000,
                  input$initial_frames %||% 6000, input$P_feed %||% 2.0,
                  mgmt_costs
                )
                if (!is.finite(result)) {
                  cat("WARNING: Non-finite result:", result, "for params:", paste(params, collapse = ", "), "\n")
                  return(1e10)
                }
                return(result)
              },
              error = function(e) {
                cat("ERROR in objective function:", e$message, "\n")
                cat("Parameters:", paste(params, collapse = ", "), "\n")
                traceback()
                return(1e10)
              }
            )
          },
          method = "L-BFGS-B",
          lower = c(rep(0, 4), rep(-100, 4)), # Cull rates 0-100%, adjust rates -100% to 100%
          upper = c(rep(100, 4), rep(100, 4)),
          hessian = TRUE, # Request Hessian matrix for debugging
          control = list(trace = 1, REPORT = 1) # Add verbose output
        )

        # Debug output: Print optimization results including Hessian
        cat("\n=== OPTIMIZATION DEBUG OUTPUT ===\n")

        # Define parameter names for all debug output (needed throughout)
        param_names <- c(
          "cull_spring", "cull_summer", "cull_fall", "cull_winter",
          "adjust_spring", "adjust_summer", "adjust_fall", "adjust_winter"
        )

        cat("Convergence code:", opt_result$convergence)
        if (opt_result$convergence == 0) {
          cat(" (SUCCESS - Normal convergence)\n")
        } else if (opt_result$convergence == 1) {
          cat(" (WARNING - Max iterations reached)\n")
        } else if (opt_result$convergence == 51) {
          cat(" (WARNING - L-BFGS-B warning)\n")
        } else if (opt_result$convergence == 52) {
          cat(" (ERROR - L-BFGS-B error)\n")
        } else {
          cat(" (UNKNOWN CODE)\n")
        }
        cat("Number of function evaluations:", opt_result$counts[1], "\n")
        cat("Optimal value (negative profit):", opt_result$value, "\n")
        cat("Positive profit value:", -opt_result$value, "\n")
        cat("Discount rate used:", mgmt_costs$annual_discount_rate, "%\n")

        # Check for potential issues with optimization bounds
        cat("Parameter bounds check:\n")
        bounds_hit <- 0
        for (i in 1:8) { # Fixed: we have 8 parameters now, not 12
          if (opt_result$par[i] <= 0.01) {
            cat(sprintf("  %s at lower bound (%.3f)\n", param_names[i], opt_result$par[i]))
            bounds_hit <- bounds_hit + 1
          } else if (opt_result$par[i] >= 99.99) {
            cat(sprintf("  %s at upper bound (%.3f)\n", param_names[i], opt_result$par[i]))
            bounds_hit <- bounds_hit + 1
          }
        }
        if (bounds_hit == 0) cat("  No parameters at bounds\n")

        # Verify calculation by re-running simulation with optimal parameters
        optimal_mgmt <- list(
          cull_spring = opt_result$par[1], cull_summer = opt_result$par[2], cull_fall = opt_result$par[3], cull_winter = opt_result$par[4],
          adjust_spring = opt_result$par[5], adjust_summer = opt_result$par[6], adjust_fall = opt_result$par[7], adjust_winter = opt_result$par[8],
          P_split = mgmt_costs$P_split, P_merge = mgmt_costs$P_merge, P_cull = mgmt_costs$P_cull
        )

        verify_results <- run_beekeeping_simulation(
          locations, input$initial_colonies %||% 1000, input$initial_frames %||% 6000,
          input$P_feed %||% 2.0, optimal_mgmt
        )
        verify_df <- results_to_dataframe(verify_results)

        # Calculate PV same way as display
        annual_rate <- mgmt_costs$annual_discount_rate / 100
        quarterly_rate <- (1 + annual_rate)^(1 / 4) - 1
        verify_df$discount_factor <- sapply(1:nrow(verify_df), function(i) {
          1 / (1 + quarterly_rate)^(i - 1)
        })
        verify_df$pv_profit <- verify_df$profit * verify_df$discount_factor
        display_method_total <- sum(verify_df$pv_profit, na.rm = TRUE)

        cat("Verification (display method):", display_method_total, "\n")
        cat("Difference from optim value:", display_method_total - (-opt_result$value), "\n")

        cat("Optimal parameters:\n")
        for (i in 1:8) { # Fixed: use 8 parameters
          cat(sprintf("  %s: %.2f\n", param_names[i], opt_result$par[i]))
        }

        # Calculate numerical gradient at optimal point
        cat("\nNumerical gradient at optimum:\n")
        gradient <- numeric(8) # Fixed: use 8 parameters
        h <- 1e-6 # Small step size for numerical differentiation

        for (i in 1:8) { # Fixed: use 8 parameters
          # Forward difference
          params_plus <- opt_result$par
          params_plus[i] <- params_plus[i] + h

          params_minus <- opt_result$par
          params_minus[i] <- params_minus[i] - h

          # Calculate function values
          f_plus <- optimize_management(
            params_plus, locations, input$initial_colonies %||% 1000,
            input$initial_frames %||% 6000, input$P_feed %||% 2.0,
            mgmt_costs
          )

          f_minus <- optimize_management(
            params_minus, locations, input$initial_colonies %||% 1000,
            input$initial_frames %||% 6000, input$P_feed %||% 2.0,
            mgmt_costs
          )

          # Central difference formula
          gradient[i] <- (f_plus - f_minus) / (2 * h)
        }

        for (i in 1:min(8, length(param_names), length(gradient))) { # Safety check
          cat(sprintf("  ∇f/%s: %.6f\n", param_names[i], gradient[i]))
        }

        # Gradient norm (should be close to zero at optimum)
        grad_norm <- sqrt(sum(gradient^2))
        cat("Gradient norm (should be ~0):", format(grad_norm, scientific = TRUE), "\n")

        # Gradient analysis for convergence diagnostics
        large_gradients <- which(abs(gradient) > 1e-3)
        if (length(large_gradients) > 0) {
          cat("\nWARNING: Large gradients detected despite convergence!\n")
          for (idx in large_gradients) {
            if (idx <= length(param_names) && idx <= length(gradient)) { # Safety check
              cat(sprintf("  %s: gradient = %.6f (large!)\n", param_names[idx], gradient[idx]))
            }
          }
          cat("This suggests potential optimization problems.\n")
        }

        # Double-check objective function value
        obj_check <- optimize_management(
          opt_result$par, locations, input$initial_colonies %||% 1000,
          input$initial_frames %||% 6000, input$P_feed %||% 2.0,
          mgmt_costs
        )
        cat("Objective function verification:", obj_check, "\n")
        cat("Match with opt_result$value:", abs(obj_check - opt_result$value) < 1e-10, "\n")

        # Estimate Lagrange multipliers for box constraints
        cat("\nEstimated Lagrange multipliers (constraint shadow prices):\n")
        # Create bounds arrays matching the actual optimization
        lower_bounds <- c(rep(0, 4), rep(-100, 4)) # Cull: 0-100%, Adjust: -100% to 100%
        upper_bounds <- c(rep(100, 4), rep(100, 4))
        boundary_tolerance <- 0.5 # Tolerance for being "at" a boundary

        for (i in 1:8) { # Fixed: use 8 parameters
          param_value <- opt_result$par[i]
          cat(sprintf("  %s = %.3f: ", param_names[i], param_value))

          lambda_lower <- 0 # Lagrange multiplier for lower bound constraint
          lambda_upper <- 0 # Lagrange multiplier for upper bound constraint

          # Check if we're at the lower bound (parameter ≈ 0)
          if (param_value <= lower_bounds[i] + boundary_tolerance) {
            if (abs(gradient[i]) > 1e-6) { # Only report non-zero multipliers
              lambda_lower <- max(0, gradient[i]) # Multiplier should be non-negative
              cat(sprintf("λ_lower = %.6f (ACTIVE lower bound)\n", lambda_lower))
            } else {
              cat(sprintf("λ_lower = 0.000000 (at lower bound, gradient ≈ 0)\n"))
            }
          }
          # Check if we're at the upper bound (parameter ≈ 50)
          else if (param_value >= upper_bounds[i] - boundary_tolerance) {
            if (abs(gradient[i]) > 1e-6) { # Only report non-zero multipliers
              lambda_upper <- max(0, -gradient[i]) # Multiplier should be non-negative
              cat(sprintf("λ_upper = %.6f (ACTIVE upper bound)\n", lambda_upper))
            } else {
              cat(sprintf("λ_upper = 0.000000 (at upper bound, gradient ≈ 0)\n"))
            }
          }
          # Interior point - both multipliers should be zero
          else {
            cat(sprintf("λ_lower = 0.000000, λ_upper = 0.000000 (interior point, grad=%.6f)\n", gradient[i]))
          }
        }

        # Summary of active constraints
        active_lower <- which(abs(opt_result$par - lower_bounds) < boundary_tolerance)
        active_upper <- which(abs(opt_result$par - upper_bounds) < boundary_tolerance)

        cat(sprintf("\nActive constraints summary:\n"))
        cat(sprintf("  Lower bounds (=0): %d parameters\n", length(active_lower)))
        if (length(active_lower) > 0) {
          cat(sprintf("    %s\n", paste(param_names[active_lower], collapse = ", ")))
        }
        cat(sprintf("  Upper bounds (=50): %d parameters\n", length(active_upper)))
        if (length(active_upper) > 0) {
          cat(sprintf("    %s\n", paste(param_names[active_upper], collapse = ", ")))
        }
        cat(sprintf("  Interior points: %d parameters\n", 12 - length(active_lower) - length(active_upper)))

        if (!is.null(opt_result$hessian)) {
          cat("\nHessian matrix (12x12):\n")
          rownames(opt_result$hessian) <- param_names
          colnames(opt_result$hessian) <- param_names
          print(opt_result$hessian)

          # Additional Hessian analysis
          cat("\nHessian eigenvalues (for convexity check):\n")
          eigenvals <- eigen(opt_result$hessian, only.values = TRUE)$values
          for (i in 1:length(eigenvals)) {
            cat(sprintf("  λ%d: %s\n", i, format(eigenvals[i], scientific = TRUE)))
          }

          positive_eigs <- sum(eigenvals > 1e-8)
          negative_eigs <- sum(eigenvals < -1e-8)
          zero_eigs <- sum(abs(eigenvals) <= 1e-8)

          cat(sprintf(
            "Eigenvalue summary: %d positive, %d negative, %d zero\n",
            positive_eigs, negative_eigs, zero_eigs
          ))

          if (negative_eigs > 0) {
            cat("*** CRITICAL PROBLEM: NEGATIVE EIGENVALUES DETECTED ***\n")
            cat("This indicates a SADDLE POINT, not a minimum!\n")
            cat("The optimization result is NOT RELIABLE.\n")
            cat("Negative eigenvalue directions:\n")
            eigenvecs <- eigen(opt_result$hessian)$vectors
            neg_idx <- which(eigenvals < -1e-8)
            for (idx in neg_idx) {
              cat(sprintf("  Eigenvalue %.6f corresponds to direction involving:\n", eigenvals[idx]))
              vec_importance <- abs(eigenvecs[, idx])
              top_params <- order(vec_importance, decreasing = TRUE)[1:3]
              for (p in top_params) {
                cat(sprintf("    %s (weight: %.3f)\n", param_names[p], vec_importance[p]))
              }
            }
          } else if (all(eigenvals > 1e-8)) {
            cat("✓ All eigenvalues positive: TRUE LOCAL MINIMUM\n")
          } else {
            cat("? Some near-zero eigenvalues: boundary of convex region\n")
          }

          # Condition number (numerical stability)
          if (min(eigenvals) > 1e-12) {
            cond_num <- max(eigenvals) / min(eigenvals)
            cat("Condition number:", format(cond_num, scientific = TRUE))
            if (cond_num > 1e12) {
              cat(" (POORLY CONDITIONED - numerical issues likely)\n")
            } else if (cond_num > 1e8) {
              cat(" (ill-conditioned)\n")
            } else {
              cat(" (well-conditioned)\n")
            }
          } else {
            cat("Condition number: infinite (singular Hessian)\n")
          }
        } else {
          cat("\nHessian matrix not available\n")
        }
        # Final convergence assessment
        cat("\n=== CONVERGENCE QUALITY ASSESSMENT ===\n")
        reliable_solution <- TRUE
        issues <- character(0)

        if (opt_result$convergence != 0) {
          reliable_solution <- FALSE
          issues <- c(issues, "Non-zero convergence code")
        }

        if (grad_norm > 1e-3) {
          reliable_solution <- FALSE
          issues <- c(issues, "Large gradient norm")
        }

        if (!is.null(opt_result$hessian)) {
          eigenvals <- eigen(opt_result$hessian, only.values = TRUE)$values
          if (any(eigenvals < -1e-8)) {
            reliable_solution <- FALSE
            issues <- c(issues, "Saddle point detected")
          }
        }

        if (reliable_solution) {
          cat("✓ SOLUTION APPEARS RELIABLE\n")
        } else {
          cat("✗ SOLUTION HAS ISSUES:\n")
          for (issue in issues) {
            cat(sprintf("  - %s\n", issue))
          }
          cat("Consider: different starting point, tighter tolerances, or model debugging\n")
        }

        cat("=== END OPTIMIZATION DEBUG ===\n\n")

        # Update sliders with optimal values
        opt <- round(opt_result$par, 1)
        updateSliderInput(session, "cull_spring", value = opt[1])
        updateSliderInput(session, "cull_summer", value = opt[2])
        updateSliderInput(session, "cull_fall", value = opt[3])
        updateSliderInput(session, "cull_winter", value = opt[4])
        updateSliderInput(session, "adjust_spring", value = opt[5])
        updateSliderInput(session, "adjust_summer", value = opt[6])
        updateSliderInput(session, "adjust_fall", value = opt[7])
        updateSliderInput(session, "adjust_winter", value = opt[8])

        # Success notification
        showNotification(paste("Optimization completed! Optimal profit: $", format(round(-opt_result$value), big.mark = ",")), type = "message", duration = 10)
      },
      error = function(e) {
        showNotification(paste("Optimization failed:", e$message), type = "error", duration = 10)
      }
    )
  })

  # Reset parameters to defaults
  observeEvent(input$reset_params, {
    # Simulation setup
    updateNumericInput(session, "n_years", value = 3)
    updateNumericInput(session, "initial_colonies", value = 1000)
    updateNumericInput(session, "initial_frames", value = 6000)

    # Key parameters
    updateSliderInput(session, "omega", value = 0.5)
    updateSliderInput(session, "theta", value = 0.3)
    updateSliderInput(session, "alpha", value = 0.1)
    updateSliderInput(session, "delta", value = 0.05)

    # Economic parameters
    updateNumericInput(session, "Ph", value = 3.0)
    updateNumericInput(session, "P_feed", value = 2.0)
    updateNumericInput(session, "Cost", value = 50)
    updateNumericInput(session, "Pc", value = 0.5)
    updateNumericInput(session, "P_split", value = 25)
    updateNumericInput(session, "P_merge", value = 15)
    updateNumericInput(session, "P_cull", value = 10)

    # Management parameters
    updateSliderInput(session, "cull_spring", value = 0)
    updateSliderInput(session, "cull_summer", value = 0)
    updateSliderInput(session, "cull_fall", value = 0)
    updateSliderInput(session, "cull_winter", value = 5)

    updateSliderInput(session, "split_spring", value = 10)
    updateSliderInput(session, "split_summer", value = 5)
    updateSliderInput(session, "split_fall", value = 0)
    updateSliderInput(session, "split_winter", value = 0)

    updateSliderInput(session, "merge_spring", value = 0)
    updateSliderInput(session, "merge_summer", value = 0)
    updateSliderInput(session, "merge_fall", value = 5)
    updateSliderInput(session, "merge_winter", value = 10)

    # Management costs
    updateNumericInput(session, "P_split", value = 20)
    updateNumericInput(session, "P_merge", value = 20)
    updateNumericInput(session, "P_cull", value = 5)

    # Advanced parameters - seasonal factors
    updateSliderInput(session, "spring_growth_factor", value = 1.0)
    updateSliderInput(session, "spring_loss_factor", value = 1.0)
    updateSliderInput(session, "summer_growth_factor", value = 1.0)
    updateSliderInput(session, "summer_loss_factor", value = 1.0)
    updateSliderInput(session, "fall_growth_factor", value = 0.3)
    updateSliderInput(session, "fall_loss_factor", value = 1.2)
    updateSliderInput(session, "winter_growth_factor", value = 0.3)
    updateSliderInput(session, "winter_loss_factor", value = 2.0)

    # Production parameters
    updateNumericInput(session, "A", value = 300)
    updateNumericInput(session, "B", value = 0.001)
    updateNumericInput(session, "D", value = 500)
    updateNumericInput(session, "E", value = 0.002)
    updateNumericInput(session, "gamma", value = 1.5)
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
          "n_years", "annual_discount_rate", "initial_colonies", "initial_frames",
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
          "Pc_spring", "Pc_summer", "Pc_fall", "Pc_winter"
        ),
        Value = c(
          # Simulation setup
          input$n_years %||% 5, input$annual_discount_rate %||% 3, input$initial_colonies %||% 1000, input$initial_frames %||% 6000,
          # Seasonal biological parameters
          input$alpha_spring %||% 0.1, input$alpha_summer %||% 0.1, input$alpha_fall %||% 0.1, input$alpha_winter %||% 0.1,
          input$delta_spring %||% 0.05, input$delta_summer %||% 0.05, input$delta_fall %||% 0.05, input$delta_winter %||% 0.05,
          input$gamma_spring %||% 0.5, input$gamma_summer %||% 0.5, input$gamma_fall %||% 0.5, input$gamma_winter %||% 0.5,
          input$omega_spring %||% 0.5, input$omega_summer %||% 0.5, input$omega_fall %||% 0.5, input$omega_winter %||% 0.5,
          input$theta_spring %||% 0.3, input$theta_summer %||% 0.3, input$theta_fall %||% 0.3, input$theta_winter %||% 0.3,
          # Economic parameters
          input$Cost %||% 50, input$P_feed %||% 0.02, input$P_split %||% 25, input$P_merge %||% 10, input$P_cull %||% 10,
          # Management parameters
          input$cull_spring %||% 0, input$cull_summer %||% 0, input$cull_fall %||% 0, input$cull_winter %||% 5,
          input$adjust_spring %||% 30, input$adjust_summer %||% 20, input$adjust_fall %||% -5, input$adjust_winter %||% -10,
          # Seasonal forage production parameters
          input$A_spring %||% 100, input$A_summer %||% 100, input$A_fall %||% 0, input$A_winter %||% 0,
          input$B_spring %||% 2000, input$B_summer %||% 2000, input$B_fall %||% 2000, input$B_winter %||% 2000,
          # Seasonal crop production parameters
          input$D_spring %||% 3, input$D_summer %||% 15, input$D_fall %||% 0, input$D_winter %||% 0,
          input$E_spring %||% 2000, input$E_summer %||% 2000, input$E_fall %||% 2000, input$E_winter %||% 2000,
          # Seasonal prices
          input$Ph_spring %||% 3.0, input$Ph_summer %||% 3.0, input$Ph_fall %||% 3.0, input$Ph_winter %||% 0,
          input$Pc_spring %||% 100, input$Pc_summer %||% 100, input$Pc_fall %||% 0, input$Pc_winter %||% 0
        ),
        stringsAsFactors = FALSE
      )

      # Add timestamp and description
      settings_data$Timestamp <- Sys.time()
      settings_data$Description <- c(
        # Simulation setup
        "Years to simulate", "Annual discount rate (%)", "Initial number of colonies", "Initial number of frames",
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
        "Spring crop price ($/lbs)", "Summer crop price ($/lbs)", "Fall crop price ($/lbs)", "Winter crop price ($/lbs)"
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
  create_forage_plot <- function(A, B, I, season_name, season_color) {
    # Get current simulation data to determine colony range
    results_df <- simulation_results()
    if (nrow(results_df) > 0) {
      avg_frames_per_colony <- mean(results_df$frames_per_colony, na.rm = TRUE)
      max_colonies <- max(results_df$colonies_end, na.rm = TRUE)
      avg_colonies <- mean(results_df$colonies_end, na.rm = TRUE)
      
      # Get forager counts for this specific season by year
      season_data <- results_df %>%
        filter(season == season_name) %>%
        select(year, total_foragers) %>%
        distinct()
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
  create_crop_plot <- function(D, E, G, season_name, season_color) {
    # Get current simulation data to determine colony range
    results_df <- simulation_results()
    if (nrow(results_df) > 0) {
      avg_frames_per_colony <- mean(results_df$frames_per_colony, na.rm = TRUE)
      max_colonies <- max(results_df$colonies_end, na.rm = TRUE)
      avg_colonies <- mean(results_df$colonies_end, na.rm = TRUE)
      
      # Get forager counts for this specific season by year
      season_data <- results_df %>%
        filter(season == season_name) %>%
        select(year, total_foragers) %>%
        distinct()
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

  # Spring plots
  output$spring_forage_plot <- renderPlot({
    create_forage_plot(
      input$A_spring %||% 70, input$B_spring %||% 2100,
      input$I_spring %||% -1, "Spring", "#2E8B57"
    )
  })

  output$spring_crop_plot <- renderPlot({
    create_crop_plot(
      input$D_spring %||% 25, input$E_spring %||% 32000,
      input$G_spring %||% -1, "Spring", "#2E8B57"
    )
  })

  # Summer plots
  output$summer_forage_plot <- renderPlot({
    create_forage_plot(
      input$A_summer %||% 20, input$B_summer %||% 60000,
      input$I_summer %||% -1, "Summer", "#FFD700"
    )
  })

  output$summer_crop_plot <- renderPlot({
    create_crop_plot(
      input$D_summer %||% 17, input$E_summer %||% 2700,
      input$G_summer %||% -1, "Summer", "#FFD700"
    )
  })

  # Fall plots
  output$fall_forage_plot <- renderPlot({
    create_forage_plot(
      input$A_fall %||% 0, input$B_fall %||% 2000,
      input$I_fall %||% -0.5, "Fall", "#FF8C00"
    )
  })

  output$fall_crop_plot <- renderPlot({
    create_crop_plot(
      input$D_fall %||% 0, input$E_fall %||% 2000,
      input$G_fall %||% -0.5, "Fall", "#FF8C00"
    )
  })

  # Winter plots
  output$winter_forage_plot <- renderPlot({
    create_forage_plot(
      input$A_winter %||% 0, input$B_winter %||% 2000,
      input$I_winter %||% -0.5, "Winter", "#4682B4"
    )
  })

  output$winter_crop_plot <- renderPlot({
    create_crop_plot(
      input$D_winter %||% 0, input$E_winter %||% 2000,
      input$G_winter %||% -0.5, "Winter", "#4682B4"
    )
  })

  # Forager share plots for each season
  create_forager_share_plot <- function(omega, theta, season_name, season_color) {
    # Get simulation data for this season's frames per colony by year
    results_df <- simulation_results()
    if (nrow(results_df) > 0) {
      season_data <- results_df %>%
        filter(season == season_name) %>%
        select(year, frames_per_colony) %>%
        distinct()
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
          size = 0.6,
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

  output$spring_forager_plot <- renderPlot({
    create_forager_share_plot(
      input$omega_spring %||% 0.5,
      input$theta_spring %||% 0.3,
      "Spring", "#2E8B57"
    )
  })

  output$summer_forager_plot <- renderPlot({
    create_forager_share_plot(
      input$omega_summer %||% 0.5,
      input$theta_summer %||% 0.3,
      "Summer", "#FFD700"
    )
  })

  output$fall_forager_plot <- renderPlot({
    create_forager_share_plot(
      input$omega_fall %||% 0.5,
      input$theta_fall %||% 0.3,
      "Fall", "#FF8C00"
    )
  })

  output$winter_forager_plot <- renderPlot({
    create_forager_share_plot(
      input$omega_winter %||% 0.5,
      input$theta_winter %||% 0.3,
      "Winter", "#4682B4"
    )
  })
}

# =============================================================================
# RUN THE APP
# =============================================================================

shinyApp(ui = ui, server = server)
