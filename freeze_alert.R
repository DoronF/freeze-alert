#!/usr/bin/env Rscript

# Freeze Alert System for Toronto Midtown
# Alerts when temperature drops to/below 0°C
# Tracks days since precipitation for ice risk assessment

library(httr)
library(jsonlite)

# ============================================================================
# CONFIGURATION - Set these in GitHub Secrets
# ============================================================================
OPENWEATHER_API_KEY <- Sys.getenv("OPENWEATHER_API_KEY")
NTFY_TOPIC <- Sys.getenv("NTFY_TOPIC")  # e.g., "myhouse-freeze-alert-x7k2"

# Location: Toronto Midtown
LAT <- 43.65
LON <- -79.38

# State file for tracking
STATE_FILE <- "freeze_state.json"

# ============================================================================
# HELPER FUNCTIONS
# ============================================================================

get_weather <- function() {
  url <- sprintf(
    "https://api.openweathermap.org/data/2.5/weather?lat=%s&lon=%s&appid=%s&units=metric",
    LAT, LON, OPENWEATHER_API_KEY
  )
  
  response <- GET(url)
  if (status_code(response) != 200) {
    stop("Weather API error: ", status_code(response))
  }
  
  content(response, as = "parsed")
}

load_state <- function() {
  if (file.exists(STATE_FILE)) {
    fromJSON(STATE_FILE)
  } else {
    list(
      last_alert_time = NULL,
      temp_was_above_zero = TRUE,
      last_precip_time = NULL
    )
  }
}

save_state <- function(state) {
  write(toJSON(state, auto_unbox = TRUE), STATE_FILE)
}

send_alert <- function(message, priority = "default") {
  url <- sprintf("https://ntfy.sh/%s", NTFY_TOPIC)
  
  response <- POST(
    url,
    body = message,
    encode = "raw",
    add_headers(
      Title = "❄️ Freeze Alert",
      Priority = priority,
      Tags = "thermometer,warning"
    )
  )
  
  if (status_code(response) == 200) {
    cat("Alert sent successfully\n")
    return(TRUE)
  } else {
    cat("Alert failed:", status_code(response), "\n")
    return(FALSE)
  }
}

days_since_precip <- function(last_precip_time) {
  if (is.null(last_precip_time)) {
    return(NULL)
  }
  
  days <- as.numeric(difftime(Sys.time(), 
                               as.POSIXct(last_precip_time, tz = "UTC"), 
                               units = "days"))
  round(days, 1)
}

# ============================================================================
# MAIN LOGIC
# ============================================================================

main <- function() {
  cat("=== Freeze Alert Check ===\n")
  cat("Time:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
  
  # Get current weather
  weather <- get_weather()
  current_temp <- weather$main$temp
  
  # Check for recent precipitation
  has_precip <- FALSE
  if (!is.null(weather$rain$`1h`) && weather$rain$`1h` > 0) {
    has_precip <- TRUE
  }
  if (!is.null(weather$snow$`1h`) && weather$snow$`1h` > 0) {
    has_precip <- TRUE
  }
  
  cat("Current temp:", current_temp, "°C\n")
  cat("Precipitation now:", has_precip, "\n")
  
  # Load state
  state <- load_state()
  
  # Update precipitation tracking
  if (has_precip) {
    state$last_precip_time <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    cat("Precipitation detected! Updated last_precip_time\n")
  }
  
  # Check if we should alert
  should_alert <- FALSE
  
  if (current_temp <= 0 && state$temp_was_above_zero) {
    # Temperature just dropped to/below freezing
    should_alert <- TRUE
    state$temp_was_above_zero <- FALSE
    
    # Build alert message
    days_dry <- days_since_precip(state$last_precip_time)
    
    if (is.null(days_dry)) {
      risk_msg <- "No recent precipitation data"
    } else if (days_dry < 0.5) {
      risk_msg <- sprintf("Rain/snow within last 12 hours - HIGH ICE RISK ⚠️")
    } else if (days_dry < 1) {
      risk_msg <- sprintf("Precipitation %.1f days ago - MODERATE RISK", days_dry)
    } else {
      risk_msg <- sprintf("Dry for %.1f days - lower ice risk", days_dry)
    }
    
    message <- sprintf(
      "Temperature at freezing: %.1f°C\n\n%s\n\nTime to salt the sidewalk!",
      current_temp,
      risk_msg
    )
    
    cat("\n*** SENDING ALERT ***\n")
    cat(message, "\n")
    
    if (send_alert(message, priority = "high")) {
      state$last_alert_time <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    }
    
  } else if (current_temp > 0) {
    # Temperature is above freezing - reset flag
    if (!state$temp_was_above_zero) {
      cat("Temperature back above freezing - resetting alert state\n")
    }
    state$temp_was_above_zero <- TRUE
  } else {
    # Temperature is below zero but we already alerted
    cat("Temperature still below freezing (already alerted)\n")
  }
  
  # Save state
  save_state(state)
  
  cat("\nState saved. Check complete.\n")
}

# ============================================================================
# RUN
# ============================================================================

if (!interactive()) {
  tryCatch({
    main()
  }, error = function(e) {
    cat("ERROR:", e$message, "\n")
    quit(status = 1)
  })
}
