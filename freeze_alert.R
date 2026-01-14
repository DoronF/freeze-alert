#!/usr/bin/env Rscript

# Freeze Alert System for Toronto Midtown
# Two-alert system:
#   1. Warning when freeze forecast in 1-6 hours
#   2. Alert when temperature actually hits 0°C
# Tracks days since precipitation for ice risk assessment

library(httr)
library(jsonlite)

# ============================================================================
# CONFIGURATION
# ============================================================================
OPENWEATHER_API_KEY <- Sys.getenv("OPENWEATHER_API_KEY")
NTFY_TOPIC <- Sys.getenv("NTFY_TOPIC")

LAT <- 43.65
LON <- -79.38

STATE_FILE <- "freeze_state.json"
LOG_FILE <- "freeze_alert.log"

TEST_MODE <- FALSE

# ============================================================================
# LOGGING
# ============================================================================

log_msg <- function(msg) {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
  log_line <- paste0("[", timestamp, "] ", msg, "\n")
  cat(log_line)  # Print to console
  cat(log_line, file = LOG_FILE, append = TRUE)  # Append to log file
}

log_state <- function(state) {
  log_msg(paste("STATE:", toJSON(state, auto_unbox = TRUE)))
}

# ============================================================================
# HELPER FUNCTIONS
# ============================================================================

get_current_weather <- function() {
  url <- sprintf(
    "https://api.openweathermap.org/data/2.5/weather?lat=%s&lon=%s&appid=%s&units=metric",
    LAT, LON, OPENWEATHER_API_KEY
  )
  
  response <- GET(url)
  if (status_code(response) != 200) {
    stop("Current weather API error: ", status_code(response))
  }
  
  content(response, as = "parsed")
}

get_forecast <- function() {
  url <- sprintf(
    "https://api.openweathermap.org/data/2.5/forecast?lat=%s&lon=%s&appid=%s&units=metric",
    LAT, LON, OPENWEATHER_API_KEY
  )
  
  response <- GET(url)
  if (status_code(response) != 200) {
    stop("Forecast API error: ", status_code(response))
  }
  
  content(response, as = "parsed")
}

load_state <- function() {
  if (file.exists(STATE_FILE)) {
    state <- fromJSON(STATE_FILE)
    
    # Fix empty objects that should be NULL
    if (length(state$last_alert_warning) == 0) state$last_alert_warning <- NULL
    if (length(state$last_alert_freeze) == 0) state$last_alert_freeze <- NULL
    if (length(state$last_precip_time) == 0) state$last_precip_time <- NULL
    
    log_msg("Loaded existing state from file")
    log_state(state)
    return(state)
  } else {
    state <- list(
      temp_was_above_zero = TRUE,
      last_alert_warning = NULL,
      last_alert_freeze = NULL,
      last_precip_time = NULL
    )
    log_msg("Created new state (no existing file)")
    log_state(state)
    return(state)
  }
}

save_state <- function(state) {
  write(toJSON(state, auto_unbox = TRUE, null = "null"), STATE_FILE)
  log_msg("State saved to file")
  log_state(state)
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
    log_msg("Alert sent successfully")
    return(TRUE)
  } else {
    log_msg(paste("Alert failed with status:", status_code(response)))
    return(FALSE)
  }
}

days_since_precip <- function(last_precip_time) {
  if (is.null(last_precip_time)) {
    return(NULL)
  }
  
  # Parse with America/Toronto timezone
  precip_time <- as.POSIXct(last_precip_time, format = "%Y-%m-%d %H:%M:%S", tz = "America/Toronto")
  days <- as.numeric(difftime(Sys.time(), precip_time, units = "days"))
  round(days, 1)
}

hours_until <- function(timestamp) {
  # timestamp is Unix epoch from API - convert to Toronto time
  future_time <- as.POSIXct(timestamp, origin = "1970-01-01", tz = "America/Toronto")
  hours <- as.numeric(difftime(future_time, Sys.time(), units = "hours"))
  round(hours, 1)
}

hours_since <- function(timestamp_str) {
  if (is.null(timestamp_str) || length(timestamp_str) == 0) {
    return(NULL)
  }
  # Parse with America/Toronto timezone
  past_time <- as.POSIXct(timestamp_str, format = "%Y-%m-%d %H:%M:%S", tz = "America/Toronto")
  hours <- as.numeric(difftime(Sys.time(), past_time, units = "hours"))
  round(hours, 1)
}

# ============================================================================
# MAIN LOGIC
# ============================================================================

main <- function() {
  Sys.setenv(TZ = "America/Toronto")
  
  log_msg("=== FREEZE ALERT CHECK START ===")
  
  # Get current weather
  current <- get_current_weather()
  current_temp <- current$main$temp
  
  log_msg(paste("Current temperature:", current_temp, "°C"))
  
  # Check for current precipitation
  has_precip <- FALSE
  if (!is.null(current$rain$`1h`) && current$rain$`1h` > 0) {
    has_precip <- TRUE
  }
  if (!is.null(current$snow$`1h`) && current$snow$`1h` > 0) {
    has_precip <- TRUE
  }
  
  log_msg(paste("Precipitation now:", has_precip))
  
  # Get forecast
  forecast <- get_forecast()
  
  # Load state
  state <- load_state()
  
  # Update precipitation tracking
  if (has_precip) {
    state$last_precip_time <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    log_msg("Precipitation detected - updated last_precip_time")
  }
  
  # Check forecast for precipitation
  for (entry in forecast$list[1:min(8, length(forecast$list))]) {
    if (!is.null(entry$rain$`3h`) && entry$rain$`3h` > 0) {
      if (is.null(state$last_precip_time)) {
        state$last_precip_time <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
        log_msg("Forecast shows rain - updated last_precip_time")
      }
      break
    }
    if (!is.null(entry$snow$`3h`) && entry$snow$`3h` > 0) {
      if (is.null(state$last_precip_time)) {
        state$last_precip_time <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
        log_msg("Forecast shows snow - updated last_precip_time")
      }
      break
    }
  }
  
  # ============================================================================
  # TEST MODE
  # ============================================================================
  if (TEST_MODE) {
    days_dry <- days_since_precip(state$last_precip_time)
    
    freeze_forecast <- NULL
    for (entry in forecast$list[1:min(4, length(forecast$list))]) {
      if (entry$main$temp <= 0) {
        freeze_forecast <- entry
        break
      }
    }
    
    if (is.null(days_dry)) {
      precip_msg <- "No recent precipitation data"
    } else if (days_dry < 0.5) {
      precip_msg <- "Rain/snow within last 12 hours"
    } else if (days_dry < 1) {
      precip_msg <- sprintf("Precipitation %.1f days ago", days_dry)
    } else {
      precip_msg <- sprintf("Dry for %.1f days", days_dry)
    }
    
    if (!is.null(freeze_forecast)) {
      hrs <- hours_until(freeze_forecast$dt)
      forecast_msg <- sprintf("\nFreeze forecast in ~%.1f hours", hrs)
    } else {
      forecast_msg <- "\nNo freeze in next 12 hours"
    }
    
    message <- sprintf(
      "🌡️ Test Mode\n\nCurrent: %.1f°C\n%s%s",
      current_temp,
      precip_msg,
      forecast_msg
    )
    
    log_msg("TEST MODE: Sending test notification")
    send_alert(message, priority = "default")
    
    save_state(state)
    log_msg("=== FREEZE ALERT CHECK END (TEST MODE) ===")
    return()
  }
  
  # ============================================================================
  # PRODUCTION MODE
  # ============================================================================
  
  build_precip_msg <- function(days_dry) {
    if (is.null(days_dry)) {
      return("No recent precipitation data")
    } else if (days_dry < 0.5) {
      return("Rain/snow within last 12 hours - HIGH ICE RISK ⚠️")
    } else if (days_dry < 1) {
      return(sprintf("Precipitation %.1f days ago - MODERATE RISK", days_dry))
    } else {
      return(sprintf("Dry for %.1f days - lower ice risk", days_dry))
    }
  }
  
  # ============================================================================
  # STEP 1: Clean up expired warnings (24-hour TTL)
  # ============================================================================
  if (!is.null(state$last_alert_warning)) {
    hrs_since_warning <- hours_since(state$last_alert_warning)
    if (!is.null(hrs_since_warning) && hrs_since_warning > 24) {
      log_msg(paste("Warning expired after", hrs_since_warning, "hours - resetting"))
      state$last_alert_warning <- NULL
    }
  }
  
  # ============================================================================
  # STEP 2: Determine current temperature state
  # ============================================================================
  currently_frozen <- current_temp <= 0
  log_msg(paste("Currently frozen:", currently_frozen))
  log_msg(paste("Was above zero (from state):", state$temp_was_above_zero))
  
  # ============================================================================
  # STEP 3: Check if we should send FREEZE WARNING (forecast-based)
  # ============================================================================
  # Only look for freeze warnings if:
  # - Temperature is currently ABOVE zero
  # - We haven't sent a warning yet (or it expired)
  
  if (!currently_frozen && is.null(state$last_alert_warning)) {
    log_msg("Checking forecast for freeze warning...")
    
    freeze_found <- NULL
    for (entry in forecast$list[1:min(3, length(forecast$list))]) {
      hrs_until_entry <- hours_until(entry$dt)
      
      if (entry$main$temp <= 0 && hrs_until_entry >= 1 && hrs_until_entry <= 6) {
        freeze_found <- list(temp = entry$main$temp, hours = hrs_until_entry)
        log_msg(paste("Freeze forecast found:", hrs_until_entry, "hours away at", freeze_found$temp, "°C"))
        break
      }
    }
    
    if (!is.null(freeze_found)) {
      days_dry <- days_since_precip(state$last_precip_time)
      risk_msg <- build_precip_msg(days_dry)
      
      message <- sprintf(
        "❄️ FREEZE WARNING\n\nCurrent temp: %.1f°C\n\nForecast to drop below freezing in approximately %.0f hours\n\nForecast: %.1f°C\n\n%s\n\nPrepare to salt the sidewalk!",
        current_temp,
        freeze_found$hours,
        freeze_found$temp,
        risk_msg
      )
      
      log_msg("SENDING FREEZE WARNING")
      if (send_alert(message, priority = "high")) {
        state$last_alert_warning <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      }
    } else {
      log_msg("No freeze in forecast (1-6 hour window)")
    }
  } else if (!currently_frozen && !is.null(state$last_alert_warning)) {
    log_msg("Freeze warning already sent - suppressing repeat")
  }
  
  # ============================================================================
  # STEP 4: Check if we should send FREEZE ALERT (actual freeze happening)
  # ============================================================================
  # Only send freeze alert if:
  # - Temperature is currently at/below zero
  # - We were previously above zero (legitimate freeze event, not script startup)
  
  if (currently_frozen && state$temp_was_above_zero) {
    log_msg("Temperature just dropped below freezing!")
    
    days_dry <- days_since_precip(state$last_precip_time)
    risk_msg <- build_precip_msg(days_dry)
    
    message <- sprintf(
      "❄️ FREEZE ALERT - ACT NOW\n\nCurrent temperature: %.1f°C\n\n%s\n\nTime to salt the sidewalk!",
      current_temp,
      risk_msg
    )
    
    log_msg("SENDING FREEZE ALERT")
    if (send_alert(message, priority = "urgent")) {
      state$last_alert_freeze <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    }
    
    # Mark that we're now frozen
    state$temp_was_above_zero <- FALSE
    
  } else if (currently_frozen && !state$temp_was_above_zero) {
    log_msg("Still frozen - freeze alert already sent, suppressing")
  } else if (!currently_frozen && !state$temp_was_above_zero) {
    # Temperature went from frozen back to above zero - RESET for next cycle
    log_msg("Temperature rose above freezing - RESETTING all alerts for next freeze cycle")
    state$temp_was_above_zero <- TRUE
    state$last_alert_warning <- NULL
    state$last_alert_freeze <- NULL
  } else {
    # currently not frozen and was above zero - normal monitoring state
    log_msg("Normal monitoring - temp above zero, waiting for freeze forecast")
  }
  
  # ============================================================================
  # STEP 5: Save state and finish
  # ============================================================================
  save_state(state)
  log_msg("=== FREEZE ALERT CHECK END ===\n")
}

# ============================================================================
# RUN
# ============================================================================

if (!interactive()) {
  tryCatch({
    main()
  }, error = function(e) {
    log_msg(paste("ERROR:", e$message))
    cat("ERROR:", e$message, "\n")
    quit(status = 1)
  })
}
