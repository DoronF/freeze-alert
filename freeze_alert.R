#!/usr/bin/env Rscript

# Freeze Alert System for Toronto Midtown
# Two-alert system:
#   1. ~3 hours before freeze (forecast-based)
#   2. At 0°C (actual temperature)
# Tracks days since precipitation for ice risk assessment

library(httr)
library(jsonlite)

# ============================================================================
# CONFIGURATION - Set these in GitHub Secrets
# ============================================================================
OPENWEATHER_API_KEY <- Sys.getenv("OPENWEATHER_API_KEY")
NTFY_TOPIC <- Sys.getenv("NTFY_TOPIC")

# Location: Toronto Midtown
LAT <- 43.65
LON <- -79.38

# State file for tracking
STATE_FILE <- "freeze_state.json"

# TEST MODE: Set to TRUE to send temp update every run, FALSE for freeze alerts only
TEST_MODE <- FALSE

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
    fromJSON(STATE_FILE)
  } else {
    list(
      last_alert_3hr = NULL,
      last_alert_freeze = NULL,
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

hours_until <- function(timestamp) {
  hours <- as.numeric(difftime(as.POSIXct(timestamp, origin = "1970-01-01", tz = "UTC"),
                               Sys.time(),
                               units = "hours"))
  round(hours, 1)
}

# ============================================================================
# MAIN LOGIC
# ============================================================================

main <- function() {
  Sys.setenv(TZ = "America/Toronto")
  
  cat("=== Freeze Alert Check ===\n")
  cat("Time:", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"), "\n")
  
  # Get current weather
  current <- get_current_weather()
  current_temp <- current$main$temp
  
  # Check for current precipitation
  has_precip <- FALSE
  if (!is.null(current$rain$`1h`) && current$rain$`1h` > 0) {
    has_precip <- TRUE
  }
  if (!is.null(current$snow$`1h`) && current$snow$`1h` > 0) {
    has_precip <- TRUE
  }
  
  cat("Current temp:", current_temp, "°C\n")
  cat("Precipitation now:", has_precip, "\n")
  
  # Get forecast
  forecast <- get_forecast()
  
  # Load state
  state <- load_state()
  
  # Update precipitation tracking
  if (has_precip) {
    state$last_precip_time <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
    cat("Precipitation detected! Updated last_precip_time\n")
  }
  
  # Check forecast for precipitation
  for (entry in forecast$list[1:min(8, length(forecast$list))]) {
    if (!is.null(entry$rain$`3h`) && entry$rain$`3h` > 0) {
      if (is.null(state$last_precip_time)) {
        state$last_precip_time <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
        cat("Forecast shows precipitation - updating last_precip_time\n")
      }
      break
    }
    if (!is.null(entry$snow$`3h`) && entry$snow$`3h` > 0) {
      if (is.null(state$last_precip_time)) {
        state$last_precip_time <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
        cat("Forecast shows precipitation - updating last_precip_time\n")
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
      precip_msg <- sprintf("Rain/snow within last 12 hours")
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
      "🌡️ Test Mode\n\nCurrent: %.1f°C\n%s%s\n\n(Will alert at freeze when test mode disabled)",
      current_temp,
      precip_msg,
      forecast_msg
    )
    
    cat("\n*** SENDING TEST NOTIFICATION ***\n")
    cat(message, "\n")
    send_alert(message, priority = "default")
    
    save_state(state)
    cat("\nTest mode active. Check complete.\n")
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
  
  # ALERT 1: Forecast warning
  freeze_in_3hrs <- NULL
  if (current_temp > 0 && is.null(state$last_alert_3hr)) {
    for (entry in forecast$list[1:min(3, length(forecast$list))]) {
      hrs_until_entry <- hours_until(entry$dt)
      
      if (entry$main$temp <= 0 && hrs_until_entry >= 1 && hrs_until_entry <= 6) {
        freeze_in_3hrs <- list(temp = entry$main$temp, hours = hrs_until_entry)
        cat("Found freeze forecast:", hrs_until_entry, "hours away at", freeze_in_3hrs$temp, "°C\n")
        break
      }
    }
  }
  
  if (!is.null(freeze_in_3hrs)) {
    days_dry <- days_since_precip(state$last_precip_time)
    risk_msg <- build_precip_msg(days_dry)
    
    message <- sprintf(
      "❄️ FREEZE WARNING\n\nCurrent temp: %.1f°C\n\nForecast to drop below freezing in approximately %.0f hours\n\nForecast: %.1f°C\n\n%s\n\nPrepare to salt the sidewalk!",
      current_temp,
      freeze_in_3hrs$hours,
      freeze_in_3hrs$temp,
      risk_msg
    )
    
    cat("\n*** SENDING 3-HOUR WARNING ***\n")
    cat(message, "\n")
    
    if (send_alert(message, priority = "high")) {
      state$last_alert_3hr <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
    }
  }
  
  # ALERT 2: Freezing now
  # CRITICAL: Only send if we've confirmed temp was recently above zero
  # This prevents false alerts if state gets reset or script starts when already frozen
  if (current_temp <= 0 && state$temp_was_above_zero) {
    
    # Double-check: Was there actually a recent warm period?
    # If last_alert_3hr exists OR last_alert_freeze is very recent, it's legitimate
    # Otherwise, this might be a state corruption - don't alert
    is_legitimate_freeze <- FALSE
    
    # Check if we sent a warning in the last 12 hours
    if (!is.null(state$last_alert_3hr)) {
      is_legitimate_freeze <- TRUE
      cat("Freeze alert is legitimate - warning was sent\n")
    }
    
    # OR check if last freeze alert was more than 6 hours ago (meaning temp cycled up and down)
    if (!is.null(state$last_alert_freeze)) {
      last_freeze_time <- as.POSIXct(state$last_alert_freeze, format = "%Y-%m-%d %H:%M:%S")
      hours_since_last <- as.numeric(difftime(Sys.time(), last_freeze_time, units = "hours"))
      if (hours_since_last > 6) {
        is_legitimate_freeze <- TRUE
        cat("Freeze alert is legitimate - last freeze was", hours_since_last, "hours ago\n")
      } else {
        cat("Suppressing - last freeze alert was only", hours_since_last, "hours ago\n")
      }
    }
    
    # If this is the first run ever and temp is already frozen, DON'T alert
    if (is.null(state$last_alert_3hr) && is.null(state$last_alert_freeze)) {
      cat("Suppressing - no prior alerts, temp already frozen (likely script just started)\n")
      is_legitimate_freeze <- FALSE
    }
    
    if (is_legitimate_freeze) {
      days_dry <- days_since_precip(state$last_precip_time)
      risk_msg <- build_precip_msg(days_dry)
      
      message <- sprintf(
        "❄️ FREEZE ALERT - ACT NOW\n\nCurrent temperature: %.1f°C\n\n%s\n\nTime to salt the sidewalk!",
        current_temp,
        risk_msg
      )
      
      cat("\n*** SENDING FREEZE ALERT ***\n")
      cat(message, "\n")
      
      if (send_alert(message, priority = "urgent")) {
        state$last_alert_freeze <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
      }
    }
    
    state$temp_was_above_zero <- FALSE
    
  } else if (current_temp > 0) {
    # Temperature is above freezing
    # Only reset flags if we were previously frozen (completing a freeze cycle)
    if (!state$temp_was_above_zero) {
      cat("Temperature back above freezing - resetting alert state for next freeze cycle\n")
      state$last_alert_3hr <- NULL
      state$last_alert_freeze <- NULL
    }
    state$temp_was_above_zero <- TRUE
    
  } else {
    cat("Temperature still below freezing (already alerted - suppressed)\n")
  }
  
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
