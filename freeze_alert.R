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
    cat(log_line) # Print to console
    cat(log_line, file = LOG_FILE, append = TRUE) # Append to log file
}

log_state <- function(state) {
    log_msg(paste("STATE:", toJSON(state, auto_unbox = TRUE)))
}

# ============================================================================
# HELPER FUNCTIONS
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
main <- function() {
    Sys.setenv(TZ = "America/Toronto")
    
    # 1. Load state and get weather
    state <- load_state()
    current <- get_current_weather()
    current_temp <- current$main$temp
    weather_id <- current$weather[[1]]$id
    
    log_msg(paste("Current temp:", current_temp, "°C | Weather ID:", weather_id))
    
    # 2. Update Precipitation State using Weather ID (200-699)
    if (weather_id >= 200 && weather_id < 700) {
        state$last_precip_time <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
        log_msg("Precipitation detected - Updating last_precip_time")
    }
    
    forecast <- get_forecast()
    days_dry <- days_since_precip(state$last_precip_time)
    
    # ============================================================================
    # PRODUCTION LOGIC
    # ============================================================================
    
    currently_frozen <- current_temp <= 0
    
    # --- STEP A: FREEZE WARNING (Forecast) ---
    # Only if currently warm AND no warning sent in last 24h
    can_warn <- !currently_frozen && is.null(state$last_alert_warning)
    
    if (can_warn) {
        freeze_found <- NULL
        # Check next 6 hours (approx 2 entries)
        for (entry in forecast$list[1:min(3, length(forecast$list))]) {
            hrs_until_entry <- hours_until(entry$dt)
            if (entry$main$temp <= 0 && hrs_until_entry >= 1 && hrs_until_entry <= 6) {
                freeze_found <- list(temp = entry$main$temp, hours = hrs_until_entry)
                break
            }
        }
        
        if (!is.null(freeze_found)) {
            risk_msg <- build_precip_msg(days_dry)
            message <- sprintf("❄️ FREEZE WARNING\nTemp: %.1f°C\nExpected in %.0f hrs (%.1f°C)\n%s", 
                               current_temp, freeze_found$hours, freeze_found$temp, risk_msg)
            if (send_alert(message, priority = "high")) {
                state$last_alert_warning <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
            }
        }
    }
    
    # --- STEP B: FREEZE ALERT (Actual) ---
    # Only if it JUST dropped below zero
    if (currently_frozen && state$temp_was_above_zero) {
        risk_msg <- build_precip_msg(days_dry)
        message <- sprintf("❄️ FREEZE ALERT\nCurrent: %.1f°C\n%s\nSALT NOW!", current_temp, risk_msg)
        
        if (send_alert(message, priority = "urgent")) {
            state$last_alert_freeze <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
            state$temp_was_above_zero <- FALSE # Disarm until it thaws
            log_msg("Freeze alert sent. System disarmed until thaw.")
        }
    } 
    # --- STEP C: RESET LOGIC ---
    # If it was frozen but now it's warm, re-arm the system
    else if (!currently_frozen && !state$temp_was_above_zero) {
        log_msg("Temperature rose above freezing - Re-arming system.")
        state$temp_was_above_zero <- TRUE
        state$last_alert_warning <- NULL
        state$last_alert_freeze <- NULL
    }
    
    # 3. Finalize
    save_state(state)
    log_msg("=== CHECK COMPLETE ===")
}
# ============================================================================
# RUN
# ============================================================================

if (!interactive()) {
    tryCatch(
        {
            main()
        },
        error = function(e) {
            log_msg(paste("ERROR:", e$message))
            cat("ERROR:", e$message, "\n")
            quit(status = 1)
        }
    )
}
