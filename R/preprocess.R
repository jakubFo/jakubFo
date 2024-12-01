library(tidytext)
library(stringr)

preprocess <- function(data_list) {
  # Identify common words in "Off-Road Description" column
  common_words(data_list$incidents, "Off-Road Description", 5)

  data_list$incidents <- data_list$incidents %>%
    mutate(defect_category = case_when(
      grepl("NO DEFECTS", `Road Condition`) ~ "No Defects",
      grepl("HOLES RUTS", `Road Condition`) ~ "Surface Issues",
      grepl("VIEW OBSTRUCTED", `Road Condition`) ~ "Obstructions",
      TRUE ~ "Other/Unknown"
    )) %>%
    mutate(
      route_category = case_when(
        `Route Type` %in% c("Maryland (State)", "US (State)", "Interstate (State)") ~ "State Roads",
        `Route Type` %in% c("Municipality", "Municipality Route", "Local Route") ~ "Municipal Roads",
        str_detect(tolower(`Off-Road Description`), "park") ~ "Parking",
        str_detect(tolower(`Off-Road Description`), "municipality|local") ~ "Municipal Roads",
        TRUE ~ NA_character_
      )
    )

  # Categorize `Collision Type`, `Weather`, `Surface Condition`, `Light Condition`, and `Driver Substance Abuse` with 3 options max
  data_list$drivers <- data_list$drivers %>%
    mutate(
      collision_category = case_when(
        grepl("SINGLE VEHICLE|Other", `Collision Type`) ~ "Single Vehicle/Other",
        grepl("HEAD ON|Front to Front", `Collision Type`) ~ "Head On/Front Collisions",
        grepl("REAR", `Collision Type`) ~ "Rear-End/Rear Collisions",
        TRUE ~ NA_character_
      ),
      weather_category = case_when(
        grepl("CLEAR|Clear", Weather) ~ "Clear/No Precipitation",
        grepl("RAINING|Rain", Weather) ~ "Rain/Snow/Freezing Precipitation",
        grepl("SNOW|WINTRY MIX", Weather) ~ "Snow/Sleet/Wintry Mix",
        TRUE ~ NA_character_
      ),
      surface_category = case_when(
        grepl("DRY|Dry", `Surface Condition`) ~ "Dry",
        grepl("WET|Wet|WATER", `Surface Condition`) ~ "Wet/Water",
        grepl("ICE|Ice|Frost", `Surface Condition`) ~ "Ice/Frost",
        TRUE ~ NA_character_
      ),
      light_condition_category = case_when(
        grepl("DAYLIGHT|Daylight", Light) ~ "Daylight",
        grepl("DARK|Dark", Light) ~ "Dark",
        grepl("DUSK|Dawn", Light) ~ "Dusk/Dawn",
        TRUE ~ NA_character_
      ),
      substance_category = case_when(
        grepl("NONE DETECTED", `Driver Substance Abuse`) ~ "No Substance Detected",
        grepl("ALCOHOL|Suspect of Alcohol", `Driver Substance Abuse`) ~ "Alcohol Involvement",
        grepl("ILLEGAL DRUG", `Driver Substance Abuse`) ~ "Drug Involvement",
        TRUE ~ NA_character_
      ),
      movement_category = case_when(
        `Vehicle Movement` %in% c("PARKED", "PARKING", "STOPPED IN TRAFFIC LANE", "STARTING FROM LANE",
                        "STARTING FROM PARKED", "STOPPED IN TRAFFIC", "DRIVERLESS MOVING VEH.",
                        "N/A") ~ "Stationary",
        `Vehicle Movement` %in% c("BACKING", "SLOWING OR STOPPING", "CHANGING LANES", "MAKING LEFT TURN",
                        "MAKING RIGHT TURN", "MAKING U TURN", "NEGOTIATING A CURVE",
                        "RIGHT TURN ON RED", "SKIDDING", "ENTERING TRAFFIC LANE",
                        "LEAVING TRAFFIC LANE", "TURNING LEFT", "TURNING RIGHT") ~ "Low-speed or Controlled Movement",
        `Vehicle Movement` %in% c("MOVING CONSTANT SPEED", "ACCELERATING", "OVERTAKING/PASSING",
                        "NEGOTIATING A CURVE", "PASSING") ~ "Active Driving",
        TRUE ~ NA # Fallback for undefined movements
      )
      )

  # Categorize non-motorist involvement and severity with 3 options max
  data_list$pedestrians <- data_list$pedestrians %>%
    mutate(
      non_motorist_category = case_when(
        grepl("PEDESTRIAN", `Related Non-Motorist`, ignore.case = TRUE) ~ "Pedestrian",
        grepl("BICYCLIST", `Related Non-Motorist`) ~ "Cyclist (Non-Electric)",
        grepl("Electric", `Related Non-Motorist`) ~ "Electric (All Types)",
        TRUE ~ "Unknown/Other"
      ),
      Severity_Category = case_when(
        str_detect(str_to_lower(`Injury Severity`), "fatal") ~ 1L,
        str_detect(str_to_lower(`Injury Severity`), "injury|apparent") ~ 0L,
        TRUE ~ as.integer(NA)
      ),
      safety_equipment_group = case_when(
        str_detect(`Safety Equipment`, regex("none|n/a|unknown", ignore_case = TRUE)) ~ "None",
        str_detect(`Safety Equipment`, regex("helmet", ignore_case = TRUE)) ~ "Helmet-related",
        str_detect(`Safety Equipment`, regex("lighting", ignore_case = TRUE)) ~ "Lighting-related",
        TRUE ~ "Other"
      )
    )

  return(data_list)
}
