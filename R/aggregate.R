#' @import (dplyr)
#' @export

aggregate <- function(data_list) {

  pedestrians <- data_list$pedestrians %>%
    dplyr::transmute(report_number = `Report Number`,
                     # report_type = `ACRS Report Type`,
                     safety_equipment_group,
                     severity_category_pedestrian = Severity_Category,
                     non_motorist_category
                     # crash_ts = as.POSIXct(`Crash Date/Time`),
                     ) %>%
    dplyr::distinct()

  drivers <- data_list$drivers %>%
    dplyr::transmute(report_number = `Report Number`,
                     # report_type = `ACRS Report Type`,
                     #data is not cleac, there are records with year 9999 etc, replace with NA
                     vehicle_year = dplyr::case_when(as.numeric(`Vehicle Year`) > 2025 ~ NA_integer_,
                                                     as.numeric(`Vehicle Year`) < 1970 ~ NA_integer_,
                                                     is.integer(as.integer(`Vehicle Year`)) ~ as.integer(`Vehicle Year`)),
                     vehicle_model = `Vehicle Model`,
                     collision_category,
                     weather_category,
                     surface_category,
                     light_condition_category,
                     substance_category,
                     movement_category
                     # crash_ts = as.POSIXct(`Crash Date/Time`),
    ) %>%
    dplyr::distinct() %>%
    dplyr::group_by(report_number) %>%
    dplyr::filter(row_number() == 1) %>%
    dplyr::ungroup()


  incidents <- data_list$incidents %>%
    dplyr::transmute(
      report_number = `Report Number`,
      report_type = `ACRS Report Type`,
      #severity calculations, I'll distinguish report type into 2 categories (Injury vs property)
      severity = dplyr::case_when(`ACRS Report Type` %in% c("Fatal Crash","Injury Crash") ~ "Injury",
                                  T ~ "Property"),
      defect_category,
      route_category,
      crash_ts = as.POSIXct(`Crash Date/Time`, format =  "%m/%d/%Y %I:%M:%S %p", tz = "UTC"),
      Latitude,
      Longitude

    ) %>%
    mutate(
      hour_of_day = hour(crash_ts)
    ) %>%
    dplyr::distinct()

  basic_table <- incidents %>%
    dplyr::left_join(drivers, by = "report_number") %>%
    dplyr::left_join(pedestrians, by = "report_number") %>%
    dplyr::mutate(fatal = dplyr::case_when(report_type == "Fatal Crash" ~ 1,
                                          T ~ 0))

}
