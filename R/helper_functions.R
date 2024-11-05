#' load_data_file
#'
#' @param file a shiny file input
#' @param separator a separator
#' @param decimal a decimal mark
#'
#' @return a list separated by id

load_data_file <- function(file,
                           separator,
                           decimal){
  #load data file with vroom
  ext <- tools::file_ext(file$datapath)
  req(file)
  validate(need(ext == "csv", "Please upload a csv file"))
  raw_data <- vroom::vroom(file$datapath,
                               show_col_types = F,
                               delim = separator,
                               locale = vroom::locale(decimal_mark = decimal)
  )
  return(raw_data)
  }

#'  extract group info from raw data
#'
#' @param raw_data a data frame
#'
#' @return group info as string

extract_group_info <- function(raw_data){
  #deduce name of test groups based on timestamp columns
  testGroups <- stringr::str_subset(colnames(raw_data),
                                    pattern = "_TIMESTAMP$")
  groups <- stringr::str_replace(testGroups,
                                 pattern = "_TIMESTAMP",
                                 replacement = "")
}

#' Pre-process loaded data
#'
#' @param raw_data a raw data data frame
#' @param file the input file
#'
#' @return

process_data <- function(raw_data, file, group_info){

  #determine timezero time point
  TimeZero <- raw_data$relativeTime[2]/3600


  #remove summary stat columns
  trimmed_data <- raw_data |>
    dplyr::select(!tidyselect::ends_with(c("QRT",
                                           "AVG",
                                           "SEM",
                                           "SAMPLES")))

  #collect and correct time stamp
  #time stamp correction is not necessary, based on error

  # trimmed_data <- timestamp_corrector(trimmed_data,
  #                                      file,
  #                                     "data")

    #remove first time point - it is not a full unit of time

  trimmed_data <- trimmed_data |>
    duckplyr::slice(-1) |>
    #remove summary columns
    duckplyr::select(-tidyselect::ends_with("TIMESTAMP"),
                     -tidyselect::ends_with("SAMPLES")) |>
    #order data by calculating TimeElapsed
    duckplyr::mutate(TimeElapsed = relativeTime / 3600 - TimeZero) |>
    duckplyr::select(-relativeTime) |>
    #make data long format
    tidyr::pivot_longer(
      cols = -c(TimeElapsed, hour, day, minute),
      names_to = "ID",
      values_to = "Rawdata"
    ) |>
    dplyr::group_by(ID) |>
    duckplyr::arrange(TimeElapsed) |>
    #rawdata often becomes character vector at this point"
  duckplyr::mutate(
    Rawdata = as.numeric(stringr::str_replace_all(Rawdata, ",", "."))
    #calculate rawdata as delta value
    ,
    Value = Rawdata - duckplyr::lead(Rawdata,
                                     default = duckplyr::last(Rawdata))
  )
  #make a elapsed time column

  trimmed_data <- trimmed_data |>
    duckplyr::mutate(duration = lubridate::days(day)+lubridate::hours(hour)+lubridate::minutes(minute))

  #add group info
  trimmed_data$Group <- NA
  group_info_strings <- unlist(purrr::map(group_info,
                                   ~paste0("^",.x, "_")))

  trimmed_data <- purrr::map2_dfr(group_info_strings,
                                  group_info,
                                 ~ dplyr::mutate(
                                   trimmed_data,
                                   Group = dplyr::case_when(
                                     stringr::str_detect(ID, .x) == TRUE ~ .y,
                                     TRUE ~ as.character(Group)
                                   )
                                 )
  ) |>
    dplyr::filter(!is.na(Group)) |>
    dplyr::arrange(TimeElapsed)





  #split based on id

  trimmed_data_split <- trimmed_data |>
    dplyr::group_by(ID) |>
    dplyr::group_split()
  #get name for group
  group_name <- trimmed_data |>
    dplyr::group_by(ID) |>
    dplyr::group_keys() |>
    duckplyr::pull(ID)
  trimmed_data_split <- purrr::set_names(trimmed_data_split, group_name)

  return(trimmed_data_split)
}


#' Load event file
#'
#' @param event_file a shiny file input
#' @param separator a separator (character)
#' @param decimal a decimal mark (character)
#'
#' @return a data

load_event_file <- function(event_file, separator, decimal){
  ext <- tools::file_ext(event_file$datapath)
  req(event_file)
  validate(need(ext == "csv", "Please upload a csv file"))
  event_data <- vroom::vroom(event_file$datapath,
                               show_col_types = F,
                               delim = separator,
                               locale = vroom::locale(decimal_mark = decimal)
  )

  if(length(event_data)==1){
    shinyWidgets::sendSweetAlert(
      type = "error",
      title = "Only one column detected",
      text = "Did you assign the right separator?"
    )
  }

  else{
    # eventfile_trimmed <- timestamp_corrector(event_data,
    #                                        event_file,
    #                                        "event")

  #filter so all we have is INSERTED (which is where events end)

  eventfile_trimmed <- event_data |>
    duckplyr::filter(event == "INSERTED") |>
    #create unique ID for join
    duckplyr::mutate(
      ID = paste(group, cage, sep = "_"),
      duration = lubridate::days(day)+lubridate::hours(hour)+lubridate::minutes(minute)
    )

  eventfile_trimmed_split <- eventfile_trimmed |>
    dplyr::group_by(ID) |>
    dplyr::group_split()
  #get name for group
  group_name <- eventfile_trimmed  |>
    dplyr::group_by(ID) |>
    dplyr::group_keys() |>
    duckplyr::pull(ID)
  eventfile_trimmed_split <- purrr::set_names(eventfile_trimmed_split, group_name)

  return(eventfile_trimmed_split)
  }

}

#'  Time stamp corrector
#'
#' @param input_data a data frame
#' @param input_file the file used to upload data
#' @param file_type character vector specifying if data frame is "event" or "data"
#'
#' @return a time-stamp corrected data frame

timestamp_corrector <- function(input_data, input_file, file_type){
  if(file_type == "event"){
    headername <- stringr::str_subset(colnames(input_data),
                                      pattern = "timestamp")[1]
  }
  else if (file_type == "data"){
    headername <- stringr::str_subset(colnames(input_data),
                                      pattern = "_TIMESTAMP$")[1]
  }
  else{
    print("specify format for timestamp corrector")
    break
  }
  #reload data column and extract time correction
  firsttimestamp <- vroom::vroom(input_file$datapath,
                                 show_col_types = F,
                                 col_select = tidyselect::all_of(headername),
                                 col_types = vroom::cols(.default = "c"))

  timeadjust <- stringr::str_extract(firsttimestamp[1,1],
                                     pattern = "[+-]?[:digit:]{4}$")
  timeadjustTime <- lubridate::hm(paste0(stringr::str_extract(timeadjust,
                                                              pattern = "(?<=[+-])[:digit:]{2}"),
                                         ":",
                                         stringr::str_extract(timeadjust,
                                                              pattern = "[:digit:]{2}$")))

  timeadjustFactor <- as.numeric(timeadjustTime)/3600


  #check if the timestamp factor is positive or negative
  if(isTRUE(stringr::str_extract(timeadjust,
                                 pattern = "^[+-]{1}")=="-")){
    trimmed_data <- input_data |>
      dplyr::mutate(hour = hour + timeadjustFactor,
                    #if adjustment makes hour larger than 23, it needs to be corrected
                    day = duckplyr::case_when(
                      hour>23 ~ day +1,
                      .default = day
                    ),
                    hour = duckplyr::case_when(
                      hour>23 ~hour - 24,
                      .default = hour
                    ))
    return(trimmed_data)
  }
  if(isTRUE(stringr::str_extract(timeadjust,
                                 pattern = "^[+-]{1}")=="+")){
    trimmed_data <- input_data |>
      dplyr::mutate(hour = hour - timeadjustFactor,
                    day = duckplyr::case_when(
                      hour<0 ~ day -1,
                      .default = day
                    ),
                    hour = duckplyr::case_when(
                      hour<0 ~24+hour,
                      .default = hour
                    ))
    return(trimmed_data)
  }

  else{
    return(input_data)
  }

}

#' Join event and data file
#'
#' @param data_file a list with data frame containing the columns ID and
#' duration (lubridate period)
#' @param event_file a list of data frames with the columns ID, duration and event
#'
#' @return a list of joined data frames where events are tagged

join_event_data <- function(data_file, event_file){

  #investigate if any data frames have no events
  if(length(data_file)!=length(event_file)){
    missing_files <- !names(data_file)%in% names(event_file)
    joined_data_missing <- data_file[missing_files]

    #create an empty column of events
    joined_data_missing <- purrr::map(.x  = joined_data_missing,
                                      ~duckplyr::mutate(.x,
                                                        duration = as.numeric(duration),
                                                        event = NA,
                                                        duration = lubridate::seconds_to_period(duration)))
  }
  else{
    joined_data_missing <- NULL
  }
  #turn lubridate to numeric, as periods cause errors
  joined_data <- data_file[names(event_file)] |>
    purrr::map(~duckplyr::mutate(.x,
                                 duration = as.numeric(duration)))
  joined_event <- purrr::map(event_file,
                             ~duckplyr::mutate(.x,
                                               duration = as.numeric(duration)
                             ) |>
                               duckplyr::select(ID, duration, event))

  #calculate the window between the data points in the joined_data
  time_factor <- as.numeric(joined_data[[1]]$duration[2]-joined_data[[1]]$duration[1])

  #join the matching data frames to the best match. remove extra IDs and
  #convert durations back
  joined_data <- purrr::map2(.x = joined_data,
                             .y = joined_event,
                             ~fuzzyjoin::fuzzy_left_join(
                               .x, .y,
                               by = c("ID"="ID", "duration"="duration"),
                               match_fun = list(`==`,
                                                function(x,y) abs(x-y)<=time_factor/2)
                             ) |>
                               duckplyr::select(-duration.y,
                                                -ID.y) |>
                               duckplyr::rename(ID = ID.x,
                                                duration = duration.x) |>
                               duckplyr::mutate(duration = lubridate::seconds_to_period(duration)))

  #join the missing data to the main dataset
  if(!is.null(joined_data_missing)){
    joined_data <- append(joined_data, joined_data_missing)
  }
  #return the joined data
  return(joined_data)
}

#' exclude data from cage changed
#'
#' @param joined_data list of data frames with
#' @param exclusion_window
#' @param data_resolution resolution of data in minutes
#'
#' @return
#' @export
#'
#' @examples
exclude_cage_changes <- function(joined_data,
                                 exclusion_window,
                                 data_resolution){
  #calculate exclusion window in seconds
  time_correction <- exclusion_window*60


  #calculate how many rows must be excluded pr. event
  row_correction <- round(time_correction/(data_resolution*60),digits = 0)

  joined_data_flagged <- purrr::map(joined_data,
                                    #change lubridate to seconds
                                    ~duckplyr::mutate(.x, duration = as.numeric(duration),
                                                      #make row_id and flag events
                                                      row_id  = dplyr::row_number(),
                                                      included_before = duckplyr::if_else(
                                                        !is.na(event), row_id, NA),
                                                      included_after = duckplyr::if_else(
                                                        !is.na(event), row_id, NA)
                                    ) |>
                                      #fill included with presence of event
                                      tidyr::fill(
                                        included_before,.direction = "up") |>
                                      tidyr::fill(
                                        included_after,.direction = "down") |>
                                      #check distance to nearest event. FALSE if close to threshold
                                      duckplyr::mutate(
                                        included_before = duckplyr::if_else(
                                        abs(row_id - included_before) >row_correction, TRUE, FALSE),
                                        included_after = duckplyr::if_else(
                                          abs(row_id - included_after) >row_correction, TRUE, FALSE),
                                        included = duckplyr::case_when(
                                          included_before== FALSE ~ FALSE,
                                          included_after == FALSE ~ FALSE,
                                          .default = TRUE
                                        ),
                                        #calculate the corrected value based on exclusions
                                        CorrectedValue = dplyr::case_when(
                                                             included == TRUE ~ Value,
                                                             included == FALSE ~ NA
                                                           ))
                                    |>
                                      #discard row_ID and re-calculate period
                                      duckplyr::select(-row_id, -included_before, -included_after) |>
                                      duckplyr::mutate(duration  =lubridate::seconds_to_period(duration))


  )

}
