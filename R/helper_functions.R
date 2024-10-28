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

  trimmed_data <- timestamp_corrector(trimmed_data,
                                       file,
                                      "data")

    #remove first time point - it is not a full unit of time

  trimmed_data <- trimmed_data |>
    duckplyr::slice(-1) |>
    #remove summary columns
    duckplyr::select(-tidyselect::ends_with("TIMESTAMP"),-tidyselect::ends_with("SAMPLES")) |>
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
  trimmed_data <- purrr::map_dfr(group_info,
                                 ~ dplyr::mutate(
                                   trimmed_data,
                                   Group = dplyr::case_when(
                                     stringr::str_detect(ID, .x) == TRUE ~ .x,
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

  eventfile_trimmed <- timestamp_corrector(event_data,
                                           event_file,
                                           "event")

  #filter so all we have is INSERTED (which is where events end)

  eventfile_trimmed <- eventfile_trimmed |>
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

#'  Time stamp corrector
#'
#' @param input_data a data frame
#' @param input_file the file used to upload data
#' @param file_type character vector specifying if data frame is "event" or "data"
#'
#' @return a time-stamp corrected data frame

timestamp_corrector <- function(input_data, input_file, file_type){
  if(file_type == "event"){
    headername <- stringr::str_subset(colnames(event_file),
                                      pattern = "timestamp")[1]
  }
  else if (file_type == "data"){
    headername <- stringr::str_subset(colnames(rawData),
                                      pattern = "_TIMESTAMP$")[1]
  }
  else{
    print("specify format for timestamp corrector")
    break
  }
  #reload data column and extract time correction
  firsttimestamp <- vroom::vroom(input_file$datapath,
                                 show_col_types = F,
                                 col_select = headername,
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
    return(trimmed_data)
  }

}
