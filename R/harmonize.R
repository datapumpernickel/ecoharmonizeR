#' Harmonize Trade Data from Multiple Sources
#'
#' This function harmonizes trade data from various sources including COMEX, Atlas, and COMTRADE datasets.
#' It standardizes the dataset structure, cleans, and prepares it for analysis by applying specific
#' transformations based on the source of the data.
#'
#' @param data The trade data frame to be harmonized.
#' @param source A character string specifying the source of the data.
#'               Valid options are "comex", "atlas", "comtrade", and "comtrade_bulk".
#'
#' @return A harmonized and cleaned data frame ready for analysis.
#'         The returned data frame includes standardized columns for frequency, reporter description,
#'         reporter ISO code, partner description, partner ISO code, product description, product code,
#'         flow description, time period, and the primary value of trade. Additionally, a source column
#'         is added to indicate the origin of the data.
#'
#' @examples
#' if (interactive()) {
#'   # Harmonize data from COMEX
#'   harmonized_comex <- harmonize(comex_data, "comex")
#'
#'   # Harmonize data from Atlas
#'   harmonized_atlas <- harmonize(atlas_data, "atlas")
#'
#'   # Harmonize data from COMTRADE
#'   harmonized_comtrade <- harmonize(comtrade_data, "comtrade")
#'
#'   # Harmonize bulk data from COMTRADE
#'   harmonized_comtrade_bulk <- harmonize(comtrade_bulk_data, "comtrade_bulk")
#' }
#'
#' @export
harmonize <- function(data, source){
  rlang::arg_match(source, values = c("comex", "atlas", "comtrade", "comtrade_bulk"))

  if(source == "comex"){

    partner <- comRex::cr_get_ref_table("partner")  |>
      dplyr::select(partner_desc = description_code,
                    iso_2 = code) |>
      dplyr::mutate(partner_iso = countrycode::countrycode(iso_2,origin = "iso2c", destination = "iso3c")) |>
      dplyr::mutate(partner_iso = dplyr::if_else(stringr::str_detect(iso_2,"WORLD"),"world",partner_iso)) |>
      dplyr::filter(!iso_2 %in% "XM") |>
      dplyr::select(-iso_2)

    reporter <- comRex::cr_get_ref_table("reporter")  |>
      dplyr::select(reporter_desc = description_code,
                    iso_2 = code) |>
      dplyr::mutate(reporter_iso = countrycode::countrycode(iso_2,origin = "iso2c", destination = "iso3c")) |>
      dplyr::mutate(reporter_iso = dplyr::if_else(stringr::str_detect(iso_2,"WORLD"),"world",reporter_iso)) |>
      dplyr::select(-iso_2)

    flow <- comRex::cr_get_ref_table("flow")  |>
      dplyr::select(flow_desc = description_code)

    product <- comRex::cr_get_ref_table("product") |>
      dplyr::transmute(cmd_desc= description_code,
                       cmd_code = code)

    clean_data <- data |>
      janitor::clean_names() |>
      dplyr::filter(indicators %in% "VALUE_IN_EUROS") |>
      dplyr::transmute(
        frequency = frequency,
        reporter_desc = reporter,
        partner_desc = partner,
        cmd_desc = product,
        flow_desc = tolower(flow),
        time = time_period,
        primary_value = value
      ) |>
      dplyr::left_join(partner, by = "partner_desc") |>
      dplyr::left_join(reporter, by = "reporter_desc") |>
      dplyr::left_join(product, by = "cmd_desc") |>
      dplyr::mutate(frequency = dplyr::if_else(stringr::str_detect(frequency,"A|a"), "A","M")) |>
      dplyr::filter(!is.na(primary_value)) |>
      dplyr::mutate(source = "comex")
  } else if(source == "atlas"){

    clean_data <- data |>
      janitor::clean_names()  |>
      dplyr::transmute(
        frequency = "A",
        reporter_iso  = location_code,
        partner_iso = partner_code,
        classification_code = "HS0",
        cmd_code = hs_product_code,
        time = year,
        export_value,
        import_value
      ) |>
      tidyr::pivot_longer(cols = c(export_value, import_value),
                          names_to = "flow_desc", values_to = "primary_value",
                          names_transform = function(x) str_remove(x,"_value"))

    partner <- clean_data |> dplyr::distinct(reporter_iso, partner_iso) |>
      dplyr::mutate(reporter_desc = countrycode::countrycode(reporter_iso, origin = "iso3c", destination = "country.name.en"))|>
      dplyr::mutate(partner_desc = countrycode::countrycode(partner_iso, origin = "iso3c", destination = "country.name.en"))

    cmd_desc <- comtradr::ct_get_ref_table("H0")  |>
      dplyr::transmute(cmd_code = id, cmd_desc = text)

    clean_data <- clean_data |>
      dplyr::left_join(partner) |>
      dplyr::left_join(cmd_desc)|>
      dplyr::mutate(source = "atlas")

  } else if(source == "comtrade"){
    clean_data <- data |>
      dplyr::transmute(
        frequency = freq_code,
        reporter_desc,
        reporter_iso,
        partner_desc,
        partner_iso,
        classification_code,
        cmd_desc,
        cmd_code,
        flow_desc = tolower(flow_desc),
        time = period,
        primary_value
      ) |>
      dplyr::mutate(frequency = dplyr::if_else(stringr::str_detect(frequency,"A|a"), "A","M")) |>
      dplyr::filter(!is.na(primary_value))|>
      dplyr::mutate(source = "comtrade")

  } else if(source == "comtrade_bulk"){

    reporter <- comtradr::ct_get_ref_table("reporter") |>
      dplyr::transmute(reporter_code = as.character(id), reporter_iso = iso_3, reporter_desc = country)

    partner <- comtradr::ct_get_ref_table("partner") |>
      dplyr::transmute(partner_code = as.character(id), partner_iso = iso_3, partner_desc = country)

    flow <- comtradr::ct_get_ref_table("flow_direction") |>
      dplyr::transmute(flow_code = as.character(id), flow_desc = tolower(text))

    clean_data <- data |>
      dplyr::filter(customs_code == "C00" & mot_code == "0" & partner2code =="0")  |>
      dplyr::left_join(reporter) |>
      dplyr::left_join(partner) |>
      dplyr::left_join(flow) |>
      dplyr::transmute(
        frequency = freq_code,
        reporter_desc,
        reporter_iso,
        partner_desc,
        partner_iso,
        classification_code,
        cmd_code,
        flow_desc = tolower(flow_desc),
        time = period,
        primary_value
      ) |>
      dplyr::mutate(frequency = dplyr::if_else(stringr::str_detect(frequency,"A|a"), "A","M")) |>
      dplyr::filter(!is.na(primary_value))|>
      dplyr::mutate(source = "comtrade_bulk")

    cmd_desc <- purrr::map_dfr(unique(clean_data$classification_code), ~comtradr::ct_get_ref_table(.x) |> dplyr::mutate(classification_code = .x)) |>
      dplyr::transmute(cmd_code = id, cmd_desc = text, classification_code)

    clean_data <- clean_data |>
      dplyr::left_join(cmd_desc)
  }

  return(clean_data)
}

