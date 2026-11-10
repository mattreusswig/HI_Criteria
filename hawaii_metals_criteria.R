

#####################################################################
#####   Compute HAWAII Hardness-based Metal Criterion   #############
#####################################################################

# Date: 2026-09-19
# Author: Matthew Reusswig
# Note: Based on Hawaii's 11-54 Water Quality Standards circa 2026.

# Function to ingest string codes, check they are valid, and return a concatenated
# output of the codes if they are.
# parameter = string giving the name of the metal
# type = string giving the type of criteria (acute or chronic)
# class = string class of water (FRESH or SALT)
make_type <- function(parameter, type, class) {
  
  '%not_in%' <- Negate('%in%')
  
  # Validate input are string values
  if(!is.character(parameter) | !is.character(type) | !is.character(class)) stop("Parameter, type, and class inputs must be strings.")
  
  # Validate only one of each input was used
  if(length(parameter) != 1 | length(type) != 1 | length(class) != 1) stop("Parameter, type, and class inputs must each have a single element input.")
  
  # Normalize capitalization of input
  parameter <- toupper(parameter)
  type      <- toupper(type)
  class       <- toupper(class)
  
  # Check in input values are valid
  valid_parameters <- c("ARSENIC",
                        "CADMIUM",
                        "CHROMIUM(VI)",
                        "COPPER",
                        "LEAD",
                        "MERCURY",
                        "NICKEL",
                        "SELENIUM",
                        "SILVER",
                        "ZINC")
  valid_types <- c("CHRONIC", "ACUTE")
  valid_classes <- c("FRESH", "SALT")
  
  if(parameter %not_in% valid_parameters) stop(paste0("Invalid parameter input. Parameter name must by one of: ", paste0(valid_parameters, collapse =  ", ")))
  if(type %not_in% valid_types) stop(paste0("Invalid type input. Type name must by one of: ", paste0(valid_types, collapse =  ", ")))
  if(class %not_in% valid_classes) stop(paste0("Invalid class input. Class name must by one of: ", paste0(valid_classes, collapse =  ", ")))
  
  # Concatenate each input into a combined code
  out <- paste0(parameter, "_", type, "_", class)
  
  return(out)
  
  }

# Function to compute the metal's conversion factor.
# hardness = numeric type, hardness value in mg/L CaCO3
# type_code = string type value taken from make_type function
calc_cf <- function(hardness, type_code) {
  
  b = switch(type_code, 
             # 0 is used for type_code values for which there is not a 
             # valid Conversion Factor
             
             "ARSENIC_ACUTE_FRESH"  = (1),
             "ARSENIC_ACUTE_SALT"   = (1),
             
             "ARSENIC_CHRONIC_FRESH"  = (1),
             "ARSENIC_CHRONIC_SALT"   = (1),
             
             "CADMIUM_ACUTE_FRESH"  = 1.136672,
             "CADMIUM_ACUTE_SALT"   = (0.994),
            
             "CADMIUM_CHRONIC_FRESH"  = 1.101672,
             "CADMIUM_CHRONIC_SALT"   = (0.994),
             
             "CHROMIUM(VI)_ACUTE_FRESH"  = (0.982),
             "CHROMIUM(VI)_ACUTE_SALT"   = (0.993),
             
             "CHROMIUM(VI)_CHRONIC_FRESH"   = (0.962),
             "CHROMIUM(VI)_CHRONIC_SALT"    = (0.993),
             
             "COPPER_ACUTE_FRESH"   = (0.960),
             "COPPER_ACUTE_SALT"    = (0.830),
             
             "COPPER_CHRONIC_FRESH"   = (0.960),
             "COPPER_CHRONIC_SALT"    = (0.830),
             
             "LEAD_ACUTE_FRESH"   = 1.46203,
             "LEAD_ACUTE_SALT"    = (0.951),
             
             "LEAD_CHRONIC_FRESH"   = 1.46203,
             "LEAD_CHRONIC_SALT"    = (0.951),
             
             "MERCURY_ACUTE_FRESH"   = (0.850),
             "MERCURY_ACUTE_SALT"    = (0.850),
             
             "MERCURY_CHRONIC_FRESH"   = (0.850),
             "MERCURY_CHRONIC_SALT"    = (0.850),
             
             "NICKEL_ACUTE_FRESH"   = (0.998),
             "NICKEL_ACUTE_SALT"    = (0.990),
             
             "NICKEL_CHRONIC_FRESH"   = (0.997),
             "NICKEL_CHRONIC_SALT"    = (0.990),
             
             "SELENIUM_ACUTE_FRESH"   = (1),
             "SELENIUM_ACUTE_SALT"    = (0.998),
             
             "SELENIUM_CHRONIC_FRESH"   = (1),
             "SELENIUM_CHRONIC_SALT"    = (0.998),
             
             "SILVER_ACUTE_FRESH"   = (0.850),
             "SILVER_ACUTE_SALT"    = (0.850),
             
             "SILVER_CHRONIC_FRESH"   = (1),
             "SILVER_CHRONIC_SALT"    = (1),
             
             "ZINC_ACUTE_FRESH"   = (0.978),
             "ZINC_ACUTE_SALT"    = (0.946),
             
             "ZINC_CHRONIC_FRESH"   = (0.986),
             "ZINC_CHRONIC_SALT"    = (0.946)
             )
  
  m = switch(type_code, 
             # 0 is used for type_code values for which there is not a 
             # valid Conversion Factor
             
             "ARSENIC_ACUTE_FRESH"  =  0,
             "ARSENIC_ACUTE_SALT"   = 0,
             
             "ARSENIC_CHRONIC_FRESH"  = 0,
             "ARSENIC_CHRONIC_SALT"   = 0,
             
             "CADMIUM_ACUTE_FRESH"  = 0.0418380,
             "CADMIUM_ACUTE_SALT"   = 0,
             
             "CADMIUM_CHRONIC_FRESH"  = 0.0418380,
             "CADMIUM_CHRONIC_SALT"   = 0,
             
             "CHROMIUM(VI)_ACUTE_FRESH"  = 0,
             "CHROMIUM(VI)_ACUTE_SALT"   = 0,
             
             "CHROMIUM(VI)_CHRONIC_FRESH"   = 0,
             "CHROMIUM(VI)_CHRONIC_SALT"    = 0,
             
             "COPPER_ACUTE_FRESH"   = 0,
             "COPPER_ACUTE_SALT"    = 0,
             
             "COPPER_CHRONIC_FRESH"   = 0,
             "COPPER_CHRONIC_SALT"    = 0,
             
             "LEAD_ACUTE_FRESH"   = 0.145712,
             "LEAD_ACUTE_SALT"    = 0,
             
             "LEAD_CHRONIC_FRESH"   = 0.145712,
             "LEAD_CHRONIC_SALT"    = 0,
             
             "MERCURY_ACUTE_FRESH"   = 0,
             "MERCURY_ACUTE_SALT"    = 0,
             
             "MERCURY_CHRONIC_FRESH"   = 0,
             "MERCURY_CHRONIC_SALT"    = 0,
             
             "NICKEL_ACUTE_FRESH"   = 0,
             "NICKEL_ACUTE_SALT"    = 0,
             
             "NICKEL_CHRONIC_FRESH"   = 0,
             "NICKEL_CHRONIC_SALT"    = 0,
             
             "SELENIUM_ACUTE_FRESH"   = 0,
             "SELENIUM_ACUTE_SALT"    = 0,
             
             "SELENIUM_CHRONIC_FRESH"   = 0,
             "SELENIUM_CHRONIC_SALT"    = 0,
             
             "SILVER_ACUTE_FRESH"   = 0,
             "SILVER_ACUTE_SALT"    = 0,
             
             "SILVER_CHRONIC_FRESH"   = 0,
             "SILVER_CHRONIC_SALT"    = 0,
             
             "ZINC_ACUTE_FRESH"   = 0,
             "ZINC_ACUTE_SALT"    = 0,
             
             "ZINC_CHRONIC_FRESH"   = 0,
             "ZINC_CHRONIC_SALT"    = 0
  )
  
  # Conversion factors that do not depend on hardness are returned as is, so
  # hardness is not needed for them.
  if (m == 0) {
    return(b)
  }

  # The Hawaii equations are b - ln(hardness) * m for Cd and Pb.  The prior
  # division produced incorrect conversion factors (and incorrect units).
  cf = b - log(hardness) * m
  
  return(cf)
}

# Function to compute the metal's total recoverable criterion in ug/L. This function
# assumes the permittee's metal translator is equal to the conversion factor (CF)
# specified in the water quality standards.
#   - Freshwater Cd, Cu, Pb, Ni, Ag, and Zn criteria are hardness-based. The
#     HAR 11-54 equation exp(m * ln(hardness) + b) gives the total recoverable
#     criterion directly.
#   - All other criteria (As, Cr(VI), Hg, and Se, and the saltwater criteria
#     for every metal) are fixed dissolved values that do not depend on
#     hardness. They are divided by the CF to give the total recoverable
#     criterion, and hardness is not needed.
# Combinations with no published criterion (chronic saltwater silver) return NA.
# hardness = numeric type, hardness value in mg/L CaCO3
# parameter = string giving the name of the metal
# type = string giving the type of criteria (acute or chronic)
# class = string giving the water class (FRESH or SALT)
calc_total_metal_criterion <- function(hardness, parameter, type, class) {
  
  # Validate input and create a type code to feed cf and total metal criterion functions.
  type_code <- make_type(parameter = parameter, type = type, class = class)
  
  # class type_code to get the slope and intercept of the hardness-based total
  # recoverable metal criterion. Metals without a hardness-based criterion are
  # not listed and return NULL.
  b = switch(type_code, 
             "CADMIUM_ACUTE_FRESH"   = -3.828,
             "CADMIUM_CHRONIC_FRESH"   = -3.49,
             
             "COPPER_ACUTE_FRESH"   = -1.464,
             "COPPER_CHRONIC_FRESH"   = -1.465,
             
             "LEAD_ACUTE_FRESH"   = -1.46,
             "LEAD_CHRONIC_FRESH"   = -4.705,
             
             "NICKEL_ACUTE_FRESH"   = 4.02,
             "NICKEL_CHRONIC_FRESH"   = 1.06,
             
             "SILVER_ACUTE_FRESH"   = -6.52,
             "SILVER_CHRONIC_FRESH"   = -6.52,
             
             "ZINC_ACUTE_FRESH"   = 1.95,
             "ZINC_CHRONIC_FRESH"   = 1.95
               )
  
  m = switch(type_code, 
             "CADMIUM_ACUTE_FRESH"   = 1.128,
             "CADMIUM_CHRONIC_FRESH"   = 0.7852,
             
             "COPPER_ACUTE_FRESH"   = 0.9422,
             "COPPER_CHRONIC_FRESH"   = 0.8545,
             
             "LEAD_ACUTE_FRESH"   = 1.273,
             "LEAD_CHRONIC_FRESH"   = 1.273,
             
             "NICKEL_ACUTE_FRESH"   = 0.760,
             "NICKEL_CHRONIC_FRESH"   = 0.760,
             
             "SILVER_ACUTE_FRESH"   = 1.72,
             "SILVER_CHRONIC_FRESH"   = 1.72,
             
             "ZINC_ACUTE_FRESH"   = 0.830,
             "ZINC_CHRONIC_FRESH"   =  0.830
  )
  
  # Fixed dissolved criteria (ug/L) from HAR 11-54 for criteria that do not
  # depend on hardness. NA is used where there is no published criterion.
  fixed_dissolved_crit = switch(type_code,
             "ARSENIC_ACUTE_FRESH"   = 360,
             "ARSENIC_CHRONIC_FRESH" = 190,
             "ARSENIC_ACUTE_SALT"    = 69,
             "ARSENIC_CHRONIC_SALT"  = 36,
             
             "CADMIUM_ACUTE_SALT"    = 43,
             "CADMIUM_CHRONIC_SALT"  = 9.3,
             
             "CHROMIUM(VI)_ACUTE_FRESH"   = 16,
             "CHROMIUM(VI)_CHRONIC_FRESH" = 11,
             "CHROMIUM(VI)_ACUTE_SALT"    = 1100,
             "CHROMIUM(VI)_CHRONIC_SALT"  = 50,
             
             "COPPER_ACUTE_SALT"     = 2.9,
             "COPPER_CHRONIC_SALT"   = 2.9,
             
             "LEAD_ACUTE_SALT"       = 140,
             "LEAD_CHRONIC_SALT"     = 5.6,
             
             "MERCURY_ACUTE_FRESH"   = 2.4,
             "MERCURY_CHRONIC_FRESH" = 0.55,
             "MERCURY_ACUTE_SALT"    = 2.1,
             "MERCURY_CHRONIC_SALT"  = 0.025,
             
             "NICKEL_ACUTE_SALT"     = 75,
             "NICKEL_CHRONIC_SALT"   = 8.3,
             
             "SELENIUM_ACUTE_FRESH"   = 20,
             "SELENIUM_CHRONIC_FRESH" = 5,
             "SELENIUM_ACUTE_SALT"    = 300,
             "SELENIUM_CHRONIC_SALT"  = 71,
             
             "SILVER_ACUTE_SALT"     = 2.3,
             "SILVER_CHRONIC_SALT"   = NA,
             
             "ZINC_ACUTE_SALT"       = 95,
             "ZINC_CHRONIC_SALT"     = 86
  )
  
  if (!is.null(b)) {
    # Hardness-based criterion, already total recoverable.
    total_crit = exp(b + log(hardness) * m)
  } else if (!is.null(fixed_dissolved_crit)) {
    # Fixed dissolved criterion, converted to total recoverable.
    total_crit = fixed_dissolved_crit / calc_cf(hardness = hardness,
                                                type_code = type_code)
  } else {
    total_crit = NA_real_
  }
  
  total_crit <- ifelse(total_crit == 0, NA, total_crit)
  
  return(total_crit)
}


# Function to compute the metal's dissolved criterion.
# hardness = numeric type, hardness value in mg/L CaCO3
# parameter = string giving the name of the metal
# type = string giving the type of criteria (acute or chronic)
# class = string giving the water class (FRESH or SALT)
calc_dissolved_metal_criterion <- function(hardness, parameter, type, class) {
  
  # Validate input and create a type code to feed cf and total metal criterion functions.
  type_code <- make_type(parameter = parameter, type = type, class = class)
  
  # Calculate the conversion factor
  cf <- calc_cf(hardness = hardness, type_code = type_code)
  
  # Calculate the total recoverable criterion in units of ug/L 
  total_crit <- calc_total_metal_criterion(hardness = hardness,
                                           parameter = parameter,
                                           type = type,
                                           class = class)
  
  # Calculate the dissolved criterion in units of ug/L
  dissolved_crit <- total_crit * cf
  
  return(dissolved_crit)
  
}

# Return the controlling criterion in hi_criteria_long_format.csv for a
# parameter.
#
# `use` may contain any of "Aquatic Life", "Fish Consumption", "General", and
# "Recreation".  `water_class` may contain any of "Freshwater", "Saltwater",
# and "Inland Freshwater"; it filters Aquatic Life and General rows (Fish
# Consumption and Recreation rows have no water class in the source).
# Freshwater metal rows marked "hardness-based" are evaluated with
# calc_total_metal_criterion() and therefore require a hardness (mg/L CaCO3).
# Other aquatic life metal criteria in the source are dissolved values; they
# are divided by the conversion factor from calc_cf() and do not need a
# hardness. Metal criteria returned by this function are total recoverable
# concentrations in ug/L.
#
# Criteria with a criteria_type of "Wet Season" or "Dry Season" require
# `season`; criteria with no criteria_type apply in either season.  `limit`
# selects MAX criteria (the lowest value is returned) or MIN criteria (the
# highest value is returned).  Fish tissue criteria (mg/kg) are excluded, and
# an error is raised if the remaining criteria are in more than one unit.
#
# With verbose = FALSE (the default), a single numeric value is returned; with
# verbose = TRUE, the winning row and its source metadata are returned as a
# one-row data frame.
lookup_hi_criterion <- function(parameter,
                                use = c("Aquatic Life", "Fish Consumption",
                                        "General", "Recreation"),
                                water_class = c("Freshwater", "Saltwater",
                                                "Inland Freshwater"),
                                season = NULL,
                                limit = c("MAX", "MIN"),
                                hardness = NULL,
                                verbose = FALSE,
                                criteria_file = "./scripts/hi_criteria_long_format.csv") {
  if (!is.character(parameter) || length(parameter) != 1L || is.na(parameter))
    stop("parameter must be one character string.")
  if (!is.character(use) || length(use) < 1L)
    stop("use must contain Aquatic Life, Fish Consumption, General, and/or Recreation.")
  if (!is.character(water_class) || length(water_class) < 1L)
    stop("water_class must contain Freshwater, Saltwater, and/or Inland Freshwater.")
  if (!is.logical(verbose) || length(verbose) != 1L || is.na(verbose))
    stop("verbose must be TRUE or FALSE.")
  limit <- match.arg(toupper(limit[1L]), c("MAX", "MIN"))
  
  normalize <- function(x) {
    x <- toupper(trimws(x))
    gsub("[^A-Z0-9]", "", x)
  }
  use_key <- normalize(use)
  use_map <- c(AQUATICLIFE = "Aquatic Life", AQUATIC = "Aquatic Life",
               FISHCONSUMPTION = "Fish Consumption", FISH = "Fish Consumption",
               GENERAL = "General", RECREATION = "Recreation")
  class_key <- normalize(water_class)
  class_map <- c(FRESHWATER = "Freshwater", FRESH = "Freshwater",
                 SALTWATER = "Saltwater", SALT = "Saltwater",
                 INLANDFRESHWATER = "Inland Freshwater",
                 INLAND = "Inland Freshwater")
  if (any(!use_key %in% names(use_map)))
    stop("use must be Aquatic Life, Fish Consumption, General, and/or Recreation.")
  if (any(!class_key %in% names(class_map)))
    stop("water_class must be Freshwater, Saltwater, and/or Inland Freshwater.")
  uses <- unname(use_map[use_key])
  classes <- unname(class_map[class_key])
  
  season_map <- c(WETSEASON = "Wet Season", WET = "Wet Season",
                  DRYSEASON = "Dry Season", DRY = "Dry Season")
  if (!is.null(season)) {
    if (!is.character(season) || length(season) != 1L || is.na(season) ||
        !normalize(season) %in% names(season_map))
      stop("season must be Wet Season or Dry Season.")
    season <- unname(season_map[normalize(season)])
  }
  
  dat <- read.csv(criteria_file, stringsAsFactors = FALSE, check.names = FALSE,
                  na.strings = c("", "NA"), fileEncoding = "UTF-8-BOM")
  names(dat) <- tolower(trimws(names(dat)))
  required <- c("substance_name", "criteria_name", "water_type",
                "criteria_class", "criteria_type", "limit_name", "value",
                "unit_name")
  if (any(!required %in% names(dat)))
    stop("criteria_file is missing: ",
         paste(setdiff(required, names(dat)), collapse = ", "))
  
  parameter_key <- normalize(parameter)
  dat_key <- normalize(dat$substance_name)
  hits <- dat[dat_key == parameter_key & dat$criteria_name %in% uses, , drop = FALSE]
  # Fish consumption and recreation criteria are not assigned a water class.
  hits <- hits[is.na(hits$water_type) | hits$water_type %in% classes, , drop = FALSE]
  hits <- hits[toupper(trimws(hits$limit_name)) == limit, , drop = FALSE]
  # Exclude fish tissue criteria such as methylmercury (mg/kg).
  hits <- hits[!grepl("/\\s*kg", hits$unit_name, ignore.case = TRUE), , drop = FALSE]
  if (!nrow(hits)) stop("No applicable criterion was found for parameter/use/water_class/limit.")
  
  seasonal <- !is.na(hits$criteria_type) &
    normalize(hits$criteria_type) %in% names(season_map)
  if (any(seasonal)) {
    if (is.null(season))
      stop("season (Wet Season or Dry Season) is required for seasonal criteria.")
    hits <- hits[!seasonal | unname(season_map[normalize(hits$criteria_type)]) == season,
                 , drop = FALSE]
  }
  
  metals <- c("ARSENIC", "CADMIUM", "CHROMIUMVI", "COPPER", "LEAD", "MERCURY",
              "NICKEL", "SELENIUM", "SILVER", "ZINC")
  is_metal <- parameter_key %in% metals
  # Parameter name and water class codes used by the metal functions above.
  metal_parameter <- if (parameter_key == "CHROMIUMVI") "CHROMIUM(VI)" else parameter_key
  class_code <- ifelse(hits$water_type %in% "Saltwater", "SALT", "FRESH")
  hits$criterion_value <- suppressWarnings(as.numeric(hits$value))
  metal_rows <- is_metal & hits$criteria_name == "Aquatic Life"
  hard_rows <- metal_rows & hits$water_type %in% "Freshwater" &
    normalize(hits$value) == "HARDNESSBASED"
  if (any(hard_rows)) {
    if (is.null(hardness) || length(hardness) != 1L || !is.numeric(hardness) ||
        is.na(hardness) || hardness <= 0)
      stop("A positive hardness (mg/L CaCO3) is required for hardness-based freshwater metals.")
    for (i in which(hard_rows)) {
      hits$criterion_value[i] <- calc_total_metal_criterion(
        hardness, metal_parameter, hits$criteria_class[i], class_code[i])
      hits$unit_name[i] <- "ug/L"
    }
  }
  # Convert fixed dissolved aquatic life metal criteria to total recoverable.
  fixed_rows <- metal_rows & !hard_rows & is.finite(hits$criterion_value)
  for (i in which(fixed_rows)) {
    type_code <- make_type(metal_parameter, hits$criteria_class[i], class_code[i])
    hits$criterion_value[i] <- hits$criterion_value[i] /
      calc_cf(hardness = hardness, type_code = type_code)
  }
  hits <- hits[is.finite(hits$criterion_value), , drop = FALSE]
  if (!nrow(hits)) stop("No numeric criterion was available for the requested inputs.")
  
  # Criteria are only comparable within one unit. A trailing "N" or "P" (as in
  # "ug/L N") names the element and is ignored; micro signs are equivalent.
  unit_key <- tolower(gsub("\\s+", "", sub("\\s+[NP]$", "", hits$unit_name)))
  unit_key <- gsub("µ|μ", "u", unit_key)
  if (length(unique(unit_key)) > 1L)
    stop("Applicable criteria are in more than one unit (",
         paste(unique(hits$unit_name), collapse = ", "),
         "); narrow use or water_class.")
  
  winner_index <- if (limit == "MAX") which.min(hits$criterion_value) else
    which.max(hits$criterion_value)
  winner <- hits[winner_index, , drop = FALSE]
  if (!verbose) return(as.numeric(winner$criterion_value[1L]))
  winner$value_original <- winner$value
  winner$value <- winner$criterion_value
  if (is_metal) winner$unit_name <- "ug/L total recoverable"
  winner$criterion_value <- NULL
  winner
}