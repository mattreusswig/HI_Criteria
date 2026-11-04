

#####################################################################
#####   Compute HAWAII Harness-based Metal Criterion   ##############
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
             
             # CHECK: Draft HIP Table 1 marks freshwater selenium as [4]
             # (bioaccumulated; do not adjust to percent dissolved), not 1.000.
             "SELENIUM_ACUTE_FRESH"   = (1),
             "SELENIUM_ACUTE_SALT"    = (0.998),
             
             "SELENIUM_CHRONIC_FRESH"   = (1),
             "SELENIUM_CHRONIC_SALT"    = (0.998),
             
             "SILVER_ACUTE_FRESH"   = (0.850),
             "SILVER_ACUTE_SALT"    = (0.850),
             
             "SILVER_CHRONIC_FRESH"   = (0.850),
             # CHECK: Draft HIP Table 1 marks chronic marine silver as [2]
             # (no published criterion), rather than a numeric CF of 1.
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
             
             "CADMIUM_CHRONIC_FRESH"  = 0.041838,
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
  
  # The Hawaii equations are b - ln(hardness) * m for Cd and Pb.  The prior
  # division produced incorrect conversion factors (and incorrect units).
  cf = b - log(hardness) * m
  
  return(cf)
}

# Function to compute the metal's total recoverable criterion in ug/L. This function
# assumes the permittee's metal translator is equal to the conversion factor (CF)
# specified in the water quality standards.
# hardness = numeric type, hardness value in mg/L CaCO3
# parameter = string giving the name of the metal
# type = string giving the type of criteria (acute or chronic)
# class = string giving the water class (FRESH or SALT)
calc_total_metal_criterion <- function(hardness, parameter, type, class) {
  
  # Validate input and create a type code to feed cf and total metal criterion functions.
  type_code <- make_type(parameter = parameter, type = type, class = class)
  
  # class type_code to get the slope and intercept of the total recoverable metal criterion.
  b = switch(type_code, 
             # -999 is used for type_code values for which there is not a 
             # valid dissolved criterion
             
             "CADMIUM_ACUTE_FRESH"   = -3.828,
             "CADMIUM_ACUTE_SALT"   = -999,
             
             
             "CADMIUM_CHRONIC_FRESH"   = -3.49,
             "CADMIUM_CHRONIC_SALT"   = -999,
             
             "COPPER_ACUTE_FRESH"   = -1.465,
             "COPPER_ACUTE_SALT"   = -999,
             
             "COPPER_CHRONIC_FRESH"   = -1.465,
             "COPPER_CHRONIC_SALT"   = -999,
             
             "LEAD_ACUTE_FRESH"   = -1.46,
             "LEAD_ACUTE_SALT"   = -999,
             
             "LEAD_CHRONIC_FRESH"   = -4.705,
             "LEAD_CHRONIC_SALT"   = -999,
             
             "NICKEL_ACUTE_FRESH"   = 4.02,
             "NICKEL_ACUTE_SALT"   = -999,
             
             "NICKEL_CHRONIC_FRESH"   = 1.06,
             "NICKEL_CHRONIC_SALT"   = -999,
             
             "SILVER_ACUTE_FRESH"   = -6.52,
             "SILVER_ACUTE_SALT"   = -999,
             
             "SILVER_CHRONIC_FRESH"   = -6.52,
             "SILVER_CHRONIC_SALT"   = -999,
             
             "ZINC_ACUTE_FRESH"   = 1.95,
             "ZINC_ACUTE_SALT"   = -999,
             
             "ZINC_CHRONIC_FRESH"   = 1.95,
             "ZINC_CHRONIC_SALT"   = -999
               )
  
  m = switch(type_code, 
             # 0 is used for type_code values for which there is not a 
             # valid dissolved criterion.
             
             "CADMIUM_ACUTE_FRESH"   = 1.128,
             "CADMIUM_ACUTE_SALT"   = 0,
             
             "CADMIUM_CHRONIC_FRESH"   = 0.7852,
             "CADMIUM_CHRONIC_SALT"   = 0,
             
             "COPPER_ACUTE_FRESH"   = 0.9422,
             "COPPER_ACUTE_SALT"   = 0,
             
             "COPPER_CHRONIC_FRESH"   = 0.8545,
             "COPPER_CHRONIC_SALT"   = 0,
             
             "LEAD_ACUTE_FRESH"   = 1.273,
             "LEAD_ACUTE_SALT"   = 0,
             
             "LEAD_CHRONIC_FRESH"   = 1.273,
             "LEAD_CHRONIC_SALT"   = 0,
             
             "NICKEL_ACUTE_FRESH"   = 0.760,
             "NICKEL_ACUTE_SALT"   = 0,
             
             "NICKEL_CHRONIC_FRESH"   = 0.760,
             "NICKEL_CHRONIC_SALT"   = 0,
             
             "SILVER_ACUTE_FRESH"   = 1.72,
             "SILVER_ACUTE_SALT"   = 0,
             
             "SILVER_CHRONIC_FRESH"   = 1.72,
             "SILVER_CHRONIC_SALT"   = 0,
             
             "ZINC_ACUTE_FRESH"   = 0.830,
             "ZINC_ACUTE_SALT"   = 0,
             
             "ZINC_CHRONIC_FRESH"   =  0.830,
             "ZINC_CHRONIC_SALT"   = 0
  )
  
  total_crit = exp(b + log(hardness) * m)
  
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


# Return the lowest applicable Hawaii criterion from hi_criteria_long_format.csv.
# `use` may be "Aquatic Life", "Fish Consumption", or both. `water_class` may
# be "Freshwater", "Saltwater", or both. Freshwater metal rows marked
# "hardness-based" are calculated with the total-recoverable metal equations
# above and require hardness in mg/L as CaCO3. The default return is a number
# in ug/L; verbose = TRUE returns that value plus the source metadata.
lookup_hi_criterion <- function(parameter,
                                use = c("Aquatic Life", "Fish Consumption"),
                                water_class = c("Freshwater", "Saltwater"),
                                hardness = NULL,
                                verbose = FALSE,
                                criteria_file = "hi_criteria_long_format.csv") {
  # Validate the required parameter and option inputs before reading the criteria table.
  if (!is.character(parameter) || length(parameter) != 1L || is.na(parameter))
    stop("parameter must be one character string.")
  if (!is.character(use) || !length(use))
    stop("use must contain Aquatic Life and/or Fish Consumption.")
  if (!is.character(water_class) || !length(water_class))
    stop("water_class must contain Freshwater and/or Saltwater.")
  if (!is.logical(verbose) || length(verbose) != 1L || is.na(verbose))
    stop("verbose must be TRUE or FALSE.")

  # Normalize labels so common variants such as "fresh" and "Freshwater"
  # can be used interchangeably by the caller.
  normalize <- function(x) gsub("[^A-Z0-9]", "", toupper(trimws(x)))
  # Map the accepted shorthand labels to the names used in the source table.
  use_map <- c(AQUATICLIFE = "Aquatic Life", AQUATIC = "Aquatic Life",
               FISHCONSUMPTION = "Fish Consumption", FISH = "Fish Consumption")
  class_map <- c(FRESHWATER = "Freshwater", FRESH = "Freshwater",
                 SALTWATER = "Saltwater", SALT = "Saltwater")
  uk <- normalize(use); ck <- normalize(water_class)
  # Reject labels that cannot be mapped to a source-table use or water class.
  if (any(!uk %in% names(use_map)))
    stop("use must be Aquatic Life, Fish Consumption, or both.")
  if (any(!ck %in% names(class_map)))
    stop("water_class must be Freshwater, Saltwater, or both.")
  uses <- unname(use_map[uk]); classes <- unname(class_map[ck])

  # Read the long-format criteria table and assign stable column names. The
  # UTF-8-BOM setting removes the byte-order mark in the first column header.
  dat <- read.csv(criteria_file, stringsAsFactors = FALSE, check.names = FALSE,
                  na.strings = c("", "NA"), fileEncoding = "UTF-8-BOM")
  names(dat)[1:8] <- c("substance_name", "cas_no", "criteria_name",
                       "water_type", "criteria_class", "limit_name",
                       "value", "unit_name")
  # Filter first by parameter and requested use, then apply the water-class
  # restriction. Fish-consumption rows have no water class and are retained
  # whenever Fish Consumption is requested.
  pkey <- normalize(parameter)
  hits <- dat[normalize(dat$substance_name) == pkey &
               dat$criteria_name %in% uses, , drop = FALSE]
  hits <- hits[hits$criteria_name == "Fish Consumption" |
                 hits$water_type %in% classes, , drop = FALSE]
  if (!nrow(hits)) stop("No applicable criterion was found for the requested inputs.")

  # Convert ordinary numeric criteria to numbers. Hardness-based metal rows
  # contain text in the source value column and are handled below.
  metal_keys <- c("ARSENIC", "CADMIUM", "CHROMIUMVI", "COPPER", "LEAD",
                  "MERCURY", "NICKEL", "SELENIUM", "SILVER", "ZINC")
  is_metal <- pkey %in% metal_keys
  hits$criterion_ug_L <- suppressWarnings(as.numeric(hits$value))
  hard <- pkey %in% metal_keys & hits$criteria_name == "Aquatic Life" &
    hits$water_type == "Freshwater" & normalize(hits$value) == "HARDNESSBASED"
  # Calculate each hardness-based freshwater metal criterion using the
  # total-recoverable equations defined earlier in this file.
  if (any(hard)) {
    # Hardness is required only when a selected row actually needs it.
    if (is.null(hardness) || length(hardness) != 1L || !is.numeric(hardness) ||
        is.na(hardness) || hardness <= 0)
      stop("A positive hardness (mg/L CaCO3) is required for hardness-based freshwater metals.")
    p <- toupper(trimws(parameter))
    if (pkey == "CHROMIUMVI") p <- "CHROMIUM(VI)"
    for (i in which(hard)) {
      hits$criterion_ug_L[i] <- calc_total_metal_criterion(
        hardness, p, hits$criteria_class[i], "FRESH")
    }
  }
  # Aquatic-life metal values in the long-format source are dissolved values.
  # Convert numeric aquatic-life metal rows to total recoverable using the
  # applicable dissolved-fraction conversion factor. Fish-consumption values
  # are already direct criteria and are not translated.
  numeric_metal <- is_metal & hits$criteria_name == "Aquatic Life" & !hard &
    is.finite(hits$criterion_ug_L)
  if (any(numeric_metal)) {
    conversion_hardness <- if (is.null(hardness)) 100 else hardness
    p <- toupper(trimws(parameter))
    if (pkey == "CHROMIUMVI") p <- "CHROMIUM(VI)"
    for (i in which(numeric_metal)) {
      class_code <- if (hits$water_type[i] == "Freshwater") "FRESH" else "SALT"
      type_code <- make_type(p, hits$criteria_class[i], class_code)
      cf <- calc_cf(conversion_hardness, type_code)
      if (is.finite(cf) && cf > 0)
        hits$criterion_ug_L[i] <- hits$criterion_ug_L[i] / cf
    }
  }
  # Remove unavailable or invalid values before selecting the most stringent
  # (lowest) applicable criterion.
  hits <- hits[is.finite(hits$criterion_ug_L) & hits$criterion_ug_L >= 0, , drop = FALSE]
  if (!nrow(hits)) stop("No numeric criterion was available for the requested inputs.")
  winner <- hits[which.min(hits$criterion_ug_L), , drop = FALSE]
  # Return only the numeric criterion unless the caller requests source
  # metadata and the classification fields for review.
  if (!verbose) return(as.numeric(winner$criterion_ug_L[1L]))
  winner$value_original <- winner$value
  winner$value <- winner$criterion_ug_L
  winner$unit_name <- "ug/L total recoverable"
  winner$criterion_ug_L <- NULL
  winner
}
