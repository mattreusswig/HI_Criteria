

#####################################################################
#####   Compute ARIZONA Harness-based Metal Criterion   #############
#####################################################################

# Date: 2022-11-19
# Author: M. Reusswig
# Note: 

# Function to ingest string codes, check they are valid, and return a concatenated
# output of the codes if they are.
# parameter = string giving the name of the metal
# type = string giving the type of criteria (acute or chronic)
# use = string giving the Arizona designated use (AWC, AWWW, AWEDW, AWE)
make_type <- function(parameter, type, use) {
  
  '%not_in%' <- Negate('%in%')
  
  # Validate input are string values
  if(!is.character(parameter) | !is.character(type) | !is.character(use)) stop("Parameter, type, and use inputs must be strings.")
  
  # Validate only one of each input was used
  if(length(parameter) != 1 | length(type) != 1 | length(use) != 1) stop("Parameter, type, and use inputs must each have a single element input.")
  
  # Normalize capitalization of input
  parameter <- toupper(parameter)
  type      <- toupper(type)
  use       <- toupper(use)
  
  # Check in input values are valid
  valid_parameters <- c("CADMIUM",
                        "CHROMIUM(VI)",
                        "COPPER",
                        "LEAD",
                        "NICKEL",
                        "SILVER",
                        "ZINC")
  valid_types <- c("CHRONIC", "ACUTE")
  # CHECK: AWC/AWW/AWEDW/AWE are Arizona use codes; the supplied Hawaii
  # template does not assign separate factor sets to these uses. In particular,
  # the AWE factors below cannot be checked against the Hawaii sources.
  valid_uses <- c("AWC", "AWW", "AWEDW", "AWE")
  
  if(parameter %not_in% valid_parameters) stop(paste0("Invalid parameter input. Parameter name must by one of: ", paste0(valid_parameters, collapse =  ", ")))
  if(type %not_in% valid_types) stop(paste0("Invalid type input. Type name must by one of: ", paste0(valid_types, collapse =  ", ")))
  if(use %not_in% valid_uses) stop(paste0("Invalid use input. Use name must by one of: ", paste0(valid_uses, collapse =  ", ")))
  
  # Concatenate each input into a combined code
  out <- paste0(parameter, "_", type, "_", use)
  
  return(out)
  
  }

# Function to compute the metal's conversion factor.
# hardness = numeric type, hardness value in mg/L CaCO3
# type_code = string type value taken from make_type function
calc_cf <- function(hardness, type_code) {
  
  b = switch(type_code, 
             # 0 is used for type_code values for which there is not a 
             # valid Conversion Factor
             
             "CADMIUM_ACUTE_AWC"   = 1.136672,
             "CADMIUM_ACUTE_AWW"   = 1.136672,
             "CADMIUM_ACUTE_AWEDW" = 1.136672,
             "CADMIUM_ACUTE_AWE"   = 1.136672,
             
             "CADMIUM_CHRONIC_AWC"   = 1.101672,
             "CADMIUM_CHRONIC_AWW"   = 1.101672,
             "CADMIUM_CHRONIC_AWEDW" = 1.101672,
             "CADMIUM_CHRONIC_AWE"   = 0,
             
             # CHECK: Draft HIP Table 1 gives Chromium (VI) freshwater CFs of 0.982 acute and 0.962 chronic; values below still differ.
             "CHROMIUM(VI)_ACUTE_AWC"   = 0.316,
             "CHROMIUM(VI)_ACUTE_AWW"   = 0.316,
             "CHROMIUM(VI)_ACUTE_AWEDW" = 0.316,
             "CHROMIUM(VI)_ACUTE_AWE"   = 0.316,
             
             "CHROMIUM(VI)_CHRONIC_AWC"   = 0.860,
             "CHROMIUM(VI)_CHRONIC_AWW"   = 0.860,
             "CHROMIUM(VI)_CHRONIC_AWEDW" = 0.860,
             "CHROMIUM(VI)_CHRONIC_AWE"   = 0,
             
             "COPPER_ACUTE_AWC"   = 0.960,
             "COPPER_ACUTE_AWW"   = 0.960,
             "COPPER_ACUTE_AWEDW" = 0.960,
             "COPPER_ACUTE_AWE"   = 0.960,
             
             "COPPER_CHRONIC_AWC"   = 0.960,
             "COPPER_CHRONIC_AWW"   = 0.960,
             "COPPER_CHRONIC_AWEDW" = 0.960,
             "COPPER_CHRONIC_AWE"   = 0,
             
             "LEAD_ACUTE_AWC"   = 1.46203,
             "LEAD_ACUTE_AWW"   = 1.46203,
             "LEAD_ACUTE_AWEDW" = 1.46203,
             "LEAD_ACUTE_AWE"   = 1.46203,
             
             "LEAD_CHRONIC_AWC"   = 1.46203,
             "LEAD_CHRONIC_AWW"   = 1.46203,
             "LEAD_CHRONIC_AWEDW" = 1.46203,
             "LEAD_CHRONIC_AWE"   = 0,
             
             "NICKEL_ACUTE_AWC"   = 0.998,
             "NICKEL_ACUTE_AWW"   = 0.998,
             "NICKEL_ACUTE_AWEDW" = 0.998,
             "NICKEL_ACUTE_AWE"   = 0.998,
             
             "NICKEL_CHRONIC_AWC"   = 0.997,
             "NICKEL_CHRONIC_AWW"   = 0.997,
             "NICKEL_CHRONIC_AWEDW" = 0.997,
             "NICKEL_CHRONIC_AWE"   = 0,
             
             "SILVER_ACUTE_AWC"   = 0.850,
             "SILVER_ACUTE_AWW"   = 0.850,
             "SILVER_ACUTE_AWEDW" = 0.850,
             "SILVER_ACUTE_AWE"   = 0.850,
             
             "SILVER_CHRONIC_AWC"   = 0,
             "SILVER_CHRONIC_AWW"   = 0,
             "SILVER_CHRONIC_AWEDW" = 0,
             "SILVER_CHRONIC_AWE"   = 0,
             
             "ZINC_ACUTE_AWC"   = 0.978,
             "ZINC_ACUTE_AWW"   = 0.978,
             "ZINC_ACUTE_AWEDW" = 0.978,
             "ZINC_ACUTE_AWE"   = 0.978,
             
             "ZINC_CHRONIC_AWC"   = 0.978, # CHECK: Draft HIP Table 1 freshwater chronic CF gives 0.986.
             "ZINC_CHRONIC_AWW"   = 0.978, # CHECK: Draft HIP Table 1 freshwater chronic CF gives 0.986.
             "ZINC_CHRONIC_AWEDW" = 0.978, # CHECK: Draft HIP Table 1 freshwater chronic CF gives 0.986.
             "ZINC_CHRONIC_AWE"   = 0
             )
  
  m = switch(type_code, 
             # 0 is used for type_code values for which there is not a 
             # valid Conversion Factor, or for which there is no slope (m) term.
             
             "CADMIUM_ACUTE_AWC"   = -0.041838,
             "CADMIUM_ACUTE_AWW"   = -0.041838,
             "CADMIUM_ACUTE_AWEDW" = -0.041838,
             "CADMIUM_ACUTE_AWE"   = -0.041838,
             
             "CADMIUM_CHRONIC_AWC"   = -0.041838,
             "CADMIUM_CHRONIC_AWW"   = -0.041838,
             "CADMIUM_CHRONIC_AWEDW" = -0.041838,
             "CADMIUM_CHRONIC_AWE"   = 0,
             
             "CHROMIUM(VI)_ACUTE_AWC"   = 0,
             "CHROMIUM(VI)_ACUTE_AWW"   = 0,
             "CHROMIUM(VI)_ACUTE_AWEDW" = 0,
             "CHROMIUM(VI)_ACUTE_AWE"   = 0,
             
             "CHROMIUM(VI)_CHRONIC_AWC"   = 0,
             "CHROMIUM(VI)_CHRONIC_AWW"   = 0,
             "CHROMIUM(VI)_CHRONIC_AWEDW" = 0,
             "CHROMIUM(VI)_CHRONIC_AWE"   = 0,
             
             "COPPER_ACUTE_AWC"   = 0,
             "COPPER_ACUTE_AWW"   = 0,
             "COPPER_ACUTE_AWEDW" = 0,
             "COPPER_ACUTE_AWE"   = 0,
             
             "COPPER_CHRONIC_AWC"   = 0,
             "COPPER_CHRONIC_AWW"   = 0,
             "COPPER_CHRONIC_AWEDW" = 0,
             "COPPER_CHRONIC_AWE"   = 0,
             
             "LEAD_ACUTE_AWC"   = -0.145712,
             "LEAD_ACUTE_AWW"   = -0.145712,
             "LEAD_ACUTE_AWEDW" = -0.145712,
             "LEAD_ACUTE_AWE"   = -0.145712,
             
             "LEAD_CHRONIC_AWC"   = -0.145712,
             "LEAD_CHRONIC_AWW"   = -0.145712,
             "LEAD_CHRONIC_AWEDW" = -0.145712,
             "LEAD_CHRONIC_AWE"   = 0,
             
             "NICKEL_ACUTE_AWC"   = 0,
             "NICKEL_ACUTE_AWW"   = 0,
             "NICKEL_ACUTE_AWEDW" = 0,
             "NICKEL_ACUTE_AWE"   = 0,
             
             "NICKEL_CHRONIC_AWC"   = 0,
             "NICKEL_CHRONIC_AWW"   = 0,
             "NICKEL_CHRONIC_AWEDW" = 0,
             "NICKEL_CHRONIC_AWE"   = 0,
             
             "SILVER_ACUTE_AWC"   = 0,
             "SILVER_ACUTE_AWW"   = 0,
             "SILVER_ACUTE_AWEDW" = 0,
             "SILVER_ACUTE_AWE"   = 0,
             
             "SILVER_CHRONIC_AWC"   = 0,
             "SILVER_CHRONIC_AWW"   = 0,
             "SILVER_CHRONIC_AWEDW" = 0,
             "SILVER_CHRONIC_AWE"   = 0,
             
             "ZINC_ACUTE_AWC"   = 0,
             "ZINC_ACUTE_AWW"   = 0,
             "ZINC_ACUTE_AWEDW" = 0,
             "ZINC_ACUTE_AWE"   = 0,
             
             "ZINC_CHRONIC_AWC"   = 0,
             "ZINC_CHRONIC_AWW"   = 0,
             "ZINC_CHRONIC_AWEDW" = 0,
             "ZINC_CHRONIC_AWE"   = 0
             )
  
  cf = b + log(hardness) * m
  
  return(cf)
}

# Function to compute the metal's total recoverable criterion in ug/L.
# hardness = numeric type, hardness value in mg/L CaCO3
# hardness = numeric type, hardness value in mg/L CaCO3
# parameter = string giving the name of the metal
# type = string giving the type of criteria (acute or chronic)
# use = string giving the Arizona designated use (AWC, AWWW, AWEDW, AWE)
calc_total_metal_criterion <- function(hardness, parameter, type, use) {
  
  # Validate input and create a type code to feed cf and total metal criterion functions.
  type_code <- make_type(parameter = parameter, type = type, use = use)
  
  # Use type_code to get the slope and intercept of the total recoverable metal criterion.
  # CHECK: Template columns L:O calculate freshwater criteria, while this function labels its output total recoverable; verify whether CF conversion is needed.
  b = switch(type_code, 
             # -999 is used for type_code values for which there is not a 
             # valid dissolved criterion
             
             "CADMIUM_ACUTE_AWC"   = -3.924, # CHECK: Hawaii template intercept (acute) gives -3.828.
             "CADMIUM_ACUTE_AWW"   = -3.6867, # CHECK: Hawaii template intercept (acute) gives -3.828.
             "CADMIUM_ACUTE_AWEDW" = -3.6867, # CHECK: Hawaii template intercept (acute) gives -3.828.
             "CADMIUM_ACUTE_AWE"   = -0.9691,
             
             "CADMIUM_CHRONIC_AWC"   = -4.719, # CHECK: Hawaii template intercept (chronic) gives -3.49.
             "CADMIUM_CHRONIC_AWW"   = -2.715, # CHECK: Hawaii template intercept (chronic) gives -3.49.
             "CADMIUM_CHRONIC_AWEDW" = -2.715, # CHECK: Hawaii template intercept (chronic) gives -3.49.
             "CADMIUM_CHRONIC_AWE"   = -999,
             
             # CHECK: Hawaii template gives fixed Chromium (VI) freshwater criteria, not hardness-based slopes/intercepts; values below still need replacement.
             "CHROMIUM(VI)_ACUTE_AWC"   = 3.7256,
             "CHROMIUM(VI)_ACUTE_AWW"   = 3.7256,
             "CHROMIUM(VI)_ACUTE_AWEDW" = 3.7256,
             "CHROMIUM(VI)_ACUTE_AWE"   = 4.936,
             
             "CHROMIUM(VI)_CHRONIC_AWC"   = 0.6848,
             "CHROMIUM(VI)_CHRONIC_AWW"   = 0.6848,
             "CHROMIUM(VI)_CHRONIC_AWEDW" = 0.6848,
             "CHROMIUM(VI)_CHRONIC_AWE"   = -999,
             
             "COPPER_ACUTE_AWC"   = -1.702, # CHECK: Hawaii template intercept (acute) gives -1.464.
             "COPPER_ACUTE_AWW"   = -1.702, # CHECK: Hawaii template intercept (acute) gives -1.464.
             "COPPER_ACUTE_AWEDW" = -1.702, # CHECK: Hawaii template intercept (acute) gives -1.464.
             "COPPER_ACUTE_AWE"   = -1.151,
             
             "COPPER_CHRONIC_AWC"   = -1.702, # CHECK: Hawaii template intercept (chronic) gives -1.465.
             "COPPER_CHRONIC_AWW"   = -1.702, # CHECK: Hawaii template intercept (chronic) gives -1.465.
             "COPPER_CHRONIC_AWEDW" = -1.702, # CHECK: Hawaii template intercept (chronic) gives -1.465.
             "COPPER_CHRONIC_AWE"   = -999,
             
             "LEAD_ACUTE_AWC"   = -1.46,
             "LEAD_ACUTE_AWW"   = -1.46,
             "LEAD_ACUTE_AWEDW" = -1.46,
             "LEAD_ACUTE_AWE"   = -0.713,
             
             "LEAD_CHRONIC_AWC"   = -4.705,
             "LEAD_CHRONIC_AWW"   = -4.705,
             "LEAD_CHRONIC_AWEDW" = -4.705,
             "LEAD_CHRONIC_AWE"   = -999,
             
             "NICKEL_ACUTE_AWC"   = 2.255, # CHECK: Hawaii template intercept (acute) gives 4.02.
             "NICKEL_ACUTE_AWW"   = 2.255, # CHECK: Hawaii template intercept (acute) gives 4.02.
             "NICKEL_ACUTE_AWEDW" = 2.255, # CHECK: Hawaii template intercept (acute) gives 4.02.
             "NICKEL_ACUTE_AWE"   = 4.438,
             
             "NICKEL_CHRONIC_AWC"   = 0.058, # CHECK: Hawaii template intercept (chronic) gives 1.06.
             "NICKEL_CHRONIC_AWW"   = 0.058, # CHECK: Hawaii template intercept (chronic) gives 1.06.
             "NICKEL_CHRONIC_AWEDW" = 0.058, # CHECK: Hawaii template intercept (chronic) gives 1.06.
             "NICKEL_CHRONIC_AWE"   = -999,
             
             "SILVER_ACUTE_AWC"   = -6.59, # CHECK: Hawaii template intercept (acute) gives -6.52.
             "SILVER_ACUTE_AWW"   = -6.59, # CHECK: Hawaii template intercept (acute) gives -6.52.
             "SILVER_ACUTE_AWEDW" = -6.59, # CHECK: Hawaii template intercept (acute) gives -6.52.
             "SILVER_ACUTE_AWE"   = -6.59,
             
             # CHECK: Hawaii template has chronic silver factors (m = 1.72, b = -6.52), but Draft HIP Table 1 says no published chronic freshwater criterion; reconcile before use.
             "SILVER_CHRONIC_AWC"   = -999,
             "SILVER_CHRONIC_AWW"   = -999,
             "SILVER_CHRONIC_AWEDW" = -999,
             "SILVER_CHRONIC_AWE"   = -999,
             
             "ZINC_ACUTE_AWC"   = 0.884, # CHECK: Hawaii template intercept (acute) gives 1.95.
             "ZINC_ACUTE_AWW"   = 0.884, # CHECK: Hawaii template intercept (acute) gives 1.95.
             "ZINC_ACUTE_AWEDW" = 0.884, # CHECK: Hawaii template intercept (acute) gives 1.95.
             "ZINC_ACUTE_AWE"   = 3.134,
             
             "ZINC_CHRONIC_AWC"   = 0.884, # CHECK: Hawaii template intercept (chronic) gives 1.95.
             "ZINC_CHRONIC_AWW"   = 0.884, # CHECK: Hawaii template intercept (chronic) gives 1.95.
             "ZINC_CHRONIC_AWEDW" = 0.884, # CHECK: Hawaii template intercept (chronic) gives 1.95.
             "ZINC_CHRONIC_AWE"   = -999
  )
  
  m = switch(type_code, 
             # 0 is used for type_code values for which there is not a 
             # valid dissolved criterion.
             
             "CADMIUM_ACUTE_AWC"   = 1.0166, # CHECK: Hawaii template slope (acute) gives 1.128.
             "CADMIUM_ACUTE_AWW"   = 1.128,
             "CADMIUM_ACUTE_AWEDW" = 1.128,
             "CADMIUM_ACUTE_AWE"   = 1.128,
             
             "CADMIUM_CHRONIC_AWC"   = 0.7409, # CHECK: Hawaii template slope (chronic) gives 0.7852.
             "CADMIUM_CHRONIC_AWW"   = 0.7852,
             "CADMIUM_CHRONIC_AWEDW" = 0.7852,
             "CADMIUM_CHRONIC_AWE"   = 0,
             
             "CHROMIUM(VI)_ACUTE_AWC"   = 0.819,
             "CHROMIUM(VI)_ACUTE_AWW"   = 0.819,
             "CHROMIUM(VI)_ACUTE_AWEDW" = 0.819,
             "CHROMIUM(VI)_ACUTE_AWE"   = 0.819,
             
             "CHROMIUM(VI)_CHRONIC_AWC"   = 0.819,
             "CHROMIUM(VI)_CHRONIC_AWW"   = 0.819,
             "CHROMIUM(VI)_CHRONIC_AWEDW" = 0.819,
             "CHROMIUM(VI)_CHRONIC_AWE"   = 0,
             
             "COPPER_ACUTE_AWC"   = 0.9422,
             "COPPER_ACUTE_AWW"   = 0.9422,
             "COPPER_ACUTE_AWEDW" = 0.9422,
             "COPPER_ACUTE_AWE"   = 0.9422,
             
             "COPPER_CHRONIC_AWC"   = 0.8545, # CHECK: Hawaii template slope (chronic) gives 0.8548.
             "COPPER_CHRONIC_AWW"   = 0.8545, # CHECK: Hawaii template slope (chronic) gives 0.8548.
             "COPPER_CHRONIC_AWEDW" = 0.8545, # CHECK: Hawaii template slope (chronic) gives 0.8548.
             "COPPER_CHRONIC_AWE"   = 0,
             
             "LEAD_ACUTE_AWC"   = 1.273,
             "LEAD_ACUTE_AWW"   = 1.273,
             "LEAD_ACUTE_AWEDW" = 1.273,
             "LEAD_ACUTE_AWE"   = 1.273,
             
             "LEAD_CHRONIC_AWC"   = 1.273,
             "LEAD_CHRONIC_AWW"   = 1.273,
             "LEAD_CHRONIC_AWEDW" = 1.273,
             "LEAD_CHRONIC_AWE"   = 0,
             
             "NICKEL_ACUTE_AWC"   = 0.846, # CHECK: Hawaii template slope (acute) gives 0.76.
             "NICKEL_ACUTE_AWW"   = 0.846, # CHECK: Hawaii template slope (acute) gives 0.76.
             "NICKEL_ACUTE_AWEDW" = 0.846, # CHECK: Hawaii template slope (acute) gives 0.76.
             "NICKEL_ACUTE_AWE"   = 0.846,
             
             "NICKEL_CHRONIC_AWC"   = 0.846, # CHECK: Hawaii template slope (chronic) gives 0.76.
             "NICKEL_CHRONIC_AWW"   = 0.846, # CHECK: Hawaii template slope (chronic) gives 0.76.
             "NICKEL_CHRONIC_AWEDW" = 0.846, # CHECK: Hawaii template slope (chronic) gives 0.76.
             "NICKEL_CHRONIC_AWE"   = 0,
             
             "SILVER_ACUTE_AWC"   = 1.72,
             "SILVER_ACUTE_AWW"   = 1.72,
             "SILVER_ACUTE_AWEDW" = 1.72,
             "SILVER_ACUTE_AWE"   = 1.72,
             
             "SILVER_CHRONIC_AWC"   = 0,
             "SILVER_CHRONIC_AWW"   = 0,
             "SILVER_CHRONIC_AWEDW" = 0,
             "SILVER_CHRONIC_AWE"   = 0,
             
             "ZINC_ACUTE_AWC"   = 0.8473, # CHECK: Hawaii template slope (acute) gives 0.83.
             "ZINC_ACUTE_AWW"   = 0.8473, # CHECK: Hawaii template slope (acute) gives 0.83.
             "ZINC_ACUTE_AWEDW" = 0.8473, # CHECK: Hawaii template slope (acute) gives 0.83.
             "ZINC_ACUTE_AWE"   = 0.8473,
             
             "ZINC_CHRONIC_AWC"   = 0.8473, # CHECK: Hawaii template slope (chronic) gives 0.83.
             "ZINC_CHRONIC_AWW"   = 0.8473, # CHECK: Hawaii template slope (chronic) gives 0.83.
             "ZINC_CHRONIC_AWEDW" = 0.8473, # CHECK: Hawaii template slope (chronic) gives 0.83.
             "ZINC_CHRONIC_AWE"   = 0
  )
  
  total_crit = exp(b + log(hardness) * m)
  
  total_crit <- ifelse(total_crit == 0, NA, total_crit)
  
  return(total_crit)
}


# Function to compute the metal's dissolved criterion.
# hardness = numeric type, hardness value in mg/L CaCO3
# parameter = string giving the name of the metal
# type = string giving the type of criteria (acute or chronic)
# use = string giving the Arizona designated use (AWC, AWWW, AWEDW, AWE)
calc_dissolved_metal_criterion <- function(hardness, parameter, type, use) {
  
  # Validate input and create a type code to feed cf and total metal criterion functions.
  type_code <- make_type(parameter = parameter, type = type, use = use)
  
  # Calculate the conversion factor
  cf <- calc_cf(hardness = hardness, type_code = type_code)
  
  # Calculate the total recoverable criterion in units of ug/L 
  total_crit <- calc_total_metal_criterion(hardness = hardness,
                                           parameter = parameter,
                                           type = type,
                                           use = use)
  
  # Calculate the dissolved criterion in units of ug/L
  dissolved_crit <- total_crit * cf
  
  return(dissolved_crit)
  
}


# Return the lowest criterion in hi_criteria_long_format.csv for a parameter.
#
# `use` may contain "Aquatic Life", "Fish Consumption", or both.  For
# aquatic-life criteria, `water_class` may contain "Freshwater", "Saltwater",
# or both.  Freshwater metal rows marked "hardness-based" are evaluated with
# calc_total_metal_criterion() and therefore require a hardness (mg/L CaCO3).
# Metal criteria returned by this function are total recoverable concentrations
# in ug/L.  With verbose = FALSE (the default), a single numeric value is
# returned; with verbose = TRUE, the winning row and its source metadata are
# returned as a one-row data frame.
lookup_hi_criterion <- function(parameter,
                                use = c("Aquatic Life", "Fish Consumption"),
                                water_class = c("Freshwater", "Saltwater"),
                                hardness = NULL,
                                verbose = FALSE,
                                criteria_file = "hi_criteria_long_format.csv") {
  if (!is.character(parameter) || length(parameter) != 1L || is.na(parameter))
    stop("parameter must be one character string.")
  if (!is.character(use) || length(use) < 1L)
    stop("use must contain Aquatic Life and/or Fish Consumption.")
  if (!is.character(water_class) || length(water_class) < 1L)
    stop("water_class must contain Freshwater and/or Saltwater.")
  if (!is.logical(verbose) || length(verbose) != 1L || is.na(verbose))
    stop("verbose must be TRUE or FALSE.")

  normalize <- function(x) {
    x <- toupper(trimws(x))
    gsub("[^A-Z0-9]", "", x)
  }
  use_key <- normalize(use)
  use_map <- c(AQUATICLIFE = "Aquatic Life", AQUATIC = "Aquatic Life",
               FISHCONSUMPTION = "Fish Consumption", FISH = "Fish Consumption")
  class_key <- normalize(water_class)
  class_map <- c(FRESHWATER = "Freshwater", FRESH = "Freshwater",
                 SALTWATER = "Saltwater", SALT = "Saltwater")
  if (any(!use_key %in% names(use_map)))
    stop("use must be Aquatic Life, Fish Consumption, or both.")
  if (any(!class_key %in% names(class_map)))
    stop("water_class must be Freshwater, Saltwater, or both.")
  uses <- unname(use_map[use_key])
  classes <- unname(class_map[class_key])

  dat <- read.csv(criteria_file, stringsAsFactors = FALSE, check.names = FALSE,
                  na.strings = c("", "NA"), fileEncoding = "UTF-8-BOM")
  names(dat)[1:8] <- c("substance_name", "cas_no", "criteria_name",
                       "water_type", "criteria_class", "limit_name",
                       "value", "unit_name")
  parameter_key <- normalize(parameter)
  dat_key <- normalize(dat$substance_name)
  hits <- dat[dat_key == parameter_key & dat$criteria_name %in% uses, , drop = FALSE]
  # Fish-consumption criteria are not assigned a water class in the source.
  hits <- hits[hits$criteria_name == "Fish Consumption" |
                 hits$water_type %in% classes, , drop = FALSE]
  if (!nrow(hits)) stop("No applicable criterion was found for parameter/use/water_class.")

  metals <- c("CADMIUM", "CHROMIUMVI", "COPPER", "LEAD", "NICKEL", "SILVER", "ZINC")
  is_metal <- parameter_key %in% metals
  hits$criterion_ug_L <- suppressWarnings(as.numeric(hits$value))
  hard_rows <- is_metal & hits$criteria_name == "Aquatic Life" &
    hits$water_type == "Freshwater" & normalize(hits$value) == "HARDNESSBASED"
  if (any(hard_rows)) {
    if (is.null(hardness) || length(hardness) != 1L || !is.numeric(hardness) ||
        is.na(hardness) || hardness <= 0)
      stop("A positive hardness (mg/L CaCO3) is required for hardness-based freshwater metals.")
    for (i in which(hard_rows)) {
      p <- toupper(trimws(parameter))
      if (p == "CHROMIUM VI") p <- "CHROMIUM(VI)"
      hits$criterion_ug_L[i] <- calc_total_metal_criterion(
        hardness, p, hits$criteria_class[i], "AWC")
    }
  }
  hits <- hits[is.finite(hits$criterion_ug_L) & hits$criterion_ug_L >= 0, , drop = FALSE]
  if (!nrow(hits)) stop("No numeric criterion was available for the requested inputs.")
  winner <- hits[which.min(hits$criterion_ug_L), , drop = FALSE]
  if (!verbose) return(as.numeric(winner$criterion_ug_L[1L]))
  winner$value_original <- winner$value
  winner$value <- winner$criterion_ug_L
  winner$unit_name <- "ug/L total recoverable"
  winner$criterion_ug_L <- NULL
  winner
}
