# functions.R

#'
#' @title load life history and fishery parameters for California 'fish' species
#' @description loads a data.frame from `data/species_parameters.csv` to be
#' used by `get_species_parms` that containslife history for each species
#' investigated, including: natural mortality (M); von Bertalannfy length-at-age parameters --
#' asymptotic maximum age (L_inf), growth rate (k), and time when average
#' length is 0 (t_0); maximum age (A_max); age of maturity (A_mat); age when
#' vulnerable to fishery (A_fish); and length-weight parameters (weight =
#' biom_const * length ^biom_exp)
#'
#' @return returns a data.frame with life history parameters for 18 species
#' used to investigate the influence of MPAs on resilience
#'
#' @param dts the time series to mimic with respect to frequency content
#' @param n_reps the number of mimic time series to create
#' @param scale_sd standard deviation of the resulting time series, default value 1
#' @param scale_mean mean  of the resulting time series, default value 0
#' @param r_seed specify random seed for reproducibility
#'
#' @examples
#'
#' sp_parms <- load_species_parms() # add how to use in targets workflow
#'

load_species_parms <-
  function(fp = file.path("data", "species_parameters.csv")) {
    spec_parms_fp <- fp
    spec_parms_df <- readr::read_csv(file = spec_parms_fp)
    return(spec_parms_df)
  }

#'
#' @title get life history and fishery parameters for each California 'fish' species
#'
#' @description a list of life history parameters for each species investigated,
#' including: natural mortality (M); von Bertalannfy length-at-age parameters
#' -- asymptotic maximum age (L_inf), growth rate (k), and time when average
#' length is 0 (t_0); maximum age (A_max); age of maturity (A_mat); age when
#' vulnerable to fishery (A_fish); and length-weight parameters (weight =
#' biom_const * length ^biom_exp)
#'
#' @return returns a list of life history parameters
#'
#' @param df_parms the time series to mimic with respect to frequency content
#' @param species_name the number of mimic time series to create
#'
#' @examples
#'
#' spec_parms <- load_species_parms()
#' lh_parms_cbzn <- get_species_parms(df_parms = spec_parms, species_name = "Cabezon")
#'
get_species_parms <- function(df_parms, species_name) {
  valid_species <-
    c(
      "Pacific cod",
      "Kelp rockfish",
      "Blue rockfish",
      "Black rockfish",
      "Gopher rockfish",
      "Lingcod",
      "Copper rockfish",
      "California scorpionfish",
      "Brown rockfish",
      "Yellowtail rockfish",
      "Vermilion rockfish",
      "Bocaccio",
      "China rockfish",
      "Cabezon",
      "Kelp greenling",
      "California sheephead",
      "Red sea urchin",
      "Kelp bass",
      "Olive rockfish",
      "Black and yellow rockfish"
    )
  
  
  df_species <- df_parms %>%
    dplyr::filter(species == species_name)
  
  species <- df_species$species
  M <- df_species$M
  L_inf <- df_species$L_inf
  k <- df_species$k
  t_0 <- df_species$t_0
  A_max <- df_species$A_max
  A_mat <- df_species$A_mat
  A_fish <- df_species$A_fish
  biom_const <- df_species$biom_const
  biom_exp <- df_species$biom_exp
  return(
    list(
      species = species,
      M = M,
      L_inf = L_inf,
      k = k,
      t_0 = t_0,
      A_max = A_max,
      A_mat = A_mat,
      A_fish = A_fish,
      biom_const = biom_const,
      biom_exp = biom_exp
    )
  )
}

#' Return a list life history parameters for multiple species
#'
#' Given a dataframe of fish life history parameters and a vector of species names,
#' this function retrieves the parameters for each species using the 
#' get_species_parms() function.
#'
#' @param parm_df Dataframe of model parameters by species
#' @param sp_names Character vector of species names
#' 
#' @return Named list of parameter lists, one list per species
#' 
#' @examples 
#' params_df <- data.frame(
#'   species = c("Cod", "Rockfish"),
#'   M = c(0.2, 0.1),
#'   L_inf = c(60, 40), 
#'   k = c(0.5, 0.7),
#'   t_0 = c(0.05, -0.05),
#'   A_max  c(15, 27),
#'   A_mat = c(3, 5),
#'   A_fish = c(4, 5),
#'   biom_const = c(1.1, 0.9),
#'   biom_exp = c(3.02, 3)
#' )
#'
#' species <- c("Cod", "Rockfish")
#'
#' species_params <- list_species_parms(params_df, species)
#'
#' @export 
#### USED 
list_species_parms <- function(parm_df,
                               sp_names) {
  sim_species_parms <- sp_names %>%
    purrr::set_names() %>%
    purrr::map(.,
               ~ get_species_parms(df_parms = parm_df, species_name =  .x))
  
  return(sim_species_parms)
}

#'
#' @title vectors and higher dimensional arrays of species specific life history
#' values
#'
#' @description a list of life history vectors and matrices for each species,
#' including: a vector of length 1 to maximum age (ages); a binary vector indicating
#' if fishing occurs at each age (F_select); a vector of a matrix of
#' cumulative survival to each age with no fishing mortality (surv_unfished); a matrix of cumulative
#' survival to each age at many levels of fishing mortality (surv_fished); vector
#' of lengths at age from von Bertalanffy parameters (L_a); vector of fecundity
#' at age (fecundity_at_age); a matrix of stable age distributions at many levels of fishing
#' mortality (SADs); a matrix with the number of eggs at each age across many
#' levels of fishing mortality (eggs), a vector of the total number of eggs produced
#' at each level of fishing mortality (EPRs).
#'
#' @return returns a list of vectors and matrices with life history values
#'
#' @param df_parms the time series to mimic with respect to frequency content
#' @param species_name the number of mimic time series to create
#'
#' @examples
#'
#' spec_parms <- load_species_parms()
#' lh_parms_cbzn <- get_species_parms(df_parms = spec_parms, species_name = "Cabezon")
#' derived_vars_cbzn <- make_derived_variables(spec_parms = lh_parms_cbzn, fish_mort = F_mort)
#'
make_derived_variables <- function(spec_parms,
                                   fish_mort) {
  max_age <- spec_parms$A_max
  max_age_minus_one <- max_age - 1
  fished_ages <- 1:max_age_minus_one
  
  age_fishing_starts <- spec_parms$A_fish
  F_select <-
    fished_ages >= age_fishing_starts # fishery selectivity
  
  natural_mortality <- spec_parms$M
  
  surv_unfished <-
    rep(exp(-natural_mortality), max_age_minus_one) # age-specific survivals in each patch
  
  # Each column is the fishing mortality at age, with F increasing left to right
  surv_fished <-
    matrix(0, nrow = max_age_minus_one, ncol = length(fish_mort))
  surv_fished <-
    purrr::map(fish_mort, ~ exp(-(natural_mortality + .x * F_select)))
  
  surv_fished <- matrix(
    unlist(surv_fished),
    ncol = length(surv_fished),
    nrow = (max_age_minus_one),
    byrow = FALSE
  )
  
  all_ages <- 1:max_age
  von_bert_L_inf <- spec_parms$L_inf
  von_bert_k <- spec_parms$k
  length_at_age_vb <-
    von_bert_L_inf * (1 - exp(-(von_bert_k * all_ages))) # vB growth
  
  age_of_maturation <- spec_parms$A_mat
  maturity_vector <- (all_ages >= age_of_maturation)
  fecundity_at_age <-
    spec_parms$biom_const * (length_at_age_vb * maturity_vector) ^ spec_parms$biom_exp
  
  SADs <- matrix(0, nrow = max_age, ncol = length(fish_mort))
  eggs <- matrix(0, nrow = max_age, ncol = length(fish_mort))
  
  for (i in 1:length(fish_mort)) {
    SADs[, i] <- cumprod(c(1, surv_fished[, i]))
    eggs[, i] <- SADs[, i] * fecundity_at_age
  }
  
  EPRs <- colSums(eggs)
  
  return(
    list(
      F_select = F_select,
      surv_unfished = surv_unfished,
      surv_fished = surv_fished,
      ages = all_ages,
      length_at_age = length_at_age_vb,
      fecundity_at_age = fecundity_at_age,
      SADs = SADs,
      eggs = eggs,
      EPRs = EPRs
    )
  )
}

#' List derived variables for multiple species
#'
#' For a list of species parameter lists and a fishing mortality rate, 
#' this function calculates derived variables for each species using
#' the make_derived_variables() function.
#'
#' @param spec_parms List of species parameter lists
#' @param sp_names Character vector of species names
#' @param fish_mort Fishing mortality rate  
#'
#' @return Named list of derived variable lists, one list per species
#'
#' @examples
#' params <- list(
#'   list(M = 0.2, L_inf = 60, k = 0.5), # Cod
#'   list(M = 0.1, L_inf = 40, k = 0.7)  # Rockfish
#' )
#' 
#' species <- c("Cod", "Rockfish")
#' 
#' derived_vars <- list_derived_variables(params, species, 0.3)
#'
#' @export
list_derived_variables <- function(spec_parms,
                                   sp_names,
                                   fish_mort) {
  # Validate inputs
  stopifnot(is.list(spec_parms))
  stopifnot(is.numeric(fish_mort))
  
  sim_species_derived_vars <- spec_parms %>%
    purrr::map(.,
               ~  make_derived_variables(spec_parms = .x, fish_mort = fish_mort))
  
  names(sim_species_derived_vars) <- sp_names
  
  return(sim_species_derived_vars)
}

#' List lifetime egg production for multiple species
#'
#' Takes a vector of species names and a list of derived variables, 
#' and returns the lifetime egg production (LEP) for each species.
#'
#' @param spec_names Character vector of species names
#' @param sim_spec_der_var List of derived variable lists, one per species
#'
#' @return Named numeric vector of LEP values, one per species  
#' 
#' @examples
#'  
#' derived_vars <- list(
#'   list(EPRs = c(2000, 4000, 6000)), # Species 1 
#'   list(EPRs = c(500, 1000, 1500)) # Species 2
#' )
#'  
#' species <- c("Cod", "Herring")
#'
#' lep_list <- list_lep(species, derived_vars)
#' 
#' @export
#### USED
list_lep <- function(spec_names, sim_spec_der_var) {
  lifetime_eggs_production <- spec_names %>%
    purrr::map(., ~ sim_spec_der_var[[.x]][["EPRs"]][1])
  names(lifetime_eggs_production) <- spec_names
  return(lifetime_eggs_production)
}

#' Downloads and processes annual MEI data from NOAA
#'
#' @description
#' This function downloads the extended Multivariate ENSO Index (MEI) data
#' from the NOAA Physical Sciences Laboratory website, processes it into an
#' annual time series, and adds a column for ENSO phase based on MEI values.
#'
#' @return
#' A data frame with columns:
#'   - year: The year (integer).
#'   - mei: The annual average MEI value (numeric).
#'   - phase: The ENSO phase, either "0" for negative MEI or "1" for positive MEI (character).
#'
#' @note
#' This function relies on an external website for data access and may be
#' affected by changes to the website's structure or data format.
#'
#' @references
#' NOAA Physical Sciences Laboratory, Multivariate ENSO Index (MEI): https://psl.noaa.gov/enso/mei/
#### USED
make_ann_mei_ext <- function() {
  
  mei_url <- "https://psl.noaa.gov/enso/mei.ext/table.ext.html"
  con <- file(mei_url, open = "rt", raw = TRUE)
  # read each line of the webpage with the monthly extended MEI data and
  # store each line as a character vector
  tmp <- readLines(con = con, encoding = 'UTF-8')
  close(con)
  
  # Drop the metadata and keep the MEI data
  tmp <- tmp[13:148]
  
  for (i in 2:length(tmp)) {
    # read each line (except the header) as as a tab delimited row of a df
    data <- read.table(textConnection(tmp[[i]]))
    if (i == 2) {
      data_out <- data
    } else {
      data_out <- rbind(data_out, data)
      
    }
  }
  
  # split the header, which is space and not tab limited
  # and use the output as names for the mei df
  names(data_out) <- strsplit(tmp[1],"\\s+")[[1]]
  
  mei_df <- data.frame(year = data_out$YEAR, 
                       mei = rowMeans(data_out[,-1])) %>%
    dplyr::mutate(phase = ifelse(mei < 0, "0", "1"))
  return(mei_df)
}

#' Generates a simulated annual MEI time series based on phase randomization
#'
#' @description
#' This function generates a simulated annual MEI (Multivariate ENSO Index)
#' time series of a specified length, using the phase randomization method.
#' It creates multiple simulations by randomizing the phases of the original
#' MEI data's Fourier transform, then concatenates them and selects a random
#' segment of the desired length.
#'
#' @param mei A numeric vector representing the original MEI time series.
#' @param sim_enso_len The desired length of the simulated MEI time series.
#' @param n_enso_sims The number of individual phase-randomized simulations to
#'   create before concatenation (default is 500).
#'
#' @return
#' A numeric vector representing the simulated MEI time series.
#### USED
make_sim_ann_mei_ext <- function(mei,
                                 sim_enso_len,
                                 n_enso_sims = 500) {
  sim_mei_matrix <- matrix(0,
                           nrow = length(mei),
                           ncol = n_enso_sims)

  # create a time series by concatenating several fft + rand theta + ifft
  # and then pick a random starting point in the series
  for (i in 1:n_enso_sims) {
    
    theta <-
      runif(n = length(mei)) * 2 * pi # 2pi converts to radians
    # Now do inverse FFT with modulus (real part) of original FFT but randomized phase
    Z <- Re(fft(mei)) * exp(1i * theta)
    Z <- scale(Re(fft(Z, inverse = TRUE)))
    sim_mei_matrix[, i] <- Z #sim_enso
  }
  # concatenate matrix by columns
  sim_mei_matrix <- c(sim_mei_matrix)
  # pick random starting point
  start_ind <-
    sample(
      x = 1:(length(sim_mei_matrix) - sim_enso_len),
      size = 1,
      replace = FALSE
    )
  # select output series
  sim_mei_ts <-
    sim_mei_matrix[start_ind:(start_ind + sim_enso_len - 1)]
  return(sim_mei_ts)
}

#' Extracts fishing mortality (F) values for specific FLEP ratios for a given species
#'
#' @description
#' This function extracts fishing mortality (f) values from a vector of f
#' values for specific FLEP ratios associated with a given species. It creates
#' a data frame with the extracted FLEP ratios, F values, and character
#' representations of the FLEP ratios.
#'
#' @param species The species name (string).
#' @param flep_inds A list of lists, where each inner list represents the indices
#'   of FLEP ratios associated with a species.
#' @param f_vals A vector of fishing mortality values for all FLEP ratios.
#'
#' @return
#' A DataFrame with columns:
#'   - flep_vals: The FLEP ratio values (numeric).
#'   - f_vals: The corresponding fishing mortality values (numeric).
#'   - flep_vals_ch: The FLEP ratio values as characters (string).
#'
#' @note
#' This function assumes that the input data structures are correctly formatted
#' and that the species name is present in the `flep_inds` list.
#### USED
get_fs_from_fleps <- function(species,
                              flep_inds,
                              f_vals) {
  fleps_2_fs <-
    data.frame(flep_vals = as.numeric(names(flep_inds[[species]])),
               f_vals = f_vals[as.numeric(flep_inds[[species]])])  %>%
    mutate(flep_vals_ch = as.character(flep_vals))
  
  return(fleps_2_fs)
}


#' Calculate the index of the fished effort (F) for each species that produces the
#' target FLEP
#'
#' @param flep_vals A vector of fishing effort values
#' @param sp_names A vector of species names
#' @param relative_flep A matrix of relative fishing effort values for each species
#'
#' @return A list of vectors, where each vector contains the indices of the fished effort values for a particular species
#'
#' @examples
#' flep_vals <- c(0.1, 0.2, 0.3, 0.4, 0.5)
#' sp_names <- c("sp1", "sp2", "sp3")
#' relative_flep <- matrix(c(0.8, 0.9, 0.7, 0.6, 0.5,
#'                          0.9, 0.8, 0.7, 0.6, 0.5,
#'                          0.7, 0.6, 0.8, 0.9, 0.5),
#'                        nrow = 3, ncol = 5)
#'
#' flep_fished_ind <- get_index_for_fleps(flep_vals, sp_names, relative_flep)
#'
#' print(flep_fished_ind)
#'
#' @export
#### USED
get_index_for_fleps <- function(flep_vals,
                                sp_names,
                                relative_flep) {
  flep_fished_ind <- vector("list", length(sp_names))
  names(flep_fished_ind) <- sp_names
  
  for (i in 1:length(sp_names)) {
    flep_fished_ind[[i]] <- flep_vals %>%
      purrr::map(., ~ which.min(abs(relative_flep[[i]] - .x)))
    names(flep_fished_ind[[i]]) <- flep_vals
  }
  return(flep_fished_ind)
}


#'
#' @title make a species specific Leslie matrix for each patch at a specified
#' level of FLEP (e.g., fishing mortality)
#'
#' @description creates a matrix (or array -- one matrix for each patch) that contains
#' age-specific fecundity values on the first row and survival from age a-1 to
#' age a on the sub-diagonal (a Leslie matrix)
#'
#' @return returns a Leslie matrix (or multiple Leslie matrices as an array, with
#' one matrix for each patch)
#'
#' @param num_patches the number of patches included in the model
#' @param surv_fished_matrix a matrix of cumulative
#' survival to each age at many levels of fishing mortality
#' @param surv_fished_ind index of the column that corresponds the survival vector for a
#' given level of fishing (or FLEP - the fraction of lifetime egg production)
#' @param age_max numeric the value of the maximum age for a selected species
#' @param fecundity_at_age vector of fecundity (number of eggs produced) at each age
#'
#' @examples
#'
#' les_mat_cbzn_F0 <- make_leslie_mat(num_patches = 1,
#'                                    surv_fished_matrix = derived_vars_cbzn$surv_fished,
#'                                    surv_fished_ind = 1,
#'                                    age_max = lh_parms_cbzn$A_max,
#'                                    fecundity_at_age = derived_vars_cbzn$fecundity_at_age
#'                                    )
#' les_mat_cbzn_FLEP40 <- make_leslie_mat(num_patches = 1,
#'                                        surv_fished_matrix = derived_vars_cbzn$surv_fished,
#'                                        surv_fished_ind = flep_ind_cbzn_40,
#'                                        age_max = lh_parms_cbzn$A_max,
#'                                        fecundity_at_age = derived_vars_cbzn$fecundity_at_age
#'                                        )
#'

make_leslie_mat <- function(num_patches = 1,
                            surv_fished_mat,
                            surv_fished_ind,
                            age_max,
                            fecundity_at_age) {
  leslie_matrix <-
    array(0, dim = c(age_max, age_max, num_patches)) # create a 3D array to contain Leslie matrices for each "patch"
  for (i in 1:num_patches) {
    leslie_matrix[2:age_max, 1:(age_max - 1), i] <-
      diag(surv_fished_mat[, surv_fished_ind[i]])
    leslie_matrix[1, , i] <-
      fecundity_at_age # add in fecundities on top row
  }
  
  return(leslie_matrix)
}

#' Create Leslie matrices to project each species based on  fishing effort level
#'
#' @param sp_names A vector of species names
#' @param fleps A vector of fraction lifetime egg production levels
#' @param n_patch The number of patches, 2, one fished patch and one unfished
#' @param sim_sp_parms A list of that contains life history parameters for each species
#' @param sim_sp_derived_vars A list of derived life history parameters for each species
#' @param sp_flep_fished_ind A list of vectors, where each vector contains indices for the fishing effort values (F) for a particular species.
#'
#' @return A list of lists of Leslie matrices, where each inner list contains the Leslie matrices for a particular species and fishing effort level.
#'
#' @examples
#' sp_names <- c("sp1", "sp2")
#' fleps <- c(0.1, 0.2)
#' n_patch <- 2
#' sim_sp_parms <- list(sp1 = list(A_max = 10, W_inf = 1), sp2 = list(A_max = 5, W_inf = 2))
#' sim_sp_derived_vars <- list(sp1 = list(surv_fished = c(0.9, 0.8), fecundity_at_age = c(0.1, 0.2)), sp2 = list(surv_fished = c(0.7, 0.6), fecundity_at_age = c(0.3, 0.4)))
#' sp_flep_fished_ind <- list(sp1 = c(1, 2), sp2 = c(2, 1))
#'
#' leslie_matrices <- make_leslie_matrix_sp_flep(sp_names, fleps, n_patch, sim_sp_parms, sim_sp_derived_vars, sp_flep_fished_ind)
#'
#' print(leslie_matrices)
#'
#' @export
#### USED
make_leslie_matrix_sp_flep <- function(sp_names,
                                       fleps,
                                       n_patch,
                                       sim_sp_parms,
                                       sim_sp_derived_vars,
                                       sp_flep_fished_ind) {
  leslie_matrices <- vector("list", length = length(sp_names))
  names(leslie_matrices) <- sp_names
  for (i in 1:length(sp_names)) {
    leslie_matrices[[i]] <- vector("list", length = length(fleps))
    names(leslie_matrices[[i]]) <- fleps
    for (j in 1:length(fleps)) {
      leslie_matrices[[i]][[j]] <- make_leslie_mat(
        num_patches = n_patch,
        surv_fished_mat = sim_sp_derived_vars[[i]][["surv_fished"]],
        surv_fished_ind = c(1, sp_flep_fished_ind[[i]][[j]]),
        age_max = sim_sp_parms[[i]][["A_max"]],
        fecundity_at_age = sim_sp_derived_vars[[i]][["fecundity_at_age"]]
      )
    }
    names(leslie_matrices[[i]]) <- fleps
  }
  return(leslie_matrices)
}

#' Set the fished equilibrium recruit levels for each species and FLEP level (i.e., fishing effort (F))
#'
#' @param sp_names A vector of species names
#' @param fleps A vector of fraction lifetime egg production levels
#' @param f_eq_recs A list of lists of fished equilibrium recruits for each species and fishing effort level.
#' @param sp_der_var A list of simulation-derived variables for each species.
#' @param sp_flep_f_ind A list of vectors, where each vector contains the indices of the fished effort values for a particular species.
#'
#' @return A list of lists of fished N1 values, where each inner list contains the fished N1 values for a particular species and fishing effort level.
#'
#' @examples
#' sp_names <- c("sp1", "sp2")
#' fleps <- c(0.1, 0.2)
#' f_eq_recs <- list(sp1 = list(flep_0.1 = c(100, 200), flep_0.2 = c(300, 400)), sp2 = list(flep_0.1 = c(50, 100), flep_0.2 = c(150, 200)))
#' sp_der_var <- list(sp1 = list(SADs = matrix(c(0.8, 0.9, 0.7, 0.6), ncol = 2)), sp2 = list(SADs = matrix(c(0.9, 0.8, 0.7, 0.6), ncol = 2)))
#' sp_flep_f_ind <- list(sp1 = c(1, 2), sp2 = c(2, 1))
#'
#' fished_N1 <- set_fished_eq_recruits(sp_names, fleps, f_eq_recs, sp_der_var, sp_flep_f_ind)
#'
#' print(fished_N1)
#'
#' @export
#### USED
set_fished_eq_recruits <- function(sp_names,
                                   fleps,
                                   f_eq_recs = fished_equilibrium_recruits,
                                   sp_der_var = sim_species_derived_vars,
                                   sp_flep_f_ind = species_flep_fished_ind) {
  fished_N1 <- sp_names %>%
    purrr::set_names() %>%
    purrr::map(., ~ vector("list", length = length(.x)))
  for (i in 1:length(sp_names)) {
    fished_N1[[i]] <- fleps %>%
      purrr::set_names() %>%
      purrr::map(., ~ vector("list", length = length(.x)))
    for (j in 1:length(fleps)) {
      fished_N1[[i]][[j]] <-
        f_eq_recs[[i]][[j]] * sp_der_var[[i]]$SADs[, sp_flep_f_ind[[i]][[j]]]
    }
  }
  return(fished_N1)
}


#### USED
set_fished_equilibrium_recruits <- function(sp_names,
                                            flep_vals,
                                            ind_species_flep_fished,
                                            species_derived_vars,
                                            alphas,
                                            beta) {
  fished_equilibrium_recruits <-
    vector("list", length = length(sp_names))
  names(fished_equilibrium_recruits) <- sp_names
  
  for (i in 1:length(fished_equilibrium_recruits)) {
    fished_equilibrium_recruits[[i]] <-
      vector("list", length = length(flep_vals))
    names(fished_equilibrium_recruits[[i]]) <- flep_vals
  }
  
  for (i in 1:length(sp_names)) {
    for (j in 1:length(flep_vals)) {
      fished_equlibrium_eggs  <-
        species_derived_vars[[i]][["EPRs"]][ind_species_flep_fished[[i]][[j]]]
      fished_equilibrium_recruits[[i]][[j]] <-
        fished_equlibrium_eggs * (fished_equlibrium_eggs * alphas[[i]]  / (1 + (alphas[[i]] / beta) * fished_equlibrium_eggs))
    }
  }
  return(fished_equilibrium_recruits)
}


#' Initialize arrays for storing simulation results
#'
#' @param sp_names A vector of species names.
#' @param sim_arrays A list of arrays, one for each species.
#' @param fleps A vector of fishing effort levels.
#' @param sim_sp_derived_vars A list of simulation-derived variables for each species.
#' @param n_years The number of years in the simulation.
#' @param n_patch The number of patches.
#'
#' @return A list of arrays, one for each species, each of which is a 3-dimensional array with dimensions representing age, year, and patch.
#'
#' @examples
#' sp_names <- c("sp1", "sp2")
#' fleps <- c(0.1, 0.2)
#' sim_sp_derived_vars <- list(sp1 = list(ages = 1:5), sp2 = list(ages = 1:4))
#' n_years <- 10
#' n_patch <- 2
#'
#' sim_arrays <- set_sim_results_arrays(sp_names, sim_arrays, fleps, sim_sp_derived_vars, n_years, n_patch)
#'
#' print(sim_arrays)
#'
#' @export
#### USED
set_sim_results_arrays <- function(sp_names,
                                   sim_arrays,
                                   fleps,
                                   sim_sp_derived_vars,
                                   n_years,
                                   n_patch) {
  for (i in 1:length(sp_names)) {
    sim_arrays[[i]] <- fleps %>%
      purrr::set_names() %>%
      purrr::map(., ~ vector("list", length = length(.x)))
    for (j in 1:length(fleps)) {
      sim_arrays[[i]][[j]] <- array(0, dim = c(length(sim_sp_derived_vars[[i]][["ages"]]), n_years, n_patch))
    }
  }
  return(sim_arrays)
}



#' Initializes simulation results arrays with initial population numbers
#'
#' @description
#' This function takes a multi-dimensional list of simulation results arrays,
#' species names, FLEP ratios, and initial population numbers, and sets the
#' first time step of each array to the corresponding initial population values.
#'
#' @param species_names A character vector of species names.
#' @param flep_ratios A numeric vector of FLEP ratios.
#' @param sim_results_arrays A list of lists of 3-dimensional arrays, where
#'   the first level of the list corresponds to species, the second level to
#'   FLEP ratios, and the arrays contain population numbers across time, age,
#'   and patch.
#' @param fished_N1 A list of lists of initial population numbers, with the
#'   same structure as `sim_results_arrays`.
#'
#' @return
#' The `sim_results_arrays` list, with the first time step of each array
#'   initialized to the corresponding values from `fished_N1`.
#'
#' @note
#' This function assumes that the dimensions of `sim_results_arrays` and
#' `fished_N1` match, and that the elements within them are appropriately
#' structured for the given species and FLEP ratios.
#### USED
init_vals_sim <- function(species_names,
                          flep_ratios,
                          sim_results_arrays,
                          fished_N1) {
  for (i in 1:length(species_names)) {
    for (j in 1:length(flep_ratios)) {
      sim_results_arrays[[i]][[j]][, 1, ] <- fished_N1[[i]][[j]]
    }
  }
  return(sim_results_arrays)
}

#' Run parameterized age-structured populations simulations and store in a DuckDB database
#'
#' @description
#' Run species specific age-structured popualtions simulations with Beverton-Holt density dependence
#' under specified dispersal regimes, recruitment variability, fishing intensity,
#' reserve fractions, and FLEP/fishing mortality ratios. Results are stored as tables in 
#' a DuckDB database.
#' 
#' @param expmts A character vector of simulated dispersal types: 'larval_pool' or 'no_dispersal'
#' @param spec_name A character vector of species names: "Blue rockfish", "Cabezon", "China rockfish"
#' @param alpha_bh A numeric value for the Beverton-Holt stock-recruitment relationship slope
#' @param beta_bh A numeric value indicating the carrying capacity of the Beverton-Holt stock-recruitment relationship 
#' @param recr_sd A numeric value/vector of recruitment standard deviations to simulate
#' @param num_sims Number of simulations to run per scenario
#' @param frac_reserves A numeric vector containing the fraction of coastline in reserves
#' @param fleps A numeric vector containing the fraction of lifetime egg production (FLEPs)
#' @param n_years Number of years to simulate
#' @param noise_vec A character vector that describes the simulate environmental noise 
#' @param noise_dat A dataframe that contains time series of environmental noise using each simulation run
#' @param num_patches Number of patches in the spatial model (2 - one with fishing; one w/o fishing)
#' @param les_mats A list of three dimensional (max. age by max. age by patch) Leslie matrices, one for each species and FLEP.
#' @param conn_mats A list of matrices detailing the dispersal regime, one for each reserve fraction.
#' @param in_out_fracs A list of arrays that describes the fraction of coastline in and out of reserves
#' @param sim_results A list of empthy arrays (1:max_age, 1:num_sims, 1:num_patches) to store population simulation results
#' @param sim_spec_dervars A list of arrays that contains derived life-history and fishing variables for each species
#' @param f_vals A numeric vector of fishing mortality values from 0 to 2 in 0.0025 increments
#' @param spec_flep_f_ind A list of values that maps fishing mortality (F) by species to target FLEPs
#' @param sim_spec_pars A list of species specific life-history and fishery parameters (natural mortality, weight-at-age, etc.)
#'
#' @return
#' A data table containing summary statistics of the simulation results, with columns for:
#'   - sim_num: the simulation number
#'   - reserve_frac: fraction of coast in reserves
#'   - flep_ratio: the fraction of lifetime egg production (FLEP); 1 = unharvested
#'   - number: List-column containing arrays of population numbers by year, age, and patch
#'   - yield: List-column containing arrays of yield by year, age, and patch
#'
#' @note
#' This function assumes that certain objects and functions are available in the global environment,
#' including:
#'   - `run_patch_sims2`
#'   - `data.table` package
#'   - `duckdb` package
#'
#' @examples
#' \dontrun{
#' # Assuming all required objects and packages are available
#' results <- run_sims_sm_write(
#'   expmts = "no_dispersal",
#'   spec_name = c("Blue rockfish", "Cabezon", "China rockfish"),
#'   alpha_bh = bev_holt_alphas,
#'   beta_bh = bev_holt_beta,
#'   recr_sd = sd_recruitment,
#'   fleps = flep_ratios,
#'   frac_reserves = reserve_fracs,
#'   noise_vec = noises_white,
#'   noise_dat = white_noise,
#'   n_years = n_years_,
#'   num_sims = sim_nums,
#'   num_patches = num_patches,
#'   les_mats = leslie_matrices,
#'   conn_mats = no_disp_conn_mat,
#'   in_out_fracs = no_disp_in_out_frac,
#'   sim_results = sim_results_arrays_init,
#'   sim_spec_dervars = sim_species_derived,
#'   f_vals = fishing_mortality_values,
#'   spec_flep_f_ind = species_flep_fish_ind,
#'   sim_spec_pars = sim_species_parms
#' )
#' }
#### USED
run_sims_sm_write <- function(expmts,
                              spec_name,
                              alpha_bh,
                              beta_bh,
                              recr_sd,
                              num_sims,
                              frac_reserves,
                              fleps,
                              n_years,
                              noise_vec,
                              noise_dat,
                              num_patches,
                              les_mats,
                              conn_mats,
                              in_out_fracs,
                              sim_results,
                              sim_spec_dervars,
                              f_vals,
                              spec_flep_f_ind,
                              sim_spec_pars) {
  
  con <-
    dbConnect(duckdb::duckdb(),
              dbdir = "data/sim_out.duckdb",
              read_only = FALSE)
  
  for (i in 1:length(expmts)) {
    for (j in 1:length(spec_name)) {
      for (k in noise_vec) {
        for (l in 1:length(recr_sd)) {
          for (m in 1:num_sims) { 
            for (n in 1:length(frac_reserves)) {
              for (o in 1:length(fleps)) {
                dt_one <- data.table(sim_num = m, 
                                     reserve_frac=as.character(frac_reserves[n]), 
                                     flep_ratio=as.character(fleps[o]), 
                                     number = list(), 
                                     yield = list())
                
                out <- run_patch_sims2(
                  t_steps = n_years,
                  num_patches = num_patches,
                  les_mat = les_mats[[j]][[o]],
                  con_mat = conn_mats[[n]],
                  frac_in_out = in_out_fracs[[n]],
                  N = sim_results[[j]][[o]],
                  # this uses  values from an unfished_N1 for each species
                  selectivity = as.integer(sim_spec_dervars[[j]][["F_select"]]),
                  fishing_mortality = c(0, f_vals[spec_flep_f_ind[[j]][[o]]]),
                  natural_mortality = sim_spec_pars[[j]][["M"]],
                  weight_at_age = sim_spec_pars[[j]][["biom_const"]] * sim_spec_dervars[[j]][["length_at_age"]] ^
                    sim_spec_pars[[j]][["biom_exp"]],
                  alpha = alpha_bh[[j]],
                  beta = beta_bh,
                  noise_series = noise_dat[,m]
                )
                
                out_Ns <-
                  setnames(
                    as.data.table(out[["Ns"]]),
                    old = c("V1", "V2", "V3", "value"),
                    new = c("age", "year", "patch_num", "number")
                  )
                setcolorder(out_Ns, c("year", "age", "patch_num"))
                setkeyv(out_Ns, c("year", "age", "patch_num"))
                
                stopifnot(max(out_Ns[j = patch_num]) == num_patches)
                
                out_Y <-
                  setnames(
                    as.data.table(out[["Y"]]),
                    old = c("V1", "V2", "V3", "value"),
                    new = c("age", "year", "patch_num", "yield")
                  )
                setcolorder(out_Y, c("year", "age", "patch_num"))
                setkeyv(out_Y, c("year", "age", "patch_num"))
                
                stopifnot(max(out_Y[j = patch_num]) == num_patches)
                
                dt_one[, j =  `:=` (number = list(out_Ns),
                                    yield = list(out_Y))] 
                
                rm(out_Ns, out_Y, out) 
                
                dt_one <- dt_one %>%
                  unnest(c(number, yield), names_repair = "minimal") 
                
                dt_one <- dt_one[, c(1,2,3,4,5,6,7,11)]
                
                # if (n == 1 & o == 1) {
                if (o == 1) {
                  dt_ddb <- dt_one
                  rm(dt_one)
                } else {
                  dt_ddb <- rbind(dt_ddb, dt_one)
                  rm(dt_one)
                }
              } # o
              tab_name <- paste0(expmts[i],
                                 "_",
                                 tolower(sub(" ", "_", spec_name[j])),
                                 "_",
                                 k,
                                 "_",
                                 substring(as.character(recr_sd[l]), 3))
              
              duckdb::dbWriteTable(con,
                                   tab_name,
                                   dt_ddb,
                                   append = TRUE) # FALSE
              rm(dt_ddb)
            } # n
            
            # tab_name <- paste0(expmts[i],
            #                    "_",
            #                    tolower(sub(" ", "_", spec_name[j])),
            #                    "_",
            #                    k,
            #                    "_",
            #                    substring(as.character(recr_sd[l]), 3))
            # 
            # duckdb::dbWriteTable(con,
            #                      tab_name,
            #                      dt_ddb,
            #                      append = TRUE) # FALSE
            # rm(dt_ddb)
            
          } # m
        } # l   
      } # k 
      print(paste0("dispersal: ", expmts[i], " and species: ", spec_name[j],
                   " and ", k, " noise ", " and recruit sd of ", recr_sd[l]))
    } # j 
  } # i
  DBI::dbDisconnect(con, shutdown = TRUE)
  return(dt)
}


#' Runs a multi-patch population simulation over parameter combinations for specified
#' simulation experiments
#'
#' @description
#' This function simulates the dynamics of a fish population across multiple (2)
#' patches, incorporating age-specific transitions, dispersal, fishing mortality,
#' natural mortality, density-dependent recruitment, and environmental noise.
#'
#' @param t_steps Number of time steps to simulate.
#' @param num_patches Number of patches in the model.
#' @param les_mat A 3-dimensional array of life-stage transition matrices,
#'   with dimensions (age, age, num_patches).
#' @param con_mat A matrix representing connectivity between patches for larval dispersal.
#' @param frac_in_out A vector of fractions of individuals remaining in each patch
#'   after dispersal.
#' @param N A 3-dimensional array of initial population numbers, with dimensions
#'   (age, t_steps, patch).
#' @param selectivity A vector of fishing selectivity values for each age class.
#' @param fishing_mortality A vector of fishing mortality rates for each age class 
#'   in fished patches.
#' @param natural_mortality A vector of natural mortality rates for each age class.
#' @param weight_at_age A vector of weight-at-age values.
#' @param alpha A parameter for the Beverton-Holt recruitment function.
#' @param beta A parameter for the Beverton-Holt recruitment function, modified
#'   by frac_in_out.
#' @param noise_series A vector of environmental noise values for each time step.
#'
#' @return
#' A list with two elements:
#'   - Ns: A 3-dimensional array of population numbers across time, age, and patch.
#'   - Y: A 3-dimensional array of yield across time, age, and patch.
#'
#' @details
#' The model assumes a fixed fishing mortality rate for each patch and a
#' Beverton-Holt recruitment function with density dependence.
#'
#' @examples
#' \dontrun{
#' # Assuming necessary data structures and parameters are set
#' simulation_results <- run_patch_sims2(t_steps = 100, num_patches = 5, ...)
#' }
#### USED 
run_patch_sims2 <- function(t_steps,
                            num_patches,
                            les_mat,
                            con_mat,
                            frac_in_out,
                            N,
                            selectivity,
                            fishing_mortality,
                            natural_mortality,
                            weight_at_age,
                            alpha,
                            beta,
                            noise_series) {
  
  E <- matrix(0, nrow = num_patches, ncol = t_steps) # eggs
  R <- E
  Y <- array(data = 0, dim = dim(N)) 
  Y[, 1,] <- 0 
  max_age <- dim(N)[1] 
  # loop over time
  for (t in 2:t_steps) {
    # loop over patches
    for (n in 1:num_patches) {
      N[, t, n] <-
        les_mat[, , n] %*% N[, t - 1, n] # advance the population
      N_deaths <- N[1:(max_age - 1), t - 1, n] - N[2:(max_age), t, n]
      N_harvest <-
        N_deaths * (
          fishing_mortality[n] * selectivity / (natural_mortality + fishing_mortality[n] * selectivity)
        )
      Y[2:max_age, t, n] <-
        N_harvest * weight_at_age[-1] / 1E6 # grams to MTi[]
      E[n, t] <- N[1, t, n] # pull out the eggs produced
    }
    # do larval pool dispersal
    E[, t] <-  con_mat %*% E[, t]
    # Apply density dependence
    for (n in 1:num_patches) {
      # if (n == 1) or something to catch dividing by zero
      
      if (frac_in_out[n] == 0) {
        R[n, t] <- 0
      } else {
        R[n, t] <-
          ((alpha * E[n, t]) / (1 + (alpha / (
            beta * frac_in_out[n]
          ))  * E[n, t])) * exp(noise_series[t])
      }
      N[1, t, n] <-
        R[n, t] # add the actual surviving recruits back in
    }
  }
  out <- list(
    Ns = N,
    Y = Y
  )
  return(out)
}

# Functions for table meta data


#' Extracts simualtion experiment parameters from a structured DuckDB table name
#'
#' @description
#' This function extracts the species, dispersal type, and noise type from a
#' DuckDB table name follow a specific naming convention.
#'
#' @param table_name A character string representing the name of a DuckDB table.
#'
#' @return
#' A list containing three character strings:
#'   - current_dispersal: The dispersal type (e.g., "larval_pool", "no_dispersal")
#'   - current_species: The species name
#'   - current_noise: The noise type
#'
#' @note
#' This function assumes that table names follow a specific format, including
#' the dispersal type, species name, and noise type in a consistent order.
#'
#' @examples
#' \dontrun{
#' exp_params <- get_current_exp("my_db_larval_pool_blue_rockfish_mei_135_75")
#' print(exp_params)  # Output: list("larval_pool", "blue_rockfish", "mei_135_75")
#' }

get_current_exp <- function(table_name) {
  # returns species, dispersal, noise params to extract from duckdb table names 
  valid_dispersal <- c("larval_pool", "no_dispersal")
  valid_species <-
    c("blue_rockfish", "cabezon", "china_rockfish")
  valid_noise <- c("mei_135_75", "white_135_75")
  
  current_dispersal <-
    valid_dispersal[stringr::str_detect(table_name, valid_dispersal)]
  current_species <-
    valid_species[stringr::str_detect(table_name, valid_species)]
  current_noise <-
    valid_noise[stringr::str_detect(table_name, valid_noise)]
  return(list(current_dispersal, current_species, current_noise))
}

#' Formats experiment parameters for plotting
#'
#' @description
#' This function takes a DuckDB table name and returns a formatted string
#' containing "pretty" versions of the species, dispersal type, and noise type,
#' suitable for use in plot titles or labels.
#'
#' @param table_name A character string representing the name of a DuckDB table.
#'
#' @return
#' A list combining the formatted dispersal type, species name,
#' and noise type.
#'
#' @note
#' This function assumes that table names follow a specific format, including
#' the dispersal type, species name, and noise type in a consistent order.
#'
#' @examples
#' \dontrun{
#' nice_exp_name <- get_nice_exp("my_db_larval_pool_blue_rockfish_mei_135_75")
#' print(nice_exp_name)  # Output: list("Larval pool", "Blue rockfish", "ENSO")
#' }
#' 
get_nice_exp <- function(table_name) {
  # returns "pretty" version of species, dispersal, noise params for plotting
  valid_dispersal <- c("larval_pool", "no_dispersal")
  valid_species <-
    c("blue_rockfish", "cabezon", "china_rockfish", "pacific_cod", "kelp_bass")
  valid_noise <- c("mei_135_75", "white_135_75")
  
  pretty_dispersal <- c("Larval pool", "No dispersal")
  pretty_species <- c("Blue rockfish", "Cabezon", "China rockfish", "Pacific cod", "Kelp bass")
  pretty_noise <- c("ENSO", "White")
  
  nice_dispersal <-
    pretty_dispersal[stringr::str_detect(table_name, valid_dispersal)]
  nice_species <-
    pretty_species[stringr::str_detect(table_name, valid_species)]
  nice_noise <-
    pretty_noise[stringr::str_detect(table_name, valid_noise)]
  
  return(list(nice_dispersal, nice_species, nice_noise))
}



#' Get Fishing Mortality Levels that Maximize Yield
#'
#' For each FLEP ratio, find the fishing mortality level  
#' that maximizes yield when there are no reserves.
#'
#' @param input_df dataframe with simulation results 
#' @param fleps_2_fs dataframe matching FLEP ratios to F values
#' 
#' @return Vector of optimal F values for maximum yield for each FLEP ratio
#'
#' @examples
#' input_df <- data.frame(sim_num = rep(1, 30), 
#'                        reserve_frac = 0.25, flep_ratio = 0.5, 
#'                        year = rep(1:6, each = 5), 
#'                        age = rep(1:5, times = 6), 
#'                        abundance = rnorm(30, 1000, 10), yield = rnorm(30, 300, 10)) 
#' fleps_2_fs <- data.frame(flep_vals = 0.5, f_vals = 0.225, flep_vals_ch = "0.5")
#'   
#' max_yield_fs <- get_f_maxes_yield(input_df, fleps_2_fs)
get_f_maxes_yield <- function(input_df,
                              fleps_2_fs) {
  # Find the fishing mortality level that maximizes yield
  # with no reserves
  
  stopifnot(colnames(input_df) == c("sim_num", "reserve_frac", "flep_ratio",
                                    "year", "age", "abundance", "yield"))
  
  F_maxes_yield <- input_df %>%
    dplyr::filter(reserve_frac == "0") %>%
    dplyr::left_join(., y = fleps_2_fs, by = join_by(flep_ratio == flep_vals_ch)) %>%
    dplyr::group_by(f_vals, flep_ratio, year) %>%
    dplyr::summarise(yield = sum(yield),
                     abundance = sum(abundance)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(f_vals, flep_ratio) %>%
    dplyr::summarize(yield = mean(yield, na.rm = TRUE),
                     abundance = mean(abundance, na.rm = TRUE)) %>%
    ungroup() %>%
    dplyr::filter(yield == max(yield)) %>%
    .$f_vals
  
  return(F_maxes_yield)
}

#' Adds fishing mortality and biomass columns to a simulation output data frame
#'
#' @description
#' This function takes a simulation output data frame and adds columns for
#' fishing mortality (F) values corresponding to FLEP (Fraction of Lifetime
#' Egg Production) values, as well as biomass at age.
#'
#' @param input_df A data frame containing simulation output, with columns for
#'   sim_num, reserve_frac, flep_ratio, year, age, abundance, and yield.
#' @param spec_fleps_2_fs A data frame mapping FLEP values to F values for the
#'   species in question.
#' @param F_maxes_yield A numeric vector of F_max values for each species,
#'   used to calculate F/F_MSY.
#' @param waa_g_df A data frame containing weight-at-age values (in grams).
#'
#' @return
#' A data frame with the same columns as the input data frame, plus additional
#' columns for f_vals (fishing mortality values), f_fmsy (F/F_MSY), and
#' biomass_at_age (in tonnes).
#'
#' @note
#' This function assumes that the input data frame has specific column names
#' and that the provided data frames for FLEP-to-F mapping and weight-at-age
#' have compatible structures.
#'
#' @examples
#' \dontrun{
#' # Assuming all required objects are available
#' output_with_fs <- df_add_fs_out(sim_output_df, flep_to_f_map, f_max_values, waa_data)
#' }
df_add_fs_out <- function(input_df,
                          spec_fleps_2_fs,
                          F_maxes_yield,
                          waa_g_df) {
  
  # Returns a dataframe with a columns of F(ishing mortality) values that correspond to (produce) FLEP 
  # (Fraction of lifetime egg production) values
  
  stopifnot(colnames(input_df) == c("sim_num", "reserve_frac", "flep_ratio",
                                    "year", "age", "abundance", "yield"))
  out_fs <- input_df %>%
    dplyr::left_join(., y = spec_fleps_2_fs, by = join_by(flep_ratio == flep_vals_ch)) %>%
    dplyr::mutate(f_fmsy = round(f_vals / F_maxes_yield, 2)) %>%
    dplyr::left_join(., y = waa_g_df, join_by(age == age)) %>%
    dplyr::mutate(biomass_at_age = (abundance * waa_g) / 1E6)
  
}

#' Calculates the mean of median annual yield at F_MSY with no reserves
#'
#' @description
#' This function calculates the mean of median annual yield across simulations
#' for scenarios where fishing mortality is at F_MSY and there are no marine
#' reserves.
#'
#' @param input_df A data frame containing simulation output, with specific
#'   columns (see Details).
#'
#' @return
#' A single numeric value representing the mean of median annual yield across
#' simulations under the specified conditions.
#'
#' @details
#' The input data frame is expected to have the following columns:
#'   - sim_num: Simulation number
#'   - reserve_frac: Reserve fraction
#'   - flep_ratio: FLEP ratio
#'   - year: Year
#'   - age: Age
#'   - abundance: Abundance
#'   - yield: Yield
#'   - flep_vals: FLEP values
#'   - f_vals: Fishing mortality values
#'   - f_fmsy: F/F_MSY
#'   - waa_g: Weight-at-age (in grams)
#'   - biomass_at_age: Biomass at age (in tonnes)
#'
#' @note
#' This function assumes that the input data frame has been processed by the
#' `df_add_fs_out` function to include the necessary columns.
#'
#' @examples
#' \dontrun{
#' # Assuming a processed data frame is available
#' median_yield_at_fmsy <- get_median_yield(processed_sim_output)
#' }
get_median_yield <- function(input_df) {
  # returns the mean of median yield at at F = F_msy 
  # for a coastline with no reserves across sims
  stopifnot(colnames(input_df) == c("sim_num", "reserve_frac", "flep_ratio",
                                    "year", "age", "abundance", "yield", 
                                    "flep_vals", "f_vals", 
                                    "f_fmsy", "waa_g", "biomass_at_age"))
  
  med_yield <- input_df %>%
    dplyr::filter(near(f_fmsy, 1), near(as.numeric(reserve_frac), 0))  %>%
    dplyr::group_by(sim_num,
                    reserve_frac,
                    f_fmsy,
                    year) %>%
    dplyr::summarise(ann_yield = sum(yield)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(sim_num) %>%
    dplyr::summarise(med_ann_yield = median(ann_yield)) %>%
    dplyr::ungroup() %>%
    dplyr::summarise(avg_med_yield_f_fmsy = mean(med_ann_yield)) %>%
    .$avg_med_yield_f_fmsy
  return(med_yield)
}


#' Calculates the mean of median biomass at F_MSY with no reserves
#'
#' @description
#' This function calculates the mean of median biomass across simulations
#' for scenarios where fishing mortality is at F_MSY and there are no marine
#' reserves.
#'
#' @param input_df A data frame containing simulation output, with specific
#'   columns (see Details).
#'
#' @return
#' A single numeric value representing the mean of median biomass across
#' simulations under the specified conditions.
#'
#' @details
#' The input data frame is expected to have the same columns as required for
#' the `get_median_yield` function (see its documentation for details).
#'
#' @note
#' This function assumes that the input data frame has been processed by the
#' `df_add_fs_out` function to include the necessary columns.
#'
#' @examples
#' \dontrun{
#' # Assuming a processed data frame and age matrix are available
#' median_biomass_at_fmsy <- get_median_biomass(processed_sim_output, age_matrix)
#' }

get_median_biomass <- function(input_df) {
  # returns the mean of median abundance at at F = F_msy 
  # for a coastline with no reserves across sims
  stopifnot(colnames(input_df) == c("sim_num", "reserve_frac", "flep_ratio",
                                    "year", "age", "abundance", "yield", 
                                    "flep_vals", "f_vals", 
                                    "f_fmsy", "waa_g", "biomass_at_age"))
  
  med_biomass <- input_df %>%
    dplyr::filter(near(f_fmsy, 1), near(as.numeric(reserve_frac), 0))  %>%
    dplyr::group_by(sim_num,
                    reserve_frac,
                    f_fmsy,
                    year) %>%
    dplyr::summarise(ann_biomass = sum(biomass_at_age)) %>% 
    dplyr::ungroup() %>%
    dplyr::group_by(sim_num) %>%
    dplyr::summarise(med_ann_biomass = median(ann_biomass)) %>%
    dplyr::ungroup() %>%
    dplyr::summarise(avg_med_biomass_f_fmsy = mean(med_ann_biomass)) %>%
    .$avg_med_biomass_f_fmsy
  return(med_biomass)
}

#' Calculates annual yield and biomass for different fishing mortality, reserve fractions, and years
#'
#' @description
#' This function takes a simulation output data frame and calculates annual
#' yield and biomass, aggregated by simulation number, reserve fraction,
#' F/F_MSY, and year.
#'
#' @param input_df A data frame containing simulation output, with specific
#'   columns (see Details).
#'
#' @return
#' A data frame with columns for sim_num, reserve_frac, f_fmsy, year,
#' annual_yield, and annual_biomass.
#'
#' @details
#' The input data frame is expected to have the same columns as required for
#' the `get_median_yield` and `get_median_biomass` functions (see their
#' documentation for details).
#'
#' @note
#' This function assumes that the input data frame has been processed by the
#' `df_add_fs_out` function to include the necessary columns.
#'
#' @examples
#' \dontrun{
#' # Assuming a processed data frame and age at maturity are available
#' yield_biomass_stats <- df_yld_bm_stats_by_sim_f_frac_year(input_df = processed_sim_output)
#' }

df_yld_bm_stats_by_sim_f_frac_year <- function(input_df) {
  
  stopifnot(colnames(input_df) == c("sim_num", "reserve_frac", "flep_ratio",
                                    "year", "age", "abundance", "yield", 
                                    "flep_vals", "f_vals", 
                                    "f_fmsy", "waa_g", "biomass_at_age"))
  
  out_df <- input_df %>%
    dplyr::group_by(sim_num, reserve_frac, f_fmsy, year) %>%
    dplyr::summarize(annual_yield = sum(yield), 
                     annual_biomass = sum(biomass_at_age)) %>%
    dplyr::ungroup()
  return(out_df)
}

#' Calculates summary statistics for yield and biomass by fishing mortality and reserve fraction
#'
#' @description
#' This function takes a data frame with annual yield and biomass, along with
#' reference median yield and biomass values, and calculates various summary
#' statistics, aggregated by simulation number, fishing mortality (F/F_MSY),
#' and reserve fraction.
#'
#' @param out_sim_f_frac_yr A data frame containing annual yield and biomass
#'   data, with columns for sim_num, reserve_frac, f_fmsy, year, annual_yield,
#'   and annual_biomass.
#' @param median_yield A numeric value representing the median yield to compare against.
#' @param median_biomass A numeric value representing the median biomass to compare against.
#'
#' @return
#' A list containing two data frames:
#'   - sim_f_frac_df: A data frame with summary statistics for each simulation,
#'     reserve fraction, and F/F_MSY combination.
#'   - summ_f_frac_df: A data frame with summary statistics aggregated across
#'     simulations for each reserve fraction and F/F_MSY combination, including
#'     labels for plotting.
#'
#' @note
#' This function assumes that the input data frame has been processed by the
#' `df_yld_bm_stats_by_sim_f_frac_year` function to include the necessary columns.
#'
#' @examples
#' \dontrun{
#' # Assuming processed data frames and median values are available
#' summary_data <- summary_stats_by_f_frac(out_sim_f_frac_yr = yield_biomass_stats, 
#'                                        median_yield = med_yield, 
#'                                        median_biomass = med_biomass)
#' }
summary_stats_by_f_frac <- function(out_sim_f_frac_yr,
                                    median_yield,
                                    median_biomass
) {
  sim_f_frac_df <- out_sim_f_frac_yr %>%
    dplyr::mutate(below_thresh_yld = if_else(annual_yield <= median_yield, 1, 0),
                  below_thresh_bm = if_else(annual_biomass <= median_biomass, 1, 0),
                  rel_annual_yield = annual_yield / median_yield,
                  rel_annual_biomass = annual_biomass / median_biomass) %>%
    dplyr::group_by(sim_num, reserve_frac, f_fmsy) %>%
    dplyr::summarise(
      mean_yield = round(mean(annual_yield, na.rm = TRUE), 2),
      mean_biomass = round(mean(annual_biomass, na.rm = TRUE), 2),
      mean_rel_yield = round(mean(rel_annual_yield, na.rm = TRUE), 2),
      mean_rel_biomass = round(mean(rel_annual_biomass, na.rm = TRUE), 2),
      sd_yield = round(sd(annual_yield, na.rm = TRUE), 2),
      sd_biomass = round(sd(annual_biomass, na.rm = TRUE), 2),
      sd_rel_yield =   round(sd(rel_annual_yield, na.rm = TRUE), 2),
      sd_rel_biomass = round(sd(rel_annual_biomass, na.rm = TRUE), 2),
      cv_yield = round(sd(annual_yield, na.rm = TRUE)/mean(annual_yield, na.rm = TRUE), 2),
      cv_biomass = round(sd(annual_biomass, na.rm = TRUE)/mean(annual_biomass, na.rm = TRUE), 2),
      cv_rel_yield = round(sd(rel_annual_yield, na.rm = TRUE)/mean(rel_annual_yield, na.rm = TRUE), 2),
      cv_rel_biomass = round(sd(rel_annual_biomass, na.rm = TRUE)/mean(rel_annual_biomass, na.rm = TRUE), 2),
      yrs_below_yield_thresh = sum(below_thresh_yld)/135,
      yrs_below_biomass_thresh = sum(below_thresh_bm)/135
    ) %>%
    dplyr::ungroup() 
  
  summ_f_frac_df <- sim_f_frac_df %>%
    dplyr::group_by(reserve_frac, f_fmsy) %>%
    dplyr::summarize(
      mean_yield = round(mean(mean_yield, na.rm = TRUE), 2),
      mean_yield_lab = paste0("Mean yield: ", mean_yield),
      mean_rel_yield = round(mean(mean_rel_yield, na.rm = TRUE), 2),
      mean_rel_yield_lab = paste0("Mean relative yield: ", mean_rel_yield),
      mean_biomass = round(mean(mean_biomass, na.rm = TRUE), 2),
      mean_biomass_lab = paste0("Mean biomass: ", mean_biomass),
      mean_rel_biomass = round(mean(mean_rel_biomass, na.rm = TRUE), 2),
      mean_rel_biomass_lab = paste0("Mean relative biomass: ", mean_rel_biomass),
      sd_yield = round(mean(sd_yield, na.rm = TRUE), 2),
      sd_yield_lab = paste0("SD yield: ", sd_yield),
      sd_rel_yield = round(mean(sd_rel_yield, na.rm = TRUE), 2),
      sd_rel_yield_lab = paste0("SD relative yield: ", sd_rel_yield),
      sd_biomass = round(mean(sd_biomass, na.rm = TRUE), 2),
      sd_biomass_lab = paste0("SD biomass: ", sd_yield),
      sd_rel_biomass = round(mean(sd_rel_biomass, na.rm = TRUE), 2),
      sd_rel_biomass_lab = paste0("SD relative biomass: ", sd_rel_biomass),
      cv_yield = round(mean(cv_yield, na.rm = TRUE), 2),
      cv_yield_lab = paste0("CV yield: ", cv_yield),
      cv_rel_yield = round(mean(cv_rel_yield, na.rm = TRUE), 2),
      cv_rel_yield_lab = paste0("CV relative yield: ", cv_rel_yield),
      cv_biomass = round(sd(cv_biomass, na.rm = TRUE), 2),
      cv_biomass_lab = paste0("CV biomass: ", cv_biomass),
      cv_rel_biomass = round(sd(cv_rel_biomass, na.rm = TRUE), 2),
      cv_rel_biomass_lab = paste0("CV relative biomass: ", cv_rel_biomass),
      yrs_below_yield_thresh =  mean(yrs_below_yield_thresh),
      yrs_below_yield_thresh_lab = paste("% YR below yield thresh: ", round(yrs_below_yield_thresh, 4)),
      yrs_below_biomass_thresh = mean(yrs_below_biomass_thresh),
      yrs_below_biomass_thresh_lab = paste("% YR below biomass thresh: ", round(yrs_below_biomass_thresh, 4))
    ) %>%
    ungroup()
  return(list(sim_f_frac_df, summ_f_frac_df))
}

#' Retrieves simulation data from a DuckDB database table for subequent analyses
#'
#' @param table_name The name of the DuckDb table containing the simulation data.
#'
#' @return A data frame containing the retrieved simulation data, with columns:
#' \itemize{
#'   \item sim_num
#'   \item reserve_frac
#'   \item flep_ratio
#'   \item year
#'   \item age
#'   \item abundance
#'   \item yield
#' }
#'
#' @details This function retrieves simulation data for years after a 500 yr burn-in period,
#' aggregates results from both the fished and unfished patches and returns
#'the results by simulation number, reserve fraction, FLEP ratio, year, and age.
#'
#' @examples
#' table_name_brf_nd_mei <- "no_dispersal_blue_rockfish_mei_135_75"
#' sim_data <- qry_get_sim_data_for_analysis(table_name = table_name_brf_nd_mei)
#'
#' @export

qry_get_sim_data_for_analysis <- function(table_name) {
    con <- DBI::dbConnect(duckdb::duckdb(),
                          dbdir = "data/sim_out.duckdb",
                          read_only = TRUE)
    
    qry <- paste0("SELECT  sim_num, reserve_frac, flep_ratio, year, age, 
              sum(number) as abundance,
              sum(yield) as yield,
              FROM '", table_name, 
                  "' WHERE year > 500 
              GROUP BY sim_num, reserve_frac, flep_ratio, year, age
              ORDER BY sim_num, reserve_frac, flep_ratio, year, age
              ")
    
    sim_res_out <- dbGetQuery(conn = con, qry)
    
    DBI::dbDisconnect(conn = con)
    
    return(sim_res_out)
  }

#' Creates a contour plot of the fraction of years below a biomass threshold.
#'
#' @param sum_stats_f_frac_df A list of data frames where the second element 
#'                            holds the data for the contour plot.
#' @param spec_disp_noise_nice A list containing strings used for constructing the plot title.
#' @param max_reserve_frac The maximum value for the reserve fraction on the x-axis.
#'                         Default is 0.3.
#' @param max_f_fmsy The maximum value for F/Fmsy on the y-axis. Default is 2.0.
#'
#' @return A ggplot object containing the contour plot.
#'
#'
#' @examples
#' p <- plot_contour_bm_below_thresh(sum_stats_f_frac_df, spec_disp_noise_nice)
#' print(p)
#'
#' @export
plot_contour_bm_below_thresh <- function(sum_stats_f_frac_df,
                                         spec_disp_noise_nice,
                                         max_reserve_frac = 0.3, 
                                         max_f_fmsy = 2.0) {
  p <- sum_stats_f_frac_df[[2]] %>%
    filter(as.numeric(reserve_frac) <= max_reserve_frac, f_fmsy <= max_f_fmsy) %>%
    ggplot(.,
           aes(x = as.numeric(reserve_frac), y = f_fmsy, z = yrs_below_biomass_thresh)) +
    metR::geom_contour_fill(bins = 12) +
    ggtitle(paste0(
      spec_disp_noise_nice[[2]],
      " ",
      spec_disp_noise_nice[[1]],
      " ",
      spec_disp_noise_nice[[3]],
      ":\nTime below mean median biomass "
    )) +
    xlab("Fraction of coastline in reserves") +
    ylab("Harvest Rate (F/Fmsy)") +
    scale_fill_viridis_c(option = "D", direction = -1) +
    geom_segment(aes(
      x = 0,
      xend = max_reserve_frac,
      y = 1,
      yend = 1
    ), colour = "black",
    linetype = 2) +
    scale_x_continuous(expand = expansion(mult = 0, add = 0), limits = c(0, max_reserve_frac)) +
    scale_y_continuous(expand = expansion(mult = 0, add = 0)) +
    theme_bw()
  
  return(p)
}


#' Creates a contour plot of the fraction of years below a yield threshold.
#'
#' @param sum_stats_f_frac_df A list of data frames where the second element 
#'                            holds the data for the contour plot.
#' @param spec_disp_noise_nice A list containing strings used for constructing the plot title.
#' @param max_reserve_frac The maximum value for the reserve fraction on the x-axis.
#'                         Default is 0.3.
#' @param max_f_fmsy The maximum value for F/Fmsy on the y-axis. Default is 2.0.
#'
#' @return A ggplot object containing the contour plot.
#'
#'
#' @examples
#' p <- plot_contour_yld_below_thresh(sum_stats_f_frac_df, spec_disp_noise_nice)
#' print(p)
#'
#' @export
plot_contour_yld_below_thresh <- function(sum_stats_f_frac_df,
                                          spec_disp_noise_nice,
                                          max_reserve_frac = 0.3, 
                                          max_f_fmsy = 2.0) {
  p <- sum_stats_f_frac_df[[2]] %>%
    filter(as.numeric(reserve_frac) <= max_reserve_frac, f_fmsy <= max_f_fmsy) %>%
    ggplot(.,
           aes(x = as.numeric(reserve_frac), y = f_fmsy, z = yrs_below_yield_thresh)) +
    metR::geom_contour_fill(bins = 12) + 
    ggtitle(paste0(
      spec_disp_noise_nice[[2]],
      " ",
      spec_disp_noise_nice[[1]],
      " ",
      spec_disp_noise_nice[[3]],
      ":\nTime below median max yield"
    )) +
    xlab("Fraction of coastline in reserves") +
    ylab("Harvest Rate (F/Fmsy)") +
    scale_fill_viridis_c(option = "D", direction = -1) +
    geom_segment(aes(
      x = 0,
      xend = max_reserve_frac,
      y = 1,
      yend = 1
    ), colour = "black",
    linetype = 2) +
    scale_x_continuous(expand = expansion(mult = 0, add = 0), limits = c(0, max_reserve_frac)) +
    scale_y_continuous(expand = expansion(mult = 0, add = 0)) +
    theme_bw()
  return(p)
}


#' Creates a faceted plot of biomass time series for a selected simulation run.
#'
#' @param bio_yield_df A data frame containing simulation results, including
#'                     columns for: sim_num, reserve_frac, f_fmsy, year,
#'                     biomass_at_age, yield, med_biomass, and med_yield.
#' @param biomass_thresh A numeric value indicating the biomass threshold.
#' @param exp_deets A list of strings providing details for the plot title:
#'                   - Species  (e.g., "blue rockfish")
#'                   - Dispersal  (e.g., "no dispersal")
#'                   - Noise  (e.g., "white")
#' @param r_seed An integer specifying the random seed for selecting a simulation run.
#' @param frac_reserves A vector of reserve fractions to include in the plot.
#' @param f_fmsys A vector of F/Fmsy values to include in the plot.
#'
#' @return A ggplot object containing the faceted biomass time series plot.
#'
#' @details This function:
#' 1. Selects a random simulation run.
#' 2. Filters data for specified reserve fractions and F/Fmsy values.
#' 3. Calculates relative biomass and yield.
#' 4. Creates a faceted plot with:
#'   - Time series of relative biomass, with visual threshold indicator.
#'   - Facets for different combinations of reserve fraction and F/Fmsy.
#'
#' @examples
#' bio_ts_plot <- facet_plot_bm_ts_plots(bio_yield_df, exp_deets)
#' print(bio_ts_plot)
#'
#' @export
facet_plot_bm_ts_plots <- function(bio_yield_df,
                                   biomass_thresh,
                                   yield_thresh,
                                   exp_deets,
                                   r_seed = 34,
                                   frac_reserves = c(0, 0.25, 0.50),
                                   f_fmsys = c(0, 0.52, 1.00, 2.09)) {
  # Randomly select a simulation run to plot
  set.seed(r_seed)
  sim_2_use <-
    sample(sort(unique(bio_yield_df$sim_num)), 1, replace = FALSE)
  
  u_f_fmsys <- unique(bio_yield_df$f_fmsy)
  
  u_f_fmsys <-
    purrr::map_dbl(f_fmsys,  ~ u_f_fmsys[which.min(abs(u_f_fmsys - .x))])
  
  x_text_val <- 135/2
  
  title_string <- paste0(
    "Simulation results: \n",
    exp_deets[[2]],
    " with ",
    exp_deets[[1]],
    " dispersal and ",
    exp_deets[[3]],
    " noise. \n"
  )
  
  bio_yield_sel_df <- bio_yield_df %>%
    dplyr::mutate(reserve_frac = as.numeric(reserve_frac),
                  year = year - 500) %>%
    dplyr::filter(sim_num == sim_2_use) %>%
    dplyr::filter(reduce(map(
      frac_reserves, near, x = reserve_frac, tol = 1e-4
    ), `|`)) %>%
    dplyr::filter(reduce(map(
      u_f_fmsys, near, x = f_fmsy, tol = 1e-4
    ), `|`)) %>%
    dplyr::group_by(sim_num, reserve_frac, f_fmsy, year) %>%
    dplyr::summarize(
      annual_biomass = sum(biomass_at_age),
      annual_yield = sum(yield),
      rel_annual_biomass = sum(biomass_at_age) / biomass_thresh,
      rel_annual_yield = sum(yield) / yield_thresh
    ) %>%
    dplyr::ungroup()
  
  ### Biomass
  biom_ts_plt <- ggplot(bio_yield_sel_df,
                        aes(x = year, y = rel_annual_biomass)) +
    facet_grid(reserve_frac ~ f_fmsy,
               labeller = labeller(f_fmsy = label_both,
                                   reserve_frac = label_both)) +
    geom_ribbon(aes(
      ymin = 1,
      ymax = pmin(rel_annual_biomass, 1),
      fill = "Biomass lower"
    )) +
    geom_ribbon(aes(
      ymin = rel_annual_biomass,
      ymax = pmin(rel_annual_biomass, 1),
      fill = "Biomass higher"
    )) +
    geom_line(colour = "black") +
    geom_segment(aes(
      x = 1,
      y = 1,
      xend = 135,
      yend = 1
    ),
    colour = "red",
    linewidth = 0.6) +
    scale_fill_manual(values = c("black", "grey80")) +
    labs(fill = "Above/Below Biomass threshold") +
    ggtitle(title_string) +
    xlab("Year") +
    ylab("Relative Biomass") +
    ylim(0, NA) +
    labs(colour = "Reserve\nfraction") +
    theme_bw() +
    scale_x_continuous(expand = expansion(mult = 0, add = 0)) +
    theme(
      axis.line = element_line(colour = "black"),
      panel.background = element_rect(fill = "white")
    )
    
    return(biom_ts_plt)
}

#' Creates a faceted plot of yield time series for a selected simulation run.
#'
#' @param bio_yield_df A data frame containing simulation results, including
#'                     columns for: sim_num, reserve_frac, f_fmsy, year,
#'                     biomass_at_age, yield, med_biomass, and med_yield.
#' @param yield_thresh A numeric value indicating the yield threshold.
#' @param exp_deets A list of strings providing details for the plot title:
#'                   - Species  (e.g., "blue rockfish")
#'                   - Dispersal  (e.g., "no dispersal")
#'                   - Noise  (e.g., "white")
#' @param r_seed An integer specifying the random seed for selecting a simulation run.
#' @param frac_reserves A vector of reserve fractions to include in the plot.
#' @param f_fmsys A vector of F/Fmsy values to include in the plot.
#'
#' @return A ggplot object containing the faceted yield time series plot.
#'
#' @details This function:
#' 1. Selects a random simulation run.
#' 2. Filters data for specified reserve fractions and F/Fmsy values.
#' 3. Calculates relative biomass and yield.
#' 4. Creates a faceted plot with:
#'   - Time series of relative yield, with visual threshold indicator.
#'   - Facets for different combinations of reserve fraction and F/Fmsy.
#'
#' @examples
#' yld_ts_plot <- facet_plot_yld_ts_plots(bio_yield_df, exp_deets)
#' print(yield_ts_plot)
#'
#' @export
facet_plot_yld_ts_plots <- function(bio_yield_df,
                                   biomass_thresh,
                                   yield_thresh,
                                   exp_deets,
                                   r_seed = 34,
                                   frac_reserves = c(0, 0.25, 0.50),
                                   f_fmsys = c(0.52, 1.00, 2.09)) {
  # Randomly select a simulation run to plot
  set.seed(r_seed)
  sim_2_use <-
    sample(sort(unique(bio_yield_df$sim_num)), 1, replace = FALSE)
  
  u_f_fmsys <- unique(bio_yield_df$f_fmsy)
  
  u_f_fmsys <-
    purrr::map_dbl(f_fmsys,  ~ u_f_fmsys[which.min(abs(u_f_fmsys - .x))])
  
  x_text_val <- 135/2
  
  title_string <- paste0(
    "Simulation results: \n",
    exp_deets[[2]],
    " with ",
    exp_deets[[1]],
    " dispersal and ",
    exp_deets[[3]],
    " noise. \n"
  )
  
  bio_yield_sel_df <- bio_yield_df %>%
    dplyr::mutate(reserve_frac = as.numeric(reserve_frac),
                  year = year - 500) %>%
    dplyr::filter(sim_num == sim_2_use) %>%
    dplyr::filter(reduce(map(
      frac_reserves, near, x = reserve_frac, tol = 1e-4
    ), `|`)) %>%
    dplyr::filter(reduce(map(
      u_f_fmsys, near, x = f_fmsy, tol = 1e-4
    ), `|`)) %>%
    dplyr::group_by(sim_num, reserve_frac, f_fmsy, year) %>%
    dplyr::summarize(
      annual_biomass = sum(biomass_at_age),
      annual_yield = sum(yield),
      rel_annual_biomass = sum(biomass_at_age) / biomass_thresh,
      rel_annual_yield = sum(yield) / yield_thresh
    ) %>%
    dplyr::ungroup()
  
  ### Biomass
  yield_ts_plt <- ggplot(bio_yield_sel_df,
                        aes(x = year, y = rel_annual_yield)) +
    facet_grid(reserve_frac ~ f_fmsy,
               labeller = labeller(f_fmsy = label_both,
                                   reserve_frac = label_both)) +
    geom_ribbon(aes(
      ymin = 1,
      ymax = pmin(rel_annual_yield, 1),
      fill = "Yield lower"
    )) +
    geom_ribbon(aes(
      ymin = rel_annual_yield,
      ymax = pmin(rel_annual_yield, 1),
      fill = "Yield higher"
    )) +
    geom_line(colour = "black") +
    geom_segment(aes(
      x = 1,
      y = 1,
      xend = 135,
      yend = 1
    ),
    colour = "red",
    linewidth = 0.6) +
    scale_fill_manual(values = c("black", "grey80")) +
    labs(fill = "Above/Below Yield threshold") +
    ggtitle(title_string) +
    xlab("Year") +
    ylab("Relative Yield") +
    ylim(0, NA) +
    labs(colour = "Reserve\nfraction") +
    theme_bw() +
    scale_x_continuous(expand = expansion(mult = 0, add = 0)) +
    theme(
      axis.line = element_line(colour = "black"),
      panel.background = element_rect(fill = "white")
    )
  
  return(yield_ts_plt)
}
