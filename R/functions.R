#' functions. R


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

#' add documentation
list_species_parms <- function(parm_df,
                               sp_names) {
  sim_species_parms <- sp_names %>%
    purrr::set_names() %>%
    purrr::map(.,
               ~ get_species_parms(df_parms = parm_df, species_name =  .x))
  
  return(sim_species_parms)
}

# test_function <- function(sp_names,
#                           df_parms) {
#   sim_species_parms <- sp_names %>%
#     purrr::set_names() %>%
#     purrr::map(.,
#                ~ get_species_parms(df_parms = parm_df, species_name =  .x))
#
#   sp_names <- species_names
#   df_parms <- species_parms
#   out <- vector(mode = "list", length = length(sp_names))
#
#   for (i in 1:length(sp_names)) {
#     df_species <- df_parms %>%
#       dplyr::filter(species == sp_names[i])
#     sp_parms_list <- list(
#       species = df_species$species,
#       M = df_species$M,
#       L_inf = df_species$L_inf,
#       k = df_species$k,
#       t_0 = df_species$t_0,
#       A_max = df_species$A_max,
#       A_mat = df_species$A_mat,
#       A_fish = df_species$A_fish,
#       biom_const = df_species$biom_const,
#       biom_exp = df_species$biom_exp
#     )
#
#     out[[i]] <- sp_parms_list
#   }
#   return(out)
# }

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


list_derived_variables <- function(spec_parms,
                                   sp_names,
                                   fish_mort) {
  sim_species_derived_vars <- spec_parms %>%
    purrr::map(.,
               ~  make_derived_variables(spec_parms = .x, fish_mort = fish_mort))
  
  names(sim_species_derived_vars) <- sp_names
  return(sim_species_derived_vars)
}

list_lep <- function(spec_names, sim_spec_der_var) {
  lifetime_eggs_production <- spec_names %>%
    purrr::map(., ~ sim_spec_der_var[[.x]][["EPRs"]][1])
  names(lifetime_eggs_production) <- spec_names
  return(lifetime_eggs_production)
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


#'
#' @title run age-structured model simulations
#'
#' @description project model populations that use a (multi-patch) Leslie matrix with
#' Beverton-Holt density dependence for a given species at a specified level
#' of FLEP (e.g., fishing mortality)
#'
#' @return a list of an array numbers at age (Ns) and matrix of recruits (R) and
#' eggs produced (Eggs)
#'
#' @param t_steps number - number of time steps in the simulations
#' @param num_patches number - number of patches (e.g., if 2 --> in and out of MPA)
#' @param les_mat array - an A_max X A_max X num_patches array of Leslie matrices
#' @param N array -  an A_max X t_steps X num_patches array to store simulation results
#' @param Cmat matrix - an num_patches X num_patches matrix to map Recruit contribution among patches. Diagonal means no dispersal among patches
#' @param alpha number - slope of SR curve at the origin
#' @param beta number - maximum density of recruits. A scaling/carrying capacity parameter
#' @param rec_sd number - standard deviation of recruitment variation
#' @param E matrix - an num_patches X t_steps matrix to store the (deterministic) number of eggs from Les matrix calcs
#' @param R matrix - an num_patches X t_steps matrix to store the (stochastic) number of recruits based on SR relationship
#'
#' @examples
#' results_cbzn_F0_2patch <- run_patch_sims(t_steps = ts, num_patches = nrow(Cmat2),
#'                                          les_mat = les_mat_cbzn_F0_F30,
#'                                          con_mat = Cmat2,
#'                                          N = N_cbzn_F0_F30,
#'                                          alpha = alpha_cbzn,
#'                                          beta = beta,
#'                                          noise = 'white',
#'                                          noise_series = white_noise
#'                                          )$Ns
#'

# t_steps = n_years
# num_patches = num_patches
# les_mat = leslie_matrices[["Blue rockfish"]]
# con_mat = connectivity_matrices[[3]]
# N = sim_results_arrays[["Blue rockfish"]]
# selectivity = sim_species_derived_vars[["Blue rockfish"]][["F_select"]]
# fishing_mortality = c(0, fishing_mortality_values[species_flep_fished_ind[["Blue rockfish"]]])
# weight_at_age = sim_species_parms[["Blue rockfish"]][["biom_const"]] *  sim_species_derived_vars[["Blue rockfish"]][["length_at_age"]] ^
#   sim_species_parms[["Blue rockfish"]][["biom_exp"]]
# alpha = bev_holt_alphas[["Blue rockfish"]]
# beta = bev_holt_beta
# noise_series = white_noise
#
# natural_mortality <- sim_species_parms[["Blue rockfish"]][["M"]]

run_patch_sims <- function(t_steps,
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
  Z <- natural_mortality + fishing_mortality
  E <- matrix(0, nrow = num_patches, ncol = t_steps) # eggs
  R <- E
  Y <- array(data = NA, dim = dim(N) - c(1, 0, 0))
  # harvest_rate_at_age <- selectivity * (1 - exp(-fishing_mortality))
  
  # loop over time
  for (t in 2:t_steps) {
    # loop over patches
    for (n in 1:num_patches) {
      catch_at_age <-
        selectivity * (1 - exp(-fishing_mortality[n])) * (fishing_mortality[n] / (natural_mortality + fishing_mortality[n]))
      N[, t, n] <-
        les_mat[, , n] %*% N[, t - 1, n] # advance the population
      Y[, t, n] <- # 2021-11-17 - check yield
        N[1:(length(weight_at_age) - 1), t, n] * catch_at_age * weight_at_age[-(length(weight_at_age))]
      # N[1:(length(weight_at_age) - 1), t - 1, n] * catch_at_age * weight_at_age[-(length(weight_at_age))]
      
      E[n, t] <- N[1, t, n] # pull out the eggs produced
    }
    
    # do larval pool dispersal
    E[, t] <-  con_mat %*% E[, t]
    
    # Apply density dependence
    for (n in 1:num_patches) {
      R[n, t] <-
        ((alpha * E[n, t]) / (1 + (alpha / (
          beta * frac_in_out[n]
        ))  * E[n, t])) * exp(noise_series[t])
      
      N[1, t, n] <-
        R[n, t] # add the actual surviving recruits back in
    }
  }
  return(list(
    Ns = N,
    Recs = R,
    Eggs = E,
    Y = Y
  ))
}



read_clean_enso <-
  function(fp = file.path("data", "enso_sst.indices.txt")) {
    enso_df <- read.table(fp ,
                          header = TRUE)
    enso_df <- enso_df %>%
      mutate(Time = enso_df$YR + enso_df$MON / 12,
             Pos = enso_df$ANOM.1 >= 0)
    return(enso_df)
  }

make_sim_enso <- function(sim_enso_len = 1012,
                          n_enso_sims = 100) {
  enso_data_fp <-
    file.path("data", "enso_sst.indices.txt")
  
  enso_df <- read.table(enso_data_fp, header = TRUE)
  
  enso_df <- enso_df %>%
    mutate(Time = enso_df$YR + enso_df$MON / 12,
           Pos = enso_df$ANOM.1 >= 0)
  
  ann_enso_data <- enso_df %>%
    group_by(YR) %>%
    summarise(ann_enso = mean(ANOM.1))
  
  enso_ts <- ann_enso_data$ann_enso
  sim_enso_matrix <- matrix(0,
                            nrow = length(enso_ts),
                            ncol = n_enso_sims)
  # create a time series by concatenating several fft + rand theta + ifft
  # and then pick a random starting point in the series
  for (i in 1:n_enso_sims) {
    theta <-
      runif(n = length(enso_ts)) * 2 * pi # 2pi converts to radians
    # Now do inverse FFT with modulus (real part) of original FFT but randomized phase
    Z <- Re(fft(enso_ts)) * exp(1i * theta)
    Z <- scale(Re(fft(Z, inverse = TRUE)))
    # ifelse(sim_enso <= 0, 0, sim_enso)
    sim_enso_matrix[, i] <- Z #sim_enso
  }
  # concatenate matrix by columns
  sim_enso_matrix <- c(sim_enso_matrix)
  # pick random starting point
  start_ind <-
    sample(
      x = 1:(length(sim_enso_matrix) - sim_enso_len),
      size = 1,
      replace = FALSE
    )
  # select output series
  sim_enso_ts <-
    sim_enso_matrix[start_ind:(start_ind + sim_enso_len - 1)]
  return(sim_enso_ts)
}

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

make_sim_ann_mei_ext <- function(mei,
                                 sim_enso_len,
                                 n_enso_sims = 100) {
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
    # ifelse(sim_enso <= 0, 0, sim_enso)
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

fft_abund <- function(mat) {
  X <- colSums(mat[7:25, 101:1e3])
  X <- detrend(X)
  Y <- fft(X)
  Y <- abs(Re(Y[1:(floor(length(Y)) / 2)]))
  Freq <- 1
  fs <- Freq * (0:(floor((length(
    X
  ) - 1) / 2))) / length(X)
  return(data.frame(fs = fs,
                    Y = Y))
}

plot_fft_abund <- function(dat) {
  ggplot(dat, aes(x = 1 / fs, y = Y ^ 2)) +
    geom_line() +
    xlim(0, 100) +
    theme_bw()
}


customFR2ts <- function(N, # number of time steps
                        reps,
                        r_seed = 1,
                        amp) {
  # LWB notes on generating time series with a specific
  # So, if you want to create white noise (equal variance at all frequencies)
  # you should generate a sine wave of frequency .1, .2, .3,....up to the maximum frequency you want,
  # then give each one a phase picked from a uniform distribution between 0 and 2 pi,
  # then add them together and divide by whatever it takes to get the variance you want.
  #
  # If you instead want to create a series to which salmon would be sensitive do not make
  # all of the sine waves the same amplitude, rather make the sine waves near 1/(generation time)
  # larger than the others somehow.  One way to do that would be to make them have the a Gaussian shape.
  #
  # Make sense?
  
  t <- 1:N # time index
  f <- (1:(N / 2)) / N # frequencies from 1/N to 0.5
  tsReps <- matrix(NA, nrow = N, ncol = reps)
  set.seed(r_seed)
  for (h in 1:reps) {
    rtheta <-
      runif(length(f), 0, 2 * pi) # the random phases to apply to each frequency
    ts <- matrix(NA, nrow = N, ncol = length(f))
    for (i in 1:length(f)) {
      if (length(amp) == 1) {
        ts[, i] <- amp * cos(2 * pi * f[i] * t - rtheta[i])
      } else {
        ts[, i] <- amp[i] * cos(2 * pi * f[i] * t - rtheta[i])
      }
    }
    
    noise <- rowSums(ts) # add up the curves
    noise <-
      (noise - mean(noise, na.rm = TRUE)) / sd(noise, na.rm = TRUE) # mean = 0, sd of 1
    tsReps[, h] <- noise
  }
  return(tsReps)
  
} # end of customFR2ts

ampf <- function(dat, N, span_num) {
  # freq, gps
  data_pgram <- spec.pgram(x = dat,
                           spans = c(span_num, span_num),
                           plot = FALSE)
  freq <- data_pgram$freq # Corresponding period information
  gps <- data_pgram$spec # change the name for use in ampf()
  appFR <- approxfun(freq, gps)
  amp <-
    curve(
      appFR(x),
      from = min(freq),
      to = 0.5,
      n = N,
      type = "l"
    )$y
  # there should be no missing values resulting from extrapolating beyond range of data
  #amp[is.na(amp)] <- 0 #  NAs are a problem when calling customFRts - multiplication by NA -> error
  return(amp)
}


# ampf <- function(dat, N) {
#   # freq, gps
#   data_pgram <- spec.pgram(x = dat,
#                            spans = c(11, 11),
#                            plot = FALSE)
#   freq <- data_pgram$freq # Corresponding period information
#   gps <- data_pgram$spec # change the name for use in ampf()
#   appFR <- approxfun(freq, gps)
#   amp <-
#     curve(
#       appFR(x),
#       from = min(freq),
#       to = 0.5,
#       n = N,
#       type = "l"
#     )$y
#   # there should be no missing values resulting from extrapolating beyond range of data
#   #amp[is.na(amp)] <- 0 #  NAs are a problem when calling customFRts - multiplication by NA -> error
#   return(amp)
# }


sim_enso_f <- function(df_col,
                       N = 100,
                       reps = 10,
                       rseed = 11) {
  out_enso <- customFR2ts(
    N = N,
    # number of time steps
    reps = reps,
    r_seed = rseed,
    amp = ampf(dat = df_col, N = N)
  )
  return(out_enso)
}

spans_val <- function(t) {
  stopifnot(!is.na(t))
  stopifnot(is.numeric(t))
  stopifnot(t > 0)
  st_val <- ceiling(sqrt(max(t)))
  if (st_val %% 2 == 0) {
    span_val <- st_val + 1
    return(c(span_val, span_val))
  }
  return(c(st_val, st_val))
}

syn_ts_from_spec <- function(dts,
                             amp_func,
                             ts_len,
                             r_seed = 5) {
  set.seed(r_seed)
  
  stopifnot(ts_len > 4)
  
  t <- 1:ts_len # index
  
  stopifnot(!is.na(t))
  stopifnot(is.numeric(t))
  stopifnot(t > 0)
  
  f <- seq(1, max(t) / 2, length = max(t)) # frequencies
  
  is.function(amp_func)
  
  amp <- amp_func(dat = dts,
                  span_num = 8,
                  N = length(f))
  # amp <- amp_func(dat = dts, span_num = spans_val(length(dts)), N = length(f))
  
  stopifnot(length(amp) == length(f)) # check that ampf_func output is correct length
  
  fs <- max(t) # sampling frequency same as fast as 0.5
  
  dat <- matrix(data = 0,
                nrow = max(t),
                ncol = length(f))
  
  for (i in 1:length(f)) {
    # scale each sine of freq f by the desired amp and assign a random phase
    dat[, i] <-
      amp[i] * sin(2 * pi * f[i] / fs * t - runif(length(f), 0, 2 * pi))
  }
  ts <- rowSums(dat)
  ts <- (ts - mean(ts)) / sd(ts)
  print("ts is of class: ")
  print(class(ts))
  return(ts)
}

make_one_over_f_gamma <- function(gamma_coef, ts_len, sd_mult) {
  bn <- primer::one_over_f(gamma = gamma_coef, N = ts_len)
  return(sd_mult * (bn / sd(bn)))
}

plotMeanFreqR <- function(dataMat, N) {
  # spectral frequency
  if (trunc(sqrt(N)) %% 2 == 0) {
    m <- trunc(sqrt(N)) + 1
  } else {
    m <- trunc(sqrt(N))
  }
  
  spcMean <- matrix(NA, nrow = N / 2, ncol = ncol(dataMat))
  
  freq <- (1:(N / 2)) / N
  for (i in 1:ncol(dataMat)) {
    ifelse(
      all(dataMat[, i] == 0),
      spcMean[, i] <- rep(NA, times = N / 2),
      spcMean[, i] <-
        spec.pgram(scale(dataMat[, i]), plot = F, c(m, m))$spec
    )
  }
  mean_spc <- rowMeans(spcMean, na.rm = TRUE)
  
  spc10 <- apply(spcMean,
                 1,
                 quantile,
                 probs = c(0.1),
                 na.rm = TRUE)
  spc25 <-
    apply(spcMean,
          1,
          quantile,
          probs = c(0.25),
          na.rm = TRUE)
  spc75 <-
    apply(spcMean,
          1,
          quantile,
          probs = c(0.75),
          na.rm = TRUE)
  spc90 <- apply(spcMean,
                 1,
                 quantile,
                 probs = c(0.9),
                 na.rm = TRUE)
  
  spec_data <- data.frame(
    freqs = freq,
    mean_spec = mean_spc,
    spec_10 = spc10,
    spec_25 = spc25,
    spec_75 = spc75,
    spec_90 = spc90
  )
  
  p <- ggplot(data = spec_data,
              aes(x = freqs, y = mean_spec)) +
    xlab("Frequency") +
    ylab("Relative Magnitude") +
    theme_bw() +
    geom_ribbon(aes(ymin = spec_10, ymax = spec_90), fill = "lightgrey") +
    geom_ribbon(aes(ymin = spec_25, ymax = spec_75), fill = "darkgrey") +
    geom_line(colour = "black", size = 1) +
    geom_line(colour = "white", size = .7) +
    geom_line(linetype = 2, size = .7)
  
  return(p)
}


#'
#' @title create 'custom' time series with specific frequency content via inverse FFT
#' @description Create 'custom' time series with a specified frequency content
#' to mimic real-world non-white (often reddened) environmental time series
#' (e.g., ENSO). This function uses the inverse FFT to create the time series.
#' The number of time series data created are specifed via the `n_reps` argument
#' The length of the time series created is the same as the input time series.
#' (This should be updated so that created time series can be of any length).
#' Returns a matrix with the number of rows equal to the length of the created
#' time series and the number of columns equal to `n_reps`.
#'
#' @return Returns a matrix with the number of rows equal to the length of the created
#' time series and the number of columns equal to `n_reps`.
#' @param dts the time series to mimic with respect to frequency content
#' @param n_reps the number of mimic time series to create
#' @param scale_sd standard deviation of the resulting time series, default value 1
#' @param scale_mean mean  of the resulting time series, default value 0
#' @param r_seed specify random seed for reproducibility
#'
#' @examples
#'
#' customFR2ts_ifft(dts = rnorm(1000, 0, 1), n_reps = 10, scale_sd = 3, scale_mean = 2, r_seed = 11)
#'

customFR2ts_ifft <- function(dts,
                             tot_sim_len,
                             scale_sd = 1,
                             # n_years,
                             scale_mean = 0,
                             r_seed = 11) {
  # require(pracma)
  set.seed(r_seed)
  ts_len <- length(dts)
  n_reps <- ceiling(1012 / ts_len)
  sim_ts_matrix <- matrix(NA, nrow = ts_len, ncol = n_reps)
  #simulated ffts all saved in one column
  
  for (j in 1:n_reps) {
    # We do this by randomizing the phase (imaginary part) of the FFT spectrum
    Theta <-
      runif(n = ts_len) * 2 * pi # 2pi converts to radians
    # Now do inverse FFT with modulus (real part) of original FFT but randomized phase
    Z <- Re(fft(dts)) * exp(1i * Theta)
    # Z <- Re(ifft(Z))
    Z <- Re(fft(Z, inverse = TRUE) / length(Z))
    sim_ts <-  as.vector(Z * scale_sd + scale_mean)
    
    sim_ts_matrix[, j] <- sim_ts
  }
  return(sim_ts_matrix)
  # return(c(sim_ts_matrix)[1:tot_sim_len])
}



#'
#' @title Age-structured population model parameters
#' @description Store life history parameters used in age-structured models for
#' multiple species.
#' @return A list/matrix/data.frame containing
#' @param species species name
#' @param Amax maximum age
#' @param Amat age of 50% maturity
#' @param Afish age of entry to fishery
#' @param M natural mortality
#' @param k von Bertalanffy rate parameter
#' @param Linf von Bertalanffy asymptotic length
#' @param fec_a multiplicative
#' @param fec_b = 3
#' @examples
#' two_patch_life_history_parms(species = "cobia", Amax = 15, Amat = 2,
#'                              Afish = 2, M = 0.2, k = 0.1, Linf = 200,
#'                              fec_a = 1, fec_b = 3)
#'
two_patch_life_history_parms <- function(species = "fake",
                                         Amax = 50,
                                         Amat = 5,
                                         Afish = 6,
                                         M = 0.2,
                                         k = 0.1,
                                         Linf = 100,
                                         fec_a = 1,
                                         fec_b = 3) {
  
}



egg_rec_calcs <-
  function(func,
           container,
           species,
           metric = c("Recs", "Eggs"),
           burn_in_len,
           f_vals) {
    fs_0_max <- f_vals[1:length(container[[species]])]
    drop_burn_in <- 1:burn_in_len
    out <- 1:length(fs_0_max) %>%
      purrr::map_dbl(., ~ func(container[[species]][[.x]][[metric]][-drop_burn_in]))
    names(out) <- paste0("F_", fs_0_max)
    return(out)
  }

yield_numbers_calcs <-
  function(func,
           container,
           species,
           metric = c("Ns", "Y"),
           burn_in_len,
           f_vals) {
    fs_0_max <- f_vals[1:length(container[[species]])]
    drop_burn_in <- 1:burn_in_len
    out <- 1:length(fs_0_max) %>%
      purrr::map_dbl(., ~ {
        dat <-
          colSums(container[[species]][[.x]][[metric]][, -drop_burn_in, 1], na.rm = TRUE)
        func(dat)
      })
    names(out) <- paste0("F_", fs_0_max)
    return(out)
  }



plot_mean_sd_cv <-
  function(metric,
           species,
           fs,
           means,
           sds,
           cvs,
           cols = "darkgrey",
           width = 1.5) {
    if (length(cols) == 1) {
      cols <- rep(cols, 3)
    } else if (length(cols) == 3) {
      pass
    } else {
      stop("colors should be a vector of length 1 or 3")
    }
    old <- par(
      mfrow = c(3, 1),
      mar = c(3, 3, 1, 1),
      mgp = c(1.5, 0.5, 0),
      tck = -0.02,
      cex.lab = 1.65,
      cex.axis = 1.35,
      cex.main = 1.8
    )
    plot(
      fs,
      means,
      type = "l",
      col = cols[1],
      xlab = "",
      ylab = paste0("Mean ", metric),
      main = paste0("Species: ", species)
    )
    plot(
      fs,
      sds,
      type = "l",
      col = cols[2],
      xlab = "",
      ylab = paste0("Standard deviation ", metric)
    )
    plot(
      fs,
      cvs,
      type = "l",
      col = cols[3],
      ylab = paste0("CV ", metric),
      xlab = "Fishing mortailty"
    )
    par(old)
  }


make_sim_storage_list <- function(sp_names = "Cabezon",
                                  n_runs = 1,
                                  flep_vals = seq(0.3, 0.8, by = 0.05),
                                  frac_vals = c(seq(0, 0.3, by = 0.02), 1)) {
  # check that sp_names are valid
  n_species <- length(sp_names)
  species_list <-  vector("list", length = n_species)
  names(species_list) <- sp_names
  
  # number of runs: n_runs <- 1
  
  run_list <- vector("list", length = n_runs)
  names(run_list) <- 1:n_runs
  
  # (number of) fleps: seq(0.3, 0.8, by = 0.05)
  n_fleps <- length(flep_vals)
  flep_list <- vector("list", length = n_fleps)
  names(flep_list) <- flep_vals
  
  # fracs of coastline: c(seq(0, 0.3, by = 0.02), 1)
  
  n_fracs <- length(frac_vals)
  frac_list <- vector("list", length = n_fracs)
  names(frac_list) <- frac_vals
  
  for (i in 1:n_species) {
    for (j in 1:n_runs) {
      for (k in 1:n_fleps) {
        flep_list[[k]] <- frac_list
      }
      run_list[[j]] <- flep_list
    }
    species_list[[i]] <- run_list
  }
  
  return(species_list)
}

## get the index for the F yielding target flep

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

#' Make Leslie matrices for population projection by one time step

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

# make_sim_dt <- function(exp_names,
#                         species_names,
#                         bev_holt_alphas,
#                         sd_recruitment,
#                         reserve_fracs,
#                         flep_ratios,
#                         noises,
#                         sim_nums) {
#   # Steps to pre-populate data.table before simulation runs.
#   
#   num_alphas_per_species <-
#     length(bev_holt_alphas) / length(species_names)
#   
#   # Create variables that define model experiment and experimental parameters
#   exp_names_vals <-
#     rep(
#       exp_names,
#       each = length(species_names) * num_alphas_per_species * length(sd_recruitment) * length(reserve_fracs) * length(flep_ratios) * length(noises) * sim_nums
#     )
#   
#   sp_names_vals <-
#     rep(
#       species_names,
#       each = num_alphas_per_species * length(sd_recruitment) * length(reserve_fracs) * length(flep_ratios) * length(noises) * sim_nums,
#       times = length(exp_names)
#     )
#   
#   bev_holt_alphas_vals <-
#     rep(
#       bev_holt_alphas,
#       each = length(sd_recruitment) * length(reserve_fracs) * length(flep_ratios) * length(noises) * sim_nums,
#       times = length(exp_names) * num_alphas_per_species
#     )
#   
#   rec_sd_vals <-
#     rep(
#       sd_recruitment,
#       each = length(reserve_fracs) * length(flep_ratios) * length(noises) * sim_nums,
#       times = length(exp_names) * length(species_names) * num_alphas_per_species
#       
#     )
#   
#   reserve_fracs_vals <-
#     rep(
#       reserve_fracs,
#       each = length(flep_ratios) * length(noises) * sim_nums,
#       times = length(exp_names) * length(species_names) * num_alphas_per_species * length(sd_recruitment)
#     )
#   
#   flep_ratios_vals <-
#     rep(
#       flep_ratios,
#       each = length(noises) * sim_nums,
#       times = length(exp_names) * length(species_names) * num_alphas_per_species * length(sd_recruitment) *
#         length(reserve_fracs)
#     )
#   
#   noises_vals <-
#     rep(
#       noises,
#       each = sim_nums,
#       times = length(exp_names) * length(species_names) * num_alphas_per_species * length(sd_recruitment) *
#         length(reserve_fracs) * length(flep_ratios)
#     )
#   
#   sim_nums_vals <-
#     rep(
#       1:sim_nums, # sim_nums
#       times = length(exp_names) * length(species_names) *  num_alphas_per_species * length(sd_recruitment) *
#         length(reserve_fracs) * length(flep_ratios) * length(noises)
#     )
#   
#   dt <- data.table(
#     experiment = exp_names_vals,
#     species = sp_names_vals,
#     bh_alpha = bev_holt_alphas_vals,
#     rec_sd = rec_sd_vals,
#     reserve_frac = reserve_fracs_vals,
#     flep_ratio = flep_ratios_vals,
#     noise = noises_vals,
#     sim_num = sim_nums_vals,
#     number = vector("list"),
#     yield = vector("list"),
#     recruits = vector("list"),
#     eggs = vector("list")
#   )
#   
#   # setting the key for "fast" indexing of data.table storage
#   setkeyv(
#     x = dt,
#     c(
#       "experiment",
#       "species",
#       "bh_alpha",
#       "rec_sd",
#       "reserve_frac",
#       "flep_ratio",
#       "noise",
#       "sim_num"
#     )
#   )
#   return(dt)
# }


make_sim_dt2 <- function(sim_nums,
                         reserve_fracs_vals,
                         flep_ratios_vals,
                         sim_years,
                         max_age,
                         check=FALSE) {
  
  # Steps to pre-populate data.table before simulation runs.
  
  sim_nums_vals <- 1:sim_nums
  year_vals <- 1:sim_years
  patch_vals <- 1:2
  age_vals <- 1:max_age
  
  num_sims_dt <- rep(
    sim_nums_vals,
    each = length(reserve_fracs_vals) * length(flep_ratios_vals) *  length(year_vals) * max(patch_vals) * length(age_vals)
  )
  
  flep_ratios_dt <- rep(
    flep_ratios_vals,
    each = length(patch_vals) * length(age_vals) *  length(year_vals) * length(reserve_fracs_vals) , 
    times = sim_nums
  )
  
  reserve_fracs_dt <- rep(
    reserve_fracs_vals,
    each = length(patch_vals) * length(age_vals) *  length(year_vals), 
    times = sim_nums * length(flep_ratios_vals) 
  )
  
  year_vals_dt <- rep(
    year_vals,
    each = length(patch_vals) * length(age_vals),
    times =  sim_nums * length(reserve_fracs_vals) * length(flep_ratios_vals) 
  )
  
  age_vals_dt <- rep(
    age_vals,
    each = length(patch_vals),
    times = sim_nums * length(reserve_fracs_vals) * length(flep_ratios_vals) *  length(year_vals) 
  )
  
  patch_num_dt <- rep(
    patch_vals,
    times = sim_nums * length(reserve_fracs_vals) * length(flep_ratios_vals) *  length(year_vals) * length(age_vals)
  )
  
  
  if (check) {
    print(table(num_sims_dt))
    print(table(flep_ratios_dt))
    print(table(reserve_fracs_dt))
    print(table(year_vals_dt))
    print(table(age_vals_dt))
    print(table(patch_num_dt))
  }
  
  # make the data table
  dt <- data.table(
    sim_num = num_sims_dt,
    reserve_frac = reserve_fracs_dt,
    flep_ratio = flep_ratios_dt,
    year = year_vals_dt,
    age = age_vals_dt,
    patch_num = patch_num_dt, 
    number = NA_real_,
    yield = NA_real_
  )
  
  # setting the key for "fast" indexing of data.table storage
  setkeyv(
    x = dt,
    c(
      "sim_num",
      "reserve_frac",
      "flep_ratio",
      "year",
      "age",
      "patch_num"
    )
  )
  return(dt)
}



yield_sp_sim_flep_frac <-
  function(results_list,
           sp_names,
           n_runs,
           flep_vals,
           frac_vals,
           burn,
           sim_len) {
    out <- vector("list", length = length(sp_names))
    for (i in 1:length(species_names)) {
      out[[i]] <- vector("list", length = length(n_sims))
      for (j in 1:length(n_sims)) {
        out[[i]][[j]] <- vector("list", length = length(fleps))
        for (k in 1:length(fleps)) {
          out[[i]][[j]][[k]] <- vector("list", length = length(reserve_fracs))
          for (l in 1:length(reserve_fracs)) {
            out[[i]][[j]][[k]][[l]] <-
              colSums(apply(results_list[[i]][[j]][[k]][[l]]$Y, c(1, 2), sum, na.rm = TRUE)[, (burn_in + 1):(n_years)])
          }
        }
      }
    }
    return(out)
  }

numbers_sp_sim_flep_frac <-
  function(results_list,
           sp_names,
           n_runs,
           flep_vals,
           frac_vals,
           burn,
           sim_len) {
    out <- vector("list", length = length(sp_names))
    for (i in 1:length(species_names)) {
      out[[i]] <- vector("list", length = length(n_sims))
      for (j in 1:length(n_sims)) {
        out[[i]][[j]] <- vector("list", length = length(fleps))
        for (k in 1:length(fleps)) {
          out[[i]][[j]][[k]] <- vector("list", length = length(reserve_fracs))
          for (l in 1:length(reserve_fracs)) {
            out[[i]][[j]][[k]][[l]] <-
              colSums(apply(results_list[[i]][[j]][[k]][[l]]$Ns, c(1, 2), sum, na.rm = TRUE)[, (burn_in + 1):(n_years)])
          }
        }
      }
    }
    return(out)
  }


biomass_sp_sim_flep_frac <-
  function(results_list,
           weight_at_age,
           sp_names,
           n_runs,
           flep_vals,
           frac_vals,
           burn,
           sim_len) {
    out <- vector("list", length = length(sp_names))
    for (i in 1:length(species_names)) {
      out[[i]] <- vector("list", length = length(n_sims))
      waa <- weight_at_age[[i]]
      for (j in 1:length(n_sims)) {
        out[[i]][[j]] <- vector("list", length = length(fleps))
        for (k in 1:length(fleps)) {
          out[[i]][[j]][[k]] <- vector("list", length = length(reserve_fracs))
          for (l in 1:length(reserve_fracs)) {
            out[[i]][[j]][[k]][[l]] <-
              colSums(apply(results_list[[i]][[j]][[k]][[l]]$Ns * waa, c(1, 2), sum, na.rm = TRUE)[, (burn_in + 1):(n_years)])
          }
        }
      }
    }
    return(out)
  }


run_sims <- function(dt,
                     expmts,
                     spec_name,
                     alpha_bh,
                     beta_bh,
                     recr_sd,
                     fleps,
                     frac_reserves,
                     noise_vec,
                     noise_dat,
                     n_years,
                     num_sims,
                     num_patches,
                     les_mats,
                     conn_mats,
                     in_out_fracs,
                     sim_results,
                     sim_spec_dervars,
                     f_vals,
                     spec_flep_f_ind,
                     sim_spec_pars) {
  for (i in 1:length(expmts)) {
    for (j in 1:length(spec_name)) {
      for (p in noise_vec) {
        for (m in 1:length(recr_sd)) {
          for (n in 1:length(fleps)) {
            for (o in 1:length(frac_reserves)) {
              # for (p in noise_vec) {
              for (q in 1:num_sims) {
                out <- run_patch_sims2(
                  t_steps = n_years,
                  num_patches = num_patches,
                  les_mat = les_mats[[j]][[n]],
                  con_mat = conn_mats[[o]],
                  frac_in_out = in_out_fracs[[o]],
                  N = sim_results[[j]][[n]],
                  # this needs the values from a unfished_N1 for each species
                  selectivity = as.integer(sim_spec_dervars[[j]][["F_select"]]),
                  fishing_mortality = c(0, f_vals[spec_flep_f_ind[[j]][[n]]]),
                  natural_mortality = sim_spec_pars[[j]][["M"]],
                  weight_at_age = sim_spec_pars[[j]][["biom_const"]] * sim_spec_dervars[[j]][["length_at_age"]] ^
                    sim_spec_pars[[j]][["biom_exp"]],
                  alpha = alpha_bh[[j]],
                  beta = beta_bh,
                  noise_series = noise_dat
                )
                
                out_Ns <-
                  setnames(
                    as.data.table(out[["Ns"]]),
                    old = c("V1", "V2", "V3", "value"),
                    new = c("age", "year", "patch_num", "number")
                  )
                setkeyv(out_Ns, c("age", "year", "patch_num"))
                
                out_Y <-
                  setnames(
                    as.data.table(out[["Y"]]),
                    old = c("V1", "V2", "V3", "value"),
                    new = c("age", "year", "patch_num", "yield")
                  )
                setkeyv(out_Y, c("age", "year", "patch_num"))
                
                dt[i = .(
                  expmts[i],
                  spec_name[j],
                  alpha_bh[j],
                  recr_sd[m],
                  frac_reserves[o],
                  fleps[n],
                  p,
                  num_sims[q]
                ), j =  `:=` (number = list(out_Ns),
                              yield = list(out_Y))] #,
                              # recs = list(out_Recs),
                              # eggs = list(out_Eggs))]
                              rm(out_Ns, out_Eggs, out) # out_Recs, out_Y,
              }
            }
          }
        }
        
        print(paste0("dispersal: ", expmts[i], " and species: ", spec_name[j],
          " and ", p, " noise ", " and recruit sd of ", recr_sd[m]))
        
        dt_sub <- dt %>%
          dplyr::filter(species == spec_name[j]) %>%
          select(-recruits,-eggs) %>%
          unnest(c(number, yield), names_repair = "minimal")
        
        dt_sub <- dt_sub[!duplicated(as.list(dt_sub))]
        
        dt <- dt %>%
          dplyr::filter(species != spec_name[j])
        
        con <-
          dbConnect(duckdb::duckdb(),
                    dbdir = "data/sim_out.duckdb",
                    read_only = FALSE)
        duckdb::dbWriteTable(con,
                             paste0(expmts[i], "_", tolower(sub(
                               " ", "_", spec_name[j]
                             )), "_", p, "_", substring(as.character(recr_sd[m]), 3)),
                             dt_sub,
                             append = TRUE) # FALSE

        DBI::dbDisconnect(con, shutdown = TRUE)
      }
    # }
    # print(paste0("dispersal: ", expmts[i], " and species: ", spec_name[j]))
    # dt_sub <- dt %>%
    #   dplyr::filter(experiment == expmts[i], species == spec_name[j]) %>%
    #   select(-recruits, -eggs) %>%
    #   unnest(c(number, yield), names_repair = "minimal")
    #
    # print(unique(dt_sub$species))
    # print("Subset data to write successful")
    #
    # dt_sub <- dt_sub[!duplicated(as.list(dt_sub))]
    # print("unnesting data and deduping cols just happend")
    #
    # dt <- dt %>%
    #   dplyr::filter(experiment != expmts[i] | species != spec_name[j])
    # print("Subset data to keep successful")
    # print(unique(dt$species))
    #
    # con <- dbConnect(duckdb::duckdb(), dbdir = "data/sim_out.duckdb", read_only = FALSE)
    # duckdb::dbWriteTable(con, paste0(expmts[i], "_", tolower(sub(" ", "_", spec_name[j]))), dt_sub)
    # print("Tables in DB:")
    # print(DBI::dbListTables(con))
    #
    # DBI::dbDisconnect(con, shutdown=TRUE)
  }
}
return(dt)
}

###
###
# out_Eggs <- data.table(
#   year = rep(1:ncol(out[["Eggs"]]), times = 2),
#   patch_num = rep(1:2, each = ncol(out[["Eggs"]])),
#   eggs = c(out[["Eggs"]][1, ], out[["Eggs"]][2, ])
# )
# setkeyv(out_Eggs, c("year", "patch_num"))
#
# out_Recs <- data.table(
#   year = rep(1:ncol(out[["Recs"]]), times = 2),
#   patch_num = rep(1:2, each = ncol(out[["Recs"]])),
#   eggs = c(out[["Recs"]][1, ], out[["Recs"]][2, ])
# )
# setkeyv(out_Recs, c("year", "patch_num"))
###
###

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
# title: loops over combinations of parameters for a given experiment to run specified simulation
# description:
# returns: a `list` data.table where each record contains the combination of parameters used in an
#          experiment and holds the simulation output variables (numbers, yield,) in a column
# params:
# sp_names:
# bh_alphas:
# sd_recruitment:
# flep_ratios:
# reserve_fracs:
# noises:
# sim_nums:
# patch_nums:
# leslie_matrices:
# connectivity_matrices:
# in_out_fracs:
# sim_results_arrays:
# sim_species_derived_vars:
# fishing_mortality_values:
# species_flep_fished_ind:
# sim_species_parms:
# bev_holt_alphas:
# bev_holt_beta:

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
  Y[, 1,] <- 0 # Y[1, ,] <- 0
  max_age <- dim(N)[1] #nrow(N)
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
        N_harvest * weight_at_age[-1] #[-(length(weight_at_age))]
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

f2flep_calcs <- function(specs,
                         inds_fished_flep,
                         fs) {
  spec_f2flep <- purrr::map_dbl(inds_fished_flep, ~ fs[[.x]])
  
  spec_f2flep <- data.table::data.table(
    species = specs,
    fleps = as.numeric(names(spec_f2flep)),
    f_vals = as.numeric(spec_f2flep)
  )
  
  return(spec_f2flep)
}
# f2flep_calcs <- function(specs,
#                          inds_fished_flep,
#                          fs) {
#   spec_f2flep <- vector("list", length(specs))
#
#   for (i in specs) {
#     spec_f2flep[[i]] <- purrr::map_dbl(inds_fished_flep[[i]], ~ fs[[.x]])
#   }
#
#   spec_f2flep <- specs %>%
#     purrr::map(., ~ data.table(
#       species = .x,
#       fleps = names(spec_f2flep[[.x]]),
#       f_vals = spec_f2flep[[.x]]
#     )) %>%
#     rbindlist(.)
#
#   spec_f2flep$fleps <- as.numeric(spec_f2flep$fleps)
#
#   return(spec_f2flep)
# }

merge_yield_flep2fvals <- function(mean_yield_by_flep,
                                   f2flep) {
  yield_flep2fvals <- merge(
    mean_yield_by_flep,
    f2flep,
    by.x = c("species", "flep_ratio"),
    by.y = c("species", "fleps")
  )
  
  
  return(yield_flep2fvals[, scaled_yield :=  V1 / max(V1), by = .(experiment, noise, species)])
}


no_reserve_yield_msy_plot <- function(msy_f_flep,
                                      spec,
                                      expmt) {
  ggplot(msy_f_flep,
         aes(x = f_vals,
             y = V1,
             color = noise)) +
    ylab("Yield") +
    xlab("F") +
    ggtitle(paste0("MSY for no coast in reserve: ", spec, " and ", expmt)) +
    geom_line() +
    geom_vline(data = msy_f_flep[msy_f_flep[, .I[V1 == max(V1)], by = .(experiment, noise, species)][, V1]],
               aes(xintercept = f_vals),
               colour = "red3") +
    xlim(0, 0.8) +
    theme_bw()
}

# Plot yield and cv recruitment contours vs F and Frac in reserve
cv_recs_yield_contour_plots <- function(rec_data,
                                        cv_rec_brks = c(seq(0.5, 2, by = 0.1), Inf),
                                        yield_data,
                                        yield_contours = c(0.8, 0.95),
                                        # f2flep,
                                        spec,
                                        expt) {
  ggplot(data = rec_data,
         aes(x = reserve_frac,
             y = f_vals,
             z = V1)) +
    xlab("Fraction in reserve") +
    ylab("F") +
    geom_contour_filled(breaks = cv_rec_brks) +
    geom_contour(
      data = yield_data,
      aes(x = reserve_frac,
          y = f_vals,
          z = scaled_yield),
      breaks = yield_contours,
      color = "lightgrey"
    ) +
    metR::geom_text_contour(
      data = yield_data,
      aes(x = reserve_frac,
          y = f_vals,
          z = scaled_yield),
      breaks = yield_contours,
      color = "lightgrey",
      skip = 0,
      label.placer = isoband::label_placer_middle(),
      size = 3
    ) +
    facet_grid(. ~ noise) +
    # geom_hline(
    #   data = f2flep,
    #   aes(yintercept = f_vals),
    #   colour = "lightgrey",
    #   linetype = 2
    # ) +
    theme(legend.position = "right") +
    scale_x_continuous(expand = expansion(mult = c(0, 0))) +
    scale_y_continuous(expand = expansion(mult = c(0, 0), add = c(0,-1))) +
    ggtitle(
      paste0(
        "CV recruitment (color) and yield (grey line) contour\nfor ",
        spec,
        " with ",
        expt
      )
    ) +
    labs(fill = 'CV recruitment') +
    theme(text = element_text(size = 12))
}

dat_yield_v_frac_res_by_flep <-
  function(base_cols = c(
    "experiment",
    "species" ,
    "bh_alpha",
    "rec_sd",
    "reserve_frac",
    "flep_ratio",
    "noise",
    "sim_num"
  ),
  out,
  species_names,
  exp_names) {
    base_cols_recs <- c(base_cols, "number")
    base_cols_yield <- c(base_cols, "yield")
    
    # expand list.frame to get yield sim results
    out_yield <- out[experiment %in% exp_names &
                       species %in% species_names,
                     j = ..base_cols_yield] %>% unnest(cols = c(yield)) %>%
      as.data.table(.)
    
    # expand list.frame to get recruitment sim results
    out_recs <- out[experiment %in% exp_names &
                      species %in% species_names,
                    j = ..base_cols_recs] %>% unnest(cols = c(number)) %>%
      as.data.table(.)
    
    out_recs <-
      out_recs[, j = .(recruits = sum(number)), by = .(
        experiment,
        species,
        bh_alpha,
        rec_sd,
        reserve_frac,
        flep_ratio,
        noise,
        sim_num,
        year
      )]
    # aggregate recruits across patches
    
    
    # get total yield (aggregate across ages and patches)
    out_yield <-
      out_yield[, j = .(yield = sum(yield)), by = .(
        experiment,
        species,
        bh_alpha,
        rec_sd,
        reserve_frac,
        flep_ratio,
        noise,
        sim_num,
        year
      )]
    
    # merge out_recs and out_yield
    dt <- merge(
      out_yield,
      out_recs,
      by = c(
        "experiment",
        "species",
        "bh_alpha",
        "rec_sd",
        "reserve_frac",
        "flep_ratio",
        "noise",
        "sim_num",
        "year"
      )
    )
    return(dt)
  }

yield_vs_resfrac_by_flep <- function(dt,
                                     species_names,
                                     exp_names,
                                     noises,
                                     flep_keeps = (3:8) / 10) {
  dt_yp <-
    dt[year > 500 &
         flep_ratio %in% flep_keeps, .(yield = mean(yield) / 1000),
       by = c(
         "experiment",
         "species",
         "bh_alpha",
         "rec_sd",
         "reserve_frac",
         "flep_ratio",
         "noise",
         "sim_num"
       )]
  
  p <-
    ggplot(dt_yp, aes(
      x = reserve_frac,
      y = yield,
      color = factor(flep_ratio)
    )) +
    geom_line() +
    theme_bw() +
    scale_color_discrete_sequential(palette = "Viridis") +
    facet_grid(species ~ ., scales = "free_y") +
    expand_limits(x = 0, y = 0) +
    ggtitle(paste0(exp_names, " and ", noises))
  
  return(p)
}

plot_cv_metric_vs_resfrac_by_flep <- function(dt,
                                              # metric,
                                              burn_in_end,
                                              species_names,
                                              exp_names,
                                              noises,
                                              flep_keeps = (3:8) / 10) {
  dt_cvp <- dt[year > burn_in_end & flep_ratio %in% flep_keeps,
               .(cv = sd(recruits) / mean(recruits)),
               by = c(
                 "experiment",
                 "species",
                 "bh_alpha",
                 "rec_sd",
                 "reserve_frac",
                 "flep_ratio",
                 "noise",
                 "sim_num"
               )]
  
  p <-
    ggplot(dt_cvp, aes(
      x = reserve_frac,
      y = cv,
      color = factor(flep_ratio)
    )) +
    geom_line()  +
    theme_bw() +
    facet_grid(species ~ .) +
    scale_color_discrete_sequential(palette = "Viridis") +
    facet_grid(species ~ ., scales = "free_y") +
    expand_limits(x = 0, y = 0) +
    ggtitle(paste0(exp_names, " and ", noises))
  
  return(p)
}

# plot two spectra on same graph

plot_3_spectra <- function(burn,
                           len_sim,
                           series1,
                           ser1name,
                           series2,
                           ser2name,
                           series3,
                           ser3name,
                           rand_seed) {
  first_sim_ind <- burn + 1
  total_sim_len <- burn + len_sim
  x <- ceiling(sqrt(length((first_sim_ind + 1):(total_sim_len))))
  span_odd_num <- ifelse(x %% 2 == 0, x + 1, x)
  
  np1 <- spectrum(series1[first_sim_ind:total_sim_len],
                  spans = c(span_odd_num, span_odd_num),
                  plot = FALSE)
  np2 <- spectrum(series2[first_sim_ind:total_sim_len],
                  spans = c(span_odd_num, span_odd_num),
                  plot = FALSE)
  np3 <- spectrum(series3,
                  spans = c(9, 9),
                  plot = FALSE)
  
  df1 <- data.frame(freq = np1[["freq"]],
                    spec = np1[["spec"]],
                    name_col = ser1name)
  df2 <- data.frame(freq = np2[["freq"]],
                    spec = np2[["spec"]],
                    name_col = ser2name)
  df3 <- data.frame(freq = np3[["freq"]],
                    spec = np3[["spec"]],
                    name_col = ser3name)
  
  df_specs <- rbind(df1, df2, df3)
  
  max_y <- 1.1 * max(df_specs$spec)
  
  p <- ggplot(data = df_specs,
              aes(x = freq,
                  y = spec,
                  colour = name_col)) +
    geom_line() +
    theme_bw() +
    ggtitle(paste0("Noise spectra -- rand seed: ", r_seed)) +
    ylim(0, max_y)
  return(p)
}


## FUNCS ADDED NOV 20 2022
get_fs_from_fleps <- function(species,
                              flep_inds,
                              f_vals) {
  fleps_2_fs <-
    data.frame(flep_vals = as.numeric(names(flep_inds[[species]])),
               f_vals = f_vals[as.numeric(flep_inds[[species]])])  %>%
    mutate(flep_vals_ch = as.character(flep_vals))
  
  return(fleps_2_fs)
}


## Extract data from list.frames and transform for plots/analyses
## get_outvar_from_species_sim_output () returns takes speci_disp (`lf`) and returns spec_disp_metric (i.e., brf_lp_yield)
get_outvar_from_species_sim_output <- function(lf,
                                               target_col,
                                               keep_cols,
                                               drop_cols,
                                               burn_in_len) {
  lf_metric <- lf %>%
    select(all_of(keep_cols)) %>%
    unnest(cols = c(!!sym(target_col))) %>%
    dplyr::filter(year > burn_in_len) %>%
    select(-all_of(drop_cols)) %>%
    as.data.table(.)
  
  return(lf_metric)
}

get_noise_from_spec_disp_var <- function(sdv,
                                         metric,
                                         noise_type) {
  if (metric == "number") {
    keeper_vars <-
      c(
        "experiment",
        "noise",
        "reserve_frac",
        "flep_ratio",
        "age",
        "year",
        "patch_num",
        "number"
      )
  }
  
  if (metric == "yield") {
    keeper_vars <-
      c(
        "experiment",
        "noise",
        "reserve_frac",
        "flep_ratio",
        "age",
        "year",
        "patch_num",
        "yield"
      )
  }
  
  return(sdv[i = noise == noise_type, ..keeper_vars])
}

## Find FLEP that gets the max yield for no reserves

flep_2_max_yield_no_reserves <- function(sp_dis_yield,
                                         fs_from_fleps) {
  sp_dis_yield_reg_sd <-
    sp_dis_yield[i = noise == "white"][reserve_frac == 0, .(reserve_frac, flep_ratio, age, year, patch_num, yield)]
  
  sp_dis_yield_reg_sd[, flep_yields := sum(yield) , by = flep_ratio]
  
  sp_dis_yield_reg_sd <- sp_dis_yield_reg_sd %>%
    dplyr::left_join(x = .,
              y = fs_from_fleps,
              by = c("flep_ratio" = "flep_vals")) %>%
    dplyr::filter(!is.na(f_vals))
  
  F_max_yield_reg_sd <- sp_dis_yield_reg_sd %>%
    dplyr::group_by(f_vals, flep_ratio) %>%
    dplyr::summarise(yields = sum(yield) / 1e6) %>%
    dplyr::ungroup(.) %>%
    dplyr::filter(yields == max(yields)) %>%
    .$f_vals
  return(F_max_yield_reg_sd)
}

## Plot prep

frac_years_below_max_yield <- function(sdmn,
                                       fs_from_fleps,
                                       f_maxes_yield,
                                       sim_len) {
  sdmn_ann_yield <- sdmn %>%
    dplyr::mutate(flep_ratio = as.character(flep_ratio)) %>%
    dplyr::left_join(
      x = .,
      y = fs_from_fleps[, c("flep_vals_ch", "f_vals")],
      by = c("flep_ratio" = "flep_vals_ch")
    )  %>%
    dplyr::group_by(reserve_frac, flep_ratio, f_vals, year) %>%
    dplyr::summarize(annual_yield = sum(yield)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(f_fmsy = f_vals / f_maxes_yield)
  
  # Find median yield where F that maxes yield
  
  median_ann_yield_f_max_yield <- sdmn_ann_yield %>%
    dplyr::filter(f_vals == f_maxes_yield) %>%
    dplyr::summarize(median_ann_yield_f_max_yield = median(annual_yield)) %>%
    .$median_ann_yield_f_max_yield
  
  
  sdmn_frac_below_med_max_yield <-  sdmn_ann_yield %>%
    dplyr::mutate(below_sdmn_ann_yield = if_else(annual_yield < median_ann_yield_f_max_yield, 1, 0)) %>%
    dplyr::group_by(reserve_frac, f_fmsy) %>%
    dplyr::summarise(n_yrs_below = sum(below_sdmn_ann_yield)) %>%
    dplyr::filter(n_yrs_below <= sim_len) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(frac_below_med_yield_f_max = round(n_yrs_below / sim_len, 3))
  
  return(sdmn_frac_below_med_max_yield)
}

##  Yield plots
## Existing functions
plot_frac_years_below_max_yield <-
  function(sdmn_frac_below_med_max_yield,
           species,
           dispersal) {
    sdmn_frac_below_med_max_yield_plt <-
      sdmn_frac_below_med_max_yield %>%
      dplyr::filter(., reserve_frac <= 0.55, f_fmsy <= 2.5) %>%
      ggplot(data = .,
             aes(x = reserve_frac, y = f_fmsy, z = frac_below_med_yield_f_max)) +
      geom_contour_filled(bins = 12) +
      ggtitle(paste0(species, " ", dispersal, ": time below median max yield")) +
      xlab("Reserve fraction") +
      ylab("F/Fmsy") +
      scale_fill_viridis_d(option = "D", direction = -1) +
      geom_segment(aes(
        x = 0,
        xend = 0.55,
        y = 1,
        yend = 1
      ), color = "red")
    return(sdmn_frac_below_med_max_yield_plt)
  }

## Bolker diff

bdiff <- function(x) {
  return(c(NA, diff(x)))
}
## CONTOUR + GRADIENT VECTOR
## YIELD
sdmn_calc_gradient_vector_yield <- function(sdmn_frac_yield) {
  
  stopifnot(ncol(sdmn_frac_yield) == 4)
  lapply(sdmn_frac_yield, function(x) stopifnot(is.numeric(x)))
  print(sdmn_frac_yield$frac_below_med_yield_f_max)
  stopifnot((sdmn_frac_yield$frac_below_med_yield_f_max <= 1) & (sdmn_frac_yield$frac_below_med_yield_f_max >= 0))
  
  sdmn_frac_yield_w <- sdmn_frac_yield %>%
    tidyr::pivot_wider(id_cols = reserve_frac,
                names_from = f_fmsy,
                names_prefix = "f_flep_",
                values_from = frac_below_med_yield_f_max)
  
  sdmn_frac_yield_mat <- as.matrix(sdmn_frac_yield_w[,2:ncol(sdmn_frac_yield_w)])
  
  rr <- row(sdmn_frac_yield_mat)
  cc <- col(sdmn_frac_yield_mat)
  dx <- t(apply(sdmn_frac_yield_mat, 1, bdiff))
  dy <- apply(sdmn_frac_yield_mat, 2, bdiff)
  
  f_vals <- unique(sdmn_frac_yield$f_fmsy)
  res_vals <- unique(sdmn_frac_yield$reserve_frac)
  
  dx_tib <- tibble(data.frame(dx))
  dx_tib$reserve_frac <- as.character(res_vals)
  
  names(dx_tib)[1] <- "f_flep_0.0000000"
  dx_tib <- pivot_longer(dx_tib, cols = c(starts_with("f_flep_")), 
                         names_to = "f_fmsy",  values_to = "dx") %>%
    dplyr::mutate(f_fmsy = substr(as.character(substring(f_fmsy, first = 8)), 1, 10))
  
  
  dy_tib <- tibble(data.frame(dy))
  dy_tib$reserve_frac <- as.character(res_vals)
  dy_tib <- dy_tib %>%
    tidyr::pivot_longer(., cols = c(starts_with("f_flep_")), 
                 names_to = "f_fmsy",  values_to = "dy") %>%
    dplyr::mutate(f_fmsy = substr(as.character(substring(f_fmsy, first = 8)), 1, 10))
  
  
  plt_dat <- sdmn_frac_yield %>%
    dplyr::mutate(reserve_frac = as.character(reserve_frac), 
           f_fmsy = as.character(f_fmsy),
           f_fmsy = substr(f_fmsy, 1, 10)) %>%
    dplyr::left_join(., dx_tib, by = c("reserve_frac", "f_fmsy")) %>%
    dplyr::left_join(., dy_tib, by = c("reserve_frac", "f_fmsy")) %>%
    dplyr::mutate(reserve_frac = as.numeric(reserve_frac),
           f_fmsy = as.numeric(f_fmsy)) 
  
  return(plt_dat)
}


plt_sdmn_yield_gradvec_cont <- function(plt_dat, species, dispersal, noise,
                                        max_res_frac = 0.5, 
                                        max_f_fmsy = 2) {
  plt_dat_preped <- plt_dat %>%
    dplyr::filter(reserve_frac <= max_res_frac, f_fmsy <= max_f_fmsy) %>%
    dplyr::mutate(f_fmsy = if_else(f_fmsy == 0, "0.0000000", 
                            as.character(f_fmsy)),
           reserve_frac = as.character(reserve_frac)) %>%
    dplyr::mutate(reserve_frac = as.numeric(reserve_frac),
           f_fmsy = as.numeric(f_fmsy)) %>%
    dplyr::mutate(end_x = reserve_frac + (dx * 0.5), 
           end_y = f_fmsy + (dy * 0.5)) 
  
  base_plt_gradvec_cont <- plt_dat_preped %>%
    ggplot(data = .,
           aes(x = reserve_frac, y = f_fmsy, z = frac_below_med_yield_f_max)) +
    geom_contour_filled(bins = 12) +
    ggtitle(paste0(species, " ", dispersal," ", noise, " - time below median max yield")) +
    xlab("Reserve fraction") +
    ylab("F/Fmsy") +
    # guides(fill="none") + 
    scale_fill_viridis_d(option = "D", direction = -1) +
    geom_segment(aes(x = reserve_frac,
                     y = f_fmsy,
                     xend = end_x, 
                     yend = end_y),
                 arrow = arrow(length = unit(0.1,"cm"))) +
    scale_x_continuous(expand = expansion(add = 0), limits = c(0, 0.5)) +
    scale_y_continuous(expand = expansion(add = 0), limits = c(0, 2)) +
    theme_bw()
  
  return(base_plt_gradvec_cont)
}

plt_sdmn_yield_gradvec_cont2 <- function(dat,
                                         species, 
                                         dispersal, 
                                         noise,
                                         scale = 0.5) {
  
  x_labs <- sort(unique(dat$reserve_frac))
  y_labs <- sort(unique(dat$f_fmsy))
  
  dat2 <- dat[, c("reserve_frac", "f_fmsy", "frac_below_med_yield_f_max")]
  names(dat2) <- c("x", "y", "value")
  
  dat_mat <- as.matrix(tidyr::pivot_wider(dat2, names_from = y, values_from = value))
  
  plt_dat <- as.cimg(dat_mat[,-1])
  plt_dat_gr <- imgradient(plt_dat, "xy")
  names(plt_dat_gr) <- c("dx", "dy")
  
  plt_dat_gr_df <- as.data.frame(plt_dat_gr)
  plt_dat_gr_df_w <- tidyr::spread(data = plt_dat_gr_df, im, value)
  plt_dat_gr_df_w_plt <- plt_dat_gr_df_w %>%
    mutate(., xend = x-(dx/sd(dx) * scale),
           yend = y-(dy/sd(dy) * scale))
  
  plt_dat_gr_df_w_plt <- bind_cols(dat[, c("reserve_frac", "f_fmsy", "frac_below_med_yield_f_max")],
                                   plt_dat_gr_df_w_plt)

  p <- ggplot(plt_dat_gr_df_w_plt %>% dplyr::filter(., x %% 2 == 0, y %% 2 == 0), aes(x = x, y = y)) +
    geom_raster(data = as.data.frame(plt_dat), aes(x=x, y=y, fill=value),
                alpha = 0.85,) +
    scale_fill_viridis_c(option = "D", direction = -1) +
    geom_segment(aes(xend = xend, yend = yend),
                 arrow = arrow(length = unit(0.01, "npc")),
                 col="black") +
    scale_x_continuous(expand = c(0,-0.5), limits = c(0, 12),
                       breaks = sort(unique(plt_dat_gr_df_w_plt$x)),
                       labels = x_labs) +
    scale_y_continuous(expand = c(0,-0.5), limits = c(0, 30),
                       breaks = sort(unique(plt_dat_gr_df_w_plt$y)),
                       labels = round(y_labs, 2)) + 
    geom_hline(yintercept = 1, colour = "red") +
                       # breaks = sort(unique(plt_dat_gr_df_w_plt$y)), labels = round(y_labs, 2)) +
    ggtitle(paste0(species, " ", dispersal," ", noise, " - time below median max yield (fraction of years)")) +
    xlab("Reserve fraction") +
    ylab("F/Fmsy") +
    theme_bw()

  return(print(p))
}

## Yield diffs: lp - nd
plt_sdmn_yield_lp_diff_nd_gradvec_cont2 <- function(dat,
                                         species, 
                                         dispersal, 
                                         noise,
                                         scale = 0.5) {
  
  x_labs <- sort(unique(dat$reserve_frac))
  y_labs <- sort(unique(dat$f_fmsy))
  
  dat2 <- dat[, c("reserve_frac", "f_fmsy", "frac_below_med_yield_diffs")]
  names(dat2) <- c("x", "y", "value")
  
  dat_mat <- as.matrix(tidyr::pivot_wider(dat2, names_from = y, values_from = value))
  
  plt_dat <- as.cimg(dat_mat[,-1])
  plt_dat_gr <- imgradient(plt_dat, "xy")
  names(plt_dat_gr) <- c("dx", "dy")
  
  plt_dat_gr_df <- as.data.frame(plt_dat_gr)
  plt_dat_gr_df_w <- tidyr::spread(data = plt_dat_gr_df, im, value)
  plt_dat_gr_df_w_plt <- plt_dat_gr_df_w %>%
    mutate(., xend = x-(dx/sd(dx) * scale),
           yend = y-(dy/sd(dy) * scale))
  
  plt_dat_gr_df_w_plt <- bind_cols(dat[, c("reserve_frac", "f_fmsy", "frac_below_med_yield_diffs")],
                                   plt_dat_gr_df_w_plt)
  
  p <- ggplot(plt_dat_gr_df_w_plt %>% dplyr::filter(., x %% 2 == 0, y %% 2 == 0), aes(x = x, y = y)) + # %>% dplyr::filter(., x %% 2 == 0, y %% 2 == 0)
    geom_raster(data = as.data.frame(plt_dat), aes(x=x, y=y, fill=value),
                alpha = 0.85,) +
    scale_fill_gradient2(low = "#440154", mid = "#21918c", high = "#fde725", midpoint = 0,
                         guide = guide_colourbar(title = "")) +
    geom_segment(aes(xend = xend, yend = yend),
                 arrow = arrow(length = unit(0.01, "npc")),
                 col="black") +
    scale_x_continuous(expand = c(0,-0.5), limits = c(0, 12),
                       breaks = sort(unique(plt_dat_gr_df_w_plt$x)),
                       labels = x_labs) +
    scale_y_continuous(expand = c(0,-0.5), limits = c(0, 30),
                       breaks = sort(unique(plt_dat_gr_df_w_plt$y)),
                       labels = round(y_labs, 2)) + 
    geom_hline(yintercept = 1, colour = "red") +
    # breaks = sort(unique(plt_dat_gr_df_w_plt$y)), labels = round(y_labs, 2)) +
    ggtitle(paste0(species, " difference in time below median max yield (fraction of years) \n", noise, " noise between larval pool and no dispersal")) +
    xlab("Reserve fraction") +
    ylab("F/Fmsy") +
    theme_bw()
  
  return(print(p))
}

## RECRUITS


calc_med_recruits <- function(sdn_recs, 
                              fleps_2_fs) {
  sdn_recs_median <- sdn_recs %>%
    dplyr::mutate(flep_ratio = as.character(flep_ratio)) %>%
    dplyr::left_join(
      x = .,
      y = fleps_2_fs[, c("flep_vals_ch", "f_vals")],
      by = c("flep_ratio" = "flep_vals_ch")
    )  %>%
    dplyr::group_by(year, reserve_frac, f_vals) %>%
    dplyr::summarize(ann_recruits = sum(number, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(reserve_frac, f_vals, year) %>%
    dplyr::mutate(reserve_frac = as.character(reserve_frac),
           f_vals = as.character(f_vals)) %>%
    dplyr::filter(reserve_frac == 0 & f_vals == 0) %>%
    dplyr::group_by(reserve_frac, f_vals) %>%
    dplyr::summarize(average_recruits = mean(ann_recruits),
              max_recruits = max(ann_recruits),
              q90_recruits = quantile(ann_recruits, 0.9),
              q75_recruits = quantile(ann_recruits, 0.75),
              q60_recruits = quantile(ann_recruits, 0.6),
              q50_recruits = quantile(ann_recruits, 0.5)) %>%
    dplyr::ungroup() %>%
    .$q50_recruits
  return(sdn_recs_median)
}

# = brf_lp_recs_white
frac_years_below_med_recs <- function(sdn_recs, 
                                      fleps_2_fs,
                                      F_max_yield_reg_sd,
                                      rec_threshold,
                                      sim_len) {

  sdmn_frac_recs_below_threshold <- sdn_recs %>%
    dplyr::mutate(flep_ratio = as.character(flep_ratio)) %>%
    dplyr::left_join(
      x = .,
      y = fleps_2_fs[, c("flep_vals_ch", "f_vals")],
      by = c("flep_ratio" = "flep_vals_ch")
    )  %>%
    dplyr::mutate(f_fmsy = f_vals / F_max_yield_reg_sd) %>%
    dplyr::group_by(year, reserve_frac, f_fmsy) %>%
    dplyr::summarize(ann_recruits = sum(number, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(reserve_frac, f_fmsy, year) %>%
    dplyr::mutate(below_threshold = ann_recruits <= rec_threshold) %>%
    dplyr::group_by(reserve_frac, f_fmsy) %>%
    dplyr::summarize(num_below_threshold = sum(below_threshold), 
                     frac_below_threshold = num_below_threshold/sim_len)
}

sdmn_calc_gradient_vector_recs <- function(sdmn_frac_recs, rec_threshold) {
  
  # sdmn_frac_recs_below_threshold <- sdmn_frac_recs %>%
  #   dplyr::mutate(flep_ratio = as.character(flep_ratio)) %>%
  #   dplyr::left_join(
  #     x = .,
  #     y = fleps_2_fs[, c("flep_vals_ch", "f_vals")],
  #     by = c("flep_ratio" = "flep_vals_ch")
  #   )  %>%
  #   dplyr::mutate(f_fmsy = f_vals / F_max_yield_reg_sd) %>%
  #   dplyr::group_by(year, reserve_frac, f_fmsy) %>%
  #   dplyr::summarize(ann_recruits = sum(number, na.rm = TRUE)) %>%
  #   dplyr::ungroup() %>%
  #   dplyr::arrange(reserve_frac, f_fmsy, year) %>%
  #   dplyr::mutate(below_threshold = ann_recruits <= rec_threshold) %>%
  #   dplyr::group_by(reserve_frac, f_fmsy) %>%
  #   dplyr::summarize(num_below_threshold = sum(below_threshold), 
  #             frac_below_threshold = num_below_threshold/sim_len)
  
  sdmn_frac_recs_below_threshold_w <- sdmn_frac_recs %>%
    tidyr::pivot_wider(id_cols = reserve_frac,
                names_from = f_fmsy,
                names_prefix = "f_flep_",
                values_from = frac_below_threshold)
  
  sdmn_frac_recs_below_threshold_mat <- as.matrix(sdmn_frac_recs_below_threshold_w[,2:ncol(sdmn_frac_recs_below_threshold_w)])
  
  rr <- row(sdmn_frac_recs_below_threshold_mat)
  cc <- col(sdmn_frac_recs_below_threshold_mat)
  dx <- t(apply(sdmn_frac_recs_below_threshold_mat, 1, bdiff))
  dy <- apply(sdmn_frac_recs_below_threshold_mat, 2, bdiff)
  
  
  ## wrangle to a single long-df for ggplot2
  
  f_vals <- unique(sdmn_frac_recs$f_fmsy)
  res_vals <- unique(sdmn_frac_recs$reserve_frac)
  
  dx_tib <- tibble(data.frame(dx))
  dx_tib$reserve_frac <- as.character(res_vals)
  
  names(dx_tib)[1] <- "f_flep_0.0000000"
  dx_tib <- pivot_longer(dx_tib, cols = c(starts_with("f_flep_")), 
                         names_to = "f_fmsy",  values_to = "dx") %>%
    mutate(f_fmsy = substr(as.character(substring(f_fmsy, first = 8)), 1, 10))
  
  
  dy_tib <- tibble(data.frame(dy))
  dy_tib$reserve_frac <- as.character(res_vals)
  dy_tib <- dy_tib %>%
    tidyr::pivot_longer(., cols = c(starts_with("f_flep_")), 
                 names_to = "f_fmsy",  values_to = "dy") %>%
    dplyr::mutate(f_fmsy = substr(as.character(substring(f_fmsy, first = 8)), 1, 10))
  
  sdmn_frac_recs_below_threshold_out <- sdmn_frac_recs %>%
    dplyr::mutate(reserve_frac = as.character(reserve_frac), 
           f_fmsy = as.character(f_fmsy),
           f_fmsy = substr(f_fmsy, 1, 10)) %>%
    dplyr::left_join(., dx_tib, by = c("reserve_frac", "f_fmsy")) %>%
    dplyr::left_join(., dy_tib, by = c("reserve_frac", "f_fmsy"))
  
  return(sdmn_frac_recs_below_threshold_out)
}


plt_sdmn_frac_recs_below_threshold_out <- function(plt_dat, species,
                                                   dispersal, noise,
                                                   max_res_frac = 0.5, 
                                                   max_f_fmsy = 2) {
  plt_dat %>%
    dplyr::mutate(reserve_frac = as.numeric(reserve_frac),
           f_fmsy = as.numeric(f_fmsy)) %>%
    dplyr::filter(reserve_frac <= max_res_frac, f_fmsy <= max_f_fmsy) %>%
    dplyr::mutate(f_fmsy = if_else(f_fmsy == 0, "0.0000000", 
                            as.character(f_fmsy)),
           reserve_frac = as.character(reserve_frac)) %>%
    dplyr::mutate(reserve_frac = as.numeric(reserve_frac),
           f_fmsy = as.numeric(f_fmsy)) %>%
    dplyr::mutate(end_x = reserve_frac + (dx * 0.9), 
           end_y = f_fmsy + (dy * 0.9)) %>%
    ggplot(data = .,
           aes(x = reserve_frac, y = f_fmsy, z = frac_below_threshold)) +
    geom_contour_filled(bins = 12) +
    ggtitle(paste0(species, " ", dispersal," ", noise, " - time below median recruits")) +
    xlab("Reserve fraction") +
    ylab("F/Fmsy") +
    # guides(fill="none") + 
    scale_fill_viridis_d(option = "D", direction = -1) +
    geom_segment(aes(x = reserve_frac,
                     y = f_fmsy,
                     xend = end_x, 
                     yend = end_y),
                 arrow = arrow(length = unit(0.1,"cm"))) +
    geom_segment(aes(x=0,xend=0.5,y=1,yend=1), colour = "red") +
    scale_x_continuous(expand = expansion(add = 0), limits = c(0, 0.5)) +
    scale_y_continuous(expand = expansion(add = 0), limits = c(0, 2)) +
    theme_bw()
  
}

plt_sdmn_frac_recs_below_threshold_out2 <- function(dat,
                                                    species, 
                                                    dispersal, 
                                                    noise,
                                                    scale = 0.5) {
  
  x_labs <- sort(unique(dat$reserve_frac))
  y_labs <- sort(unique(dat$f_fmsy))
  
  dat2 <- dat[, c("reserve_frac", "f_fmsy", "frac_below_threshold")]
  names(dat2) <- c("x", "y", "value")
  
  dat_mat <- as.matrix(tidyr::pivot_wider(dat2, names_from = y, values_from = value))
  
  plt_dat <- as.cimg(dat_mat[,-c(1)])
  plt_dat_gr <- imgradient(plt_dat, "xy")
  names(plt_dat_gr) <- c("dx", "dy")
  
  plt_dat_gr_df <- as.data.frame(plt_dat_gr)
  plt_dat_gr_df_w <- tidyr::spread(data = plt_dat_gr_df, im, value)
  plt_dat_gr_df_w_plt <- plt_dat_gr_df_w %>%
    mutate(., xend = (x-dx/sd(dx) * scale),
           yend = y-(dy/sd(dy) * scale))
  
  plt_dat_gr_df_w_plt <- bind_cols(dat[, c("reserve_frac", "f_fmsy", "frac_below_threshold")],
                                   plt_dat_gr_df_w_plt)
  
  p <- ggplot(plt_dat_gr_df_w_plt %>% dplyr::filter((x %% 2) ==0,(y %% 2) == 0), aes(x = x, y = y)) +
    geom_raster(data = as.data.frame(plt_dat), aes(x=x, y=y, fill=value),
                alpha = 0.85) +
    scale_fill_viridis_c(option = "D", direction = -1, limits = c(0,1)) +
    geom_segment(aes(xend = xend, yend = yend),
                 arrow = arrow(length = unit(0.01, "npc")),
                 col = "black") +
    scale_x_continuous(expand = c(0,-0.5), limits = c(0, 12),
                       breaks = sort(unique(plt_dat_gr_df_w_plt$x)),
                       labels = x_labs) +
    scale_y_continuous(expand = c(0,-0.5), limits = c(3, 30),
                       breaks = sort(unique(plt_dat_gr_df_w_plt$y)),
                       labels = round(y_labs, 2)
                       # breaks = sort(unique(plt_dat_gr_df_w_plt$y)), labels = round(y_labs, 2)
                       ) +
    geom_hline(yintercept = 1, colour = "red") +
    ggtitle(paste0(species, " ", dispersal," ", noise, " - time below recruit threshold (fraction of years)")) +
    xlab("Reserve fraction") +
    ylab("F/Fmsy") +
    theme_bw()
  return(print(p))
}

## Recs diffs: lp - nd
plt_sdmn_recs_lp_diff_nd_gradvec_cont2 <- function(dat,
                                                    species, 
                                                    noise,
                                                    scale = 0.5) {
  
  x_labs <- sort(unique(dat$reserve_frac))
  y_labs <- sort(unique(dat$f_fmsy))
  
  dat2 <- dat[, c("reserve_frac", "f_fmsy", "frac_below_threshold_rec_diffs")]
  names(dat2) <- c("x", "y", "value")
  
  dat_mat <- as.matrix(tidyr::pivot_wider(dat2, names_from = y, values_from = value))
  
  plt_dat <- as.cimg(dat_mat[,-1])
  plt_dat_gr <- imgradient(plt_dat, "xy")
  names(plt_dat_gr) <- c("dx", "dy")
  
  plt_dat_gr_df <- as.data.frame(plt_dat_gr)
  plt_dat_gr_df_w <- tidyr::spread(data = plt_dat_gr_df, im, value)
  plt_dat_gr_df_w_plt <- plt_dat_gr_df_w %>%
    mutate(., xend = x-(dx/sd(dx) * scale),
           yend = y-(dy/sd(dy) * scale))
  
  plt_dat_gr_df_w_plt <- bind_cols(dat[, c("reserve_frac", "f_fmsy", "frac_below_threshold_rec_diffs")],
                                   plt_dat_gr_df_w_plt)
  
  p <- ggplot(plt_dat_gr_df_w_plt, aes(x = x, y = y)) + # %>% dplyr::filter(., x %% 2 == 0, y %% 2 == 0)
    geom_raster(data = as.data.frame(plt_dat), aes(x=x, y=y, fill=value),
                alpha = 0.85,) +
    scale_fill_gradient2(low = "#440154", mid = "#21918c", high = "#fde725", midpoint = 0,
                         guide = guide_colourbar(title = "")) +
    # scale_fill_gradient2(low = "#fde725", mid = "#21918c", high = "#440154", midpoint = 0,
    #                      guide = guide_colourbar(title = "")) +
    geom_segment(aes(xend = xend, yend = yend),
                 arrow = arrow(length = unit(0.01, "npc")),
                 col="black") +
    scale_x_continuous(expand = c(0,-0.5), limits = c(0, 12),
                       breaks = sort(unique(plt_dat_gr_df_w_plt$x)),
                       labels = x_labs) +
    scale_y_continuous(expand = c(0,-0.5), limits = c(0, 30),
                       breaks = sort(unique(plt_dat_gr_df_w_plt$y)),
                       labels = round(y_labs, 2)) + 
    geom_hline(yintercept = 1, colour = "red") +
    # breaks = sort(unique(plt_dat_gr_df_w_plt$y)), labels = round(y_labs, 2)) +
    ggtitle(paste0(species, " difference in time below median recruitment leve (fraction of years) \n", noise, " noise between larval pool and no dispersal")) +
    xlab("Reserve fraction") +
    ylab("F/Fmsy") +
    theme_bw()
  
  return(print(p))
}
