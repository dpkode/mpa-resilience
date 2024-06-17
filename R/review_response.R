# Supp figs 4a 4b: sensitivity of biomass and yield metrics to choice of BH alpha
# The effect of steepness is to shift the range of values
# of F and proportion in reserves where there is buffering. Steeper
# SR curves make the whole system less variable, so F has to be much higher
# for reserves to have a buffering benefit, and vice versa. 

start_time <- Sys.time()

source(file.path("R", "functions.R"))
options(tidyverse.quiet = TRUE)
library(renv)
library(DBI)
library(tidyverse)
library(data.table)
library(duckdb)
library(cowplot)

# set constants
exp_names_lp <-  "larval_pool"

species_parms_csv <- file.path("data", "species_parameters.csv")

species_parms <- load_species_parms(fp = species_parms_csv)

species_names <- "Blue rockfish"

# Load species life history params to use in simulations
sim_species_parms <- list_species_parms(parm_df = species_parms, sp_names = species_names)
ages <- species_names %>%
  purrr::set_names() %>%
  purrr::map(., ~ 1:sim_species_parms[[.x]]$A_max)

# Fishing mortality - many so that Fs for given FLEPs can be mapped
fishing_mortality_values <- seq(0, 2, by = 0.0025)

# population dynamics and simulation parameters
# 2-patch model, one fished, one patch unfished
num_patches <- 2 
# Simulation length burn_in and actual sim_len:
burn_in <- 500
sim_len_135 <- 135
n_years_135 <- burn_in + sim_len_135
sim_yrs_135 <- 1:n_years_135

# Run how many replicate simulations? FLEP values and fractions of coast in reserves
# Fraction of lifetime egg production (FLEP)
# Lower FLEPs correspond to populations that have experienced higher exploitation
# 0.3 is close to population collapse, 0.4 is close enough to get cohort resonance

# use "test" settings to check that simulation pipeline runs, before running "prod" model

run_type <- "prod" # "prod" or "test"

if (run_type == "test") {
  sim_nums <- 2
  flep_ratios <- c(0.4, 0.6, 0.8)
  reserve_fracs <- c(0.1, 0.25, 0.4) # seq(0, 0.5, by = 0.1)
  # Critical replacement threshold (CRT): EPR (FLEP) can't go below this proportion of the unfished value
  # Note that the eq. recruitment is the point where line with 1/FLEP intersects SR curve
  crt_vals <- 0.3 
} else if (run_type == "prod") {
  sim_nums <- 10
  flep_ratios <- c(seq(0.2, 0.975, by = 0.025), 0.998)
  reserve_fracs <- seq(0, 0.5, by = 0.05)
  crt_vals <- c(0.15, 0.3, 0.45)
} else {
  stop("error: run_type must be 'test' or 'prod'")
}

# Maximum density of recruits. Serves as a scalar carrying capacity parameter
bev_holt_beta <- 1E6
# Scale the variabilty of recruitment/early life history
sd_recruitment <- 0.75

# Make derived parms for selected species
# 'Derived' because they are calculated from the species parameters data
sim_species_derived_vars <- list_derived_variables(spec_parms = sim_species_parms,
                                                   sp_names = species_names,
                                                   fish_mort = fishing_mortality_values)

lifetime_eggs_production <- list_lep(spec_names = species_names, sim_spec_der_var = sim_species_derived_vars)

relative_lifetime_eggs_production <- species_names %>%
  purrr::set_names() %>%
  purrr::map(., ~ sim_species_derived_vars[[.x]][["EPRs"]] / lifetime_eggs_production[[.x]])

## get the index for the F yielding target flep
species_flep_fished_ind <- get_index_for_fleps(flep_vals = flep_ratios,
                                               sp_names = species_names,
                                               relative_flep = relative_lifetime_eggs_production)

# Beverton-Holt stock recruitment slope parameters: this specifies that pop will persist if EPR > CRT

bev_holt_alphas <- vector("list", length = length(species_names))
names(bev_holt_alphas) <- species_names

for (i in species_names) {
  bev_holt_alphas[[i]] <- vector("numeric", length = length(crt_vals))
  for (j in 1:length(crt_vals)) {
    bev_holt_alphas[[i]][j] <- 1 / (crt_vals[j] * lifetime_eggs_production[[i]])
  }
}

# Equilibrium recruitment unfished and fished (F to get to X% FLEPs)

equilibrium_recruits <- vector("list", length = length(species_names))
names(equilibrium_recruits) <- species_names

for (i in species_names) {
  for (j in 1:length(crt_vals)) {
    equilibrium_recruits[[i]][j] <- lifetime_eggs_production[[i]] * (
      lifetime_eggs_production[[i]] * bev_holt_alphas[[i]][j]  / (
        1 + (bev_holt_alphas[[i]][j] / bev_holt_beta) * lifetime_eggs_production[[i]]
      )
    )
  }
}

# set the fished levels of equilibrium recruits

fished_equilibrium_recruits <- vector("list", length = length(species_names))
names(fished_equilibrium_recruits) <- species_names

for (i in 1:length(species_names)) {
  fished_equilibrium_recruits[[i]] <-
    vector("list", length = length(flep_ratios))
  names(fished_equilibrium_recruits[[i]]) <- flep_ratios
  for (j in 1:length(flep_ratios)) {
    fished_equilibrium_recruits[[i]][[j]] <- vector("list", length = length(crt_vals))
    names(fished_equilibrium_recruits[[i]][[j]]) <- crt_vals
  }
}

for (i in 1:length(species_names)) {
  for (j in 1:length(flep_ratios)) {
    for (k in 1:length(crt_vals)) {
      fished_equlibrium_eggs  <-
        sim_species_derived_vars[[i]][["EPRs"]][species_flep_fished_ind[[i]][[j]]]
      fished_equilibrium_recruits[[i]][[j]][[k]] <-
        fished_equlibrium_eggs * (fished_equlibrium_eggs * bev_holt_alphas[[i]][[k]] / (
          1 + (bev_holt_alphas[[i]][[k]] / bev_holt_beta) * fished_equlibrium_eggs
        ))
    }
  }
}

## make_leslie_matrix_sp_flep
## Need a Leslie matrix for each species and flep

leslie_matrices <- vector("list", length = length(species_names))
names(leslie_matrices) <- species_names

for (i in 1:length(species_names)) {
  leslie_matrices[[i]] <- vector("list", length = length(flep_ratios))
  names(leslie_matrices[[i]]) <- flep_ratios
  
  age_max <- sim_species_parms[[i]][["A_max"]]
  fecundity_at_age = sim_species_derived_vars[[i]][["fecundity_at_age"]]
  
  for (j in 1:length(flep_ratios)) {
    ## make_leslie_mat
    leslie_matrices[[i]][[j]] <-
      array(0, dim = c(age_max, age_max, num_patches)) # create a 3D array to contain Leslie matrices for each "patch"
    
    # Leslie matrix reserve (unfished) patch
    leslie_matrices[[i]][[j]][2:age_max, 1:(age_max - 1), 1] <-
      diag(sim_species_derived_vars[[i]][["surv_unfished"]])
    leslie_matrices[[i]][[j]][1, , 1] <- fecundity_at_age # add in fecundities on top row
    # Leslie matrix fished patch
    leslie_matrices[[i]][[j]][2:age_max, 1:(age_max - 1), 2] <-
      diag(sim_species_derived_vars[[i]][["surv_fished"]][, species_flep_fished_ind[[i]][[j]]])
    leslie_matrices[[i]][[j]][1, , 2] <- fecundity_at_age # add in fecundities on top row
  }
}

# initialize nums at age for unfished and fished populations using SAD and FLEP values
# unfished

unfished_N1 <- vector("list", length = length(species_names))
names(unfished_N1) <- species_names

for (i in species_names) {
  unfished_N1[[i]] <- vector("list", length = length(flep_ratios))
  names(unfished_N1[[i]]) <- flep_ratios
  for (j in 1:length(flep_ratios)) {
    unfished_N1[[i]][[j]] <- equilibrium_recruits[[i]][j] * sim_species_derived_vars[[i]]$SADs[, 1]
  }
}

## SADs for fished populations
fished_N1 <- vector("list", length = length(species_names))
names(fished_N1) <- species_names

for (i in species_names) {
  fished_N1[[i]] <- vector("list", length = length(flep_ratios))
  names(fished_N1[[i]]) <- flep_ratios
  for (j in 1:length(flep_ratios)) {
    fished_N1[[i]][[j]] <- vector("list", length = length(crt_vals))
    names(fished_N1[[i]][[j]]) <- crt_vals
    for (k in 1:length(crt_vals)) {
      fished_N1[[i]][[j]][[k]] <-  fished_equilibrium_recruits[[i]][[j]][[k]] * sim_species_derived_vars[[i]]$SADs[, species_flep_fished_ind[[i]][[j]]]
    }
  }
}

## Check on the EQ fished age structures (uncomment)
# plot(fished_N1[[1]][[2]][[1]], type = 'b', col="black")
# lines(fished_N1[[1]][[2]][[2]], type = 'b', col="orange")
# lines(fished_N1[[1]][[2]][[3]], type = 'b', col="blue")

# store simulation results (Numbers (n), Yield (biomass), Eggs (n), Recs (n))
# stored in a named list
sim_arrays <- species_names %>%
  purrr::set_names() %>%
  purrr::map(., ~ vector("list", length = length(.x)))

## set_sim_results_arrays and set initial (t = 1) nums at age for different F(leps)
for (i in 1:length(species_names)) {
  sim_arrays[[i]] <- flep_ratios %>%
    purrr::set_names() %>%
    purrr::map(., ~ vector("list", length = length(.x)))
  for (j in 1:length(flep_ratios)) {
    sim_arrays[[i]][[j]] <- vector("list", length = length(crt_vals))
    names(sim_arrays[[i]][[j]]) <- crt_vals
    for (k in 1:length(crt_vals)) {
      sim_arrays[[i]][[j]][[k]] <- array(0, dim = c(
        length(sim_species_derived_vars[[i]][["ages"]]),
        n_years_135,
        num_patches
      ))
      sim_arrays[[i]][[j]][[k]][, 1, ] <- fished_N1[[i]][[j]][[k]]
    }
  }
}


#### Connectivity matrices

no_disp_conn_mat <- purrr::map(1:length(reserve_fracs), ~ matrix(c(1, 0, 0, 1), nrow = 2, byrow = FALSE))

no_disp_in_out_frac <- reserve_fracs %>%
  purrr::map(., ~ matrix(c(.x, (1 - .x)), nrow = 2))

larvpool_disp_conn_mat <- purrr::map(reserve_fracs,
                                     ~ matrix(
                                       c(.x, .x, (1 - .x), (1 - .x)),
                                       nrow = num_patches,
                                       ncol = num_patches,
                                       byrow = TRUE
                                     ))

larvpool_in_out_frac <- larvpool_disp_conn_mat %>%
  purrr::map(., ~ matrix(.x[, 1], nrow = 2))

## Environmental noise time series

noises_enso_135 <- "mei_135"

enso_dat_135 <- make_ann_mei_ext()

enso_noise_135 <- matrix(NA_real_, ncol = sim_nums, nrow = n_years_135)

for (i in 1:sim_nums) {
  enso_noise_135[, i] <- make_sim_ann_mei_ext(mei = enso_dat_135$mei,
                                              sim_enso_len = n_years_135,
                                              n_enso_sims = 100) * sd_recruitment
}

## Simulation code

## Scenarios evaluated: 3 different slopes at the origin (alpha) of the BH stock recruit function

## Population simulation function:
run_patch_sims_rev <- function(t_steps,
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
  Y[, 1, ] <- 0
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
        N_harvest * weight_at_age[-1] / 1E6 # grams to MT
      E[n, t] <- N[1, t, n] # pull out the eggs produced
    }
    # do larval pool dispersal
    E[, t] <-  con_mat %*% E[, t]
    # Apply density dependence
    for (n in 1:num_patches) {
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
  out <- list(Ns = N, Y = Y)
  return(out)
}

## Population simulation scenarios function:

run_sims_sm_write_rev <- function(expmts,
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
                                  sim_spec_pars,
                                  ddb_name = "data/rev_response_sim_out.duckdb") {
  con <-
    dbConnect(duckdb::duckdb(), dbdir = ddb_name, read_only = FALSE)
  
  for (j in 1:length(spec_name)) {
    for (m in 1:num_sims) {
      print(paste0("Current simulation run:", m))
      for (n in 1:length(frac_reserves)) {
        print(paste0("Frac reserve: ", n, " out of ", length(frac_reserves), " coastline fractions"))
        for (o in 1:length(fleps)) {
          for (p in 1:length(crt_vals)) {
            dt_one <- data.table(
              sim_num = m,
              reserve_frac = as.character(frac_reserves[n]),
              flep_ratio = as.character(fleps[o]),
              alpha = as.character(crt_vals[p]),
              number = list(),
              yield = list()
            )

            out <- run_patch_sims_rev(
              t_steps = n_years,
              num_patches = num_patches,
              les_mat = les_mats[[j]][[o]],
              con_mat = conn_mats[[n]],
              frac_in_out = in_out_fracs[[n]],
              N = sim_results[[j]][[o]][[p]],
              selectivity = as.integer(sim_spec_dervars[[j]][["F_select"]]),
              fishing_mortality = c(0, f_vals[spec_flep_f_ind[[j]][[o]]]),
              natural_mortality = sim_spec_pars[[j]][["M"]],
              weight_at_age = sim_spec_pars[[j]][["biom_const"]] * sim_spec_dervars[[j]][["length_at_age"]] ^
                sim_spec_pars[[j]][["biom_exp"]],
              alpha = alpha_bh[[j]][p],
              beta = beta_bh,
              noise_series = noise_dat[, m]
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
            
            dt_one <-
              dt_one[, c(
                "sim_num",
                "reserve_frac",
                "flep_ratio",
                "alpha",
                "year",
                "age",
                "patch_num",
                "number",
                "yield"
              )]
            if (o == 1 & p == 1) { ####
            # if (n == 1 & o == 1 & p == 1) { ####
              dt_ddb <- dt_one
              rm(dt_one)
            } else {
              dt_ddb <- rbind(dt_ddb, dt_one)
              rm(dt_one)
            }
          } # p
        } # o
        tab_name <- paste0(expmts, ####
                           "_",
                           tolower(sub(" ", "_", spec_name[j])),
                           "_",
                           noise_vec,
                           "_",
                           substring(as.character(recr_sd), 3))
        duckdb::dbWriteTable(con, tab_name, dt_ddb, append = TRUE) 
        rm(dt_ddb)
      } # n
    } # m
  } # j
  DBI::dbDisconnect(con, shutdown = TRUE)
  return(dt)
}

## name duckdb database file to store simulaiton output
duckdb_run_name <- paste0("data/rev_response_sim_out_db_", run_type, ".duckdb")

## delete the database file, if it exists, so that only current run is kept.
if (file.exists(duckdb_run_name)) {
  rm(duckdb_run_name)
}

## Run BH alpha sensitivity scenarios

run_sims_sm_write_rev(
  expmts = exp_names_lp,
  spec_name = species_names,
  alpha_bh = bev_holt_alphas,
  beta_bh = bev_holt_beta,
  recr_sd = sd_recruitment,
  fleps = flep_ratios,
  frac_reserves = reserve_fracs,
  noise_vec = noises_enso_135,
  noise_dat = enso_noise_135,
  n_years = n_years_135,
  num_sims = sim_nums,
  num_patches = num_patches,
  les_mats = leslie_matrices,
  conn_mats = larvpool_disp_conn_mat,
  in_out_fracs = larvpool_in_out_frac,
  sim_results = sim_arrays,
  sim_spec_dervars = sim_species_derived_vars,
  f_vals = fishing_mortality_values,
  spec_flep_f_ind = species_flep_fished_ind,
  sim_spec_pars = sim_species_parms,
  ddb_name = duckdb_run_name
)

## Connect to the database file to verify that it exists and then to run 
## preliminary checks/plots

con <- DBI::dbConnect(duckdb::duckdb(), dbdir = duckdb_run_name, read_only = TRUE)

DBI::dbListTables(con)

DBI::dbGetQuery(conn = con, "SELECT alpha, flep_ratio, reserve_frac, sum(number) 
               nums, sum(yield) ylds from 'larval_pool_blue_rockfish_mei_135_75' 
               group by alpha, flep_ratio, reserve_frac order by alpha, flep_ratio, reserve_frac")

tmp <- DBI::dbGetQuery(conn = con, "SELECT flep_ratio, reserve_frac, year, sum(number) number, sum(yield) yield from 'larval_pool_blue_rockfish_mei_135_75' where flep_ratio = '0.4' and reserve_frac in ('0.1', '0.4') group by flep_ratio, reserve_frac, year order by flep_ratio, reserve_frac, year")
ggplot(tmp, aes(x = year, y = number)) + facet_grid(flep_ratio ~ reserve_frac) + geom_point()



## Set query to pull data
table_name = "larval_pool_blue_rockfish_mei_135_75"
qry <- paste0(
  "SELECT  sim_num, reserve_frac, flep_ratio, alpha, year, age,
              sum(number) as abundance,
              sum(yield) as yield,
              FROM '",
  table_name,
  "' WHERE year > 500
              GROUP BY sim_num, reserve_frac, flep_ratio, alpha, year, age
              ORDER BY sim_num, reserve_frac, flep_ratio, alpha, year, age
              "
)

## Pull data
sim_res_out <- dbGetQuery(conn = con, qry)

## Close DB connection
DBI::dbDisconnect(conn = con)

## check simulation results to show that 
## for a given flep (F) abundance increases with increasing reserves
## and yield has the complex relationship between F and reserve fraction

test_df <- sim_res_out %>%
  group_by(alpha, reserve_frac, flep_ratio, year) %>%
  summarise(ann_abundance = sum(abundance),
            ann_yield = sum(yield)) %>%
  ungroup() %>%
  group_by(alpha, reserve_frac, flep_ratio) %>%
  summarise(
    mean_ann_abundance = mean(ann_abundance),
    mean_ann_yield = mean(ann_yield)
  ) %>%
  ungroup()

ggplot(data = test_df,
       aes(x = reserve_frac, y = mean_ann_abundance, color = flep_ratio)) +
  facet_wrap(alpha ~ .) +
  geom_point() +
  geom_line() +
  theme_bw()

ggplot(data = test_df,
       aes(x = reserve_frac, y = mean_ann_yield, color = flep_ratio)) +
  facet_wrap(alpha ~ .) +
  geom_point() +
  geom_line() +
  theme_bw()


## Plots Supp figs 4a and 4b
## plotting params
max_reserve_frac <- 0.3
max_f_fmsy <- 2.5

supp_figs_path <- file.path("output", "supp_figs")

if (!dir.exists(supp_figs_path)) {
  dir.create(supp_figs_path, recursive = TRUE)
}

## Pre plot processing steps

spec_fleps_2_fs_brf <-
  get_fs_from_fleps(species = "Blue rockfish",
                    flep_inds = species_flep_fished_ind,
                    f_vals =  fishing_mortality_values)

F_brf_maxes_yield_brf_lp_mei <- sim_res_out %>%
  dplyr::filter(reserve_frac == "0") %>%
  dplyr::left_join(., y = spec_fleps_2_fs_brf, by = join_by(flep_ratio == flep_vals_ch)) %>%
  dplyr::group_by(f_vals, flep_ratio, alpha, year) %>%
  dplyr::summarise(yield = sum(yield), abundance = sum(abundance)) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(alpha, f_vals, flep_ratio) %>%
  dplyr::summarize(yield = mean(yield, na.rm = TRUE),
                   abundance = mean(abundance, na.rm = TRUE)) %>%
  ungroup() %>%
  dplyr::filter(!is.na(f_vals)) %>%
  dplyr::group_by(alpha) %>%
  dplyr::filter(yield  == max(yield)) %>%
  ungroup() %>%
  dplyr:: select(alpha, f_vals) %>%

## use species specific vB params to create a weight at age data.frame that can
## be used to convert abundance at age to biomass at age

brf_waa_g <- sim_species_parms[["Blue rockfish"]][["biom_const"]] * sim_species_derived_vars[["Blue rockfish"]][["length_at_age"]] ^
  sim_species_parms[["Blue rockfish"]][["biom_exp"]]

brf_waa_g_df <- data.frame(age = 1:sim_species_parms[["Blue rockfish"]]$A_max, waa_g = brf_waa_g)

## Map Fs to FLEPS, set Fs that maxes yields, compute f in terms relative to f_msy.
## and compute Nums at age to biamass at age

out_fs_brf_lp_mei <- sim_res_out %>%
  dplyr::left_join(., y = spec_fleps_2_fs_brf, by = join_by(flep_ratio == flep_vals_ch)) %>%
  dplyr::left_join(., y = F_brf_maxes_yield_brf_lp_mei, by = join_by(alpha == alpha)) %>%
  dplyr::mutate(f_fmsy = round(f_vals / f_vals_max_y, 2)) %>%
  dplyr::left_join(., y = brf_waa_g_df, join_by(age == age)) %>%
  dplyr::mutate(biomass_at_age = (abundance * waa_g) / 1E6)

## Sum annual biomass and yield for each
## simulation run (sim_num), and scenario (BH alpha, reserve_frac, and f_fmsy)

out_yld_bm_sims_f_frac_yr_brf_lp_mei <- out_fs_brf_lp_mei %>%
  dplyr::group_by(sim_num, alpha, reserve_frac, f_fmsy, year) %>%
  dplyr::summarize(annual_yield = sum(yield),
                   annual_biomass = sum(biomass_at_age)) %>%
  dplyr::ungroup()

## Compute med_yield_brf and med_biomass

med_yield_biomass_brf <- out_fs_brf_lp_mei %>%
  dplyr::filter(near(f_fmsy, 1), near(as.numeric(reserve_frac), 0))  %>%
  dplyr::group_by(sim_num, alpha, reserve_frac, f_fmsy,  year) %>%
  dplyr::summarise(ann_yield = sum(yield),
                   ann_biomass = sum(biomass_at_age)) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(sim_num, alpha) %>%
  dplyr::summarise(med_ann_yield = median(ann_yield),
                   med_ann_biomass = median(ann_biomass)) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(alpha) %>%
  dplyr::summarise(
    avg_med_yield_f_fmsy = mean(med_ann_yield),
    avg_med_biomass_f_fmsy = mean(med_ann_biomass)
  )

## Calculate the time below biomass and yield thresholds for each scenario
## and other simulation scenario metrics

sim_f_frac_df <- out_yld_bm_sims_f_frac_yr_brf_lp_mei %>%
  left_join(., med_yield_biomass_brf, by = "alpha") %>%
  dplyr::mutate(
    below_thresh_yld = if_else(annual_yield <= avg_med_yield_f_fmsy, 1, 0),
    below_thresh_bm = if_else(annual_biomass <= avg_med_biomass_f_fmsy, 1, 0),
    rel_annual_yield = annual_yield / avg_med_yield_f_fmsy,
    rel_annual_biomass = annual_biomass / avg_med_biomass_f_fmsy
  ) %>%
  dplyr::group_by(sim_num, alpha, reserve_frac, f_fmsy) %>%
  dplyr::summarise(
    mean_yield = round(mean(annual_yield, na.rm = TRUE), 2),
    mean_biomass = round(mean(annual_biomass, na.rm = TRUE), 2),
    mean_rel_yield = round(mean(rel_annual_yield, na.rm = TRUE), 2),
    mean_rel_biomass = round(mean(rel_annual_biomass, na.rm = TRUE), 2),
    sd_yield = round(sd(annual_yield, na.rm = TRUE), 2),
    sd_biomass = round(sd(annual_biomass, na.rm = TRUE), 2),
    sd_rel_yield =   round(sd(rel_annual_yield, na.rm = TRUE), 2),
    sd_rel_biomass = round(sd(rel_annual_biomass, na.rm = TRUE), 2),
    cv_yield = round(
      sd(annual_yield, na.rm = TRUE) / mean(annual_yield, na.rm = TRUE),
      2
    ),
    cv_biomass = round(
      sd(annual_biomass, na.rm = TRUE) / mean(annual_biomass, na.rm = TRUE),
      2
    ),
    cv_rel_yield = round(
      sd(rel_annual_yield, na.rm = TRUE) / mean(rel_annual_yield, na.rm = TRUE),
      2
    ),
    cv_rel_biomass = round(
      sd(rel_annual_biomass, na.rm = TRUE) / mean(rel_annual_biomass, na.rm = TRUE),
      2
    ),
    yrs_below_yield_thresh = sum(below_thresh_yld) / 135,
    yrs_below_biomass_thresh = sum(below_thresh_bm) / 135
  ) %>%
  dplyr::ungroup()

## Compute helper dataframe for plotting

sum_stats_f_frac_df_brf_lp_mei <- sim_f_frac_df %>%
  dplyr::group_by(alpha, reserve_frac, f_fmsy) %>%
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

## Supp 4a Make Biomass time below threshold plots

p1 <- sum_stats_f_frac_df_brf_lp_mei %>%
  filter(as.numeric(reserve_frac) <= max_reserve_frac,
         f_fmsy <= max_f_fmsy) %>%
  ggplot(., aes(
    x = as.numeric(reserve_frac),
    y = f_fmsy,
    z = yrs_below_biomass_thresh
  )) +
  metR::geom_contour_fill(bins = 12) +
  facet_grid(alpha ~ .) +
  ggtitle(
    paste0(
      "Blue rockfish",
      " ",
      "Larval pool dispersal",
      " ",
      "ENSO",
      ":\nTime below mean median biomass "
    )
  ) +
  xlab("Fraction of coastline in reserves") +
  ylab("Harvest Rate (F/Fmsy)") +
  scale_fill_viridis_c(option = "D", direction = -1) +
  geom_segment(
    aes(
      x = 0,
      xend = max_reserve_frac,
      y = 1,
      yend = 1
    ),
    colour = "black",
    linetype = 2
  ) +
  scale_x_continuous(expand = expansion(mult = 0, add = 0),
                     limits = c(0, max_reserve_frac)) +
  scale_y_continuous(expand = expansion(mult = 0, add = 0)) +
  theme_bw()

p1

## Save biomass plots

ggsave(
  filename = "output/supp_figs/supp_fig_4a-brf-lp-mei-biomass-alpha-sens.png",
  plot = p1,
  height = 10,
  width = 8
)
ggsave(
  filename = "output/supp_figs/supp_fig_4b-brf-lp-mei-biomass-alpha-sens.svg",
  plot = p1,
  height = 10,
  width = 8
)

## Supp 4b Make Yield time below threshold plots

y1 <- sum_stats_f_frac_df_brf_lp_mei %>%
  filter(as.numeric(reserve_frac) <= max_reserve_frac,
         f_fmsy <= max_f_fmsy) %>%
  ggplot(., aes(
    x = as.numeric(reserve_frac),
    y = f_fmsy,
    z = yrs_below_yield_thresh
  )) +
  metR::geom_contour_fill(bins = 12) +
  facet_grid(alpha ~ ., scales = "free_y") +
  ggtitle(
    paste0(
      "Blue rockfish",
      " ",
      "Larval pool dispersal",
      " ",
      "ENSO",
      ":\nTime below mean median yield "
    )
  ) +
  xlab("Fraction of coastline in reserves") +
  ylab("Harvest Rate (F/Fmsy)") +
  scale_fill_viridis_c(option = "D", direction = -1) +
  geom_segment(
    aes(
      x = 0,
      xend = max_reserve_frac,
      y = 1,
      yend = 1
    ),
    colour = "black",
    linetype = 2
  ) +
  scale_x_continuous(expand = expansion(mult = 0, add = 0),
                     limits = c(0, max_reserve_frac)) +
  scale_y_continuous(expand = expansion(mult = 0, add = 0)) +
  theme_bw()

y1

## Save yield plots

ggsave(
  filename = "output/supp_figs/supp_fig_4b-brf-lp-mei-yield-alpha-sens.png",
  plot = y1,
  height = 10,
  width = 8
)

ggsave(
  filename = "output/supp_figs/supp_fig_4b-brf-lp-mei-yield-alpha-sens.svg",
  plot = y1,
  height = 10,
  width = 8
)

end_time <- Sys.time()
time_taken <- end_time - start_time
print(paste0("The simulation to evaluate sensitivity to BH alpha ", 
             round(time_taken, 3), " ", attributes(time_taken)$units))


