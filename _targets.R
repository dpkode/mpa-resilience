# add unit/data tests (via testthat and assertr)
# add version control?


library(targets)
library(tarchetypes)
source(file.path("R", "functions.R"))
source(file.path("R", "test_functions.R"))
options(tidyverse.quiet = TRUE)
tar_option_set(
  packages = c(
    "tidyverse",
    "data.table",
    "duckdb",
    "colorspace",
    "fst",
    "qs",
    "metR",
    "isoband",
    "signal",
    "imager"
  )
)


r_seed <- 11
tar_seed(default = r_seed)
## TODO: check for duck.db files and delete before running
list(
  tar_target(exp_names_nd,
             "no_dispersal"),
  tar_target(exp_names_lp,
             "larval_pool"),
  tar_target(
    species_parms_csv,
    file.path("data", "species_parameters.csv"),
    format = "file"
  ),
  tar_target(species_parms,
             load_species_parms(fp = species_parms_csv)),
  tar_target(
    species_names,
    c(
      "Blue rockfish",
      "Cabezon",
      # "Kelp bass",
      # "Pacific cod",
      "China rockfish"
    )
  ),
  # get parms for selected species
  tar_target(
    sim_species_parms,
    list_species_parms(parm_df = species_parms,
                       sp_names = species_names)
  ),
  # Get age from parms file
  tar_target(
    ages,
    species_names %>%
      purrr::set_names() %>%
      purrr::map(., ~ 1:sim_species_parms[[.x]]$A_max)
  ),
  # Fishing mortality - many so that Fs for given FLEPs can be computed
  tar_target(fishing_mortality_values,
             seq(0, 2, by = 0.0025)),
  # Make derived parms for selected species
  # 'Derived' because they are calculated from the species parameters data
  tar_target(num_patches,
             2),
  # population dynamics and simulation parameters
  # Simulation length burn_in and actual sim_len:
  tar_target(burn_in,
             500),
  tar_target(sim_len,
             512),
  tar_target(sim_len_135,
             135),
  tar_target(n_years,
             burn_in + sim_len),
  tar_target(n_years_135,
             burn_in + sim_len_135),
  tar_target(sim_yrs,
             1:n_years),
  tar_target(sim_yrs_135,
             1:n_years_135),
  # Run how many replicate simulations?
  tar_target(sim_nums,
             5),
  tar_target(sd_recruitment,
             0.75),
  # Fraction of lifetime egg production (FLEP)
  # Lower FLEPs correspond to populations that have experienced higher exploitation
  # 0.3 is close to population collapse, so 0.4 is close enough to get cohort resonance
  tar_target(flep_ratios,
             c(seq(0.2, 0.975, by = 0.025), 0.998)),
  tar_target(reserve_fracs,
             seq(0, 0.8, by = 0.05)),
  # EPR (FLEP) can't go below this proportion of the unfished value
  # Note that the eq. recruitment is the point where line with 1/FLEP intersects SR curve
  tar_target(critical_replacement_threshold,
             0.3),
  # Maximum density of recruits. Serves as a scalar carrying capacity parameter
  tar_target(bev_holt_beta,
             1E6),
  tar_target(
    sim_species_derived_vars,
    list_derived_variables(
      spec_parms = sim_species_parms,
      sp_names = species_names,
      fish_mort = fishing_mortality_values
    )
  ),
  tar_target(
    lifetime_eggs_production,
    list_lep(spec_names = species_names,
             sim_spec_der_var = sim_species_derived_vars)
  ),
  tar_target(
    relative_lifetime_eggs_production,
    species_names %>%
      purrr::set_names() %>%
      purrr::map(., ~ sim_species_derived_vars[[.x]][["EPRs"]] / lifetime_eggs_production[[.x]])
  ),
  ## get the index for the F yielding target flep
  tar_target(
    species_flep_fished_ind,
    get_index_for_fleps(
      flep_vals = flep_ratios,
      sp_names = species_names,
      relative_flep = relative_lifetime_eggs_production
    )
  ),
  # Beverton-Holt stock recruitment slope parameters: this specifies that pop will persist if EPR > CRT
  tar_target(
    bev_holt_alphas,
    species_names %>%
      purrr::set_names() %>%
      purrr::map(., ~ (
        1 / (critical_replacement_threshold * lifetime_eggs_production[[.x]])
      )) %>%
      unlist()
  ),
  # Equilibrium recruitment unfished and fished (F to get to X% FLEPs)
  tar_target(
    equilibrium_recruits,
    species_names %>%
      purrr::set_names() %>%
      purrr::map(
        .,
        ~ lifetime_eggs_production[[.x]] * (
          lifetime_eggs_production[[.x]] * bev_holt_alphas[[.x]]  / (
            1 + (bev_holt_alphas[[.x]] / bev_holt_beta) * lifetime_eggs_production[[.x]]
          )
        )
      )
  ),
  # set the fished levels of equilibrium recruits
  tar_target(
    fished_equilibrium_recruits,
    set_fished_equilibrium_recruits(
      sp_names = species_names,
      flep_vals = flep_ratios,
      ind_species_flep_fished = species_flep_fished_ind,
      species_derived_vars = sim_species_derived_vars,
      alphas = bev_holt_alphas,
      beta = bev_holt_beta
    )
  ),
  tar_target(
    leslie_matrices,
    make_leslie_matrix_sp_flep(
      sp_names = species_names,
      fleps = flep_ratios,
      n_patch = num_patches,
      sim_sp_parms = sim_species_parms,
      sim_sp_derived_vars = sim_species_derived_vars,
      sp_flep_fished_ind = species_flep_fished_ind
    )
  ),
  # initialize nums at age for unfished and fished populations using SAD and FLEP values
  # unfished
  tar_target(
    unfished_N1,
    species_names %>%
      purrr::set_names() %>%
      purrr::map(., ~ equilibrium_recruits[[.x]] * sim_species_derived_vars[[.x]]$SADs[, 1])
  ),
  # fished
  tar_target(
    fished_N1,
    set_fished_eq_recruits(
      sp_names = species_names,
      fleps = flep_ratios,
      f_eq_recs = fished_equilibrium_recruits,
      sp_der_var = sim_species_derived_vars,
      sp_flep_f_ind = species_flep_fished_ind
    )
  ),
  # store simulation results (Numbers (n), Yield (biomass), Eggs (n), Recs (n))
  # stored in a list
  tar_target(
    sim_results_arrays_empty,
    species_names %>%
      purrr::set_names() %>%
      purrr::map(., ~ vector("list", length = length(.x)))
  ),
  tar_target(
    sim_results_arrays,
    set_sim_results_arrays(
      sp_names = species_names,
      sim_arrays = sim_results_arrays_empty,
      fleps = flep_ratios,
      sim_sp_derived_vars = sim_species_derived_vars,
      n_years = n_years,
      n_patch = num_patches
    )
  ),
  tar_target(
    sim_results_arrays_init,
    init_vals_sim(
      species_names = species_names,
      flep_ratios = flep_ratios,
      sim_results_arrays = sim_results_arrays,
      fished_N1 = fished_N1
    )
  ),
  tar_target(no_disp_conn_mat,
             purrr::map(
               1:length(reserve_fracs), ~ matrix(c(1, 0, 0, 1), nrow = 2, byrow = FALSE)
             )),
  tar_target(no_disp_in_out_frac,
             reserve_fracs %>% purrr::map(., ~ matrix(c(
               .x, (1 - .x)
             ), nrow = 2))),
  tar_target(larvpool_disp_conn_mat,
             purrr::map(
               reserve_fracs,
               ~ matrix(
                 c(.x, .x, (1 - .x), (1 - .x)),
                 nrow = num_patches,
                 ncol = num_patches,
                 byrow = TRUE
               )
             )),
  tar_target(
    larvpool_in_out_frac,
    larvpool_disp_conn_mat %>% purrr::map(., ~ matrix(.x[, 1], nrow = 2))
  ),
  # Environmental noise
  tar_target(noises_white,
             "white"),
  tar_target(noises_white_135,
                        "white_135"),
  tar_target(noises_enso,
             "enso"),
  tar_target(noises_enso_135,
             "mei_135"),
  tar_target(white_noise,
             rnorm(n_years, 0, sd_recruitment)),
  tar_target(white_noise_135,
             rnorm(635, 0, sd_recruitment)),
  tar_target(
    pre_enso_noise,
    make_sim_enso(sim_enso_len = 1012, n_enso_sims = 100)
  ),
  tar_target(enso_noise,
             sd_recruitment * pre_enso_noise),
  tar_target(enso_dat_135,
             make_ann_mei_ext()),
  tar_target(
    enso_noise_135,
    make_sim_ann_mei_ext(
      mei = enso_dat_135$mei,
      sim_enso_len = 635,
      n_enso_sims = 100
    ) * sd_recruitment
  ),
  ## NEW BASE CASE
  ## SIM ESNO DERIVED 135 YR MEI
  ## SIM TS = 135 YRS, NO LOW
  ## POWER AT LOW FREQS
  ##-------------------###
  ##-------------------###
  ## SD RECRUIT 0.75  135
  ## NO DISPERSAL ENSO
  ## OUT TABLES
  tar_target(
    dt_no_disp_white_135_75,
    make_sim_dt2(
      exp_names = exp_names_nd,
      species_names = species_names,
      bev_holt_alphas = bev_holt_alphas,
      sd_recruitment = sd_recruitment,
      reserve_fracs = reserve_fracs,
      flep_ratios = flep_ratios,
      noises = noises_white_135,
      sim_nums = sim_nums
    ),
    format = "qs"
  ),
  ## SIMULATION DATA TABLES
  tar_target(
    out_no_disp_white_135_75,
    run_sims_sm_write(
      dt = dt_no_disp_white_135_75,
      expmts = exp_names_nd,
      spec_name = species_names,
      alpha_bh = bev_holt_alphas,
      beta_bh = bev_holt_beta,
      recr_sd = sd_recruitment,
      fleps = flep_ratios,
      frac_reserves = reserve_fracs,
      noise_vec = noises_white_135,
      noise_dat = white_noise_135,
      n_years = n_years_135,
      num_sims = sim_nums,
      num_patches = num_patches,
      les_mats = leslie_matrices,
      conn_mats = no_disp_conn_mat,
      in_out_fracs = no_disp_in_out_frac,
      sim_results = sim_results_arrays_init,
      sim_spec_dervars = sim_species_derived_vars,
      f_vals = fishing_mortality_values,
      spec_flep_f_ind = species_flep_fished_ind,
      sim_spec_pars = sim_species_parms
    ),
    format = "qs",
    memory = "transient",
    garbage_collection = TRUE
  ),
  ## SD RECRUIT 0.75  135
  ## NO DISPERSAL ENSO
  ## OUT TABLES
  tar_target(
    dt_no_disp_enso_135_75,
    make_sim_dt2(
      exp_names = exp_names_nd,
      species_names = species_names,
      bev_holt_alphas = bev_holt_alphas,
      sd_recruitment = sd_recruitment,
      reserve_fracs = reserve_fracs,
      flep_ratios = flep_ratios,
      noises = noises_enso_135,
      sim_nums = sim_nums
    ),
    format = "qs"
  ),
  ## SIMULATION DATA TABLES
  tar_target(
    out_no_disp_enso_135_75,
    run_sims_sm_write(
      dt = dt_no_disp_enso_135_75,
      expmts = exp_names_nd,
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
      conn_mats = no_disp_conn_mat,
      in_out_fracs = no_disp_in_out_frac,
      sim_results = sim_results_arrays_init,
      sim_spec_dervars = sim_species_derived_vars,
      f_vals = fishing_mortality_values,
      spec_flep_f_ind = species_flep_fished_ind,
      sim_spec_pars = sim_species_parms
    ),
    format = "qs",
    memory = "transient",
    garbage_collection = TRUE
  ),
  
  ## SD RECRUIT 0.75  135
  ## LARVAL POOL WHITE
  ## OUT TABLES
  tar_target(
    dt_lp_white_135_75,
    make_sim_dt2(
      exp_names = exp_names_lp,
      species_names = species_names,
      bev_holt_alphas = bev_holt_alphas,
      sd_recruitment = sd_recruitment,
      reserve_fracs = reserve_fracs,
      flep_ratios = flep_ratios,
      noises = noises_white_135,
      sim_nums = sim_nums
    ),
    format = "qs"
  ),
  ## SIMULATION DATA TABLES
  tar_target(
    out_lp_white_135_75,
    run_sims_sm_write(
      dt = dt_lp_white_135_75,
      expmts = exp_names_lp,
      spec_name = species_names,
      alpha_bh = bev_holt_alphas,
      beta_bh = bev_holt_beta,
      recr_sd = sd_recruitment,
      fleps = flep_ratios,
      frac_reserves = reserve_fracs,
      noise_vec = noises_white_135,
      noise_dat = white_noise_135,
      n_years = n_years_135,
      num_sims = sim_nums,
      num_patches = num_patches,
      les_mats = leslie_matrices,
      conn_mats = larvpool_disp_conn_mat,
      in_out_fracs = larvpool_in_out_frac,
      sim_results = sim_results_arrays_init,
      sim_spec_dervars = sim_species_derived_vars,
      f_vals = fishing_mortality_values,
      spec_flep_f_ind = species_flep_fished_ind,
      sim_spec_pars = sim_species_parms
    ),
    format = "qs",
    memory = "transient",
    garbage_collection = TRUE
  ),
  
  ## SD RECRUIT 0.75  135
  ## LARVAL POOL ENSO
  ## OUT TABLES
  tar_target(
    dt_lp_enso_135_75,
    make_sim_dt2(
      exp_names = exp_names_lp,
      species_names = species_names,
      bev_holt_alphas = bev_holt_alphas,
      sd_recruitment = sd_recruitment,
      reserve_fracs = reserve_fracs,
      flep_ratios = flep_ratios,
      noises = noises_enso_135,
      sim_nums = sim_nums
    ),
    format = "qs"
  ),
  ## SIMULATION DATA TABLES
  tar_target(
    out_lp_enso_135_75,
    run_sims_sm_write(
      dt = dt_lp_enso_135_75,
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
      sim_results = sim_results_arrays_init,
      sim_spec_dervars = sim_species_derived_vars,
      f_vals = fishing_mortality_values,
      spec_flep_f_ind = species_flep_fished_ind,
      sim_spec_pars = sim_species_parms
    ),
    format = "qs",
    memory = "transient",
    garbage_collection = TRUE
  ),
  
  ##### calcs for plotting  #####
  ## CONTOUR and GRADIENT VECTROR PLOTS
  ## Blue rockfish
  tar_target(
    brf_fleps_2_fs,
    get_fs_from_fleps(
      species = "Blue rockfish",
      flep_inds = species_flep_fished_ind,
      f_vals = fishing_mortality_values
    )
  ),
  ## Cabezon
  tar_target(
    cab_fleps_2_fs,
    get_fs_from_fleps(
      species = "Blue rockfish",
      flep_inds = species_flep_fished_ind,
      f_vals = fishing_mortality_values
    )
  ),
  ## China rockfish
  tar_target(
    chr_fleps_2_fs,
    get_fs_from_fleps(
      species = "China rockfish",
      flep_inds = species_flep_fished_ind,
      f_vals = fishing_mortality_values
    )
  ))

   
###-----------------------###
###-----------------------###
###--- END SIMULATIONS ---###
###--- END SIMULATIONS ---###
###-----------------------###
