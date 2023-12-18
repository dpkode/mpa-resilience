# Functions for table meta data

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

## POSSIBLE TODO: Combine next two `get_median_...()`
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

get_median_biomass <- function(input_df,
                               age_mat) {
  # returns the mean of median abundance at at F = F_msy 
  # for a coastline with no reserves across sims
  stopifnot(colnames(input_df) == c("sim_num", "reserve_frac", "flep_ratio",
                                    "year", "age", "abundance", "yield", 
                                    "flep_vals", "f_vals", 
                                    "f_fmsy", "waa_g", "biomass_at_age"))
  
  med_biomass <- input_df %>%
    dplyr::filter(near(f_fmsy, 1), near(as.numeric(reserve_frac), 0))  %>%
    # dplyr::mutate(mat_fish = age >= age_mat) %>% 
    dplyr::group_by(sim_num,
                    reserve_frac,
                    f_fmsy,
                    year) %>%
    # dplyr::summarise(ann_biomass = sum(biomass_at_age[mat_fish])) %>% 
    dplyr::summarise(ann_biomass = sum(biomass_at_age)) %>% 
    dplyr::ungroup() %>%
    dplyr::group_by(sim_num) %>%
    dplyr::summarise(med_ann_biomass = median(ann_biomass)) %>%
    dplyr::ungroup() %>%
    dplyr::summarise(avg_med_biomass_f_fmsy = mean(med_ann_biomass)) %>%
    .$avg_med_biomass_f_fmsy
  return(med_biomass)
}

df_yld_bm_stats_by_sim_f_frac_year <- function(input_df,
                                               age_mat) {
  
  stopifnot(colnames(input_df) == c("sim_num", "reserve_frac", "flep_ratio",
                                    "year", "age", "abundance", "yield", 
                                    "flep_vals", "f_vals", 
                                    "f_fmsy", "waa_g", "biomass_at_age"))

  out_df <- input_df %>%
    dplyr::mutate(mat_fish = age >= age_mat) %>% 
    dplyr::group_by(sim_num, reserve_frac, f_fmsy, year) %>%
    dplyr::summarize(annual_yield = sum(yield), 
                     annual_biomass = sum(biomass_at_age[mat_fish])) %>%
                     # annual_biomass = sum(biomass_at_age[mat_fish])) %>%
    dplyr::ungroup()
  return(out_df)
}


summary_stats_by_f_frac <- function(out_sim_f_frac_yr,
                                    median_yield,
                                    median_biomass
                                    ) {
  sim_f_frac_df <- out_sim_f_frac_yr %>%
    dplyr::mutate(below_thresh_yld = if_else(annual_yield <= median_yield, 1, 0),
                  below_thresh_bm = if_else(annual_biomass <= median_biomass, 1, 0)) %>%
    dplyr::group_by(sim_num, reserve_frac, f_fmsy) %>%
    dplyr::summarise(
      mean_yield = round(mean(annual_yield, na.rm = TRUE), 2),
      mean_biomass = round(mean(annual_biomass, na.rm = TRUE), 2),
      sd_yield = round(sd(annual_yield, na.rm = TRUE), 2),
      sd_biomass = round(sd(annual_biomass, na.rm = TRUE), 2),
      cv_yield = round(sd(annual_yield, na.rm = TRUE)/mean(annual_yield, na.rm = TRUE), 2),
      cv_biomass = round(sd(annual_biomass, na.rm = TRUE)/mean(annual_biomass, na.rm = TRUE), 2),
      yrs_below_yield_thresh = sum(below_thresh_yld)/135,
      yrs_below_biomass_thresh = sum(below_thresh_bm)/135
    ) %>%
    dplyr::ungroup() 
  
  summ_f_frac_df <- sim_f_frac_df %>%
    dplyr::group_by(reserve_frac, f_fmsy) %>%
    dplyr::summarize(
      mean_yield = round(mean(mean_yield, na.rm = TRUE), 2),
      mean_yield_lab = paste0("Mean yield: ", mean_yield),
      mean_biomass = round(mean(mean_biomass, na.rm = TRUE), 2),
      mean_biomass_lab = paste0("Mean biomass: ", mean_yield),
      sd_yield = round(mean(sd_yield, na.rm = TRUE), 2),
      sd_yield_lab = paste0("SD yield: ", sd_yield),
      sd_biomass = round(mean(sd_biomass, na.rm = TRUE), 2),
      sd_biomass_lab = paste0("SD biomass: ", sd_yield),
      cv_yield = round(mean(cv_yield, na.rm = TRUE), 2),
      cv_yield_lab = paste0("CV yield: ", cv_yield),
      cv_biomass = round(sd(cv_biomass, na.rm = TRUE), 2),
      cv_biomass_lab = paste0("CV biomass: ", cv_biomass),
      yrs_below_yield_thresh =  mean(yrs_below_yield_thresh),
      yrs_below_yield_thresh_lab = paste("% YR below yield thresh: ", round(yrs_below_yield_thresh, 4)),
      yrs_below_biomass_thresh = mean(yrs_below_biomass_thresh),
      yrs_below_biomass_thresh_lab = paste("% YR below biomass thresh: ", round(yrs_below_biomass_thresh, 4))
    ) %>%
    ungroup()
  return(list(sim_f_frac_df, summ_f_frac_df))
}



