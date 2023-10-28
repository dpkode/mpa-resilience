# 
# sim_num = num_sims_dt
# reserve_frac = reserve_fracs_dt
# flep_ratio = flep_ratios_dt
# year = year_vals_dt
# age = age_vals_dt
# patch_num = patch_num_dt
# number = NA_real_
# yield = NA_real_


run_sims_sm_write <- function(dt,
                              expmts,
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
  
  for (i in 1:num_sims) {
    for (j in 1:length(frac_reserves)) {
      for (k in 1:length(fleps)) {
        dt_one <- dt[i = sim_num == q & near(reserve_frac, frac_reserves[j]) & near(flep_ratio, fleps[n]),]
      }
    }
  }
  
  for (i in 1:length(expmts)) {
    for (j in 1:length(spec_name)) {
      for (p in noise_vec) {
        for (m in 1:length(recr_sd)) {
          for (n in 1:length(fleps)) {
            for (o in 1:length(frac_reserves)) {
              for (q in 1:num_sims) {
                
                dt_one <- dt[i = .(
                  expmts[i],
                  spec_name[j],
                  alpha_bh[j],
                  recr_sd[m],
                  frac_reserves[o],
                  fleps[n],
                  p,
                  q
                ), ]
                
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
                setcolorder(out_Ns, c("year", "age", "patch_num"))
                setkeyv(out_Ns, c("year", "age", "patch_num"))
                
                stopifnot(max(out_Ns[patch_num]) = num_patches)
                
                out_Y <-
                  setnames(
                    as.data.table(out[["Y"]]),
                    old = c("V1", "V2", "V3", "value"),
                    new = c("age", "year", "patch_num", "yield")
                  )
                setcolorder(out_Y, c("year", "age", "patch_num"))
                setkeyv(out_Y, c("year", "age", "patch_num"))
                
                stopifnot(max(out_Y[patch_num]) = num_patches)
                
                dt_one[, j =  `:=` (number = list(out_Ns),
                                    yield = list(out_Y))] 
                
                rm(out_Ns, out_Y, out) 
                
                dt_sub <- dt_one %>%
                  select(-recruits,-eggs) %>%
                  unnest(c(number, yield), names_repair = "minimal") 
                
                dt_sub <- dt_sub[!duplicated(as.list(dt_sub))]
                
                
                duckdb::dbWriteTable(con,
                                     paste0(expmts[i], "_", tolower(sub(
                                       " ", "_", spec_name[j]
                                     )), "_", p, "_", substring(as.character(recr_sd[m]), 3)),
                                     dt_sub,
                                     append = TRUE) # FALS
              }
            }
          }
        }
      }
      print(paste0("dispersal: ", expmts[i], " and species: ", spec_name[j],
                   " and ", p, " noise ", " and recruit sd of ", recr_sd[m]))
    }
  }
  DBI::dbDisconnect(con, shutdown = TRUE)
  return(dt)
}


# run_sims_sm_write <- function(dt,
#                      expmts,
#                      spec_name,
#                      alpha_bh,
#                      beta_bh,
#                      recr_sd,
#                      fleps,
#                      frac_reserves,
#                      noise_vec,
#                      noise_dat,
#                      n_years,
#                      num_sims,
#                      num_patches,
#                      les_mats,
#                      conn_mats,
#                      in_out_fracs,
#                      sim_results,
#                      sim_spec_dervars,
#                      f_vals,
#                      spec_flep_f_ind,
#                      sim_spec_pars) {
# 
#   con <-
#     dbConnect(duckdb::duckdb(),
#               dbdir = "data/sim_out.duckdb",
#               read_only = FALSE)
#   
#   for (i in 1:length(expmts)) {
#     for (j in 1:length(spec_name)) {
#       for (p in noise_vec) {
#         for (m in 1:length(recr_sd)) {
#           for (n in 1:length(fleps)) {
#             for (o in 1:length(frac_reserves)) {
#               for (q in 1:num_sims) {
#                 
#                 # print(paste0("q -- sim num: ", q))
#                 
#                 dt_one <- dt[i = .(
#                   expmts[i],
#                   spec_name[j],
#                   alpha_bh[j],
#                   recr_sd[m],
#                   frac_reserves[o],
#                   fleps[n],
#                   p,
#                   q
#                 ), ]
#                 
#                 out <- run_patch_sims2(
#                   t_steps = n_years,
#                   num_patches = num_patches,
#                   les_mat = les_mats[[j]][[n]],
#                   con_mat = conn_mats[[o]],
#                   frac_in_out = in_out_fracs[[o]],
#                   N = sim_results[[j]][[n]],
#                   # this needs the values from a unfished_N1 for each species
#                   selectivity = as.integer(sim_spec_dervars[[j]][["F_select"]]),
#                   fishing_mortality = c(0, f_vals[spec_flep_f_ind[[j]][[n]]]),
#                   natural_mortality = sim_spec_pars[[j]][["M"]],
#                   weight_at_age = sim_spec_pars[[j]][["biom_const"]] * sim_spec_dervars[[j]][["length_at_age"]] ^
#                     sim_spec_pars[[j]][["biom_exp"]],
#                   alpha = alpha_bh[[j]],
#                   beta = beta_bh,
#                   noise_series = noise_dat
#                 )
#                 
#                 out_Ns <-
#                   setnames(
#                     as.data.table(out[["Ns"]]),
#                     old = c("V1", "V2", "V3", "value"),
#                     new = c("age", "year", "patch_num", "number")
#                   )
#                 setkeyv(out_Ns, c("age", "year", "patch_num"))
#                 
#                 out_Y <-
#                   setnames(
#                     as.data.table(out[["Y"]]),
#                     old = c("V1", "V2", "V3", "value"),
#                     new = c("age", "year", "patch_num", "yield")
#                   )
#                 setkeyv(out_Y, c("age", "year", "patch_num"))
#                 
#                 dt_one[, j =  `:=` (number = list(out_Ns),
#                               yield = list(out_Y))] 
#                 
#                 rm(out_Ns, out_Y, out) 
#                 
#                 dt_sub <- dt_one %>%
#                   select(-recruits,-eggs) %>%
#                   unnest(c(number, yield), names_repair = "minimal") 
#                 
#                 dt_sub <- dt_sub[!duplicated(as.list(dt_sub))]
# 
#                 
#                 duckdb::dbWriteTable(con,
#                                      paste0(expmts[i], "_", tolower(sub(
#                                        " ", "_", spec_name[j]
#                                      )), "_", p, "_", substring(as.character(recr_sd[m]), 3)),
#                                      dt_sub,
#                                      append = TRUE) # FALS
#               }
#             }
#           }
#         }
#       }
#       print(paste0("dispersal: ", expmts[i], " and species: ", spec_name[j],
#                    " and ", p, " noise ", " and recruit sd of ", recr_sd[m]))
#     }
#   }
#   DBI::dbDisconnect(con, shutdown = TRUE)
#   return(dt)
# }
