#' Plot time effects
#'
#' @param po Posterior draws containing time effects
#' @param outdir Directory to save the generated plot
#'
#' @return ggplot2 object
plot_time_effects <- function(po, outdir=NA){
  p <- bayesplot::mcmc_intervals(po, regex_pars = c("tau"),
                                 prob = 0.5,
                                 prob_outer = 0.95,
                                 point_est = "median") +
    scale_y_discrete(labels = scales::label_parse()) +
    coord_flip()
  if(!is.na(outdir)){
    ggsave(file.path(outdir, "figures", "time_effect.png"), plot = p,
           units = "cm", width = 14.7, height = 4.5)
  } else {
    return(p)
  }
}

#' Plot repeated response effects
#'
#' @param po Posterior draws containing the repeated response effects
#' @param outdir Directory to save the generated plot
#'
#' @return ggplot2 object
plot_repeated_response_effects <- function(po, outdir=NA){
  p <- bayesplot::mcmc_intervals(po, regex_pars = c("rho"),
                                 prob = 0.5,
                                 prob_outer = 0.95,
                                 point_est = "median") +
    scale_y_discrete(labels = scales::label_parse()) +
    coord_flip() +
    theme_bw() +
    theme(axis.text = element_text(size = 8))

  if(!is.na(outdir)){
    ggsave(file.path(outdir, "figures", "repeated_response_effect.png"), plot = p,
           units = "cm", width = 12, height = 5, dpi = 300)
  } else {
    return(p)
  }
}

plot_time_repeat_pairs <- function(po, outdir=NA){
  p <- bayesplot::mcmc_pairs(po, regex_pars=c("tau", "rho"),
                             off_diag_args=list(size=0.3, alpha=0.3))

  if(!is.na(outdir)){
    ggsave(file.path(outdir, "figures", "time_repeat_pairs.png"), plot = p, limitsize = F)
  } else {
    return(p)
  }
}

#' Plots different contact intensity figures for each wave
#'
#' @param dt data.table containing posterior draws
#' @param f plotting function to use
#' @param fig.name name of the figure
#' @param outdir directory to save the figure
#
plot_intensities_wave <- function(dt, f, fig.name=NA, outdir = NA){
  W <- max(dt$wave)
  for (w in 1:W){
    f(dt[wave == w], fig.name = paste0(fig.name, "_w", w, ".png"), outdir = outdir)
  }
}

#' Make a predicted contact intensities plot
#'
#' @param dt Output of `extract_posterior_predictions()`
#' @param outdir Directory to save generated plots
#'
#' @return ggplot object
#' @export
#'
#' @examples
#' \dontrun{
#' dt.po <- extract_posterior_predictions(fit, dt)
#' plot_posterior_intensities(dt.po)
#' }
plot_posterior_intensities <- function(dt, fig.name=NA, outdir=NA){
  p <- ggplot(dt) +
    geom_tile(aes(x = age, y = alter_age, fill = intensity_M)) +
    labs(x = "Participants' age", y = "Contacts' age", fill = "Contact intensity" ) +
    coord_equal() +
    facet_grid(paste(alter_gender, "(Contacts)") ~ paste(gender, "(Participants)")) +
    scale_x_continuous(expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0)) +
    viridis::scale_fill_viridis(na.value="white", option="H") +
    theme_bw() +
    theme(
      legend.position = "bottom",
      strip.background = element_rect(color=NA, fill = "transparent")
    )

  if(!is.na(outdir)){
    ggsave(file.path(outdir, "figures", fig.name), plot = p)
  } else {
    return(p)
  }
}

#' Make a predicted contact rates plot
#'
#' @param dt Output of `extract_posterior_predictions()`
#' @param dt.strata Output of `extract_posterior_predictions()` (age stratified)
#' @param outdir Directory to save generated plots
#'
#' @return ggplot object
#' @export
#'
#' @examples
#' \dontrun{
#' dt.po <- extract_posterior_predictions(fit, dt)
#' plot_predicted_rates(dt.po)
#' }
plot_predicted_rates <- function(dt, outdir=NA){
  plt <- ggplot(dt) +
    geom_tile(aes(x = age, y = alter_age, fill = cntct_rate_predict)) +
    labs(x = "Age of contacting individuals", y = "Age of contacted individuals", fill = "Predicted contact rates" ) +
    coord_equal() +
    facet_grid(alter_gender_label ~ gender_label) +
    scale_x_continuous(expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0)) +
    guides(fill = guide_colorbar(title.position = "top", barwidth = 10)) +
    scale_fill_continuous(type = "viridis", na.value = "white",
                          limits = c(min(dt$cntct_rate), max(dt$cntct_rate))) +
    theme_bw() +
    theme(
      legend.position = "bottom",
      strip.background = element_rect(color=NA, fill = "transparent")
    )

  if(!is.na(outdir)){
    ggsave(file.path(outdir, "figures", "predicted_rates.png"), plot = plt)
  } else {
    return(plt)
  }
}

#' Plot the predicted contact rates sliced at different contacting ages
#'
#' @param dt data.table with extracted posterior predictions
#' @param age.cut a vector specifying the ages to slice at
#' @param .gender gender of contacting individual
#' @param .alter_gender gender of contacted individual
#' @param outdir directory to save plot
#'
#' @return
#' @export
#'
#' @examples
plot_sliced_intensities <- function(dt, age.cut = c(20, 40, 60), fig.name=NA, outdir=NA)
{
  tmp <- dt[age %in% age.cut]
  tmp[, age_label := paste("Participants' age:", age)]
  tmp[, comb := paste(gender, "to", alter_gender)]

  p <- ggplot(tmp, aes(x=alter_age)) +
    geom_stepribbon(aes(ymin = intensity_CL, ymax = intensity_CU), alpha=0.3) +
    geom_step(aes(y = intensity_M)) +
    labs(x="Contacts' age", y="Contact intensity") +
    facet_grid(age_label ~ comb) +
    theme_bw() +
    theme(
      legend.position = "bottom",
      strip.background = element_rect(color=NA, fill = "transparent"),
      strip.text = element_text(face = "bold")
    )

  if(!is.na(outdir)){
    ggsave(file.path(outdir, "figures", fig.name), plot = p)
  }

  return(p)
}

#' Title
#'
#' @param dt
#' @param fig.name
#' @param outdir
#'
#' @return
#' @export
#'
#' @examples
plot_marginal_intensities <- function(dt, fig.name=NA, outdir=NA){

  p <- ggplot(dt, aes(age, intensity_M)) +
    geom_stepribbon(aes(ymin = intensity_CL, ymax = intensity_CU, fill = gender), alpha=0.3) +
    geom_step(aes(color = gender)) +
    scale_y_continuous(limits = c(0, NA)) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_color_brewer(palette = "Set1") +
    scale_fill_brewer(palette = "Set1") +
    labs(y="Marginal contact intensity", x="Age of contacting individual",
         color="Gender", fill="Gender") +
    theme_bw() +
    theme(
      legend.position = "right",
      plot.background = element_rect(fill='transparent', color=NA),
      strip.background = element_rect(color=NA, fill = "transparent"),
      legend.background = element_rect(fill="transparent", color=NA)
    )

  if(!is.na(outdir)){
    ggsave(file.path(outdir, "figures", fig.name), plot = p, height = 3, width = 7)
  }

  return(p)
}

#' Plots the empirical contact intensity patterns
#'
#' @param dt A data.table with the aggregated counts (ideally with completed zeros)
#' @param outdir Directory path the save the outputs
#'
#' @return ggplot
#' @export
#'
#' @examples
plot_empirical_intensities <- function(dt, outdir=NA){
  dt[, intensity := y/part]
  dt[is.nan(intensity), intensity := 0]

  p <- ggplot(dt, aes(age, alter_age_strata)) +
    geom_tile(aes(fill = intensity)) +
    scale_x_continuous(expand=c(0,0)) +
    scale_y_discrete(expand=c(0,0)) +
    viridis::scale_fill_viridis(na.value = "white", option="H") +
    labs(x="Participants' age", y="Contacts' age strata", fill="Contact intensity") +
    facet_grid( paste(alter_gender, "(Contacts)") ~ paste(gender, "(Participants)") ) +
    theme_bw() +
    theme(
      aspect.ratio = 1,
      legend.position = "bottom",
      strip.background = element_rect(color=NA, fill = "transparent")
    )

  if(!is.na(outdir)){
    ggsave(file.path(outdir, "figures", "empirical_intensities.png"), plot = p)
  }

  return(p)
}

