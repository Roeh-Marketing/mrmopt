#' Compare multiple fitted response models
#'
#' Plots response curves, return curves, or cost-per curves from multiple
#' fitted models on a single axis or faceted. Useful for comparing curve types
#' for the same channel, or comparing channels side by side.
#'
#' @param models A named list of \code{mrmfit} objects.
#' @param plot_type Character; one of \code{"response"} (default),
#'   \code{"return"}, or \code{"costper"}.
#' @param x_var Character; \code{"spend"} (default) or \code{"units"}.
#' @param layout Character; \code{"overlay"} (default) plots all models on one
#'   axis using color, or \code{"facet"} uses \code{facet_wrap}.
#' @param interval Type of credible interval. \code{"prediction"} (default)
#'   includes observation noise. \code{"confidence"} shows uncertainty about
#'   the mean curve only (tighter bands). \code{"none"} draws the center curves
#'   only, with no interval ribbons.
#' @return A ggplot object.
#' @export

mrms_plot_compare <- function(models,
                             plot_type = c("response", "return", "costper"),
                             x_var = c("spend", "units"),
                             layout = c("overlay", "facet"),
                             interval = c("prediction", "confidence", "none")) {

  plot_type <- match.arg(plot_type)
  x_var <- match.arg(x_var)
  layout <- match.arg(layout)
  interval <- match.arg(interval)

  if (!is.list(models) || length(models) < 2) {
    stop("models must be a named list with at least 2 mrmfit objects.", call. = FALSE)
  }

  pal <- mrmopt_palette()

  # --- Detect comparison mode: same channel vs different channels ---
  channels <- vapply(models, hlpr_channel_name, character(1))
  types <- vapply(models, function(m) m$rc_type, character(1))

  # User-supplied names take precedence; otherwise auto-generate
  if (!is.null(names(models))) {
    model_labels <- names(models)
    color_title  <- "Model"
  } else if (length(unique(channels)) == 1) {
    model_labels <- types
    color_title  <- "Curve Type"
  } else {
    model_labels <- paste0(channels, " (", types, ")")
    color_title  <- "Model"
  }

  # --- Resolve label collisions using date range ---
  # Applies when same channel + same type are compared across time periods
  if (anyDuplicated(model_labels)) {
    date_tags <- vapply(models, function(m) {
      if (!is.null(m$date_range)) {
        paste0(format(m$date_range, "%b '%y"), collapse = "\u2013")
      } else {
        ""
      }
    }, character(1))

    is_dup <- duplicated(model_labels) | duplicated(model_labels, fromLast = TRUE)

    # Fall back to positional index if date_range is unavailable
    model_labels <- ifelse(
      is_dup & nchar(date_tags) > 0,
      paste0(model_labels, " (", date_tags, ")"),
      ifelse(is_dup, paste0(model_labels, " (", seq_along(model_labels), ")"), model_labels)
    )
  }

  names(models) <- model_labels

  # --- Build combined data frame ---
  combined <- do.call(rbind, lapply(seq_along(models), function(i) {
    m <- models[[i]]
    rdf <- m$response_df

    x_col <- names(rdf)[1]
    has_units <- !is.null(m$units_col) && !is.null(m$cost_per_unit)

    if (x_var == "units") {
      if (!has_units) {
        stop("Units not available for model '", names(models)[i],
             "'; fit with a `units` column.", call. = FALSE)
      }
      if (!"units" %in% names(rdf)) {
        rdf$units <- rdf[[x_col]] / m$cost_per_unit
      }
      x_vals <- rdf$units
    } else {
      x_vals <- rdf[[x_col]]
    }

    # Select interval columns based on interval type
    if (interval == "confidence" && "lower_mu" %in% names(rdf)) {
      rdf$lower <- rdf$lower_mu
      rdf$upper <- rdf$upper_mu
      if ("cp_upper_mu" %in% names(rdf)) rdf$cp_upper <- rdf$cp_upper_mu
    }

    out <- data.frame(
      x = x_vals,
      center = rdf$center,
      lower = rdf$lower,
      upper = rdf$upper,
      ar = rdf$ar,
      mr = rdf$mr,
      cp = rdf$cp,
      cp_lower = rdf$cp_lower,
      cp_upper = if ("cp_upper" %in% names(rdf)) rdf$cp_upper else NA_real_,
      model_id = model_labels[i],
      stringsAsFactors = FALSE
    )
    out
  }))

  x_lab <- if (x_var == "units") "Units" else "Spend"
  x_scale <- if (x_var == "units") {
    ggplot2::scale_x_continuous(labels = scales::comma)
  } else {
    ggplot2::scale_x_continuous(labels = scales::dollar_format())
  }

  # --- Build plot based on type ---
  if (plot_type == "response") {
    p <- ggplot2::ggplot(combined, ggplot2::aes(
      x = .data$x, y = .data$center, color = .data$model_id, fill = .data$model_id
    ))
    if (interval != "none") {
      p <- p + ggplot2::geom_ribbon(
        ggplot2::aes(ymin = .data$lower, ymax = .data$upper),
        alpha = 0.15, color = NA
      )
    }
    p <- p +
      ggplot2::geom_line(linewidth = 0.7) +
      ggplot2::scale_y_continuous(labels = scales::comma) +
      ggplot2::labs(y = "KPI", title = "Response Curve Comparison")

  } else if (plot_type == "return") {
    # Pivot AR and MR into long form
    ar_df <- combined[, c("x", "ar", "model_id")]
    ar_df$metric <- "AR"
    names(ar_df)[2] <- "value"
    mr_df <- combined[, c("x", "mr", "model_id")]
    mr_df$metric <- "MR"
    names(mr_df)[2] <- "value"
    long <- rbind(ar_df, mr_df)

    p <- ggplot2::ggplot(long, ggplot2::aes(
      x = .data$x, y = .data$value,
      color = .data$model_id, linetype = .data$metric
    )) +
      ggplot2::geom_line(linewidth = 0.7, na.rm = TRUE) +
      ggplot2::scale_linetype_manual(values = c(AR = "solid", MR = "dashed"), name = "Metric") +
      ggplot2::scale_y_continuous(labels = scales::comma) +
      ggplot2::labs(y = "Rate", title = "Return Curve Comparison")

  } else {
    cp_data <- combined[is.finite(combined$cp), ]

    p <- ggplot2::ggplot(cp_data, ggplot2::aes(
      x = .data$x, y = .data$cp, color = .data$model_id
    )) +
      ggplot2::geom_line(linewidth = 0.7) +
      ggplot2::scale_y_continuous(labels = scales::dollar_format(), limits = c(0, NA)) +
      ggplot2::labs(y = "Cost per KPI", title = "Cost per KPI Comparison")
  }

  # --- Common elements ---
  p <- p +
    x_scale +
    ggplot2::labs(x = x_lab, color = color_title, fill = color_title) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "top")

  if (layout == "facet") {
    p <- p + ggplot2::facet_wrap(~ model_id, scales = "free")
  }

  p
}
