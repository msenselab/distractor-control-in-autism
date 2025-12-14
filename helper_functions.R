# Helper Functions for Distractor Control Analysis
# =================================================
# This file contains reusable functions for data visualization and statistical
# reporting in the distractor control study.

# Table Generation Functions --------------------------------------------------

#' Create formatted ANOVA table
#'
#' Converts ANOVA results into a styled HTML table using kable.
#'
#' @param aov_result ANOVA result object from afex (e.g., from aov_ez or aov_car)
#' @param caption Character string for table caption
#' @return A kable table with bootstrap styling
create_anova_table <- function(aov_result, caption) {
  # Get standard ANOVA table
  nice_table <- nice(aov_result)

  # Calculate partial eta-squared with 95% CI
  eta_sq <- eta_squared(aov_result, partial = TRUE, ci = 0.95, verbose = FALSE)
  eta_df <- as.data.frame(eta_sq)

  # Format: "value [CI_low, CI_high]"
  eta_df$eta2_formatted <- sprintf("%.3f [%.3f, %.3f]",
                                    eta_df$Eta2_partial,
                                    eta_df$CI_low,
                                    eta_df$CI_high)

  # Match and merge effect sizes
  nice_table$eta2_p <- eta_df$eta2_formatted[match(nice_table$Effect, eta_df$Parameter)]

  # Reorder columns (keeping ges for comparison)
  nice_table <- nice_table[, c("Effect", "df", "MSE", "F", "eta2_p", "ges", "p.value")]
  names(nice_table)[names(nice_table) == "eta2_p"] <- "ηp² [95% CI]"

  # Return styled table
  nice_table %>%
    kable(caption = caption, escape = FALSE) %>%
    kable_styling(bootstrap_options = c("striped", "hover"))
}

#' Create formatted post-hoc comparison table
#'
#' Converts pairwise comparison results into a styled HTML table.
#'
#' @param contrasts_result Contrast result object from emmeans (e.g., from pairs())
#' @param caption Character string for table caption
#' @return A kable table with rounded numeric values and bootstrap styling
create_posthoc_table <- function(contrasts_result, caption) {
  contrasts_result %>%
    as.data.frame() %>%
    mutate(across(where(is.numeric), ~round(.x, 3))) %>%
    kable(caption = caption) %>%
    kable_styling(bootstrap_options = c("striped", "hover"))
}

# Visualization Functions -----------------------------------------------------

#' Create line plot with error bars for experimental data
#'
#' This function creates a publication-ready line plot with points and error bars,
#' supporting up to two grouping variables (color and linetype) and optional faceting.
#'
#' @param data Data frame containing the variables to plot
#' @param x_var Name of x-axis variable (quoted string)
#' @param y_var Name of y-axis variable (quoted string)
#' @param group_var Name of grouping variable for color (quoted string)
#' @param line_var Optional name of grouping variable for linetype (quoted string, default NULL)
#' @param facet_var Optional variable for faceting (quoted string, default NULL)
#' @param title Optional plot title (default NULL)
#' @param x_label X-axis label
#' @param y_label Y-axis label
#' @param group_label Legend label for color grouping
#' @param line_label Legend label for linetype grouping (default NULL)
#'
#' @return A ggplot object with NPG color palette and minimal theme
#'
#' @details
#' - Automatically converts numeric line_var to factor with custom labels
#' - Creates interaction groups for proper positioning when using two grouping variables
#' - Error bars require 'se' column in data
#' - Uses position_dodge to separate overlapping points/lines
create_line_plot <- function(data, x_var, y_var, group_var, line_var = NULL, facet_var = NULL,
                            title = NULL, x_label, y_label, group_label, line_label = NULL) {

  # Handle two-way grouping (color + linetype)
  if (!is.null(line_var)) {
    # Convert numeric line_var to factor with readable labels
    # (e.g., distractor_prevalence: 0.5 -> "Low (0.5)", 0.9 -> "High (0.9)")
    if (is.numeric(data[[line_var]])) {
      data[[line_var]] <- factor(data[[line_var]],
                                levels = sort(unique(data[[line_var]])),
                                labels = c("Low (0.5)", "High (0.9)"))
    }

    # Create interaction group to ensure proper line/point positioning
    # This prevents lines from connecting across different combinations
    data$interaction_group <- interaction(data[[group_var]], data[[line_var]])

    p <- data %>%
      ggplot(aes_string(x = x_var, y = y_var, color = group_var,
                       linetype = line_var, group = "interaction_group")) +
      geom_point(size = 3, position = position_dodge(width = 0.1)) +
      geom_line(position = position_dodge(width = 0.1), linewidth = 1) +
      geom_errorbar(aes(ymin = !!sym(y_var) - se, ymax = !!sym(y_var) + se),
                    width = 0.1, position = position_dodge(width = 0.1)) +
      scale_color_npg() +
      scale_linetype_manual(values = c("solid", "dashed")) +
      labs(x = x_label, y = y_label, color = group_label,
           linetype = line_label %||% line_var)
  } else {
    # Simple one-way grouping (color only)
    p <- data %>%
      ggplot(aes_string(x = x_var, y = y_var, color = group_var, group = group_var)) +
      geom_point(size = 3, position = position_dodge(width = 0.1)) +
      geom_line(position = position_dodge(width = 0.1), linewidth = 1) +
      geom_errorbar(aes(ymin = !!sym(y_var) - se, ymax = !!sym(y_var) + se),
                    width = 0.1, position = position_dodge(width = 0.1)) +
      scale_color_npg() +
      labs(x = x_label, y = y_label, color = group_label)
  }

  # Apply minimal theme
  p <- p +
    theme_classic(base_size = 11) +
    theme(legend.position = "bottom", panel.grid.minor = element_blank(), 
        strip.background = element_blank())

  # Add title if provided
  if (!is.null(title)) {
    p <- p + ggtitle(title)
  }

  # Add faceting if requested
  if (!is.null(facet_var)) {
    p <- p + facet_wrap(as.formula(paste("~", facet_var)), labeller = label_value) +
      theme(strip.text = element_text(size = 10, face = "bold"))
  }

  return(p)
}

#' Create bar plot with error bars for experimental data
#'
#' This function creates a publication-ready bar plot with error bars,
#' supporting up to two grouping variables (fill and pattern) and optional faceting.
#'
#' @param data Data frame containing the variables to plot
#' @param x_var Name of x-axis variable (quoted string)
#' @param y_var Name of y-axis variable (quoted string)
#' @param group_var Name of grouping variable for fill color (quoted string)
#' @param pattern_var Optional name of grouping variable for bar patterns (quoted string, default NULL)
#' @param facet_var Optional variable for faceting (quoted string, default NULL)
#' @param title Optional plot title (default NULL)
#' @param x_label X-axis label
#' @param y_label Y-axis label
#' @param group_label Legend label for fill grouping
#' @param pattern_label Legend label for pattern grouping (default NULL)
#'
#' @return A ggplot object with NPG color palette and minimal theme
#'
#' @details
#' - Automatically converts numeric pattern_var to factor with custom labels
#' - Creates interaction groups for proper positioning when using two grouping variables
#' - Error bars require 'se' column in data
#' - Uses position_dodge to separate grouped bars
create_bar_plot <- function(data, x_var, y_var, group_var, pattern_var = NULL, facet_var = NULL,
                           title = NULL, x_label, y_label, group_label, pattern_label = NULL) {

  # Handle two-way grouping (fill + alpha/pattern)
  if (!is.null(pattern_var)) {
    # Convert numeric pattern_var to factor with readable labels
    # (e.g., distractor_prevalence: 0.5 -> "Low (0.5)", 0.9 -> "High (0.9)")
    if (is.numeric(data[[pattern_var]])) {
      data[[pattern_var]] <- factor(data[[pattern_var]],
                                levels = sort(unique(data[[pattern_var]])),
                                labels = c("Low (0.5)", "High (0.9)"))
    }

    # Create interaction group to ensure proper bar positioning
    data$interaction_group <- interaction(data[[pattern_var]],data[[group_var]])

    p <- data %>%
      ggplot(aes_string(x = x_var, y = y_var, fill = group_var,
                       alpha = pattern_var, group = "interaction_group")) +
      geom_col(position = position_dodge(width = 0.9), color = "black", linewidth = 0.3) +
      geom_errorbar(aes(ymin = !!sym(y_var) - se, ymax = !!sym(y_var) + se),
                    width = 0.2, position = position_dodge(width = 0.9)) +
      scale_fill_npg() +
      scale_alpha_manual(values = c(0.5, 1)) +
      labs(x = x_label, y = y_label, fill = group_label,
           alpha = pattern_label %||% pattern_var)
  } else {
    # Simple one-way grouping (fill only)
    p <- data %>%
      ggplot(aes_string(x = x_var, y = y_var, fill = group_var)) +
      geom_col(position = position_dodge(width = 0.9), color = "black", linewidth = 0.3) +
      geom_errorbar(aes(ymin = !!sym(y_var) - se, ymax = !!sym(y_var) + se),
                    width = 0.2, position = position_dodge(width = 0.9)) +
      scale_fill_npg() +
      labs(x = x_label, y = y_label, fill = group_label)
  }

  # Apply minimal theme
  p <- p +
    theme_classic(base_size = 11) +
    theme(legend.position = "bottom", panel.grid.minor = element_blank(), strip.background = element_blank())

  # Add title if provided
  if (!is.null(title)) {
    p <- p + ggtitle(title)
  }

  # Add faceting if requested
  if (!is.null(facet_var)) {
    p <- p + facet_wrap(as.formula(paste("~", facet_var)), labeller = label_value) +
      theme(strip.text = element_text(size = 10, face = "bold"))
  }

  return(p)
}

#' Plot fixation patterns with stimulus items
#'
#' Creates a spatial visualization of eye fixation sequences overlaid with
#' stimulus locations. Matches the Python implementation from the original analysis.
#'
#' @param fix_data Data frame containing fixation data with columns:
#'   - average_gaze_x or fix_x: x-coordinate of fixation
#'   - average_gaze_y or fix_y: y-coordinate of fixation
#'   - duration: fixation duration (optional, for sizing points)
#'   - tar_pos: target position 0-5 (optional, for stimulus overlay)
#'   - dis_pos: distractor position 0-5 (optional, for stimulus overlay)
#' @param title Optional plot title (default NULL)
#' @param circle_rad Radius for stimulus circle arrangement (default 6)
#'
#' @return A ggplot object showing fixation sequence with stimulus locations
#'
#' @details
#' - Stimulus items arranged in circle at 60-degree intervals (6 positions)
#' - Fixations shown as points sized by duration (if available)
#' - Fixation sequence connected by lines
#' - First two fixations numbered
#' - Target labeled "T", non-targets labeled "N" (black)
#' - Salient distractor labeled "D" (red, at dis_pos)
#' - Coordinate system: -8 to 8 on both axes, fixed aspect ratio
plot_fixations_and_items <- function(fix_data, title = NULL, circle_rad = 6) {
  # Create base plot with fixed coordinate system
  # Matches Python implementation: fixed aspect ratio, black border
  p <- ggplot() +
    coord_fixed(ratio = 1) +
    xlim(-8, 8) +
    ylim(-8, 8) +
    theme_void() +
    theme(
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
      plot.title = element_text(hjust = 0.5, size = 10)
    )

  if (!is.null(title)) {
    p <- p + ggtitle(title)
  }

  # Generate circular coordinates for 6 stimulus positions
  # Matches Python: t = np.arange(0, 301, 60); x = circle_rad * np.cos(t * pi / 180)
  # Positions 0-5 at 0°, 60°, 120°, 180°, 240°, 300°
  angles <- seq(0, 300, by = 60) * pi / 180  # Convert degrees to radians
  circle_x <- circle_rad * cos(angles)
  circle_y <- circle_rad * sin(angles)

  # Add stimulus items if position information is available
  if (nrow(fix_data) > 0 && "tar_pos" %in% names(fix_data) && "dis_pos" %in% names(fix_data)) {
    tar_pos <- fix_data$tar_pos[1]  # Target position (0-5)
    dis_pos <- fix_data$dis_pos[1]  # Distractor position (0-5)

    # Create data frame for stimulus positions
    stimulus_df <- data.frame(
      x = circle_x,
      y = circle_y,
      position = 0:5,
      stringsAsFactors = FALSE
    )

    # Initialize all items as non-targets ("N") in black
    stimulus_df$label <- "N"
    stimulus_df$color <- "black"

    # Mark target position
    if (!is.na(tar_pos)) {
      stimulus_df$label[tar_pos + 1] <- "T"  # +1 for R's 1-based indexing
    }

    # Highlight salient distractor in red with "D" label
    if (!is.na(dis_pos)) {
      stimulus_df$label[dis_pos + 1] <- "D"  # Salient distractor labeled "D"
      stimulus_df$color[dis_pos + 1] <- "red"
    }

    # Add stimulus items as text labels
    p <- p + geom_text(data = stimulus_df,
                      aes(x = x, y = y, label = label, color = I(color)),
                      size = 5, fontface = "bold")
  }

  # Add fixation data if available
  if (nrow(fix_data) > 0) {
    # Handle both column naming conventions from different preprocessing pipelines
    x_col <- if ("average_gaze_x" %in% names(fix_data)) "average_gaze_x" else "fix_x"
    y_col <- if ("average_gaze_y" %in% names(fix_data)) "average_gaze_y" else "fix_y"

    # Verify required columns exist
    if (!x_col %in% names(fix_data) || !y_col %in% names(fix_data)) {
      warning("Required fixation coordinate columns not found in data")
      return(p)
    }

    # Add connecting lines between fixations (scanpath)
    # Matches Python implementation (lw=1)
    if (nrow(fix_data) > 1) {
      p <- p + geom_path(data = fix_data,
                        aes(.data[[x_col]], .data[[y_col]]),
                        color = "steelblue",
                        linewidth = 0.5,
                        alpha = 0.7)
    }

    # Add fixation points, sized by duration if available
    # Matches Python: s=fix1['duration'] * 20
    if ("duration" %in% names(fix_data)) {
      p <- p + geom_point(data = fix_data,
                         aes(.data[[x_col]], .data[[y_col]], size = duration * 20),
                         color = "steelblue", alpha = 0.8) +
        scale_size_identity(guide = "none")
    } else {
      # Fixed size if duration not available
      p <- p + geom_point(data = fix_data,
                         aes(.data[[x_col]], .data[[y_col]]),
                         color = "steelblue", size = 3, alpha = 0.8)
    }

    # Label first two fixations with numbers
    # Matches Python implementation
    if (nrow(fix_data) >= 1) {
      p <- p + geom_text(data = fix_data[1, ],
                        aes(.data[[x_col]], .data[[y_col]]),
                        label = "1", size = 4, color = "black", fontface = "bold")
    }

    if (nrow(fix_data) >= 2) {
      p <- p + geom_text(data = fix_data[2, ],
                        aes(.data[[x_col]], .data[[y_col]]),
                        label = "2", size = 4, color = "black", fontface = "bold")
    }
  }

  return(p)
}
