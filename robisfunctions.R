rob_traffic_light <-
  function(data,
           tool,
           colour = "cochrane",
           psize = 10,
           overall = TRUE,
           ...) {
    
    check_tool(tool)
    check_data(data)
    colour <- weird_spelling(colour)
    
    check_colour(tool = tool, colour = colour)
    
    # Define colours
    rob_colours <- get_colour(tool = tool, colour = colour)
    
    if (tool == "ROB2") {
      plot <- rob_traffic_light_rob2(
        data = data,
        tool = tool,
        rob_colours = rob_colours,
        psize = psize,
        overall = overall
      )
    }
    
    if (tool == "ROB2-Cluster") {
      plot <- rob_traffic_light_rob2_cluster(
        data = data,
        tool = tool,
        rob_colours = rob_colours,
        psize = psize,
        overall = overall
      )
    }
    
    if (tool == "ROBINS-I") {
      plot <- rob_traffic_light_robinsi(
        data = data,
        tool = tool,
        rob_colours = rob_colours,
        psize = psize,
        overall = overall
      )
    }
    
    if (tool == "QUADAS-2") {
      plot <- rob_traffic_light_quadas2(
        data = data,
        tool = tool,
        rob_colours = rob_colours,
        psize = psize,
        overall = overall
      )
    }
    
    if (tool == "QUIPS") {
      plot <- rob_traffic_light_quips(
        data = data,
        tool = tool,
        rob_colours = rob_colours,
        psize = psize,
        overall = overall
      )
    }
    
    if (tool %in% c("Generic", "ROB1")) {
      plot <- rob_traffic_light_generic(
        data = data,
        tool = tool,
        rob_colours = rob_colours,
        psize = psize,
        overall = overall,
        ...
      )
    }
    
    # Add recommended saving height to the plot object
    plot$rec_height <- get_height(
      data = data,
      tool = tool,
      psize = psize,
      type = "tf"
    )
    
    # Add recommended saving width to the plot object
    plot$rec_width <- get_width(data = data,
                                psize = psize,
                                type = "tf")
    
    plot$rob_type <- "traffic"
    
    return(plot)
  }

# ROB-2=========================================================================

rob_traffic_light_rob2 <- function(data,
                                   tool,
                                   rob_colours,
                                   psize,
                                   overall) {
  
  max_domain_column <- 7
  domain_names <- c("Study", "1", "2", "3", "4", "5", "Overall")
  
  rob.tidy <- tidy_data(data,
                        max_domain_column = max_domain_column,
                        domain_names = domain_names,
                        overall = overall,
                        levels = c("h", "s", "l", "n", "x"))
  
  ssize <- psize - (psize / 4)
  
  adjust_caption <- get_caption_adjustment(rob.tidy)
  
  trafficlightplot <-
    ggplot2::ggplot(rob.tidy,
                    ggplot2::aes(x = 1,
                                 y = 1,
                                 colour = judgement)) +
    rob_tf_theme(rob.tidy,
                 domain_names,
                 psize,
                 ssize,
                 adjust_caption,
                 overall) +
    ggplot2::labs(
      caption = "  Domains:
  1: 1
  2: 2
  3: 3
  4: 4
  5: 5
                "
    ) +
    ggplot2::scale_colour_manual(
      values = c(
        h = rob_colours$high_colour,
        s = rob_colours$concerns_colour,
        l = rob_colours$low_colour,
        n = rob_colours$ni_colour,
        x = rob_colours$na_colour
      ),
      labels = c(
        h = "High",
        s = "Some concerns",
        l = "Low",
        n = "No information",
        x = "Not applicable"
      )
    ) +
    ggplot2::scale_shape_manual(
      values = c(
        h = 120,
        s = 45,
        l = 43,
        n = 63,
        x = 32
      ),
      labels = c(
        h = "High",
        s = "Some concerns",
        l = "Low",
        n = "No information",
        x = "Not applicable"
      )
    )
  
  return(trafficlightplot)
}

# ROB-2 Cluster=================================================================

rob_traffic_light_rob2_cluster <- function(data,
                                           tool,
                                           rob_colours,
                                           psize,
                                           overall) {
  
  
  max_domain_column <- 8
  domain_names <- c("Study", "1", "2", "3", "4", "5", "6", "Overall")
  
  rob.tidy <- tidy_data(data,
                        max_domain_column = max_domain_column,
                        domain_names = domain_names,
                        overall = overall,
                        levels = c("h", "s", "l", "n", "x"))
  
  ssize <- psize - (psize / 4)
  
  adjust_caption <- get_caption_adjustment(rob.tidy)
  
  trafficlightplot <- ggplot2::ggplot(rob.tidy,
                                      ggplot2::aes(x = 1,
                                                   y = 1,
                                                   colour = judgement)) +
    rob_tf_theme(rob.tidy,
                 domain_names,
                 psize,
                 ssize,
                 adjust_caption,
                 overall) +
    ggplot2::labs(
      caption = "  Domains:
  1:  Bias arising from the randomization process.
  2: Bias arising from the timing of identification
          and recruitment of Individual participants in
          relation to timing of randomization.
  3:  Bias due to deviations from intended intervention.
  4:  Bias due to missing outcome data.
  5:  Bias in measurement of the outcome.
  6:  Bias in selection of the reported result.
                "
    ) +
    ggplot2::scale_colour_manual(
      values = c(
        h = rob_colours$high_colour,
        s = rob_colours$concerns_colour,
        l = rob_colours$low_colour,
        n = rob_colours$ni_colour,
        x = rob_colours$na_colour
      ),
      labels = c(
        h = "High",
        s = "Some concerns",
        l = "Low",
        n = "No information",
        x = "Not applicable"
      )
    ) +
    ggplot2::scale_shape_manual(
      values = c(
        h = 120,
        s = 45,
        l = 43,
        n = 63,
        x = 32
      ),
      labels = c(
        h = "High",
        s = "Some concerns",
        l = "Low",
        n = "No information",
        x = "Not applicable"
      )
    )
  
  
  return(trafficlightplot)
}



# ROBINS-I======================================================================

rob_traffic_light_robinsi <- function(data,
                                      tool,
                                      rob_colours,
                                      psize,
                                      overall) {
  
  
  max_domain_column <- 9
  domain_names <-
    c("Study", "1", "2", "3", "4", "5", "6", "7", "Overall")
  
  rob.tidy <- tidy_data(data,
                        max_domain_column = max_domain_column,
                        domain_names = domain_names,
                        overall = overall,
                        levels = c("c", "s", "m", "l", "n", "x"))
  
  ssize <- psize - (psize / 4)
  
  adjust_caption <- get_caption_adjustment(rob.tidy)
  
  trafficlightplot <- ggplot2::ggplot(rob.tidy,
                                      ggplot2::aes(x = 1,
                                                   y = 1,
                                                   colour = judgement)) +
    rob_tf_theme(rob.tidy,
                 domain_names,
                 psize,
                 ssize,
                 adjust_caption,
                 overall) +
    ggplot2::labs(
      caption = "  Domains:
  1: Bias due to confounding.
  2: Bias due to selection of participants.
  3: Bias in classification of interventions.
  4: Bias due to deviations from intended interventions.
  5: Bias due to missing data.
  6: Bias in measurement of outcomes.
  7: Bias in selection of the reported result.
                  "
    ) +
    ggplot2::scale_colour_manual(
      values = c(
        c = rob_colours$critical_colour,
        s = rob_colours$high_colour,
        m = rob_colours$concerns_colour,
        l = rob_colours$low_colour,
        n = rob_colours$ni_colour,
        x = rob_colours$na_colour
      ),
      labels = c(
        c = "Critical",
        s = "Serious",
        m = "Moderate",
        l = "Low",
        n = "No information",
        x = "Not applicable"
      )
    ) +
    ggplot2::scale_shape_manual(
      values = c(
        c = 33,
        s = 120,
        m = 45,
        l = 43,
        n = 63,
        x = 32
      ),
      labels = c(
        c = "Critical",
        s = "Serious",
        m = "Moderate",
        l = "Low",
        n = "No information",
        x = "Not applicable"
      )
    )
  
  return(trafficlightplot)
}

# QUADAS-2======================================================================


rob_traffic_light_quadas2 <- function(data,
                                      tool,
                                      rob_colours,
                                      psize,
                                      overall) {
  
  max_domain_column <- 6
  domain_names <- c("Study", "1", "2", "3", "4", "Overall")
  
  rob.tidy <- tidy_data(data,
                        max_domain_column = max_domain_column,
                        domain_names = domain_names,
                        overall = overall,
                        levels = c("h", "s", "l", "n", "x"))
  
  ssize <- psize - (psize / 4)
  
  adjust_caption <- get_caption_adjustment(rob.tidy)
  
  trafficlightplot <- ggplot2::ggplot(rob.tidy,
                                      ggplot2::aes(x = 1,
                                                   y = 1,
                                                   colour = judgement)) +
    rob_tf_theme(rob.tidy,
                 domain_names,
                 psize,
                 ssize,
                 adjust_caption,
                 overall) +
    ggplot2::labs(
      caption = "  Domains:
  1: Patient selection.
  2: Index test.
  3: Reference standard.
  4: Flow & timing.
                  "
    ) +
    ggplot2::scale_colour_manual(
      values = c(
        h = rob_colours$high_colour,
        s = rob_colours$concerns_colour,
        l = rob_colours$low_colour,
        n = rob_colours$ni_colour,
        x = rob_colours$na_colour
      ),
      labels = c(
        h = "High",
        s = "Some concerns",
        l = "Low",
        n = "No information",
        x = "Not applicable"
      )
    ) +
    ggplot2::scale_shape_manual(
      values = c(
        h = 120,
        s = 45,
        l = 43,
        n = 63,
        x = 32
      ),
      labels = c(
        h = "High",
        s = "Some concerns",
        l = "Low",
        n = "No information",
        x = "Not applicable"
      )
    )
  
  return(trafficlightplot)
}

# QUIPS ========================================================================


rob_traffic_light_quips <- function(data,
                                    tool,
                                    rob_colours,
                                    psize,
                                    overall) {
  
  max_domain_column <- 8
  #domain_names <- c("Study", "D1", "D2", "D3", "D4", "D5", "D6", "Overall")
  #domain_names <- c("Study", "D1.1", "D1.2", "D2.1", "D2.2", "D3", "D4", "Overall")
  domain_names <- c("Study", 1, 2, 3, 4, 5, 6, "Overall")
  rob.tidy <- tidy_data(data,
                        max_domain_column = max_domain_column,
                        domain_names = domain_names,
                        overall = overall,
                        levels = c("h", "m", "l", "n","x"))
  
  ssize <- psize - (psize / 4)
  
  adjust_caption <- get_caption_adjustment(rob.tidy)
  
  trafficlightplot <- ggplot2::ggplot(rob.tidy,
                                      ggplot2::aes(x = 1,
                                                   y = 1,
                                                   colour = judgement)) +
    rob_tf_theme(rob.tidy,
                 domain_names,
                 psize,
                 ssize,
                 adjust_caption,
                 overall) +
    ggplot2::labs(
      caption = "  Domains:
  1: Bias due to participation.
  2: Bias due to attrition.
  3: Bias due to prognostic factor measurement.
  4: Bias due to outcome measurement.
  5: Bias due to confounding.
  6: Bias in statistical analysis and reporting.
                  "
    ) +
    ggplot2::scale_colour_manual(
      values = c(
        h = rob_colours$high_colour,
        m = rob_colours$concerns_colour,
        l = rob_colours$low_colour,
        n = rob_colours$ni_colour,
        x = rob_colours$na_colour
      ),
      labels = c(
        h = "High",
        m = "Moderate",
        l = "Low",
        n = "No information",
        x = "Not applicable"
      )
    ) +
    ggplot2::scale_shape_manual(
      values = c(
        h = 120,
        m = 45,
        l = 43,
        n = 63,
        x = 32
      ),
      labels = c(
        h = "High",
        m = "Moderate",
        l = "Low",
        n = "No information",
        x = "Not applicable"
      )
    )
  
  return(trafficlightplot)
}

# ROB-1/Generic=================================================================

rob_traffic_light_generic <- function(data,
                                      tool,
                                      rob_colours,
                                      psize,
                                      overall,
                                      x_title = "Risk of bias items",
                                      y_title = "Study",
                                      judgement_title = "Judgement",
                                      judgement_labels = c("High",
                                                           "Unclear",
                                                           "Low"
                                                           )) {
  
  rob1_warning(tool)
  
  # Determine if the uploaded dataset contains weights
  if (unique(grepl("^[-]{0,1}[0-9]{0,}.{0,1}[0-9]{1,}$",
                   data[[ncol(data)]])) == TRUE) {
    for (i in 2:(ncol(data) - 1)) {
      # Convert "serious" to "high" so that it maps appropriately
      data[[i]] <- gsub("\\bse", "High", stringr::str_to_lower(data[[i]]))
      data[[i]] <- clean_data(data[[i]])
      data[[i]] <- gsub("u", "s", data[[i]])
      data[[i]] <- gsub("m", "s", data[[i]])
    }
    
    # Select relevant columns, excluding the 'Weights' column
    data <- data[, c(1:(ncol(data) - 1))]
  } else {
    for (i in 2:(ncol(data))) {
      # Convert "serious" to "high" so that it maps appropriately
      data[[i]] <- gsub("\\bse", "High", stringr::str_to_lower(data[[i]]))
      data[[i]] <- clean_data(data[[i]])
      data[[i]] <- gsub("u", "s", data[[i]])
      data[[i]] <- gsub("m", "s", data[[i]])
    }
  }
  
  max_domain_column <- dim(data)[2] - 1
  
  data.tmp <- data
  
  # Remove dots from column names
  for (i in 1:(ncol(data.tmp))) {
    names(data.tmp)[i] <- invisible(gsub(".", " ",
                                         names(data.tmp)[i],
                                         fixed = TRUE))
  }
  
  # Create caption vector, and add line breaks to maintain spacing
  captiondf <- data.frame(V1 = rep("", 8), stringsAsFactors = FALSE)
  for (i in 2:max_domain_column) {
    if (i == 2) {
      captiondf[i - 1, 1] <- paste0("", i - 1,
                                    ": ", names(data.tmp)[i], "\n")
    } else {
      captiondf[i - 1, 1] <- paste0("", i - 1, ": ",
                                    names(data.tmp)[i], "\n")
    }
  }
  
  captiondf[captiondf == ""] <- "\n"
  
  caption <- paste(captiondf$V1, collapse = " ")
  # Rename columns headings
  names(data.tmp)[1] <- "Study"
  for (i in 2:max_domain_column) {
    names(data.tmp)[i] <- paste0("", i - 1)
  }
  
  # Convert to long format
  rob.tidy <- suppressWarnings(tidyr::gather(data.tmp,
                                             domain, judgement,-Study))
  
  # Relevel rob.tidy$domain
  for (i in 1:(ncol(data.tmp))) {
    levels(rob.tidy$domain)[i] <- colnames(data.tmp)[i]
  }
  
  domain_names <- levels(rob.tidy$domain)
  
  # rob.tidy$domain <-
  #   factor(rob.tidy$domain, levels = levels(rob.tidy$domain))
  
  rob.tidy$Study <-
    factor(rob.tidy$Study, levels = unique(data.tmp$Study))
  
  rob.tidy$judgement <- as.factor(rob.tidy$judgement)
  # add judgment levels variable
  judgement_levels <- c("h", "s", "l")
  
  rob.tidy$judgement <-
    factor(rob.tidy$judgement, levels = judgement_levels)
  
  adjust_caption <- get_caption_adjustment(rob.tidy)
  
  # Set sizes
  ssize <- psize - (psize / 4)
  
  # name the provided judgement labels with appropriate judgement levels to
  # enable this to be passed as a named character variable to the
  # ggplot::scale_colour_manual()
  names(judgement_labels) <- judgement_levels
  
  overall_name <- names(data.tmp)[length(names(data.tmp))]
  
  # PLot graph
  trafficlightplot <- ggplot2::ggplot(rob.tidy,
                                      ggplot2::aes(x = 1,
                                                   y = 1,
                                                   colour = judgement)) +
    rob_tf_theme(rob.tidy,
                 domain_names,
                 psize,
                 ssize,
                 adjust_caption,
                 overall,
                 judgement_title,
                 overall_name,
                 x_title,
                 y_title) +
    ggplot2::labs(caption = caption) +
    ggplot2::scale_colour_manual(
      values = c(
        l = rob_colours$low_colour,
        s = rob_colours$concerns_colour,
        h = rob_colours$high_colour
      ),
      labels = judgement_labels
    ) +
    ggplot2::scale_shape_manual(values = c(
      l = 43,
      s = 45,
      h = 120
    ),
    labels = judgement_labels)
  
  return(trafficlightplot)
  
}

check_cols <- function(data,
                       max_domain_column,
                       overall,
                       type = "tf",
                       weight = FALSE){
  
  expected_col <- max_domain_column + 1
  
  if (!overall & !weight) {
    expected_col <- expected_col - 2
    domain_text = paste0(expected_col,
                         ": a \"Study\" column and ",
                         max_domain_column - 2,
                         " \"Domain\" columns.")
    var_ind <- "neither"
    
  }
  
  if (!overall & weight) {
    expected_col <- expected_col - 1
    domain_text = paste0(
      expected_col,
      ": a \"Study\" column, ",
      max_domain_column - 1,
      " \"Domain\" columns, and \"Weight\" column."
    )
    var_ind <- "weight"
  }
  
  if (overall & !weight) {
    expected_col <- expected_col - 1
    domain_text = paste0(
      expected_col,
      ": a \"Study\" column, ",
      max_domain_column - 1,
      " \"Domain\" columns, and an \"Overall\" column."
    )
    var_ind <- "overall"
  }
  
  if (overall & weight) {
    expected_col <- expected_col
    domain_text = paste0(
      expected_col,
      ": a \"Study\" column, ",
      max_domain_column - 2,
      " \"Domain\" columns, an \"Overall\" column and a \"Weight\" column."
    )
    var_ind <- "both"
  }
  
  if (type == "summ") {
    weighted_text <- paste(" and weighted =", weight)
  } else {
    weighted_text <- ""
  }
  
  if (ncol(data) == expected_col) {
    if ((var_ind %in% c("both", "weight")) &&
        unique(grepl("^[-]{0,1}[0-9]{0,}.{0,1}[0-9]{1,}$",
                     data[[ncol(data)]])) == FALSE) {
      stop(
        "Error. The final column does not seem to contain numeric values ",
        "(expected for weighted = TRUE)."
      )
    }} else {
      if (ncol(data) != expected_col) {
        stop(
          "The number of columns in your data (",
          ncol(data),
          ") does not match the number expected for this",
          " tool when using overall = ", overall, weighted_text,
          ". The expected number of columns is ",
          domain_text
        )}
    }
}


tidy_data <- function(data,
                      max_domain_column,
                      domain_names,
                      overall,
                      levels) {
  
  # Deal with legacy versions of the example datasets
  # if (ncol(data) == max_domain_column + 1) {
  #   if (overall == FALSE) {
  #     data <- data[,c(1:(ncol(data)-2))]
  #   } else {
  #     data <- data[,-ncol(data)]
  #   }
  # }
  
  check_cols(data = data,
             max_domain_column = max_domain_column,
             overall = overall,
             weight = FALSE)
  
  if (!overall) {
    max_domain_column <- max_domain_column - 1
    domain_names <- domain_names[1:max_domain_column]
  }
  
  data.tmp <-
    cbind(data[, 1], data.frame(lapply(data[, 2:max_domain_column], clean_data),
                                stringsAsFactors = F))
  
  names(data.tmp) <- domain_names
  
  rob.tidy <- suppressWarnings(tidyr::gather(data.tmp,
                                             domain, judgement,-Study))
  
  
  rob.tidy$Study <-
    factor(rob.tidy$Study, levels = unique(data.tmp$Study))
  
  rob.tidy$judgement <- as.factor(rob.tidy$judgement)
  
  rob.tidy$judgement <-
    factor(rob.tidy$judgement, levels = levels)
  
  rob.tidy
}

tidy_data_summ <- function(data,
                           max_domain_column,
                           overall,
                           weighted,
                           domain_names,
                           levels) {
  
  # Deal with legacy versions of the example datasets
  if (ncol(data) == max_domain_column + 1) {
    if (overall == FALSE) {
      data <- data[,c(1:max_domain_column-1,max_domain_column + 1)]
    }
    if (weighted == FALSE) {
      data <- data[,-ncol(data)]
    }
  }
  
  # Check columns are as expected, given the options
  check_cols(
    data = data,
    max_domain_column = max_domain_column,
    overall = overall,
    type = "summ",
    weight = weighted
  )
  
  if (overall ==  FALSE) {
    max_domain_column <- max_domain_column - 1
    domain_names <- domain_names[c(1:max_domain_column, length(domain_names))]
  }
  
  if (weighted == FALSE) {
    data[, max_domain_column + 1] <- rep(1, length(nrow(data)))
  }
  
  data.tmp <-
    cbind(data[,1],data.frame(lapply(data[, 2:max_domain_column], clean_data),
                              data[, ncol(data)],
                              stringsAsFactors = F))
  
  names(data.tmp) <- domain_names
  
  rob.tidy <- suppressWarnings(tidyr::gather(
    data.tmp[-1],
    domain, judgement, -Weights
  ))
  
  rob.tidy$domain <- as.factor(rob.tidy$domain)
  
  rob.tidy$domain <-
    factor(rob.tidy$domain,
           levels = rev(domain_names))
  
  rob.tidy$judgement <-
    factor(rob.tidy$judgement, levels = levels)
  
  rob.tidy
}


rob_summ_theme <- function(overall = TRUE, max_domain_column){
  standard <- list(
    ggplot2::geom_bar(
      mapping = ggplot2::aes(
        x = domain,
        fill = judgement,
        weight = Weights
      ),
      width = 1,
      position = "fill",
      color = "black"
    ),
    ggplot2::coord_flip(ylim = c(
      0,
      1
    )),
    ggplot2::guides(fill = ggplot2::guide_legend(reverse = TRUE)),
    ggplot2::scale_y_continuous(labels = scales::percent),
    ggplot2::theme(
      axis.title.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      axis.line.x = ggplot2::element_line(
        colour = "black",
        size = 2,
        linetype = "solid"
      ),
      legend.position = "bottom",
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank(),
      legend.background = ggplot2::element_rect(
        linetype = "solid",
        colour = "black"
      ),
      legend.title = ggplot2::element_blank(),
      legend.key.size = ggplot2::unit(1, "lines"),
      legend.text = ggplot2::element_text(size = 15)
    ),
    bold_overall = ggplot2::theme(axis.text.y = ggplot2::element_text(
      size = 15,
      color = "black"
    ))
  )
  
  if (overall) {
    standard[["bold_overall"]] <-
      ggplot2::theme(axis.text.y = ggtext::element_markdown(size = 15,
                                                            color = "black",
                                                            face = c("bold", rep("plain",max_domain_column))))
  }
  
  return(standard)
}


rob_tf_theme <-function(rob.tidy,
                        domain_names,
                        psize,
                        ssize,
                        adjust_caption,
                        overall,
                        judgement_title = "Judgement",
                        overall_name = "Overall",
                        x_title = "Risk of bias items",
                        y_title = "Study"){
  standard <- list(
    ggplot2::facet_grid(Study ~
                          factor(domain, levels = domain_names),
                        switch = "y",
                        space = "free"),
    ggplot2::geom_point(size = 4),
    ggplot2::geom_point(size = 2,
                        colour = "black",
                        #colour = "white",
                        ggplot2::aes(shape = judgement)),
    ggplot2::geom_rect(
      data = rob.tidy[which(rob.tidy$domain !=
                              overall_name),],
      fill = "#ffffff",
      xmin = -Inf,
      xmax = Inf,
      ymin = -Inf,
      ymax = Inf,
      show.legend = FALSE
    ),
    overall_name = ggplot2::geom_rect(
      data = rob.tidy[which(rob.tidy$domain ==
                              overall_name),],
      fill = "#d3d3d3",
      xmin = -Inf,
      xmax = Inf,
      ymin = -Inf,
      ymax = Inf,
      show.legend = FALSE
    ),
    ggplot2::geom_point(size = psize, show.legend = FALSE),
    ggplot2::geom_point(
      data = rob.tidy[which(rob.tidy$judgement !=
                              "x"),],
      shape = 1,
      colour = "black",
      size = psize,
      show.legend = FALSE
    ),
    ggplot2::geom_point(
      size = ssize,
      colour = "black",
      ggplot2::aes(shape = judgement),
      show.legend = FALSE
    ),
    ggplot2::scale_x_discrete(position = "top", name = x_title),
    ggplot2::scale_y_continuous(
      limits = c(1, 1),
      labels = NULL,
      breaks = NULL,
      name = y_title,
      position = "left"
    ),
    ggplot2::scale_size(range = c(5,20)),
    ggplot2::theme_bw(),
    ggplot2::theme(
      panel.border = ggplot2::element_rect(colour = "grey"),
      panel.spacing = ggplot2::unit(0, "line"), legend.position = "bottom",
      legend.justification = "right", legend.direction = "vertical",
      legend.margin = ggplot2::margin(
        t = -0.2, r = 0,
        b = adjust_caption, l = -10, unit = "cm"
      ),
      strip.text.x = ggplot2::element_text(size = 15),
      strip.text.y.left = ggplot2::element_text(
        angle = 0,
        size = 10
      ), legend.text = ggplot2::element_text(size = 15),
      legend.title = ggplot2::element_text(size = 20),
      strip.background = ggplot2::element_rect(fill = "#ffffff"),
      plot.caption = ggplot2::element_text(
        size = 15,
        hjust = 0, vjust = 1
      )
    ),
    ggplot2::guides(shape = ggplot2::guide_legend(
      override.aes = list(fill = NA))),
    ggplot2::labs(shape = judgement_title, colour = judgement_title)
    
  )
  
  # Remove element that draws dark box for "Overall" column
  if (!overall) {
    standard[["overall_name"]] <- ggplot2::geom_blank()
  }
  
  return(standard)
  
}


get_caption_adjustment <- function(data){
  -0.7 + length(unique(data$judgement)) * -0.6
}

check_data <- function(data){
  
  header <- stringr::str_to_lower(data[1,])
  
  if (any(c("study","overall", "weight") %in% header)) {
    stop(paste("It looks like the first row of your dataset contains column",
               "headings (e.g. \"Study\", \"Overall\"). Did you set ",
               "\"header = TRUE\" when reading in your data?")
    )
  }
  
}

# Check colours
check_colour <- function(tool, colour) {
  if(!(colour[1] %in% c("cochrane","colourblind"))){
    if (tool == "ROB2" || tool == "ROB2-Cluster" || tool == "QUADAS-2") {
      if(length(colour)!=4){
        stop(paste("Wrong number of colours specified.",
                   "This template expects 4 colours."))
      }
      
    } else{
      if(length(colour)!=5){
        stop(paste("Wrong number of colours specified.",
                   "This template expects 5 colours."))
      }
    }
  }
}

# Define colours used
get_colour <- function(tool, colour) {
  rob_colours <- c()
  
  rob_colours$na_colour <- "#cccccc"
  
  if (tool == "ROB2" || tool == "ROB2-Cluster" || tool == "QUADAS-2") {
    if (length(colour) > 1) {
      rob_colours$low_colour <- colour[c(1)]
      rob_colours$concerns_colour <- colour[c(2)]
      rob_colours$high_colour <- colour[c(3)]
      rob_colours$ni_colour <- colour[c(4)]
    } else {
      if (colour == "colourblind") {
        rob_colours$low_colour <- "#fed98e"
        rob_colours$concerns_colour <- "#fe9929"
        rob_colours$high_colour <- "#d95f0e"
        rob_colours$ni_colour <- "#ffffff"
      }
      if (colour == "cochrane") {
        rob_colours$low_colour <- "#02C100"
        rob_colours$concerns_colour <- "#E2DF07"
        rob_colours$high_colour <- "#BF0000"
        rob_colours$ni_colour <- "#4EA1F7"
      }
    }
  } else {
    if (length(colour) > 1) {
      rob_colours$low_colour <- colour[c(1)]
      rob_colours$concerns_colour <- colour[c(2)]
      rob_colours$high_colour <- colour[c(3)]
      rob_colours$critical_colour <- colour[c(4)]
      rob_colours$ni_colour <- colour[c(5)]
    } else {
      if (colour == "colourblind") {
        rob_colours$low_colour <- "#fed98e"
        rob_colours$concerns_colour <- "#fe9929"
        rob_colours$high_colour <- "#d95f0e"
        rob_colours$critical_colour <- "#993404"
        rob_colours$ni_colour <- "#ffffff"
      }
      if (colour == "cochrane") {
        rob_colours$low_colour <- "#02C100"
        rob_colours$concerns_colour <- "#E2DF07"
        rob_colours$high_colour <- "#BF0000"
        rob_colours$critical_colour <- "#820000"
        rob_colours$ni_colour <- "#4EA1F7"
      }
    }
  }
  
  return(rob_colours)
}

# Make sure specified tool is allowed
check_tool <- function(tool, forest = FALSE) {
  
  if (forest) {
    tools <- c(suppressMessages(rob_tools(forest = TRUE)))
    message_content <- "rob_tools(forest = TRUE)"
  }else {
    tools <- c(suppressMessages(rob_tools()), "ROB1")
    message_content <- "rob_tools()"
  }
  
  if ((tool %in% tools) == FALSE) {
    stop(
      paste(
        "\nTool name \"",
        tool,
        "\" not recognised \nAcceptable tools" ,
        "names can be found by running",
        message_content
      )
    )
  }
}

# Give depreciated warning if "ROB1" specified
rob1_warning <- function(tool) {
  if (tool == "ROB1") {
    message(
      paste0(
        "Note: In future versions of robvis, the 'tool = \"ROB1\"' ",
        "argument will be depreciated.\n",
        "Please use 'tool = \"Generic\"' instead."
      )
    )
  }
}

# Get recommended height of figure, used by rob_save
get_height <- function(data, tool, psize, type = "tf") {
  if (type == "tf") {
    tool_adj <- ifelse(tool %in% c("ROBINS-I", "Generic"), 2.5, 2)
    nrows <- nrow(data)
    height <- tool_adj + nrows * .4 / (10 / psize)
  } else {
    height <- 2.41
  }
  return(height)
}

# Get recommended width of figure, used by rob_save
get_width <- function(data, psize, type = "tf") {
  if (type == "tf") {
    # Account for long study names
    nchar_study <- max(nchar(as.character(data$Study)))
    nchar_domain <- max(nchar(as.character(colnames(data)))) + 3
    width_adj <- ifelse(nchar_study > 8, 6 + nchar_study * 0.05, 6)
    width <-
      ifelse(nchar_domain > 42,
             width_adj + (nchar_domain - 42) * 0.05,
             width_adj
      )
  } else {
    width <- 8
  }
  return(width)
}

# Allow for US spelling of "colourblind"
weird_spelling <- function(colour) {
  colour <- ifelse(colour == "colorblind", "colourblind", colour)
  return(colour)
}

# Acceptable file extensions for saving

get_extension <- function(file){
  ex <- strsplit(basename(file), split="\\.")[[1]]
  return(ex[-1])
}

check_extension <- function(file){
  if (!(get_extension(file) %in% c("png","jpeg","tiff","eps"))) {
    stop(paste0("Saving to this file type is not supported by robvis. ",
                "Acceptable file types are \".png\", \".jpeg\", ",
                " \".tiff\", and \".eps\". "))
  }
}


clean_data <- function(col) {
  col <- gsub("\\b(\\pL)\\pL{1,}|.", "\\U\\1", col, perl = TRUE)
  col <- trimws(tolower(col))
  col <- ifelse(col %in% c("na", "n") | is.na(col), "x", col)
  col <- substr(col, 0, 1)
  return(col)
}

rob_tools <- function(forest = FALSE) {
  
  if (forest) {
    tools <- c("ROB2",
               "ROBINS-I")
  } else {
    tools <- c("ROB2",
               "ROB2-Cluster",
               "ROBINS-I",
               "QUADAS-2",
               "QUIPS",
               "Generic"
    )
    message(
      paste0("Note: the \"ROB2-Cluster\" template is only available ",
             "for the rob_traffic_light() function.")
    )
  }
  
  
  return(tools)
}

#' Extract weights from metafor results object and append to risk-of-bias data.
#'
#' @description Used to prepare a risk-of-bias dataset to be passed to the
#'   weighted barplot function: rob_summary(..., weighted = TRUE)
#'
#' @param data Risk of bias dataset (without a weight column)
#' @param res metafor results object
#'
#' @family helper
#'
#' @export
#'
#' @examples
#' \donttest{
#' dat.bcg <- metafor::dat.bcg[c(1:9),]
#'
#' dat <-
#'   metafor::escalc(
#'     measure = "RR",
#'     ai = tpos,
#'     bi = tneg,
#'     ci = cpos,
#'     di = cneg,
#'     data = dat.bcg,
#'     slab = paste(author, year)
#'   )
#'
#' res <- metafor::rma(yi, vi, data = dat)
#'
#' data_rob2$Study <- paste(dat$author,dat$year)
#'
#' rob_weighted_data <- rob_append_weights(data_rob2[,1:7], res)
#'
#' rob_summary(rob_weighted_data, tool = "ROB2", weighted = TRUE)
#' }

rob_append_weights <- function(data, res){
  
  if (!("rma" %in% class(res))) {
    stop("Result objects need to be of class \"meta\" - output from metafor package functions")
  }
  
  # Extract weights
  weights <- data.frame(Study = names(stats::weights(res)),
                        Weight = stats::weights(res),
                        row.names = NULL)
  
  # Merge by Study name to create new dataframe
  rob_df <- dplyr::left_join(data, weights, by = "Study")
  
  # Employ check to see if data has merged properly If a merge has failed, one
  # of the Weight cells will be NA, meaning the sum will also be NA
  if (is.na(sum(rob_df$Weight))) {
    stop(paste0("Problem with matching - weights do not equal 100. ",
                "Check that the names of studies are the same in the ROB ",
                "data and the res object (stored in slab)"))
  }
  
  return(rob_df)
}
