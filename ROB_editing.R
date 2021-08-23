###code adapted from robvis package
###https://github.com/mcguinlu/robvis/ 


rob_traffic_light_edit <-
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
    
    if (tool %in% c("Generic", "ROB1")) {
      plot <- rob_traffic_light_custom(
        data = data,
        tool = tool,
        rob_colours = rob_colours,
        psize = psize,
        overall = NULL,
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

# ROB-1/Generic=================================================================

rob_traffic_light_custom <- function(data,
                                      tool,
                                      rob_colours,
                                      psize,
                                      overall,
                                      x_title = "Risk of bias domains",
                                      y_title = "Study",
                                      judgement_title = "Judgement",
                                      judgement_labels = c("Critical",
                                                           "High",
                                                           "Unclear",
                                                           "Low")) {
  
  rob1_warning(tool)
  
  max_domain_column <- dim(data)[2] - 1
  
  data.tmp <- data
  
  # Create caption vector, and add line breaks to maintain spacing
  #captiondf <- data.frame(V1 = rep("", 8), stringsAsFactors = FALSE)
  #captiondf$V1 <- c("D1: Representativeness of the sample (selection bias)\n",
                    #"D2: Characteristics of non-respondents (selection bias)\n",
                    #"D3: Symptom assessment (information bias)\n",
                    #"D4: Recording of symptoms (information bias)\n",
                    #"D5: Classification of asymptomatic status (misclassification bias)\n",
                    #"D6: Selective reporting of symptoms status (attrition bias)\n", "", "")
  #caption <- paste(captiondf$V1, collapse = " ")
  # Rename columns headings
  names(data.tmp)[1] <- "Study"
  for (i in 2:max_domain_column) {
    names(data.tmp)[i] <- paste0("D", i - 1)
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
  judgement_levels <- c("c", "h", "s", "l", "n", "x")

  rob.tidy$judgement <-
    factor(rob.tidy$judgement, levels = judgement_levels)
  
  #adjust_caption <- get_caption_adjustment(rob.tidy)
  
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
                 #adjust_caption,
                 overall,
                 judgement_title,
                 overall_name,
                 x_title,
                 y_title) +
    #ggplot2::labs(caption = caption) +
    ggplot2::scale_colour_manual(
      values = c(
        l = rob_colours$low_colour,
        s = rob_colours$concerns_colour,
        h = rob_colours$high_colour,
        c = rob_colours$critical_colour,
        n = rob_colours$ni_colour,
        x = rob_colours$na_colour
      ),
      labels = judgement_labels
    ) +
    ggplot2::scale_shape_manual(values = c(
      l = 43,
      s = 45,
      h = 120,
      c = 33,
      n = 63,
      x = 32
    ),
    labels = judgement_labels)
  
  return(trafficlightplot)
  
}

#test function
trafficlight_rob <- rob_traffic_light_edit(data = rob_data,
                                              tool = "Generic",
                                              psize = 10,
                                              overall = FALSE)

trafficlight_rob

####TO DO
#Rename 6th column 'D6'
#Add 6th question to caption below
#remove no info from judgement