## PSI trajectories
#plot_1 = function(){
  
  ## helper functions:
  # function to get confidence intervals used in plot_intervals.
  ci <- function(x) {
    list <- list(
      which(cumsum(x) / sum(x) > 0.025)[1],
      last(which(cumsum(x) / sum(x) < 0.975))
    )
    return(list)
  }
  
  #get the lines and the confidence iiintervals
  #function that uses ci()
  
  get_line_intervals <- function(data) {
    #lower and lower conf. interval
    upper <- array(NA, np$size(data[, 1, 1]))
    lower <- array(NA, np$size(data[, 1, 1]))
    
    #loop through the np array
    for (i in 1:np$size(data[, 1, 1])) {
      
      confidence <- ci(rowMeans(data, dims = 2)[i, ])
      rg <- seq(-50.5, 50.5, by = 1)
      upper[i] <- rg[confidence[[1]]]
      lower[i] <- rg[confidence[[2]]]
    }
    
    data <- data.frame(upper = upper, lower = lower, x = seq(0, nrow(upper), length.out = nrow(upper)))
    
    return(data)
  }
  
  #function that uses get_line_intervals(.)
  plot_interval <- function(df, interoPost = NA) {
    d <- data.frame()
    dd <- data.frame()
    
    
    if (!is.na(interoPost)[1] == TRUE) {
      dd <- get_line_intervals(interoPost)
    }
    d <- rbind(d, dd) %>% mutate(trials = 1:n())
    
    dataline <- df %>%
      filter(TrialType == "psi") %>%
      filter(Modality == "Intero") %>%
      mutate(x = seq(0, nrow(.), length.out = (nrow(.))))
    
    
    # making trials go from 0-60 in each Modality
    df <- df %>%
      group_by(Modality) %>%
      mutate(trials = 1:n()) %>%
      ungroup()
    
    q = inner_join(d,df, by = "trials")
    return(q)
  }
  
  
  # function that uses plot_interval(.)
  intervals = function(df, interoPost = NA){
    
    df$ResponseCorrect <- ifelse(df$ResponseCorrect == "", NA, df$ResponseCorrect)
    df$Decision <- ifelse(df$Decision == "", NA, df$Decision)
    
    df <- df %>%
      mutate(
        Decision = as.character(df$Decision),
        ConfidenceRT = as.numeric(df$ConfidenceRT),
        DecisionRT = as.numeric(df$DecisionRT),
        Confidence = as.numeric(df$Confidence),
        Condition = as.character(df$Condition),
        listenBPM = as.numeric(df$listenBPM),
        responseBPM = as.numeric(df$responseBPM),
        ResponseCorrect = as.numeric(as.factor(df$ResponseCorrect)) - 1,
        EstimatedThreshold = as.numeric(df$EstimatedThreshold),
        EstimatedSlope = as.numeric(df$EstimatedSlope),
      )
    
    # check for NA's in the critical columns of the data:
    # specify different ways of coding missing values:
    missing_pres <- c(NA, "na", "N/A", "NaN", NaN, "n/a")
    
    trials_missing <- df %>%
      select(Decision, Confidence, ConfidenceRT, Confidence, Condition, listenBPM, responseBPM, nTrials) %>%
      filter_all(any_vars(. %in% missing_pres)) %>%
      .$nTrials
    
    
    
    if (length(trials_missing) != 0) {
      print(paste("Number of NA's = ", length(trials_missing), " detected in trials : "))
      print(as.character(trials_missing))
    }
    
    # remove the NA's
    df1 <- df %>% filter(!nTrials %in% trials_missing)
    
    interval <- plot_interval(df1, interoPost)
    
    return(interval)
    
  }
  
  
  #psychometric equation HRD.
  psycho_fit = function(x, alpha,lapse,beta){
    
    return(lapse + (1 - 2 * lapse) * (0.5 + 0.5 *pracma::erf((x-alpha)/(beta*sqrt(2)))))
  }
  
  #getting individual subject posterior fits HRD (for the chosen subject)
  get_indi = function(model,draw_id){
    
    individual_params = as_draws_df(model$draws(paste0("param[11,",1:11,"]")))
    
    alpha_beta_lapse = individual_params %>% 
      select(-contains(".")) %>% 
      mutate(draw = 1:n()) %>% 
      pivot_longer(-draw, names_to = "variable") %>% 
      filter(draw %in% draw_id) %>% 
      mutate(parameter = as.numeric(sub(".*,(\\d+)\\]", "\\1", variable))) %>% 
      mutate(subject = as.numeric(gsub("^param\\[(\\d+),\\d+\\]", "\\1", variable))) %>% 
      mutate(parameter = ifelse(parameter == 1, "alpha_placebo1",
                                ifelse(parameter == 2, "alpha_biso_dif",
                                       ifelse(parameter == 3, "alpha_prop_dif",
                                              ifelse(parameter == 4, "alpha_control",
                                                     ifelse(parameter == 5, "beta_placebo1",
                                                            ifelse(parameter == 6, "beta_biso_dif",
                                                                   ifelse(parameter == 7, "beta_prop_dif",
                                                                          ifelse(parameter == 8, "beta_control",
                                                                                 ifelse(parameter == 9, "lapse_placebo1",
                                                                                        ifelse(parameter == 10, "lapse_biso_dif",
                                                                                               ifelse(parameter == 11, "lapse_prop_dif",NA)))))))))))) %>%
      select(parameter,draw,value) %>% pivot_wider(names_from = "parameter", values_from = "value") %>% 
      mutate(alpha_placebo = alpha_placebo1,
             alpha_biso = alpha_placebo1+alpha_biso_dif,
             alpha_prop = alpha_placebo1+alpha_prop_dif,
             beta_placebo= exp(beta_placebo1),
             beta_biso = exp(beta_placebo1+beta_biso_dif),
             beta_prop = exp(beta_placebo1+beta_prop_dif),
             lapse_placebo = brms::inv_logit_scaled(lapse_placebo1)/2,
             lapse_biso = brms::inv_logit_scaled(lapse_placebo1+lapse_biso_dif)/2,
             lapse_prop = brms::inv_logit_scaled(lapse_placebo1+lapse_prop_dif)/2
      ) %>% select(-contains("dif")) %>% 
      dplyr::select(contains(c("placebo","biso","prop","draw"))) %>% select(-c("alpha_placebo1","beta_placebo1","lapse_placebo1")) %>% 
      pivot_longer(cols = starts_with(c("alpha", "beta", "lapse")), 
                   names_to = c(".value", "status"), 
                   names_sep = "_")
    
    
    
    individual_means = alpha_beta_lapse %>% 
      ungroup() %>% 
      rowwise() %>%
      mutate(prob = list(psycho_fit(seq(-35,55,by = 0.1),alpha, lapse,beta)),
             Alpha = list(seq(-35,55,by = 0.1))) %>% 
      unnest() %>% mutate(status = ifelse(status == "placebo","placebo",ifelse(status == "biso","bisoprolol","propranolol"))) %>% 
      mutate(status = factor(status, levels = c("bisoprolol","placebo", "propranolol")))
    
    
  }
  
  # psychometric equation (RRST)
  psycho_fit_rrst = function(x, alpha,lapse,beta){
    return(0.5 + (1 - 0.5 - lapse) * (1-exp(-10^(beta * (x-alpha)))))
  }

  #getting individual subject posterior fits RRST (for the chosen subject)
  get_indi_rrst = function(model,draw_id){

    individual_params = as_draws_df(model$draws(paste0("param[11,",1:9,"]")))


    individual_means = individual_params %>%
      select(-contains(".")) %>%
      mutate(draw = 1:n()) %>%
      pivot_longer(-draw, names_to = "variable") %>%
      filter(draw %in% draw_id) %>%
      mutate(parameter = as.numeric(sub(".*,(\\d+)\\]", "\\1", variable))) %>%
      mutate(subject = as.numeric(gsub("^param\\[(\\d+),\\d+\\]", "\\1", variable))) %>%
      mutate(parameter = ifelse(parameter == 1, "beta_placebo1",
                                ifelse(parameter == 2, "beta_biso_dif",
                                       ifelse(parameter == 3, "beta_prop_dif",
                                              ifelse(parameter == 4, "lapse_placebo1",
                                                     ifelse(parameter == 5, "lapse_biso_dif",
                                                            ifelse(parameter == 6, "lapse_prop_dif",
                                                                   ifelse(parameter == 7, "alpha_placebo1",
                                                                          ifelse(parameter == 8, "alpha_biso_dif",
                                                                                 ifelse(parameter == 9, "alpha_prop_dif",NA)))))))))) %>%
      select(parameter,draw,value) %>% pivot_wider(names_from = "parameter", values_from = "value") %>%
      mutate(beta_placebo= exp(beta_placebo1),
             beta_biso = exp(beta_placebo1+beta_biso_dif),
             beta_prop = exp(beta_placebo1+beta_prop_dif),
             lapse_placebo = brms::inv_logit_scaled(lapse_placebo1)/2,
             lapse_biso = brms::inv_logit_scaled(lapse_placebo1+lapse_biso_dif)/2,
             lapse_prop = brms::inv_logit_scaled(lapse_placebo1+lapse_prop_dif)/2,
             alpha_placebo = (alpha_placebo1),
             alpha_biso = (alpha_placebo1+alpha_biso_dif),
             alpha_prop = (alpha_placebo1+alpha_prop_dif))%>%
      select(-contains("dif")) %>%
      dplyr::select(contains(c("placebo","biso","prop","draw"))) %>%
      select(-c("beta_placebo1","lapse_placebo1","alpha_placebo1")) %>%
      pivot_longer(cols = starts_with(c("alpha", "beta", "lapse")),
                   names_to = c(".value", "status"),
                   names_sep = "_")


    
    
    individual_means = individual_means %>% 
      ungroup() %>% 
      rowwise() %>%
      mutate(prob = list(psycho_fit_rrst(seq(-2.5,22.5,by = 0.1),alpha,lapse,beta)),
             Alpha = list(seq(-2.5,22.5,by = 0.1))) %>% 
      unnest() %>% mutate(status = ifelse(status == "placebo","placebo",ifelse(status == "biso","bisoprolol","propranolol"))) %>% 
      mutate(status = factor(status, levels = c("bisoprolol","placebo", "propranolol")))
    
    return(individual_means)
    
  }
  
  

#   plot1_hrd = function(){
#     
#     ############################### HRD plot#############################################
#     
#     sub = '0404'
#     
#     # load HRDT raw subject data for first visit
#     df = read_delim(paste0("/mnt/cortex/ecg/raw/PROJECTS/VMP_AUX/AUX/hrd/", sub,"HRD/", sub, "HRD_final.txt")) #/mnt/raid0/scratch/BIDS/sub-0541/ses-session1/beh/sub-0263_ses-session1_task-hrd1_beh.tsv") #final.txt
#     df <- df %>% filter(Modality == "Intero")
#     
#     # load HRDT subject numpy array (PSI trajectory) for first visit    
#     interoPost = np$load(paste0("/mnt/cortex/ecg/raw/PROJECTS/VMP_AUX/AUX/hrd/", sub, "HRD/", sub, "Intero_posterior.npy"))
#     interoPost_hrd = intervals(df,interoPost)%>% mutate(Modality = "Intero") #get the intervals for Intero condition
#     
#     #get the subjects fitted psychometric (from Hierarchical model):
#     model_intero <- readRDS('/mnt/raid0/scratch/BIDS/derivatives/hrd/R/HierarchicalEstimates/HRD/stanmodel/Normal_intero.RDS') 
#     
#     # find subject index 
#     model_intero_summary <- read.csv('/mnt/raid0/scratch/BIDS/derivatives/hrd/R/HierarchicalEstimates/HRD/Hierarchical_HRD_Intero.csv')
#     sub_model_idx <- model_intero_summary$idx[which(grepl(sub, model_intero_summary$participant_id))]
#     
#     fontsize = 28 
#     ## Only placebo first part
#     plot1_hrd = rbind(interoPost_hrd) %>% 
#       mutate(Response = ifelse(Decision == "More","Faster","Slower")) %>% 
#       ggplot(aes()) +
#       geom_point(aes(x = trials, y = Alpha, shape = Response), size = 3) +
#       scale_shape_manual(values = c(15, 17)) +
#       geom_line(aes(x = x, y = EstimatedThreshold), linewidth = 1.1) +
#       geom_hline(yintercept = 0, linetype = "dashed", linewidth = 1) +
#       geom_ribbon(aes(x = x, ymin = lower, ymax = upper), fill = '#BB4A4C', alpha = 0.5) +
#       guides(color = "none", alpha = "none", fill = "none",
#              shape = guide_legend(direction = "horizontal")) +
#       scale_x_continuous(name = "#Trials", limits = c(0, nrow(interoPost_hrd)), breaks = seq(0, nrow(df), by = 20)) +
#       scale_y_continuous(name = expression(paste("Intensity  (", Delta, "BPM)")), limits = c(-40,50), breaks = seq(-40,50,20))+
#       theme_classic()+
#       theme(legend.position = c(0.6,0.8),
#             text = element_text(size = fontsize),         
#             axis.title = element_text(size = fontsize),   
#             axis.text = element_text(size = fontsize),    
#             legend.text = element_text(size = fontsize),  
#             legend.title = element_text(size = fontsize)  
#       )
#     
#     #dd = interoPost_hrd %>% mutate(Decision_num = ifelse(Decision == "More",1,0),
#     #                               Decision = ifelse(Decision == "More","Faster","Slower"))
#     
#     
#     
#     # draw the subject from the hierarchical HRDT model
#     parameters <- as_draws_df(model_intero$draws(c(paste0('alpha[', sub_model_idx,']'),paste0('beta[', sub_model_idx,']'),paste0('lapse[', sub_model_idx,']'))))%>% select(-contains(".")) %>% 
#       mutate(draw = 1:n())
#     # select the draws to plot:
#     draw_id = sample(1:nrow(parameters),200) # sample 200 
#     # get probability of responding: through stimulus values of -40 through 40
#     trialwise_draws = parameters %>% filter(draw %in% draw_id) %>%
#       rename_with(~c("alpha","beta","lapse","draw")) %>%
#       mutate(x = list(seq(-40,40,by = 0.1))) %>% unnest(x) %>%
#       mutate(prob = lapse+(1-2*lapse) * (0.5+0.5*pracma::erf((x-alpha)/beta*sqrt(2))))
#     
#     # plot hrdt intero prob
#     fontsize = 28
#     plot2_hrd = trialwise_draws %>% ggplot(aes(x = x, y = prob, group = draw))+ geom_line(alpha = 0.1, color = '#BB4A4C')  + #linewidth = 0.75,
#       coord_flip()+
#       scale_x_continuous(name = expression(paste("Intensity  (", Delta, " BPM)")), limits = c(-40,40), breaks = seq(-40,40,10))+
#       scale_y_continuous(name = "P(Faster)", limits = c(0,1), breaks = seq(0,1,0.5)) +
#       theme_classic()+
#       theme(axis.text.y = element_blank(),
#             axis.ticks.y = element_blank(),
#             axis.title.y = element_blank(),
#             legend.position = "none") +
#       guides(col = "none")+
#       theme(text = element_text(size = fontsize),         
#             axis.title = element_text(size = fontsize),   
#             axis.text = element_text(size = fontsize),    
#             legend.text = element_text(size = fontsize), 
#             legend.title = element_text(size = fontsize)  
#       )
#     
#     
#     #combine
#     plot_draws_hrd = plot1_hrd+plot2_hrd+ plot_layout(widths = c(4, 1))
#     #save
#     ggsave("~/Git/multi-intero/figs/TaskFigs/hrd_fig.png",plot_draws_hrd, height = 7, width = 12, units = "in", dpi = 400)
# 
#     ############################################################################
#     
#     
#     
#     
#     
#     
#     
#     model_rrst <- readRDS('/mnt/raid0/scratch/BIDS/derivatives/rrst/HierarchicalEstimates/saved model/RRST_noguess.RDS')  
#     # get draws of parameters
#     parameters = as_draws_df(model_rrst$draws(c("alpha[1]","beta[1]","lapse[1]"))) %>% select(-contains(".")) %>% mutate(draw = 1:n())
#     # select the draws to plot:
#     draw_id = sample(1:nrow(parameters),200)
#     # rrst
#     trialwise_draws = parameters %>% filter(draw %in% draw_id) %>%
#       rename_with(~c("alpha","beta","lapse","draw")) %>%
#       mutate(x = list(seq(0,20,by = 0.1))) %>% unnest(x) %>% # change what "x" is thus between 0 and 20 but if you want to have it on the same scale of the % intensity you can do a conversion between these 0 to 20  -> 0 to 100
#       mutate(beta = exp(beta), lapse = brms::inv_logit_scaled(lapse) / 2) %>%
#       mutate(prob = 0.5 + (0.5 - lapse) * (1 - exp(-10^(beta*(x-alpha))))) #lapse+(1-2*lapse) * (0.5+0.5*pracma::erf((x-alpha)/beta*sqrt(2))))
#     trialwise_draws %>% ggplot(aes(x = x, y = prob, group = draw))+ geom_line(alpha = 0.1)
#     
#     
#     
#     
#     
#     
#     
#     model <-readRDS('/mnt/raid0/Neurodrenaline/behav/STAN_models/HRD.RDS')
#    
#     
#     ############### Placebo only plot for Manuscript:
#     '#BB4A4C''#fabd14' '#da2c26''#2196a5''#6c4c98' 
#     fontsize = 28 
#     ## Only placebo first part
#     rbind(interoPost_hrd) %>% #plot1_hrd = 
#       mutate(Response = ifelse(Decision == "More","Faster","Slower")) %>% 
#       ggplot(aes()) +
#       geom_point(aes(x = trials, y = Alpha, shape = Response), size = 3) +
#       scale_shape_manual(values = c(15, 17)) +
#       geom_line(aes(x = x, y = EstimatedThreshold), linewidth = 1.1) +
#       geom_hline(yintercept = 0, linetype = "dashed", linewidth = 1) +
#       geom_ribbon(aes(x = x, ymin = lower, ymax = upper), fill = '#b17e99', alpha = 0.5) +
#       guides(color = "none", alpha = "none", fill = "none",
#              shape = guide_legend(direction = "horizontal")) +
#       scale_x_continuous(name = "#Trials", limits = c(0, nrow(interoPost_hrd)), breaks = seq(0, nrow(df), by = 20)) +
#       scale_y_continuous(name = expression(paste("Intensity  (", Delta, "BPM)")), limits = c(-40,50), breaks = seq(-40,50,20))+
#       theme_classic()+
#       theme(legend.position = c(0.6,0.8),
#             text = element_text(size = fontsize),         
#             axis.title = element_text(size = fontsize),   
#             axis.text = element_text(size = fontsize),    
#             legend.text = element_text(size = fontsize),  
#             legend.title = element_text(size = fontsize)  
#       )
#     
#     dd = interoPost_hrd %>% mutate(Decision_num = ifelse(Decision == "More",1,0),
#                        Decision = ifelse(Decision == "More","Faster","Slower"))
#     
#     
#     # second part only placebo
#     plot2_placebo = parameters_intero_inidv %>% filter(Modality == "Intero") %>% ggplot() + #
#       geom_line(aes(x = Alpha, y = prob, group = interaction(status,draw), col = status),
#                 linewidth = 0.75, alpha = 0.25)+coord_flip()+
#       scale_x_continuous(name = expression(paste("Intensity  (", Delta, "BPM)")), limits = c(-35,55), breaks = seq(-35,55,10))+
#       scale_y_continuous(name = "P(Faster)", limits = c(0,1), breaks = seq(0,1,0.5))+
#       theme_classic()+
#       theme(axis.text.y = element_blank(),
#             axis.ticks.y = element_blank(),
#             axis.title.y = element_blank(),
#             legend.position = "none") +
#       guides(col = "none")+
#       theme(text = element_text(size = fontsize),         
#             axis.title = element_text(size = fontsize),   
#             axis.text = element_text(size = fontsize),    
#             legend.text = element_text(size = fontsize), 
#             legend.title = element_text(size = fontsize)  
#       )
#     #combine
#     plot_draws_placebo = plot1_placebo+plot2_placebo+ plot_layout(widths = c(4, 1))
#     #save
#     ggsave(here::here("Figures","hrd_fig1.tiff"),plot_draws_placebo, height = 7, width = 12, units = "in", dpi = 400)
#     #return
#     return(list(plot_draws_placebo,plot_draws_all))
#   }
#   
#   ############# RRST: plot 1
#   plot1_rrst = function(){
#     
#     # only for placebo read data first PSI
#     library(readr)
#     RRST_threshold <- read_csv(here::here("data","sub_3025","visit_0003","InteroceptionTasks","RRST","RRST_threshold.csv"))
#     thresholds = as.numeric(colnames(RRST_threshold))
#     library(readr)
#     RRST_thresholdse <- read_csv(here::here("data","sub_3025","visit_0003","InteroceptionTasks","RRST","RRST_thresholdse.csv"))
#     thresholdse = as.numeric(colnames(RRST_thresholdse))
#     
#     # then the raw data
#     df = read_csv(here::here("data","RRST_trial_level_data.csv")) %>% filter(subject == "sub_3025" & visit == "visit_0003")
#     
#     #renaming for the x-axis:
#     stimms = c(0,8.5,17)
#     labelss = round((stimms/ 17) * 100,0)
#     
#     #color for the plot
#     color = "#0B96BC"
#     
#     # first plot
#     placebo_plot1 = df %>% mutate(mean = thresholds, se = thresholdse, Response = ifelse(Resp == 1, "Correct","Incorrect")) %>% 
#       mutate(trial = 1:n()) %>% 
#       ggplot(aes(fill = drugs))+
#       geom_point(aes(x = trial, y = Stim, shape = Response), size = 3) +
#       # shapes' shape
#       scale_shape_manual(values = c(16, 4)) +
#       # 0 line
#       # geom_hline(yintercept = 0, linetype = "dashed") +
#       # line that is inside Confidence interval
#       geom_line(aes(x = trial, y = mean),size = 1.1) +
#       # confidence interval
#       geom_ribbon(aes(x = trial, y = mean, ymin = mean-2*se, ymax = mean+2*se), alpha = 0.3)+
#       # themes and text
#       guides(color = "none", alpha = "none", fill = "none",
#              shape = guide_legend(direction = "horizontal")) +
#       scale_x_continuous(name = "#Trials", limits = c(0, nrow(df)), breaks = seq(0, nrow(df), by = 20)) +
#       scale_y_continuous(name = expression(paste("Intensity (% resistance)")), limits = c(-2.5,22.5), breaks = stimms, labels = labelss)+
#       theme_classic()+
#       theme(legend.position = c(0.6,0.8),
#             text = element_text(size = 28),         # Global text size
#             axis.title = element_text(size = 28),   # Axis titles
#             axis.text = element_text(size = 28),    # Axis tick labels
#             legend.text = element_text(size = 28),  # Legend text
#             legend.title = element_text(size = 28)  # Legend title (if applicable)
#       )+
#       scale_fill_manual(values = color)
#     
#     #get the subjects fitted psychometric:
#     model <- readRDS(here::here("STAN models","RRST.RDS"))
#     draw_id = sample(1:4000,50)
#     individual_means = get_indi_rrst(model,draw_id)
#     
#     # plot 2
#     placebo_plot2 = individual_means %>% filter(status == "placebo") %>% ggplot() +
#       geom_line(aes(x = Alpha, y = prob, group = interaction(status,draw), col = status),
#                 linewidth = 0.75, alpha = 0.25)+coord_flip()+
#       scale_x_continuous(name = expression(paste("Intensity")), limits = c(-2.5,22.5), breaks = seq(-2.5,22.5,10))+
#       scale_y_continuous(name = "P(Correct)", limits = c(0.3,1), breaks = c(0.5,1))+
#       theme_classic()+
#       scale_color_manual(values = color)+
#       theme(axis.text.y = element_blank(),
#             axis.ticks.y = element_blank(),
#             axis.title.y = element_blank(),
#             text = element_text(size = 28),         # Global text size
#             axis.title = element_text(size = 28),   # Axis titles
#             axis.text = element_text(size = 28),    # Axis tick labels
#             legend.text = element_text(size = 28),  # Legend text
#             legend.title = element_text(size = 28),  # Legend title (if applicable)
#             legend.position = "none") +  # Remove the legend
#       guides(col = "none")
#     
#     #combine
#     placebo = placebo_plot1+placebo_plot2+ plot_layout(widths = c(4, 1))
#     #save
#     ggsave(here::here("Figures","rrst_fig1.tiff"),placebo, height = 7, width = 12, units = "in", dpi = 400)
#     #return
#     return(list(placebo))
#     
#   }
#   
#   hrd = plot1_hrd()
#   rrst = plot1_rrst()
#   
#   #return the two plots (Only placebo for manuscript)
#   return(list(hrd[[1]], rrst[[1]]))
#   
# }
