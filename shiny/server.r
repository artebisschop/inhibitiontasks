library(shiny)
library(quid)
library(dplyr)
library(tidyr)
library(ggplot2)
library(brio)
library(data.table)
library(rsconnect)
library(plotly)
library(RColorBrewer)
library(scales)
load("plottable.rda")



shinyServer(
  function(input, output, session){
    
    output$instructions1 <- renderText({
      "
Welcome to this web application!

Within this application, you can discover the different datasets that are included in the database and 
explore potential relationships between the evidence for qualitative individual differences and the 
properties of the datasets. 

In the current tab, you can view the properties of all the datasets in the database.
To view these properties, select a dataset in the left panel, and navigate to the Summary tab.

It is also possible to view the individual observed effects of each dataset. To do so, select a dataset in 
the left panel, and navigate to the Plot tab. The left plot shows the ordered observed effects, as points. 
The right plot shows the ordered Bayesian estimates of individual effects. The points are the estimates 
from the unstructured model.

You can always change the selected dataset in the left panel and easily navigate between different datasets.

To plot the evidence for qualitative individual differences against the properties of the datasets, you can 
navigate to the Properties tab at the top of this page.
      
      "
    })
    
    output$myPlot <- renderImage({
      
      Dataset <- input$Dataset
      
      quidplot <- function(x){
        list(src = paste("plot", x, ".png", sep = ""),
             contentType = 'image/png',
             width = 800)
      }
      
      if(Dataset == "Select a dataset"){
        list(src = paste("plot0.png"),
             contentType = 'image/png',
             width = 700)
      } else if(Dataset == "Dataset 1: Stroop (Von Bastian, Souza, & Gade, 2015)"){
        quidplot(1)
      } else if(Dataset == "Dataset 2: Stroop (Pratte, Rouder, Morey, & Feng, 2010)"){
        quidplot(2)
      } else if(Dataset == "Dataset 3: Stroop (Pratte, Rouder, Morey, & Feng, 2010)"){
        quidplot(3)
      } else if(Dataset == "Dataset 4: Stroop (Rey-Mermet, Gade, & Oberauer, 2018)"){
        quidplot(4)
      } else if(Dataset == "Dataset 5: Stroop (Rey-Mermet, Gade, & Oberauer, 2018)"){
        quidplot(5)
      } else if(Dataset == "Dataset 6: Stroop (Hedge, Powell, & Sumner, 2018)"){
        quidplot(6)
      } else if(Dataset == "Dataset 7: Simon (Von Bastian, Souza, & Gade, 2015)"){
        quidplot(7)
      } else if(Dataset == "Dataset 8: Simon (Pratte, Rouder, Morey, & Feng, 2010)"){
        quidplot(8)
      } else if(Dataset == "Dataset 9: Simon (Pratte, Rouder, Morey, & Feng, 2010)"){
        quidplot(9)
      } else if(Dataset == "Dataset 10: Flanker (Von Bastian, Souza, & Gade, 2015)"){
        quidplot(10)
      } else if(Dataset == "Dataset 11: Flanker (Rey-Mermet, Gade, & Oberauer, 2018)"){
        quidplot(11)
      } else if(Dataset == "Dataset 12: Flanker (Rey-Mermet, Gade, & Oberauer, 2018)"){
        quidplot(12)
      } else if(Dataset == "Dataset 13: Flanker (Hedge, Powell, & Sumner, 2018)"){
        quidplot(13)
      } else if(Dataset == "Dataset 14: Stroop (Cebersole et al., 2016)"){
        quidplot(14)
      } else if(Dataset == "Dataset 15: Stroop (Cebersole et al., 2016)"){
        quidplot(15)
      } else if(Dataset == "Dataset 16: Stroop (Cebersole et al., 2016)"){
        quidplot(16)
      } else if(Dataset == "Dataset 17: Stroop (Cebersole et al., 2016)"){
        quidplot(17)
      } else if(Dataset == "Dataset 18: Stroop (Cebersole et al., 2016)"){
        quidplot(18)
      } else if(Dataset == "Dataset 19: Stroop (Cebersole et al., 2016)"){
        quidplot(19)
      } else if(Dataset == "Dataset 20: Stroop (Cebersole et al., 2016)"){
        quidplot(20)
      } else if(Dataset == "Dataset 21: Stroop (Cebersole et al., 2016)"){
        quidplot(21)
      } else if(Dataset == "Dataset 22: Stroop (Cebersole et al., 2016)"){
        quidplot(22)
      } else if(Dataset == "Dataset 23: Stroop (Cebersole et al., 2016)"){
        quidplot(23)
      } else if(Dataset == "Dataset 24: Stroop (Cebersole et al., 2016)"){
        quidplot(24)
      } else if(Dataset == "Dataset 25: Stroop (Cebersole et al., 2016)"){
        quidplot(25)
      } else if(Dataset == "Dataset 26: Stroop (Cebersole et al., 2016)"){
        quidplot(26)
      } else if(Dataset == "Dataset 27: Stroop (Cebersole et al., 2016)"){
        quidplot(27)
      } else if(Dataset == "Dataset 28: Stroop (Cebersole et al., 2016)"){
        quidplot(28)
      } else if(Dataset == "Dataset 29: Stroop (Cebersole et al., 2016)"){
        quidplot(29)
      } else if(Dataset == "Dataset 30: Stroop (Cebersole et al., 2016)"){
        quidplot(30)
      } else if(Dataset == "Dataset 31: Stroop (Cebersole et al., 2016)"){
        quidplot(31)
      } else if(Dataset == "Dataset 32: Stroop (Cebersole et al., 2016)"){
        quidplot(32)
      } else if(Dataset == "Dataset 33: Stroop (Cebersole et al., 2016)"){
        quidplot(33)
      } else if(Dataset == "Dataset 34: Stroop (Cebersole et al., 2016)"){
        quidplot(34)
      }
      
    }, deleteFile = FALSE)
    
    output$title <- renderText({
      Dataset <- input$Dataset
      print(Dataset)
    })
    
    output$stats <- renderTable({
      Dataset <- input$Dataset
      rownames <- c("Task",
                    "Authors original study",
                    "Year conducted",
                    "Mean effect (ms)",
                    "Standard deviation effect (ms)",
                    "Mean age",
                    "Percentage female",
                    "Percentage male",
                    "Number of participants",
                    "Number of experimental blocks",
                    "Number of trials",
                    "Usage of neutral trials",
                    "Percentage incongruent trials",
                    "Feedback",
                    "Fixation cross",
                    "Time limit per trial",
                    "Bayes Factor (positive model/unconstrained model)",
                    "Preferred model (positive model/unconstrained model)",
                    "Attenuation factor η")
      tab <- function(x){
        df <- transpose(plottable[x,2:ncol(plottable)])
        df[,2] <- rownames
        colnames(df) <- c(" ", paste("Dataset", x, sep = " "))
        df[,c(2,1)]
      }
      
      if(Dataset == "Select a dataset"){
        df <- data.frame(V1 = "Please select a dataset to view its properties")
        colnames(df) <- NULL
        df
      } else if(Dataset == "Dataset 1: Stroop (Von Bastian, Souza, & Gade, 2015)"){
        tab(1)
      } else if(Dataset == "Dataset 2: Stroop (Pratte, Rouder, Morey, & Feng, 2010)"){
        tab(2)
      } else if(Dataset == "Dataset 3: Stroop (Pratte, Rouder, Morey, & Feng, 2010)"){
        tab(3)
      } else if(Dataset == "Dataset 4: Stroop (Rey-Mermet, Gade, & Oberauer, 2018)"){
        tab(4)
      } else if(Dataset == "Dataset 5: Stroop (Rey-Mermet, Gade, & Oberauer, 2018)"){
        tab(5)
      } else if(Dataset == "Dataset 6: Stroop (Hedge, Powell, & Sumner, 2018)"){
        tab(6)
      } else if(Dataset == "Dataset 7: Simon (Von Bastian, Souza, & Gade, 2015)"){
        tab(7)
      } else if(Dataset == "Dataset 8: Simon (Pratte, Rouder, Morey, & Feng, 2010)"){
        tab(8)
      } else if(Dataset == "Dataset 9: Simon (Pratte, Rouder, Morey, & Feng, 2010)"){
        tab(9)
      } else if(Dataset == "Dataset 10: Flanker (Von Bastian, Souza, & Gade, 2015)"){
        tab(10)
      } else if(Dataset == "Dataset 11: Flanker (Rey-Mermet, Gade, & Oberauer, 2018)"){
        tab(11)
      } else if(Dataset == "Dataset 12: Flanker (Rey-Mermet, Gade, & Oberauer, 2018)"){
        tab(12)
      } else if(Dataset == "Dataset 13: Flanker (Hedge, Powell, & Sumner, 2018)"){
        tab(13)
      } else if(Dataset == "Dataset 14: Stroop (Cebersole et al., 2016)"){
        tab(14)
      } else if(Dataset == "Dataset 15: Stroop (Cebersole et al., 2016)"){
        tab(15)
      } else if(Dataset == "Dataset 16: Stroop (Cebersole et al., 2016)"){
        tab(16)
      } else if(Dataset == "Dataset 17: Stroop (Cebersole et al., 2016)"){
        tab(17)
      } else if(Dataset == "Dataset 18: Stroop (Cebersole et al., 2016)"){
        tab(18)
      } else if(Dataset == "Dataset 19: Stroop (Cebersole et al., 2016)"){
        tab(19)
      } else if(Dataset == "Dataset 20: Stroop (Cebersole et al., 2016)"){
        tab(20)
      } else if(Dataset == "Dataset 21: Stroop (Cebersole et al., 2016)"){
        tab(21)
      } else if(Dataset == "Dataset 22: Stroop (Cebersole et al., 2016)"){
        tab(22)
      } else if(Dataset == "Dataset 23: Stroop (Cebersole et al., 2016)"){
        tab(23)
      } else if(Dataset == "Dataset 24: Stroop (Cebersole et al., 2016)"){
        tab(24)
      } else if(Dataset == "Dataset 25: Stroop (Cebersole et al., 2016)"){
        tab(25)
      } else if(Dataset == "Dataset 26: Stroop (Cebersole et al., 2016)"){
        tab(26)
      } else if(Dataset == "Dataset 27: Stroop (Cebersole et al., 2016)"){
        tab(27)
      } else if(Dataset == "Dataset 28: Stroop (Cebersole et al., 2016)"){
        tab(28)
      } else if(Dataset == "Dataset 29: Stroop (Cebersole et al., 2016)"){
        tab(29)
      } else if(Dataset == "Dataset 30: Stroop (Cebersole et al., 2016)"){
        tab(30)
      } else if(Dataset == "Dataset 31: Stroop (Cebersole et al., 2016)"){
        tab(31)
      } else if(Dataset == "Dataset 32: Stroop (Cebersole et al., 2016)"){
        tab(32)
      } else if(Dataset == "Dataset 33: Stroop (Cebersole et al., 2016)"){
        tab(33)
      } else if(Dataset == "Dataset 34: Stroop (Cebersole et al., 2016)"){
        tab(34)
      }
    })
    
    output$instructions2 <- renderText({
      "
In this tab, you can explore potential relationships between the evidence for qualitative individual
differences and the properties of the datasets. 

You can choose which of the properties you would like to plot in the left panel:

1. Select whether you want to plot Bayes Factors or the preferred model.
2. Select which property you would like to explore.
3. Navigate to the Plot tab to view the chosen graph.

Note that when plotting the preferred model, this comparison is restricted to the Positive and 
the Unconstrained model.

Additional statistics can be revealed when hovering your cursor over plot elements (points and regression 
line in scatterplots, boxes in boxplots and bars in barplots).
Points of specific tasks can be hidden by clicking on a task in the legend of scatterplots.
The point sizes in the scatterplots reflect the number of participants in each dataset.

You can always change the selected input in the left panel and easily navigate between different plots.

To discover the different datasets that are included in the database, you can navigate to the Datasets
tab at the top of this page. 

      "
    })
    
    output$myPlot2 <- renderPlotly({

      q <- input$quid
      p <- input$property
      
      scatter_layers = list(
        geom_point(aes(color = task, 
                       size = n_participants, 
                       text = paste("Dataset", ID)), 
                   alpha = .7),
        scale_y_log10(labels = function(x) format(x, scientific = FALSE)),
        ylab("Bayes Factor"),
        xlab(paste(p)),
        theme(legend.title = element_blank()),
        geom_hline(yintercept = 1, 
                   size = .3, 
                   linetype = "dashed"),
        theme_classic(),
        theme(text = element_text(size = 15)))  
      
      box_layers = list(
        geom_boxplot(lwd = .3),
        scale_y_log10(labels = function(x) format(x, scientific = FALSE)),
        ylab("Bayes Factor"),
        xlab(paste(p)),
        geom_hline(yintercept = 1, 
                   size = .3,
                   linetype = "dashed"),
        theme_classic(),
        theme(text = element_text(size = 15),
              legend.position = "none"),
        scale_fill_manual(values = c("#F8766D", "#39C162", "#619CFF", "#FFCA00"))
      )
      
      hzbox_layers = list(
        geom_boxplot(lwd = .3),
        xlab("Preferred model"),
        ylab(paste(p)),
        theme_classic(),
        theme(text = element_text(size = 15)),
        coord_flip(),
        theme(legend.position = 'none'),
        scale_fill_manual(values=c("#619CFF", "#F8766D"))
      )
      
      bar_layers = list(
        geom_bar(stat = "identity", position = "dodge", color = "grey20", lwd = .2),
        ylab("Number of datasets"),
        theme_classic(),
        scale_fill_manual("Preferred model", 
                          values = c("#D60E00", "#F8766D", "#017825", "#39C162", 
                                     "#0356E2", "#619CFF", "#FFCA00", "#FFE47F"))
      )
      
      if(q == "Bayes Factor"){
        if(p == "Cognitive Inhibition Task"){
          plottable <- plottable %>% mutate(task = ifelse(task == "Flanker task", "Flanker task \n (n = 4)",
                                                          ifelse(task == "Simon task", "Simon task \n (n = 3)",
                                                                 "Stroop task \n (n = 27)")))
          ggplotly(ggplot(plottable, aes(x = task, y = BF, fill = task)) +
                     box_layers) %>%
            layout(hoverlabel = list(bgcolor = "white")) %>%
            config(displayModeBar = F)
        } else if(p == "Mean effect (ms)"){
          ggplotly(ggplot(plottable, aes(x = mean_effect, y = BF)) +
                     scatter_layers +
                     geom_smooth(aes(text = paste("R =", round(cor(log(BF), mean_effect, use = "complete.obs"),2))),
                                 method = "lm", 
                                 formula = y ~ x, 
                                 se = FALSE, 
                                 color = "grey", 
                                 size = .5),
                   tooltip = "text") %>% 
            layout(hoverlabel = list(bgcolor = "white"),
                   legend = list(itemsizing='constant')) %>% 
            config(displayModeBar = F)
        } else if(p == "Standard deviation effect (ms)"){
          ggplotly(ggplot(plottable, aes(x = sd_effect, y = BF)) +
                     scatter_layers +
                     geom_smooth(aes(text = paste("R =", round(cor(log(BF), sd_effect, use = "complete.obs"),2))),
                                 method = "lm", 
                                 formula = y ~ x, 
                                 se = FALSE, 
                                 color = "grey", 
                                 size = .5),
                   tooltip = "text") %>% 
            layout(hoverlabel = list(bgcolor = "white"),
                   legend = list(itemsizing='constant')) %>% 
            config(displayModeBar = F)
        } else if(p == "Mean age"){
          ggplotly(ggplot(plottable, aes(x = mean_age, y = BF)) +
                     scatter_layers +
                     geom_smooth(aes(text = paste("R =", round(cor(log(BF), mean_age, use = "complete.obs"),2))),
                                 method = "lm", 
                                 formula = y ~ x, 
                                 se = FALSE, 
                                 color = "grey", 
                                 size = .5),
                   tooltip = "text") %>% 
            layout(hoverlabel = list(bgcolor = "white"),                    
                   legend = list(itemsizing='constant')) %>% 
            config(displayModeBar = F)
        } else if(p == "Percentage female"){
          ggplotly(ggplot(plottable, aes(x = percentage_female, y = BF)) +
                     scatter_layers +
                     geom_smooth(aes(text = paste("R =", round(cor(log(BF), percentage_female, use = "complete.obs"),2))),
                                 method = "lm", 
                                 formula = y ~ x, 
                                 se = FALSE, 
                                 color = "grey", 
                                 size = .5),
                   tooltip = "text") %>% 
            layout(hoverlabel = list(bgcolor = "white"),                    
                   legend = list(itemsizing='constant')) %>% 
            config(displayModeBar = F)
        } else if(p == "Percentage male"){
          ggplotly(ggplot(plottable, aes(x = percentage_male, y = BF)) +
                     scatter_layers +
                     geom_smooth(aes(text = paste("R =", round(cor(log(BF), percentage_male, use = "complete.obs"),2))),
                                 method = "lm", 
                                 formula = y ~ x, 
                                 se = FALSE, 
                                 color = "grey", 
                                 size = .5),
                   tooltip = "text") %>% 
            layout(hoverlabel = list(bgcolor = "white"),                    
                   legend = list(itemsizing='constant')) %>% 
            config(displayModeBar = F)
        } else if(p == "Number of participants"){
          ggplotly(ggplot(plottable, aes(x = n_participants, y = BF)) +
                     scatter_layers +
                     geom_smooth(aes(text = paste("R =", round(cor(log(BF), n_participants, use = "complete.obs"),2))),
                                 method = "lm", 
                                 formula = y ~ x, 
                                 se = FALSE, 
                                 color = "grey", 
                                 size = .5),
                   tooltip = "text") %>% 
            layout(hoverlabel = list(bgcolor = "white"),                    
                   legend = list(itemsizing='constant')) %>% 
            config(displayModeBar = F)
        } else if(p == "Number of experimental blocks"){
          ggplotly(ggplot(plottable, aes(x = n_blocks, y = BF)) +
                     scatter_layers +
                     geom_smooth(aes(text = paste("R =", round(cor(log(BF), n_blocks, use = "complete.obs"),2))),
                                 method = "lm", 
                                 formula = y ~ x, 
                                 se = FALSE, 
                                 color = "grey", 
                                 size = .5),
                   tooltip = "text") %>% 
            layout(hoverlabel = list(bgcolor = "white"),                    
                   legend = list(itemsizing='constant')) %>% 
            config(displayModeBar = F)
        } else if(p == "Number of trials"){
          ggplotly(ggplot(plottable, aes(x = n_trials, y = BF)) +
                     scatter_layers +
                     geom_smooth(aes(text = paste("R =", round(cor(log(BF), n_trials, use = "complete.obs"),2))),
                                 method = "lm", 
                                 formula = y ~ x, 
                                 se = FALSE, 
                                 color = "grey", 
                                 size = .5),
                   tooltip = "text") %>% 
            layout(hoverlabel = list(bgcolor = "white"),                    
                   legend = list(itemsizing='constant')) %>% 
            config(displayModeBar = F)
        } else if(p == "Usage of neutral trials"){
          plottable <- plottable %>% mutate(neutral_trials = ifelse(neutral_trials == "No", "No \n (n = 25)", "Yes \n (n = 9)"))
          ggplotly(ggplot(plottable, aes(x = neutral_trials, y = BF, fill = neutral_trials)) +
                     box_layers) %>%
            layout(hoverlabel = list(bgcolor = "white")) %>%
            config(displayModeBar = F)
        } else if(p == "Percentage incongruent trials"){
          ggplotly(ggplot(plottable, aes(x = percentage_incongruent, y = BF)) +
                     scatter_layers +
                     geom_smooth(aes(text = paste("R =", round(cor(log(BF), percentage_incongruent, use = "complete.obs"),2))),
                                 method = "lm", 
                                 formula = y ~ x, 
                                 se = FALSE, 
                                 color = "grey", 
                                 size = .5),
                   tooltip = "text") %>% 
            layout(hoverlabel = list(bgcolor = "white"),                    
                   legend = list(itemsizing='constant')) %>% 
            config(displayModeBar = F)
        } else if(p == "Feedback"){
          plottable <- plottable %>% mutate(feedback = ifelse(feedback == "After each block", "After block \n (n = 2)",
                                                          ifelse(feedback == "After each trial", "After trial \n (n = 4)",
                                                                 ifelse(feedback == "Throughout the experiment", 
                                                                        "Throughout \n experiment \n (n = 2)", 
                                                                        "None \n (n = 2)")))) %>% 
            filter(!is.na(feedback))
          ggplotly(ggplot(plottable, aes(x = feedback, y = BF, fill = feedback)) +
                     box_layers +
                     theme(axis.text.x = element_text(angle = 45))) %>%
            layout(hoverlabel = list(bgcolor = "white")) %>%
            config(displayModeBar = F)
        } else if(p == "Fixation cross"){
          plottable <- plottable %>% mutate(fixation_cross = ifelse(fixation_cross == "250 ms", "250 ms \n (n = 1)",
                                                              ifelse(fixation_cross == "500 ms", "500 ms \n (n = 4)",
                                                                     ifelse(fixation_cross == "700 ms", "700 ms \n (n = 4)", 
                                                                            "None \n (n = 2)")))) %>% 
            filter(!is.na(fixation_cross))
          ggplotly(ggplot(plottable, aes(x = fixation_cross, y = BF, fill = fixation_cross)) +
                     box_layers) %>%
            layout(hoverlabel = list(bgcolor = "white")) %>%
            config(displayModeBar = F)
        } else if(p == "Time limit"){
          plottable <- plottable %>% mutate(time_limit = ifelse(time_limit == "2000 ms", "Yes \n (n = 4)", "No \n (n = 30)"))
          ggplotly(ggplot(plottable, aes(x = time_limit, y = BF, fill = time_limit)) +
                     box_layers) %>%
            layout(hoverlabel = list(bgcolor = "white")) %>%
            config(displayModeBar = F)
        } else if(p == "Attenuation factor η"){
          ggplotly(ggplot(plottable, aes(x = eta, y = BF)) +
                     scatter_layers +
                     geom_smooth(aes(text = paste("R =", round(cor(log(BF), eta, use = "complete.obs"),2))),
                                 method = "lm", 
                                 formula = y ~ x, 
                                 se = FALSE, 
                                 color = "grey", 
                                 size = .5),
                   tooltip = "text") %>% 
            layout(hoverlabel = list(bgcolor = "white"),                    
                   legend = list(itemsizing='constant')) %>% 
            config(displayModeBar = F)
          }
        } else if(q == "Preferred model"){
        
        if(p == "Cognitive Inhibition Task"){
          data <- plottable %>% select(task, preferred_model) %>% group_by(task) %>% count(preferred_model)
          ggplotly(ggplot(data, aes(x = task, y = n, fill = interaction(preferred_model, task), text = paste("n = ", n))) +
                     bar_layers,
                   tooltip = "text") %>% 
            layout(hoverlabel = list(bgcolor = "white"),
                   xaxis = list(title = "")) %>% 
            config(displayModeBar = F)
        } else if(p == "Mean effect (ms)"){
          plottable <- plottable %>% mutate(preferred_model = ifelse(preferred_model == "Positive Model", "Positive \n model \n (n = 25)", 
                                                                       "Unconstrained \n model \n (n = 9)"))
          ggplotly(ggplot(plottable, aes(x = preferred_model, y = mean_effect, fill = preferred_model)) +
                     hzbox_layers) %>%
            layout(hoverlabel = list(bgcolor = "white")) %>%
            config(displayModeBar = F)
        } else if(p == "Standard deviation effect (ms)"){
          plottable <- plottable %>% mutate(preferred_model = ifelse(preferred_model == "Positive Model", "Positive \n model \n (n = 25)", 
                                                                     "Unconstrained \n model \n (n = 9)"))
          ggplotly(ggplot(plottable, aes(x = preferred_model, y = sd_effect, fill = preferred_model)) +
                     hzbox_layers) %>%
            layout(hoverlabel = list(bgcolor = "white")) %>%
            config(displayModeBar = F)
        } else if(p == "Mean age"){
          plottable <- plottable %>% mutate(preferred_model = ifelse(preferred_model == "Positive Model", "Positive \n model \n (n = 25)", 
                                                                       "Unconstrained \n model \n (n = 9)"))
          ggplotly(ggplot(plottable, aes(x = preferred_model, y = mean_age, fill = preferred_model)) +
                     hzbox_layers) %>%
            layout(hoverlabel = list(bgcolor = "white")) %>%
            config(displayModeBar = F)
        } else if(p == "Percentage female"){
          plottable <- plottable %>% mutate(preferred_model = ifelse(preferred_model == "Positive Model", "Positive \n model \n (n = 25)", 
                                                                       "Unconstrained \n model \n (n = 9)"))
          ggplotly(ggplot(plottable, aes(x = preferred_model, y = percentage_female, fill = preferred_model)) +
                     hzbox_layers) %>%
            layout(hoverlabel = list(bgcolor = "white")) %>%
            config(displayModeBar = F)
        } else if(p == "Percentage male"){
          plottable <- plottable %>% mutate(preferred_model = ifelse(preferred_model == "Positive Model", "Positive \n model \n (n = 25)", 
                                                                       "Unconstrained \n model \n (n = 9)"))
          ggplotly(ggplot(plottable, aes(x = preferred_model, y = percentage_male, fill = preferred_model)) +
                     hzbox_layers) %>%
            layout(hoverlabel = list(bgcolor = "white")) %>%
            config(displayModeBar = F)
        } else if(p == "Number of participants"){
          plottable <- plottable %>% mutate(preferred_model = ifelse(preferred_model == "Positive Model", "Positive \n model \n (n = 25)", 
                                                                       "Unconstrained \n model \n (n = 9)"))
          ggplotly(ggplot(plottable, aes(x = preferred_model, y = n_participants, fill = preferred_model)) +
                     hzbox_layers) %>%
            layout(hoverlabel = list(bgcolor = "white")) %>%
            config(displayModeBar = F)
        } else if(p == "Number of experimental blocks"){
          plottable <- plottable %>% mutate(preferred_model = ifelse(preferred_model == "Positive Model", "Positive \n model \n (n = 25)", 
                                                                       "Unconstrained \n model \n (n = 9)"))
          ggplotly(ggplot(plottable, aes(x = preferred_model, y = n_blocks, fill = preferred_model)) +
                     hzbox_layers) %>%
            layout(hoverlabel = list(bgcolor = "white")) %>%
            config(displayModeBar = F)
        } else if(p == "Number of trials"){
          plottable <- plottable %>% mutate(preferred_model = ifelse(preferred_model == "Positive Model", "Positive \n model \n (n = 25)", 
                                                                       "Unconstrained \n model \n (n = 9)"))
          ggplotly(ggplot(plottable, aes(x = preferred_model, y = n_trials, fill = preferred_model)) +
                     hzbox_layers) %>%
            layout(hoverlabel = list(bgcolor = "white")) %>%
            config(displayModeBar = F)
        } else if(p == "Usage of neutral trials"){
          data <- plottable %>% select(neutral_trials, preferred_model) %>% group_by(neutral_trials) %>% count(preferred_model)
          ggplotly(ggplot(data, aes(x = neutral_trials, y = n, fill = interaction(preferred_model, neutral_trials), text = paste("n = ", n))) +
                     bar_layers,
                   tooltip = "text") %>% 
            layout(hoverlabel = list(bgcolor = "white"),
                   xaxis = list(title = "Neutral trials")) %>% 
            config(displayModeBar = F)
        } else if(p == "Percentage incongruent trials"){
          plottable <- plottable %>% mutate(preferred_model = ifelse(preferred_model == "Positive Model", "Positive \n model \n (n = 25)", 
                                                                       "Unconstrained \n model \n (n = 9)"))
          ggplotly(ggplot(plottable, aes(x = preferred_model, y = percentage_incongruent, fill = preferred_model)) +
                     hzbox_layers) %>%
            layout(hoverlabel = list(bgcolor = "white")) %>%
            config(displayModeBar = F)
        } else if(p == "Feedback"){
          data <-  rbind(as.data.frame(plottable %>% filter(!is.na(feedback)) %>% select(feedback, preferred_model) %>%
                                         group_by(feedback) %>% count(preferred_model)), 
                         data.frame("feedback" = "After each block", "preferred_model" = "Unconstrained Model", "n" = 0))
          ggplotly(ggplot(data, aes(x = feedback, y = n, fill = interaction(preferred_model, feedback), text = paste("n = ", n))) +
                     bar_layers,
                   tooltip = "text") %>% 
            layout(hoverlabel = list(bgcolor = "white"),
                   xaxis = list(title = "Feedback")) %>% 
            config(displayModeBar = F)
        } else if(p == "Fixation cross"){
          data <-  rbind(as.data.frame(plottable %>% filter(!is.na(fixation_cross)) %>% select(fixation_cross, preferred_model) %>%
                                         group_by(fixation_cross) %>% count(preferred_model)), 
                         data.frame("fixation_cross" = c("250 ms", "None"), "preferred_model" = rep("Unconstrained Model", 2), 
                                    "n" = rep(0, 2)))
          ggplotly(ggplot(data, aes(x = fixation_cross, y = n, fill = interaction(preferred_model, fixation_cross), text = paste("n = ", n))) +
                     bar_layers,
                   tooltip = "text") %>% 
            layout(hoverlabel = list(bgcolor = "white"),
                   xaxis = list(title = "Fixation cross")) %>% 
            config(displayModeBar = F)
        } else if(p == "Time limit"){
          data <- plottable %>% mutate(time_limit = ifelse(time_limit == "2000 ms", "Yes", "No")) %>%
            select(time_limit, preferred_model) %>% group_by(time_limit) %>% count(preferred_model)
          ggplotly(ggplot(data, aes(x = time_limit, y = n, fill = interaction(preferred_model, time_limit), text = paste("n = ", n))) +
                     bar_layers,
                   tooltip = "text") %>% 
            layout(hoverlabel = list(bgcolor = "white"),
                   xaxis = list(title = "Time limit")) %>% 
            config(displayModeBar = F)
        } else if(p == "Attenuation factor η"){
          plottable <- plottable %>% mutate(preferred_model = ifelse(preferred_model == "Positive Model", "Positive \n model \n (n = 25)", 
                                                                     "Unconstrained \n model \n (n = 9)"))
          ggplotly(ggplot(plottable, aes(x = preferred_model, y = eta, fill = preferred_model)) +
                     hzbox_layers +
                     ylab("Attenuation factor η")) %>%
            layout(hoverlabel = list(bgcolor = "white")) %>%
            config(displayModeBar = F)
        }
      }
    })
  }
)



