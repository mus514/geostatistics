###########################
## Calculate indices
##########################
## Date initiale: 13 septembre 2024
## Author : Mederic Durand

## To do:
# [x] load all data and concatenate it
# [x] generate time data col and plot
# [x] run all indices
# [x] loop over each codeSite
# [x] boxplots
# [x] descriptive stats
# [x] write to disk
# [] comment code
# [] normalize data x recording duration


#### Set Environment####
site <- "MON_CG" # Site to load + used to plot the whole site
codeSite <- "MON_CG_Foret1" # choose audiomoth
milieuxType <- "Foret" # Additional column for later data management
source("./6.Soundscape/0.Environment.R")

# Remove this sometime and loop over folders
#pathAudio <- paste0("D:/Audiomoths_2024_Summer/", codeSite,"/")
pathAudio <- paste0("P:/Projets/Actif/2023_ECCC4_Biodiv/3-Analyses/1-Data/AudioMoth_recordings/Audiomoths_2024_Summer/", codeSite, "/")
pathInput <- paste0("C:/Projects/AudiomothData/TimingAnalysis/", codeSite,"/") # Indices 
pathOutput <- paste0("C:/Projects/AudiomothData/TimingAnalysis/") 
pathPlot <- pathOutput
# pathAudio <- paste0("C:/Projects/AudiomothData/Audiomoths_2024_Spring_test/", codeSite,"/")
# pathOutput <- paste0("C:/Projects/AudiomothData/Audiomoths_2024_Spring_test_SS/", codeSite,"/")



#### SELECT INDICES ####
AudioIndices <- c("ndsi", "H", "acoustic_complexity")

#### PROCESS OUTPUT ####

# LOAD DATA
DFday <- load_daily(path = pathAudio, site = codeSite)

results_H <- processOutput(df = DFday, index = "H", path = pathInput, milieu = milieuxType)
results_ACI <- processOutput(df = DFday, index = "acoustic_complexity", path = pathInput, milieu = milieuxType)
results_ndsi <- processOutput(df = DFday, index = "ndsi", path = pathInput, milieu = milieuxType)

# #### WRITE TO DISK ####
write.csv(results_H, paste0(pathOutput, codeSite, "H.csv"))
write.csv(results_ACI, paste0(pathOutput, codeSite, "aci.csv"))
write.csv(results_ndsi, paste0(pathOutput, codeSite, "ndsi.csv"))

#### READ CSV ####

# List of all result files for chosen index and chosen site
#LAN_MF
#MON_CG
#CDQ_EB
site <- "All" #either a specific site or "All"

results_H <- load_site(site = site, AudioIndex = "H", path = pathOutput)
results_ACI <- load_site(site = site, AudioIndex = "aci", path = pathOutput)
results_ndsi <- load_site(site = site, AudioIndex = "ndsi", path = pathOutput)

#### CLEAN UP
# Discard soundfiles less than 5 mins #####
# And fix so plot starts at dawn if recording went past midnight #####
# Certain Audiomoths kept recording for longer, cut extra days out ###
# Cut the audio tests out 
# Filter out outliers

results <- list(results_H, results_ACI, results_ndsi)

for(i in seq_along(results)){
        results[[i]] <- results[[i]] %>%
                dplyr::filter(DURATION >= 300) %>%
                #dplyr::filter(date < as.Date("2024-06-08")) %>% #MON_CG seulement
                dplyr::filter(!(site %in% c("MON_CG_Coulee1", "MON_CG_Foret1") & date > as.Date("2024-06-08")) | !(site %in% c("MON_CG_Coulee1", "MON_CG_Foret1")))%>% # For MON_CG, discard sites where recording went on for too long
                #dplyr::filter(site != "CDQ_EB_Chrono2") %>%    # CDQ seulement (discard poorly fitting sites)
                dplyr::filter(time > hms("04:00:00")) %>%
                dplyr::filter(time != hms("13:36:24")) %>% # Discard that one recording of Vincent doing an audio test
                dplyr::filter(time != hms("10:24:37")) %>% # Discard that one other recording of Vincent doing an audio test
                dplyr::filter(LEFT_CHANNEL >= (quantile(results[[i]]$LEFT_CHANNEL, 0.25) - IQR(LEFT_CHANNEL)) & # Removing outliers (values that are beyond the 1st or 3rd quantiles + the IQR)
                                LEFT_CHANNEL <= (quantile(results[[i]]$LEFT_CHANNEL, 0.75) + IQR(LEFT_CHANNEL))) %>%
                #dplyr::filter(site_type %in% c("Foret", "Coulee")) # Keep foret and coulee
                dplyr::filter(site_type == "Chrono")

}

View(results[[1]])

##### GGPLOT2 ####

#### Scatter plot time of day

plot_daily <- function(df){
        index <- df$INDEX[1]
        a <- ggplot(df %>% dplyr::filter(AM_PM == "AM"), aes(time, LEFT_CHANNEL, color = site_type)) +
                scale_color_manual(values = c("Coulee" = "#e1d750", "Foret" = "#5ba366", "Chrono" = "#e98465"))+
                geom_point()+
                geom_smooth(aes(group = site_type), method = "loess", se = FALSE) + 
                ggtitle(paste0(index, " thru morning"))
        
        b <- ggplot(df %>% dplyr::filter(AM_PM == "PM"), aes(time, LEFT_CHANNEL, color = site_type)) +
                scale_color_manual(values = c("Coulee" = "#e1d750", "Foret" = "#5ba366", "Chrono" = "#e98465"))+
                geom_point()+
                geom_smooth(aes(group = site_type), method = "loess", se = FALSE) + 
                ggtitle(paste0(index, " thru evening")) 

        return(list(a, b))
}


plot_list <- lapply(results, plot_daily) # apply the above function to each result index

plot_list <- unlist(plot_list, recursive = FALSE) #un list to access each plot individually

grid <- grid.arrange(grobs = plot_list, ncol = 2, nrow = 3, top = site) # outputs n(index) * 2 plots

ggsave(paste0(pathPlot, site, "_ScatterTime.png"), grid)

### Boxplots AM vs PM

boxplot_daily <- function(df){
        index <- df$INDEX[1]
        
        a <- ggplot(df %>% dplyr::filter(AM_PM == "AM"), aes(, LEFT_CHANNEL, color = site_type)) +
                geom_boxplot() + 
                scale_color_manual(values = c("Coulee" = "#e1d750", "Foret" = "#5ba366", "Chrono" = "#e98465"))+
                theme(
                        axis.text.x=element_blank(),
                        axis.ticks.x = element_blank())+
                ggtitle(paste0(index, " thru morning"))
        
        b <- ggplot(df %>% dplyr::filter(AM_PM == "PM"), aes(, LEFT_CHANNEL, color = site_type)) +
                geom_boxplot() + 
                scale_color_manual(values = c("Coulee" = "#e1d750", "Foret" = "#5ba366", "Chrono" = "#e98465"))+
                theme(
                        axis.text.x=element_blank(),
                        axis.ticks.x = element_blank())+
                ggtitle(paste0(index, " thru evening")) 

        return(list(a, b))
}

               
plot_list <- lapply(results, boxplot_daily) # apply the above function to each result index

plot_list <- unlist(plot_list, recursive = FALSE) #un list to access each plot individually

grid <- grid.arrange(grobs = plot_list, ncol = 2, nrow = 3, top = site) # outputs n(index) * 2 plots

ggsave(paste0(pathPlot, site, "_BoxTime.png"), grid)

### Loess + confidence interval plots

#plot_name <- site
plot_name <- "All_coulee_foret"

loess_ci_daily <- function(df){
        index <- df$INDEX[1]
        a <- ggplot(df %>% dplyr::filter(AM_PM == "AM"), aes(time, LEFT_CHANNEL, color = site_type)) +
                scale_color_manual(values = c("Coulee" = "#e1d750", "Foret" = "#5ba366", "Chrono" = "#e98465"))+
                geom_smooth(aes(group = site_type), method = "loess", se = TRUE) + 
                labs(y = NULL) + 
                theme(legend.position = "none") + 
                ggtitle(paste0(index, " - AM"))
        
        b <- ggplot(df %>% dplyr::filter(AM_PM == "PM"), aes(time, LEFT_CHANNEL, color = site_type)) +
                scale_color_manual(values = c("Coulee" = "#e1d750", "Foret" = "#5ba366", "Chrono" = "#e98465"))+
                geom_smooth(aes(group = site_type), method = "loess", se = TRUE) + 
                labs(y = NULL) + 
                ggtitle(paste0(index, " - PM")) 

        return(list(a, b))
}


plot_list <- lapply(results, loess_ci_daily) # apply the above function to each result index

plot_list <- unlist(plot_list, recursive = FALSE) #un list to access each plot individually

grid <- grid.arrange(grobs = plot_list, ncol = 2, nrow = 3) # outputs n(index) * 2 plots # top = plot_name

ggsave(paste0(pathPlot, plot_name, "_loessTime.png"), grid)



### Loess + confidence interval plots for individual sites

#plot_name <- site
plot_name <- "All_chrono"


loess_ci_individual <- function(df){
        
        index <- df$INDEX[1]
        
        # Set colours        
        colors <- colorRampPalette(c("#f8501d", "#e9d83d"))(length(unique(df$site)))
        names(colors) <- unique(df$site)


        # Set custom legend labels for joelle
        labs <- c("CDQ_EB_Chrono1 (7 yrs)", "CDQ_EB_Chrono2 (35 yrs)")


        a <- ggplot(df %>% dplyr::filter(AM_PM == "AM"), aes(time, LEFT_CHANNEL, color = site)) +
                scale_color_manual(values = colors) +
                geom_smooth(aes(group = site), method = "loess", se = TRUE) + 
                labs(y = NULL) + 
                theme(legend.position = "none") + 
                ggtitle(paste0(index, " - AM"))
        
        b <- ggplot(df %>% dplyr::filter(AM_PM == "PM"), aes(time, LEFT_CHANNEL, color = site)) +
                scale_color_manual(values = colors, labels = labs) +
                geom_smooth(aes(group = site), method = "loess", se = TRUE) + 
                labs(y = NULL) + 
                ggtitle(paste0(index, " - PM"))

        return(list(a, b))
}


plot_list <- lapply(results, loess_ci_individual) # apply the above function to each result index

plot_list <- unlist(plot_list, recursive = FALSE) #un list to access each plot individually

grid <- grid.arrange(grobs = plot_list, ncol = 2, nrow = 3) # outputs n(index) * 2 plots # top = plot_name

ggsave(paste0(pathPlot, plot_name, "_loessTime.png"), grid)


# Boxplots all sites


boxplot_all <- function(df){
        index <- df$INDEX[1]
        
        a <- ggplot(df %>% dplyr::filter(AM_PM == "AM"), aes(, LEFT_CHANNEL, color = site)) +
                geom_boxplot() + 
                #scale_color_manual(values = c("Coulee" = "#F8766D", "Foret" = "#619CFF", "Chrono" = "#00BA38"))+
                theme(
                        axis.text.x=element_blank(),
                        axis.ticks.x = element_blank())+
                ggtitle(paste0(index, " thru morning"))
        
        b <- ggplot(df %>% dplyr::filter(AM_PM == "PM"), aes(, LEFT_CHANNEL, color = site)) +
                geom_boxplot() + 
                #scale_color_manual(values = c("Coulee" = "#F8766D", "Foret" = "#619CFF", "Chrono" = "#00BA38"))+
                theme(
                        axis.text.x=element_blank(),
                        axis.ticks.x = element_blank())+
                ggtitle(paste0(index, " thru evening")) 

        return(list(a, b))
}


plot_list <- lapply(results, boxplot_all) # apply the above function to each result index

plot_list <- unlist(plot_list, recursive = FALSE) #un list to access each plot individually

grid <- grid.arrange(grobs = plot_list, ncol = 2, nrow = 3, top = site) # outputs n(index) * 2 plots

ggsave(paste0(pathPlot, site, "_BoxAllTime.png"), grid)

#################
# Plot based on frog activity
#################

frog_sites <-c("CAR-2024-004", "CAR-2024-007", "CAR-2024-009", "CAR-2024-011")

h_plot <- ggplot(results_H, aes(time, LEFT_CHANNEL, color = site, shape = site)) +
            geom_point() +
           scale_color_manual(values = c( 
                    "CAR-2024-004" = "dark green", "CAR-2024-007" = "dark green", "CAR-2024-009" = "dark green", "CAR-2024-011" = "dark green", 
                    "CAR-2024-006" = "light blue", "CAR-2024-008" = "light blue", "CAR-2024-010" = "light blue", "CAR-2024-012" = "light blue")) +
            scale_shape_manual(values = c( 
                    "CAR-2024-004" = 4, "CAR-2024-007" = 4, "CAR-2024-009" = 4, "CAR-2024-011" = 4, 
                    "CAR-2024-006" = 16, "CAR-2024-008" = 16, "CAR-2024-010" = 16, "CAR-2024-012" = 16)) +
            xlab("Time (days)") +
            ggtitle("Acoustic entropy index (H) thru time")

aci_plot <- ggplot(results_ACI, aes(time, LEFT_CHANNEL, color = site, shape = site)) +
            geom_point() +
            scale_color_manual(values = c( 
                    "CAR-2024-004" = "dark green", "CAR-2024-007" = "dark green", "CAR-2024-009" = "dark green", "CAR-2024-011" = "dark green", 
                    "CAR-2024-006" = "light blue", "CAR-2024-008" = "light blue", "CAR-2024-010" = "light blue", "CAR-2024-012" = "light blue")) +
            scale_shape_manual(values = c( 
                    "CAR-2024-004" = 4, "CAR-2024-007" = 4, "CAR-2024-009" = 4, "CAR-2024-011" = 4, 
                    "CAR-2024-006" = 16, "CAR-2024-008" = 16, "CAR-2024-010" = 16, "CAR-2024-012" = 16)) +
            xlab("Time (days)") +
            ggtitle("Acoustic complexity index thru time")

ndsi_plot <- ggplot(results_ndsi, aes(time, LEFT_CHANNEL, color = site, shape = site)) +
            geom_point() +
            scale_color_manual(values = c( 
                    "CAR-2024-004" = "dark green", "CAR-2024-007" = "dark green", "CAR-2024-009" = "dark green", "CAR-2024-011" = "dark green", 
                    "CAR-2024-006" = "light blue", "CAR-2024-008" = "light blue", "CAR-2024-010" = "light blue", "CAR-2024-012" = "light blue")) +
            scale_shape_manual(values = c( 
                    "CAR-2024-004" = 4, "CAR-2024-007" = 4, "CAR-2024-009" = 4, "CAR-2024-011" = 4, 
                    "CAR-2024-006" = 16, "CAR-2024-008" = 16, "CAR-2024-010" = 16, "CAR-2024-012" = 16)) +
            xlab("Time (days)") +
            ggtitle("NDSI thru time")

grid <- grid.arrange(h_plot, aci_plot, ndsi_plot, ncol = 1)


ggsave(paste0(pathPlot, site, "_ScatterFrog.png"), grid)
