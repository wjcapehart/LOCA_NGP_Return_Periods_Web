#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:x
#
#    http://shiny.rstudio.com/
#

library(package = "shiny")
library(package = "tidyverse")
library(package = "lubridate")
library(package = "ClimClass") 
library(package = "extRemes") 
library(package = "scPDSI")
library(package = "foreach")

#
# Determine if OpenDAP is needed

hostname = system("hostname", intern=TRUE)

local_drives = str_detect(hostname, "ias.sdsmt.edu")

#


#
# Input Cliamte Data
#
if (local_drives) 
  {
    root_LOCA_URL = "/maelstrom2/LOCA_GRIDDED_ENSEMBLES/LOCA_NGP/climate_divisions/"
  } else
  {
    root_LOCA_URL = "http://kyrill.ias.sdsmt.edu:8080/thredds/fileServer/LOCA_NGP/climate_divisions/"
  }

if (local_drives) 
{
  load(file = "/maelstrom2/LOCA_GRIDDED_ENSEMBLES/LOCA_NGP/climate_divisions/Completed_Divisions.RData")
} else {
  load(file = url(description = "http://kyrill.ias.sdsmt.edu:8080/thredds/fileServer/LOCA_NGP/climate_divisions/Completed_Divisions.RData"))
}
Completed_Divisions = tibble(Full_Zone_Code = Completed_Divisions)


if (local_drives) 
{
  load(file = "/projects/THREDDS/local_academic_repo/CLASS_Examples/nCLIMDIV.Rdata")
} else {
  load(file = url(description = "http://kyrill.ias.sdsmt.edu:8080/thredds/fileServer/CLASS_Examples/nCLIMDIV.Rdata"))
}

nCLIMDIV$State_Name = gsub(pattern     = "\u00A0", 
                           replacement = "", 
                           x           = nCLIMDIV$State_Name, 
                           fixed       = TRUE)

nCLIMDIV = right_join(x = nCLIMDIV,
                      y = Completed_Divisions,
                      by = "Full_Zone_Code")

nCLIMDIV = nCLIMDIV %>% select(Full_Zone_Code,
                               Date,
                               State_Name,
                               Zone_Name,
                               Center_Lon,
                               Center_Lat,
                               TMAX,
                               TMIN,
                               TMPC,
                               PCPN)

#
# Create Pulldown Look-up-Tables
#  

state_zone_lut =  nCLIMDIV %>% 
    select(c(Full_Zone_Code,State_Name,Zone_Name))   %>%
    mutate(State_Code    = substring(text  = Full_Zone_Code,
                                     first = 1,
                                     last  = 2),
           SubState_Code = substring(text  = Full_Zone_Code,
                                     first = 3,
                                     last  = 4)) %>%
    mutate(Zone_Name_and_Code = str_c(SubState_Code,
                                      Zone_Name,
                                      sep = " : "),
           Zones_Per_State    = max(SubState_Code) ) %>%
    unique()



state_code_lut = state_zone_lut %>% 
    group_by(State_Name) %>% 
    mutate(Zones_Per_State = max(SubState_Code)) %>%
    select(c(State_Name,
             State_Code,
             Zones_Per_State))  %>%
    unique()


#
# Input Available LOCA Files
#


    



selected_zones = state_zone_lut %>% 
    filter(State_Name == "South Dakota")


state_number_init =  as.numeric(unique(selected_zones$State_Code))


state_number = state_number_init





#
# Pull Atlas 14
#




if (local_drives) 
{
  load(file = "/projects/THREDDS/local_academic_repo/CLASS_Examples/Atlas_14_NGP_by_Climate_Divisions.RData")
} else {
  load(file = url(description = "http://kyrill.ias.sdsmt.edu:8080/thredds/fileServer/CLASS_Examples/Atlas_14_NGP_by_Climate_Divisions.RData"))
}





###############################################################################
###############################################################################
##
## User Interface Function
##


ui = fluidPage(
    
    title = "CMIP5-LOCA NCEI Climate Division Precip Extreme Event Analysis",  
    
    titlePanel(title = "CMIP5-LOCA NCEI Climate Division Precip Extreme Event Analysis"),   # Application title
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel = sidebarPanel(
            
            selectInput(inputId  = "target_state_name",
                        label    = "US State",
                        choices  = state_code_lut$State_Name,
                        selected = state_code_lut$State_Name[1]),
            
            selectInput(inputId  = "target_climate_division",
                        label    = "State Climate Zone Division",
                        choices  = selected_zones$Zone_Name_and_Code,
                        selected = selected_zones$Zone_Name_and_Code[1]),
            
            uiOutput("target_climate_division"),
            
            p("BE PATIENT!"),  
            
            
            
            h5("State Climate Division Map"),
            
            imageOutput(outputId = "state_division_map")
        ),
        
        
        
        
        
        # Show a plot of the generated distribution
        mainPanel = mainPanel(
          h2("Max Precip Returns"), 
          plotOutput(outputId = "ReturnViolinPlot"),
          plotOutput(outputId = "ReturnPlot"),
          
            h2("Introduction"), 
            
             
            h2("Instructions")
            
            

        )
    )
)


##
###############################################################################
###############################################################################




#######################################################
#
# Credit for Split Violin Plot for GGPLOT2 Library
#   
# Jan Gleixner & Wouter van der Bijl : https://stackoverflow.com/questions/35717353/split-violin-plot-with-ggplot2/45614547#45614547
#
GeomSplitViolin <- ggproto("GeomSplitViolin", 
                           GeomViolin, 
                           draw_group = function(self, 
                                                 data, 
                                                 ..., 
                                                 draw_quantiles = NULL) {
                             
                             # Original function by Jan Gleixner (@jan-glx)
                             # Adjustments by Wouter van der Bijl (@Axeman)
                             
                             data <- transform(data, 
                                               xminv = x - violinwidth * (x - xmin), 
                                               xmaxv = x + violinwidth * (xmax - x))
                             
                             grp <- data[1, "group"]
                             
                             newdata <- plyr::arrange(transform(data, x = if (grp %% 2 == 1) 
                               xminv 
                               else 
                                 xmaxv), 
                               if (grp %% 2 == 1) 
                                 y 
                               else 
                                 -y)
                             
                             newdata <- rbind(newdata[1, ], newdata, newdata[nrow(newdata), ], newdata[1, ])
                             
                             newdata[c(1, nrow(newdata) - 1, nrow(newdata)), "x"] <- round(newdata[1, "x"])
                             
                             if (length(draw_quantiles) > 0 & 
                                 !scales::zero_range(range(data$y))) {
                               
                               stopifnot(all(draw_quantiles >= 0), 
                                         all(draw_quantiles <= 1))
                               
                               quantiles <- create_quantile_segment_frame(data, 
                                                                          draw_quantiles, 
                                                                          split = TRUE,
                                                                          grp   = grp)
                               aesthetics <- data[rep(1, 
                                                      nrow(quantiles)), 
                                                  setdiff(names(data), 
                                                          c("x", "y")), 
                                                  drop = FALSE]
                               
                               aesthetics$alpha <- rep(1, 
                                                       nrow(quantiles))
                               
                               both <- cbind(quantiles, 
                                             aesthetics)
                               
                               quantile_grob <- GeomPath$draw_panel(both, ...)
                               
                               ggplot2:::ggname("geom_split_violin", 
                                                grid::grobTree(GeomPolygon$draw_panel(newdata, 
                                                                                      ...), 
                                                               quantile_grob))
                               
                             } else {
                               
                               ggplot2:::ggname("geom_split_violin", 
                                                GeomPolygon$draw_panel(newdata, 
                                                                       ...))
                               
                             }
                           }
)

create_quantile_segment_frame <- function(data, 
                                          draw_quantiles, 
                                          split = FALSE, 
                                          grp = NULL) 
{
  
  dens <- cumsum(data$density) / sum(data$density)
  
  ecdf <- stats::approxfun(dens, 
                           data$y)
  
  ys <- ecdf(draw_quantiles)
  
  violin.xminvs <- (stats::approxfun(data$y, 
                                     data$xminv))(ys)
  
  violin.xmaxvs <- (stats::approxfun(data$y, 
                                     data$xmaxv))(ys)
  
  violin.xs <- (stats::approxfun(data$y, 
                                 data$x))(ys)
  
  if (grp %% 2 == 0) {
    
    data.frame(x     = ggplot2:::interleave(violin.xs, 
                                            violin.xmaxvs),
               y     = rep(ys, each = 2), 
               group = rep(ys, each = 2))
    
  } else {
    data.frame(x     = ggplot2:::interleave(violin.xminvs,
                                            violin.xs),
               y     = rep(ys, each = 2), 
               group = rep(ys, each = 2))
  }
}

geom_split_violin <- function(mapping        = NULL, 
                              data           = NULL, 
                              stat           = "ydensity", 
                              position       = "identity", 
                              ..., 
                              draw_quantiles = NULL, 
                              trim           = TRUE, 
                              scale          = "area", 
                              na.rm          = FALSE, 
                              show.legend    = NA, 
                              inherit.aes    = TRUE) {
  
  layer(data        = data, 
        mapping     = mapping, 
        stat        = stat, 
        geom        = GeomSplitViolin, 
        position    = position, 
        show.legend = show.legend, 
        inherit.aes = inherit.aes, 
        params      = list(trim          = trim, 
                           scale         = scale, 
                           draw_quantiles = draw_quantiles, 
                           na.rm          = na.rm, 
                           ...))
}
#
#######################################################




###############################################################################
###############################################################################
##
## Server Function
##

server = function(input, 
                  output,
                  session) {
    
    
    
    
    
    
    
    ###############################################################################
    #
    # In-State Climate Zone Selection
    #
    
    observeEvent(input$target_state_name,  {
        
        selected_zones = state_zone_lut %>% 
            filter(State_Name == input$target_state_name) 
        
        state_number = as.numeric(unique(selected_zones$State_Code))
        
        updateSelectInput(session = session, 
                          inputId = "target_climate_division", 
                          choices = selected_zones$Zone_Name_and_Code)
    }
    )
    
    #
    ###############################################################################
    
    
    
    
    ###############################################################################
    #
    # In-State Climate Map for Division Selection
    #
    
    output$state_division_map = renderImage({
        
        
        
        selected_zones = state_zone_lut %>% 
            filter(State_Name == input$target_state_name) 
        
        state_number = as.numeric(unique(selected_zones$State_Code))
        
        
        filename <- normalizePath(file.path('./state_climate_division_images',
                                            paste('state_', 
                                                  sprintf("%02d",
                                                          state_number), 
                                                  '.gif', 
                                                  sep='')))
        
        # Return a list containing the filename
        list(src   = filename,
             width = "100%")
    }, 
    deleteFile = FALSE)
    
    #
    ###############################################################################
    
    
    
    ###############################################################################
    ###############################################################################
    ##
    ## Pulling Local Annual NCEI Data 
    
    local_nCLIMDIV_monthly <- reactive({

        target_climate_division = state_code_lut %>% 
            filter(State_Name == input$target_state_name)
        
        target_climate_division = str_c(target_climate_division$State_Code,
                                        substring(text  = input$target_climate_division,
                                                  first = 1,
                                                  last  = 2), 
                                        sep = "")
        
        local_nCLIMDIV_monthly = nCLIMDIV %>% 
            filter((Full_Zone_Code == target_climate_division) & 
                   (year(Date)     >= 1950),
                   (year(Date)     <  2019)) %>%
            mutate(TMAX = (TMAX - 32) * 5./9,
                   TMIN = (TMIN - 32) * 5./9,
                   TMPC = (TMPC - 32) * 5./9,
                   PCPN =  PCPN * 25.4)
      
        remove(target_climate_division)
        print(local_nCLIMDIV_monthly[1,])
        
        return(local_nCLIMDIV_monthly)
        
    })
    
    #
    ###############################################################################
    ###############################################################################
    
    
    
    ###############################################################################
    ###############################################################################
    ##
    ## Pulling Local Annual LOCA Output
    
    atlas14_returns <- reactive({
      
      target_climate_division = state_code_lut %>% 
        filter(State_Name == input$target_state_name)
      
      target_climate_division = str_c(target_climate_division$State_Code,
                                      substring(text  = input$target_climate_division,
                                                first = 1,
                                                last  = 2), 
                                      sep = "")
      
      atlas14_returns = Atlas_14_NGP %>% 
        filter((Full_Zone_Code == target_climate_division))
      
      print(atlas14_returns)
      
      return(atlas14_returns)
      
    })
    
    #
    ###############################################################################
    ###############################################################################
    
    
    ###############################################################################
    ###############################################################################
    ##
    ## Pulling Local Annual LOCA Output
    
    loca_daily <- reactive({

        # get the target climate division
        
        target_climate_division = state_code_lut %>% 
            filter(State_Name == input$target_state_name)
        
        target_climate_division = str_c(target_climate_division$State_Code,
                                        substring(text  = input$target_climate_division,
                                                  first = 1,
                                                  last  = 2), 
                                        sep = "")
        
        loca_filename = str_c("NGP_LOCA_nCLIMDIV_",
                              target_climate_division,
                              ".RData",
                              sep = "")
        
        LOCA_URL     = str_c(root_LOCA_URL,
                             loca_filename,
                             sep = "")
        
        
        
        if (local_drives) 
        {
          load(file    = LOCA_URL,
               verbose = TRUE)
        } else {
          load(file    = url(LOCA_URL),
               verbose = TRUE)
        }
        
        
        


        loca_daily %>% 
            filter(Percentile == "P100") %>% 
            select(Time,
                   Division,
                   Ensemble,
                   Scenario,
                   pr)
        
        
        remove(LOCA_URL)
        remove(loca_filename)
        remove(target_climate_division)
        
        print(loca_daily[1,])
        
        
        return(loca_daily)
        
    })
    
    #
    ###############################################################################
    ###############################################################################
    
    
    ###############################################################################
    ###############################################################################
    ##
    ## Calculating Return Periods
    ##
    
    
    return_events <- reactive({
      
      local_local_daily = loca_daily()
      
      target_climate_descriptions = state_zone_lut %>% 
        filter(State_Name         == input$target_state_name,
               Zone_Name_and_Code == input$target_climate_division)
      

      
      target_climate_division = str_c(target_climate_descriptions$State_Code,
                                      substring(text  = input$target_climate_division,
                                                first = 1,
                                                last  = 2), 
                                      sep = "")
      
      # Select Periods
      
      # "Historical" Period
      
      reference_period_start = 1981
      reference_period_end   = 2005
      
      # "Future" Period
      
      future_period_start    = 2036
      future_period_end      = 2065
      
      loca_histo = local_local_daily %>%
        filter((year(Time) >= reference_period_start) &
                 (year(Time) <= reference_period_end)   &
                 (Percentile == "P100")                 ) %>%
        mutate(Scenario = str_c(Scenario,
                                " (",
                                reference_period_start,
                                "-",
                                reference_period_end,
                                ")",
                                sep = ""))   
      
      loca_futr = local_local_daily %>%
        filter((year(Time) >= future_period_start)    &
                 (year(Time) <= future_period_end)      &
                 (Percentile == "P100")                 )  %>%
        mutate(Scenario = str_c(Scenario,
                                " (",
                                future_period_start,
                                "-",
                                future_period_end,
                                ")",
                                sep = "")) 
      
      loca_periods = rbind(loca_histo,
                           loca_futr)
      

      
      remove(loca_histo)
      remove(loca_futr)
      
      Scenarios = unique(loca_periods$Scenario)
      Ensembles = unique(loca_periods$Ensemble)
      
      loca_location_data = state_zone_lut %>%
        filter(Full_Zone_Code == target_climate_division)
      
      State_Name    = unique(loca_location_data$State_Name)
      Division_Name = unique(loca_location_data$Zone_Name)
      
      foreach (i = 1:length(Ensembles), .combine = "rbind") %do%
      {
        ensemble = Ensembles[i]
        first = TRUE
        for (scenario in Scenarios) 
        {
          
          loca_scen_ens = loca_periods %>%
            filter((Ensemble == ensemble),
                   (Scenario == scenario))
          
          
          threshold = quantile(x     = loca_scen_ens$pr,
                               probs = 0.95)
          
          fit_GP_daily = fevd(x          = loca_scen_ens$pr, 
                              threshold  = threshold,
                              units      = "mm",
                              time.units = "365/year",
                              type       = "GP"
          )
          
          year_return   = c( 2:100.,
                             141.,
                             200.,
                             316.,
                             500.)
          
          
          return_ci = ci(x             = fit_GP_daily, 
                         return.period = year_return)


          if ((scenario == Scenarios[1]) & (ensemble == Ensembles[1]))
          {
            return_events = tibble(Ensemble           = ensemble,
                                   Scenario           = scenario,
                                   State              = State_Name,
                                   Division           = Division_Name,
                                   Return_Period      = year_return,
                                   Return_Estimate_05 = return_ci[,1],
                                   Return_Estimate    = return_ci[,2],
                                   Return_Estimate_95 = return_ci[,3])
            
          } else # first run
          {
            delete_me     = tibble(Ensemble           = ensemble,
                                   Scenario           = scenario,
                                   State              = State_Name,
                                   Division           = Division_Name,
                                   Return_Period      = year_return,
                                   Return_Estimate_05 = return_ci[,1],
                                   Return_Estimate    = return_ci[,2],
                                   Return_Estimate_95 = return_ci[,3])
            return_events = rbind(return_events,
                                  delete_me)
            
          } # not the first run
          
        } # ensemble
        
      } # scenario
      
      remove(loca_scen_ens)
      remove(threshold)
      remove(fit_GP_daily)
      remove(delete_me)
      
      return(return_events)
    })
    
    ##
    ###############################################################################
    ###############################################################################
    
    
    ###############################################################################
    #
    #  Plot Violin  
    #
    
    output$ReturnViolinPlot = renderPlot(expr = {
      
      return_events_local = return_events()
      atlas14_returns_local = atlas14_returns()
      
      
      
      State_Name    =  unique(return_events_local$State)
      Division_Name =  unique(return_events_local$Division)
      
      
      ###############################################################################
      #
      # Draw Plot
      #
      print("Entering Draw Prec")
      
      
      
      subset = return_events_local %>% 
        filter( ((Return_Period %% 2) ==  0)       &
                  (Return_Period       <= 30)       
                &   (!str_detect(string  =   Scenario,
                               pattern = "RCP 4.5") ) 
                )
      
      subset$Scenario = factor(x = as.character(subset$Scenario),
                                levels = c("Historical (1981-2005)",
                                           "RCP 8.5 (2036-2065)",
                                           "RCP 4.5 (2036-2065)"))
      
      
      atlas14_returns_local = atlas14_returns_local%>% 
        filter( (Return_Period >=   2) &
                (Return_Period <= 30))
      
    
      
      subset2 = return_events_local %>% 
        filter( ((Return_Period %% 2) == 0) &
                  (Return_Period <= 30)) %>%
        group_by(Scenario,
                 Return_Period,
                 State,
                 Division) %>%
        summarize(Return_Estimate    = median(Return_Estimate),
                  Median_Return_Estimate_05 = median(Return_Estimate_05),
                  Median_Return_Estimate_95 = median(Return_Estimate_95))
      
      subset$Return_Period  = as.factor(subset$Return_Period)
      subset2$Return_Period = as.factor(subset2$Return_Period)
      
      subset2$Scenario = factor(x = as.character(subset2$Scenario),
                                levels = c("Historical (1981-2005)",
                                           "RCP 8.5 (2036-2065)",
                                           "RCP 4.5 (2036-2065)"))
      print(subset[1,])
      print(subset2[1,])
      print("ggplot me!")
      
      print(subset2$Scenario)
      
      myplot = ggplot(data = subset) + 
        
        aes(x     = Return_Period,
            y     = Return_Estimate) +
        
        theme_bw() +
        
        theme(legend.position       = c(0.02, 0.98),
              legend.justification  = c(0.00, 1.00)) + 
        
        ggtitle(label    = "LOCA-Dervied Extreme Events",
                subtitle = str_c(Division_Name,
                                 " Climate Division, ",
                                 State_Name,
                                 sep = "")) + 
        
        ylab(label = "Daily Rainfall (mm)") +
        
        xlab(label = "Return Periods (years)") +
        labs(caption = "LOCA Downscaled CMIP5 Analyses") +
        
        geom_split_violin(data = subset,
                          mapping = aes(x     = Return_Period,
                                        y     = Return_Estimate,
                                        fill  = Scenario),
                          color = NA,
                          trim  = FALSE,
                          alpha = 0.5) +
        
        geom_line(data = subset2,
                  mapping = aes(x     = Return_Period,
                                y     = Return_Estimate,
                                color = Scenario,
                                group = Scenario),
                  fill = NA) +
      
        
        scale_fill_manual(values = c( "cyan",      "magenta", "yellow")) + 
        scale_color_manual(values = c( "darkcyan", "darkred", "goldenrod")) 
        
        

      
      remove(subset, subset2)
      return(myplot)  
      
      #
      ###############################################################################
      
      
    })
    
    #
    ###############################################################################
    
    
    ###############################################################################
    #
    #  Plot ReturnPlot  
    #
    
    output$ReturnPlot = renderPlot(expr = {
      
      return_events_local = return_events()
      
      atlas14_returns_local = atlas14_returns()
      print("in")

      return_events_local$Scenario   = as.character(return_events_local$Scenario)
      atlas14_returns_local$Scenario = "NOAA Atlas 14"
      atlas14_returns_local$Scenario = as.character(atlas14_returns_local$Scenario)
      
      State_Name    =  unique(return_events_local$State)
      Division_Name =  unique(return_events_local$Division)
      
      
      ###############################################################################
      #
      # Draw Plot
      #
      print("Entering Draw Prec")
      
      
      atlas14_returns_local = atlas14_returns_local%>% 
        filter( Return_Period <= 100 ) %>%
        rename( Median_Return_Estimate    = Mean_Precip_Amount,
                Median_Return_Estimate_05 = Mean_Precip_Amount_High,
                Median_Return_Estimate_95 = Mean_Precip_Amount_Low)
      
      atlas14_returns_local$Scenario = "NOAA Atlas 14"
      
        
      
      subset2 = return_events_local %>% 
        filter( Return_Period <= 100) %>%
        group_by(Scenario,
                 Return_Period,
                 State,
                 Scenario,
                 Division) %>%
        summarize(Median_Return_Estimate    = median(Return_Estimate),
                  Median_Return_Estimate_05 = median(Return_Estimate_05),
                  Median_Return_Estimate_95 = median(Return_Estimate_95))
      
      subset2$Return_Period = as.numeric(subset2$Return_Period)
      

      

      
      
      subset3 = rbind(subset2,
                      atlas14_returns_local)
      subset3$Scenario = as.character(subset3$Scenario)
      print(unique(subset3$Scenario))
                                
      subset3$Scenario = factor(x = as.character(subset3$Scenario),
                                levels = c("Historical (1981-2005)",
                                           "RCP 8.5 (2036-2065)",
                                           "RCP 4.5 (2036-2065)",
                                           "NOAA Atlas 14"))
      
     
      
      print("ggplot me 2!")
      
      myplot = ggplot(data = subset3) + 
        
        aes(x     = Return_Period,
            y     = Median_Return_Estimate,
            ymax  = Median_Return_Estimate_05,
            ymin  = Median_Return_Estimate_95,
            group = Scenario,
            color = Scenario,
            fill  = Scenario) +
        
        theme_bw() +
        
        theme(legend.position       = c(0.02, 0.98),
              legend.justification  = c(0.00, 1.00)) + 
        
        ggtitle(label    = "LOCA-Dervied Extreme Events",
                subtitle = str_c(Division_Name,
                                 " Climate Division, ",
                                 State_Name,
                                 sep = "")) + 
        
        ylab(label = "Daily Rainfall (mm)") +
        
        xlab(label = "Return Periods (years)") +
        labs(caption = "LOCA Downscaled CMIP5 Analyses") +


  
        
                
        scale_fill_manual(values = c("cyan",  "magenta","yellow", "grey")) + 
        
        scale_color_manual(values = c( "darkblue", "darkred", "goldenrod", "darkgrey")) + 
        
      geom_ribbon(alpha = 0.2,
                  color = NA)  + 
        
        
        geom_line() 
        

      myplot
      
      remove(subset2)
      return(myplot)  
      
      #
      ###############################################################################
      
      
    })
    
    #
    ###############################################################################
    
    
}
    
        

    


  
##
###############################################################################
###############################################################################







###############################################################################
###############################################################################
##
## shinyApp Function
##

# Run the application 
shinyApp(ui     = ui, 
         server = server)

##
###############################################################################
###############################################################################

