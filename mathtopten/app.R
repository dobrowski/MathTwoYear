

#  Load libraries needed ------

library(tidyverse)
library(here)
library(janitor)
library(ggthemes)
library(knitr)
library(kableExtra)
library(formattable)
library(readxl)
library(ggalt)
library(grid)
library(scales)

# UI -------

ui <- fluidPage(
        
        # App title ----
        titlePanel("CAASPP Math Improvement Over Two Years"),
        
        # Sidebar layout with input and output definitions ----
        sidebarLayout(
                
                # Sidebar panel for inputs ----
                sidebarPanel(
                        
                        # checkboxInput("district", label = "Districts Only", value = TRUE),
                        checkboxInput("mry", label = "Monterey County Only", value = TRUE),
                        
                        sliderInput("ELslider", "Percent EL",
                                    min = 0, max = 1, value = c(0, 1)),
                        
                        sliderInput("FRPMslider", "Percent FRPM",
                                    min = 0, max = 1, value = c(0, 1)),
                        
                        sliderInput("gradeslider", "Include Schools that have Highest Grade in this Range",
                                    min = 1, max = 12, value = c(1, 12)),
                        
                        # radioButtons("topgrade", label = h3("Highest Grade at School"),
                        #              choices = list("5" = 5, "6" = 6, "8" = 8, "12" = 12), 
                        #              selected = 12),
                        
                        
                        p(span("Red is 2015-16,", style = "color:red"),
                          span("Orange is 2016-17", style = "color:orange"),
                          " and",
                          span( "Dark blue is 2017-18",  style = "color:darkblue")
                          )
                ),
                
                
                # Main panel for displaying outputs ----
                mainPanel(
                        
                        # Output: Chart ----
                        plotOutput(outputId = "distPlot")
                        
                )
        )


)





#  Load raw data ------



# Clean and refine the data ----


#  Join datasets together ------


sbac2016 <- read_delim( here("data",  "sb_ca2016_1_csv_v3.txt"), delim = ",") %>% select(`County Code`,`District Code`, `School Code`, `Test Year`, `Subgroup ID`, `Test Id`, Grade, `Percentage Standard Met and Above` )

sbac2017 <- read_delim( here("data", "sb_ca2017_1_csv_v2.txt"), delim = ",") %>% select(`County Code`,`District Code`, `School Code`, `Test Year`, `Subgroup ID`, `Test Id`, Grade, `Percentage Standard Met and Above` )

sbac2018 <- read_delim( here("data", "sb_ca2018_1_csv_v3.txt"), delim = ",") %>% select(`County Code`,`District Code`, `School Code`, `Test Year`, `Subgroup ID`, `Test Id`, Grade, `Percentage Standard Met and Above` )




sbac.all <- sbac2016 %>% rbind(sbac2017) %>% rbind(sbac2018)

local.test <- sbac.all %>% 
        filter(
                #              `District Code` %in% c("66159", "66068" ),
                #             `School Code`== "0000000",
                Grade == "13", # All grades 
                `Subgroup ID` == "1", # All students 
                `Test Id` == "2",  # Math
                #             `Subgroup ID` %in% c("8","180","120","142") 
        ) %>% 
        mutate(`Percentage Standard Met and Above` = as.numeric(`Percentage Standard Met and Above`) ) %>%
        select(-Grade, -`Subgroup ID`,-`Test Id`) %>%
        spread(key = `Test Year` , value = `Percentage Standard Met and Above`)


sba.entity <- read_delim(here("data", "sb_ca2018entities_csv.txt"), delim = ",") %>%
        select(`County Code`,`District Code`, `School Code`, `County Name`,`District Name`, `School Name`) #%>% 
#   filter(`County Code` %in% c("00", "27"))

math.local <- local.test %>%
        left_join(sba.entity) %>%
        mutate(`2017` = round(`2017`,digits = 1),
               `2018` = round(`2018`,digits = 1),
               two.year.change = `2018` - `2016`,
               `School Name` = if_else(`School Code` == "0000000", "District", `School Name`),
               isdistrict = if_else(`School Code` == "0000000", TRUE, FALSE),
               ismry = if_else(`County Code` == "27", TRUE, FALSE),
               usename = paste0(`School Name`, "\n",`District Name`)) %>%
        mutate(cds = str_c( str_pad(  as.character(`County Code`) , width = 2, side = "left", pad = "0"  ) ,
                            str_pad(  as.character(`District Code`), width = 5, side = "left", pad = "0"  ) ,
                            str_pad( as.character(`School Code`), width = 7, side = "left", pad = "0"  )  )
        ) 







EL.schools <-read.delim(here("data",  "LtelDownload.txt"))

EL.schools <- EL.schools %>% 
        mutate_at(vars(ends_with("Code")), funs(as.double(.) ) ) %>%
        mutate_at(vars(ends_with("Code")), funs(if_else( is.na(.), 0, .) ) ) %>%
        # filter(str_detect(SchoolName, "Monte Bella")) %>%
        mutate(cds = str_c( str_pad(  as.character(CountyCode) , width = 2, side = "left", pad = "0"  ) ,
                            str_pad(  as.character(DistrictCode), width = 5, side = "left", pad = "0"  ) ,
                            str_pad( as.character(SchoolCode), width = 7, side = "left", pad = "0"  )  )
        ) %>%  # current EL
        filter(Gender == "ALL") %>%
        #        filter(str_detect(CountyName , "Monterey") ) %>%
        group_by(cds) %>%
        mutate(sumEL = sum(EL),
               sumTotal = sum(TotalEnrollment),
               ELpercent = sumEL/sumTotal) %>%
        select(CountyCode,DistrictCode,SchoolCode,DistrictName,SchoolName, Charter, cds, ELpercent) %>%
        ungroup() %>%
        distinct() 


frpm1718 <- read_excel(here("data", "frpm1718.xlsx"), sheet = "FRPM School-Level Data ", range = "A2:AB10477") %>% 
        mutate(cds = str_c(`County Code`,`District Code`,`School Code`)) %>%
        select(`County Code`,`District Code`,`School Code`, cds, starts_with("Percent"), `High Grade`   ) %>%
        select(cds, 6, `High Grade` ) 

frpm1718.county <- read_excel(here("data", "frpm1718.xlsx"), sheet = "FRPM School-Level Data ", range = "A2:AB10477") %>% 
        select(`County Name`, starts_with("Enroll"), starts_with("FRPM")) %>%
        select(1,2,4)


colnames(frpm1718.county) <- (c("county", "enrollment", "frpm"))

frpm1718.county <- frpm1718.county %>%
        group_by(county) %>%
        mutate(total.enroll = sum(enrollment),
               total.frpm = sum(frpm),
               frpm.rate = total.frpm/total.enroll) %>%
        select(county, total.enroll, frpm.rate) %>%
        distinct()



colnames(frpm1718) <- (c("cds", "frpm", "highgrade"))


school.EL.FRPM <- EL.schools %>% left_join(frpm1718) # %>% mutate(cds = as.numeric(cds))



math.local <- math.local %>% left_join(school.EL.FRPM, by = c("cds" = "cds")) %>% mutate(highgrade = as.numeric(highgrade))


#  Graphing ------





server <- function(input, output) {
        
        
        
        
        
        output$distPlot <- renderPlot({
                
                i <- input$select
                lowEL  <- input$ELslider[1] 
                hiEL  <- input$ELslider[2] 
                lowFRPM  <- input$FRPMslider[1] 
                hiFRPM  <- input$FRPMslider[2] 
                lowgrade  <- input$gradeslider[1] 
                higrade  <- input$gradeslider[2] 
#                highestgrade <- input$topgrade
                districtcheck <- input$district
                mrycheck <- input$mry                
                
                
              graph.this<-  math.local %>%
                        arrange(desc(two.year.change)) %>%
                        filter(
                                ismry == mrycheck,
                                isdistrict == FALSE,
                                Charter != "Y",
                                `2016` != 0,
                                ELpercent >= lowEL,
                                 ELpercent <= hiEL,
                                 frpm >= lowFRPM,
                                 frpm <= hiFRPM,              
                                highgrade >= lowgrade,
                                highgrade <= higrade,
                                !is.na(`District Name`),
                  #              !str_detect(`District Name`, "Big Sur")
                        ) %>%
                      head(10)
                        
                        
                        
                        
                
                ggplot(graph.this, aes(x=`2016`/100, xend = `2018`/100, y = fct_reorder( usename,two.year.change) ) ) +
                        geom_dumbbell(color= "grey" ,       #  "#a3c4dc", 
                                      size=1.5, 
                                      colour_x = "red",
                                      colour_xend = "dark blue"         #"#0e668b"
                        ) +
                        geom_dumbbell(aes(x=`2016`/100, xend = `2017`/100),
                                      color= "grey" ,       #  "#a3c4dc", 
                                      size=1.5, 
                                      colour_x = "red",
                                      colour_xend = "orange"         #"#0e668b"
                        ) +
                        theme_hc() +
                        scale_x_continuous(labels = percent ) +
                        labs(y="",
                             x="",
                             title = paste0(" Change in Math \nPercent Meeting or Exceeding \nfrom 2015-16 to 2017-18")
                        ) +
                        geom_text(color="red", size=3, vjust=1.5,
                                  aes(x=`2016`/100, label=`2016`))+
                        geom_text(color="orange", size=3, vjust=1.5,
                                  aes(x=`2017`/100, label=`2017`))+
                        geom_text(aes(x=`2018`/100, label=`2018`), 
                                  color="dark blue", size=3, vjust=-0.5)
                
        }, height=700)
        
}




shinyApp(ui = ui, server = server)