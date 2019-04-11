
### Libraries -------

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
        tidyverse
        ,here
        ,janitor
        ,ggthemes
        ,knitr
        ,kableExtra
        ,formattable
        ,readxl
        ,ggalt
        ,grid
        ,scales
)


### Data Sources ------

# https://caaspp.cde.ca.gov/sb2018/research_fixfileformat18

# sbac2015 <- read_delim( here("data",  "sb_ca2015_all_27_csv_v3.txt"), delim = ",") %>% select(`District Code`, `School Code`, `Test Year`, `Subgroup ID`, `Test Id`, Grade, `Percentage Standard Met and Above` )
# 
# sbac2016 <- read_delim( here("data",  "sb_ca2016_all_27_csv_v3.txt"), delim = ",") %>% select(`District Code`, `School Code`, `Test Year`, `Subgroup ID`, `Test Id`, Grade, `Percentage Standard Met and Above` )
# 
# sbac2017 <- read_delim( here("data", "sb_ca2017_all_27_csv_v2.txt"), delim = ",") %>% select(`District Code`, `School Code`, `Test Year`, `Subgroup ID`, `Test Id`, Grade, `Percentage Standard Met and Above` )
# 
# sbac2018 <- read_delim( here("data", "sb_ca2018_all_27_csv_v3.txt"), delim = ",") %>% select(`District Code`, `School Code`, `Test Year`, `Subgroup ID`, `Test Id`, Grade, `Percentage Standard Met and Above` )



sbac2016 <- read_delim( here("data",  "sb_ca2016_1_csv_v3.txt"), delim = ",") %>% select(`County Code`,`District Code`, `School Code`, `Test Year`, `Subgroup ID`, `Test Id`, Grade, `Percentage Standard Met and Above` )

sbac2017 <- read_delim( here("data", "sb_ca2017_1_csv_v2.txt"), delim = ",") %>% select(`County Code`,`District Code`, `School Code`, `Test Year`, `Subgroup ID`, `Test Id`, Grade, `Percentage Standard Met and Above` )

sbac2018 <- read_delim( here("data", "sb_ca2018_1_csv_v3.txt"), delim = ",") %>% select(`County Code`,`District Code`, `School Code`, `Test Year`, `Subgroup ID`, `Test Id`, Grade, `Percentage Standard Met and Above`, `Students Tested` )



# To identify small districts with high levels of achievement

smalltest <- sbac2018 %>%
        mutate(cds = str_c( str_pad(  as.character(`County Code`) , width = 2, side = "left", pad = "0"  ) ,
                            str_pad(  as.character(`District Code`), width = 5, side = "left", pad = "0"  ) ,
                            str_pad( as.character(`School Code`), width = 7, side = "left", pad = "0"  )  )
        ) %>%
        mutate(`Students Tested` = as.numeric(`Students Tested`),
               `Percentage Standard Met and Above` = as.numeric(`Percentage Standard Met and Above`)) %>%
        left_join(school.EL.FRPM, by = c("cds" = "cds")) %>% 
        filter(`Students Tested` <150,
               Grade == 13,
               frpm >= .85,
               Charter == "N",
               highgrade %in% c("5","6","8")) %>%
        filter(`Test Id` == "1") %>% #1 = ELA and 2 = Math
        arrange(desc(`Percentage Standard Met and Above`)) %>%
        head(10) %>%
        select(`Test Id`,`Percentage Standard Met and Above`, `Students Tested`, DistrictName, SchoolName, ELpercent, frpm)

clipr::write_clip(smalltest)



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



math.local <- math.local %>% left_join(school.EL.FRPM, by = c("cds" = "cds"))




#### Dumbell Graphs ------

t1 <- textGrob(expression(phantom("Red is 2015-16")  * " and" * phantom(bold("Dark blue is 2017-18")) ),
               x = 0.1, y = 1.1, gp = gpar(col = "black"))

t2 <- textGrob(expression( bold("Red is 2015-16") * phantom(" and Dark blue is 2017-18")),
               x = 0.1, y = 1.1, gp = gpar(col = "red"))

t3 <- textGrob(expression(phantom("Red is 2015-16 and ") * bold("Dark blue is 2017-18") ),
               x = 0.1, y = 1.1, gp = gpar(col = "dark blue"))



change.graph  <- math.local %>%
        arrange(desc(two.year.change)) %>%
        filter(`School Code` != "0000000") %>%
        head(10) %>%
        ggplot( aes(x=`2016`/100, xend = `2018`/100, y = fct_reorder( `School Name`,two.year.change) ) ) +
        geom_dumbbell(color= "grey" ,       #  "#a3c4dc", 
                      size=1.5, 
                      colour_x = "red",
                      colour_xend = "dark blue"         #"#0e668b"
        ) +
        theme_hc() +
        scale_x_continuous(label= percent) +
        labs(y="",
             x="",
             title = paste0("Change in Math % Meeting or Exceeding \nfrom 2015-16 to 2017-18")
        ) +
        geom_text(color="red", size=3, vjust=1.5,
                  aes(x=`2016`/100, label=`2016`))+
        geom_text(aes(x=`2018`/100, label=`2018`), 
                  color="dark blue", size=3, vjust=-0.5)+
        annotation_custom(grobTree(t1, t2, t3),xmin=0.2,xmax=0.3,ymin=-0.1,ymax=-0.1)

# create gtable and remove clipping
g <- ggplot_gtable(ggplot_build(change.graph))
g$layout$clip[g$layout$name == "panel"] <- "off"

# re-draw

png(here("figs", paste0("Change in Math Percent Meeting or Exceeding.png") ),width = 500, height = 650, units = "px" ) 
grid.draw(g) 
dev.off()



#### Try for three points -----



tt1 <- textGrob(expression(phantom("Red is 2015-16, ")  * phantom("Orange is 2016-17")  * " and" * phantom(bold("Dark blue is 2017-18")) ),
               x = 0.1, y = 1.1, gp = gpar(col = "black"))

tt2 <- textGrob(expression( bold("Red is 2015-16, ") * phantom("Orange is 2016-17") * phantom(" and Dark blue is 2017-18")),
               x = 0.1, y = 1.1, gp = gpar(col = "red"))

tt3 <- textGrob(expression( phantom("Red is 2015-16, ") * bold("Orange is 2016-17") * phantom(" and Dark blue is 2017-18")),
                x = 0.1, y = 1.1, gp = gpar(col = "orange"))

tt4 <- textGrob(expression(phantom("Red is 2015-16, ") * phantom("Orange is 2016-17 and ") * bold("Dark blue is 2017-18") ),
               x = 0.1, y = 1.1, gp = gpar(col = "dark blue"))




change.graph  <- math.local %>%
        arrange(desc(two.year.change)) %>%
        filter(`School Code` != "0000000") %>%
        head(10) %>%
        ggplot( aes(x=`2016`/100, xend = `2018`/100, y = fct_reorder( `School Name`,two.year.change) ) ) +
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
             title = paste0("Change in Math % Meeting or Exceeding \nfrom 2015-16 to 2017-18")
        ) +
        geom_text(color="red", size=3, vjust=1.5,
                  aes(x=`2016`/100, label=`2016`)) +
        geom_text(color="orange", size=3, vjust=1.5,
                  aes(x=`2017`/100, label=`2017`)) + 
        geom_text(aes(x=`2018`/100, label=`2018`), 
                  color="dark blue", size=3, vjust=-0.5) +
        annotation_custom(grobTree(tt1, tt2, tt3, tt4),xmin=0.2,xmax=0.3,ymin=-0.1,ymax=-0.1)

# create gtable and remove clipping
g <- ggplot_gtable(ggplot_build(change.graph))
g$layout$clip[g$layout$name == "panel"] <- "off"

# re-draw

png(here("figs", paste0("Change in Math Percent Meeting or Exceeding.png") ),width = 500, height = 650, units = "px" ) 
grid.draw(g) 
dev.off()




three.pt.dumbbell <- function(df, title) {
        
        change.graph  <- ggplot(df, aes(x=`2016`/100, xend = `2018`/100, y = fct_reorder( usename,two.year.change) ) ) +
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
                     title = paste0(title," Change in Math \nPercent Meeting or Exceeding \nfrom 2015-16 to 2017-18")
                ) +
                geom_text(color="red", size=3, vjust=1.5,
                          aes(x=`2016`/100, label=`2016`))+
                geom_text(color="orange", size=3, vjust=1.5,
                          aes(x=`2017`/100, label=`2017`))+
                geom_text(aes(x=`2018`/100, label=`2018`), 
                          color="dark blue", size=3, vjust=-0.5)+
                annotation_custom(grobTree(tt1, tt2, tt3, tt4),xmin=0.2,xmax=0.3,ymin=-0.1,ymax=-0.1)
        
        # create gtable and remove clipping
        g <- ggplot_gtable(ggplot_build(change.graph))
        g$layout$clip[g$layout$name == "panel"] <- "off"
        
        # re-draw
        
        png(here("figs", paste0(title, " Change in Math Percent Meeting or Exceeding.png") ),width = 700, height = 650, units = "px" ) 
        grid.draw(g) 
        dev.off()
        
}


three.pt.dumbbell.no <- function(df, title) {
        
        change.graph  <- ggplot(df, aes(x=`2016`/100, xend = `2018`/100, y = fct_reorder( usename,two.year.change) ) ) +
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
                     title = paste0(title," Change in Math \nPercent Meeting or Exceeding \nfrom 2015-16 to 2017-18")
                ) +
                geom_text(color="red", size=3, vjust=1.5,
                          aes(x=`2016`/100, label=`2016`))+
                geom_text(color="orange", size=3, vjust=1.5,
                          aes(x=`2017`/100, label=`2017`))+
                geom_text(aes(x=`2018`/100, label=`2018`), 
                          color="dark blue", size=3, vjust=-0.5)
        
        ggsave(here("figs", paste0(title, " Change in Math Percent Meeting or Exceeding.png") ), height = 6 ) 

}






math.local %>%
        arrange(desc(two.year.change)) %>%
        filter(
                `County Code` == "27",
               `School Code` == "0000000",
               !is.na(`District Name`),
               !str_detect(`District Name`, "Big Sur")
               ) %>%
        three.pt.dumbbell.no("Monterey District")

math.local %>%
        arrange(desc(two.year.change)) %>%
        filter(
                `County Code` == "27",
               `School Code` != "0000000",
               Charter == "N",
               ) %>%
        head(10) %>%
        three.pt.dumbbell.no("Monterey Schools")




math.local %>%
        arrange(desc(two.year.change)) %>%
        filter(
                `School Code` != "0000000",
                `2016` != 0,
                !is.na(`2016`),
                Charter == "N"
                ) %>%
        head(10) %>%
        three.pt.dumbbell.no("Schools Statewide")



highschool.state <- math.local %>%
        arrange(`2018`) %>%
        filter(
                `School Code` != "0000000",
                #           `County Code`== "27",
           #     `2016` != 0,
         #       !is.na(`2016`),
                Charter == "N",
         
         #       highgrade %in% c(8,12)
                )
                



highschool.state <- math.local %>%
        arrange(desc(two.year.change)) %>%
        filter(
                `School Code` != "0000000",
     #           `County Code`== "27",
                `2016` != 0,
                !is.na(`2016`),
                Charter == "N",
                highgrade %in% c(8,12),
 #    highgrade == 12,
     
         #       str_detect("Salinas", as.character(DistrictName))
                 # ELpercent >= .2,
                 # frpm >= .7
        ) # %>%
  #      head(10) 

highschool.state %>%
        three.pt.dumbbell.no("HS .2 EL .7 FRPM")

hs.sum <- highschool.state %>% select(`County Name`,`District Name`,`School Name`, `2016`, `2018`,two.year.change, ELpercent, frpm )

clipr::write_clip(hs.sum)


math.local %>%
        arrange(desc(two.year.change)) %>%
        filter(
                `County Code` == "27",
                `School Code` != "0000000",
                `2016` != 0,
                !is.na(`2016`),
                Charter == "N",
                highgrade == 12
        ) %>%
        head(5) %>%
        three.pt.dumbbell.no("Monterey High Schools")



math.local %>%
        arrange(desc(two.year.change)) %>%
        filter(
                `School Code` != "0000000",
                `2016` != 0,
                !is.na(`2016`),
                Charter == "N",
                frpm >= .7,
                ELpercent >= .5
        ) %>%
        head(10) %>%
        three.pt.dumbbell.no("Schools Statewide with above .7 FRPM and .5 EL")

math.local %>%
        arrange(desc(two.year.change)) %>%
        filter(
                `County Code` == "27",
                `School Code` != "0000000",
                Charter == "N",
                frpm >= .7,
                ELpercent >= .5
        ) %>%
        head(10) %>%
        three.pt.dumbbell.no("Monterey Schools with above .7 FRPM and .5 EL")





####  Achievement ----

math.local %>%
        arrange(desc(`2018`)) %>%
        filter(
                `School Code` != "0000000",
                `2016` != 0,
                !is.na(`2016`),
                Charter == "N",
                frpm >= .7,
                ELpercent >= .5
        ) %>%
        head(10) %>%
        ggplot() +
        geom_col(aes(x=fct_reorder(usename, `2018`), y= `2018`),  fill = "dark blue") +
        coord_flip() +
        theme_hc() +
        labs(y="Percent Meeting and Exceeding Math Standard",
             x="School and its District",
             title = paste0("Percent Meeting or Exceeding in Math \nfor 2017-18 with above 70% FRPM and 50% EL")
        )

ggsave(here("figs","Highest Achieving Schools with High FRPM and EL.png"))


math.local %>%
        arrange(desc(`2018`)) %>%
        filter(
                `School Code` != "0000000",
                `2016` != 0,
                !is.na(`2016`),
                Charter == "N",
                # frpm >= .5,
                # ELpercent >= .5
        ) %>%
        head(10) %>%
        ggplot() +
        geom_col(aes(x=fct_reorder(usename, `2018`), y= `2018`),  fill = "dark blue") +
        coord_flip() +
        theme_hc() +
        labs(y="Percent Meeting and Exceeding Math Standard",
             x="School and its District",
             title = paste0("Percent Meeting or Exceeding in Math \nfor 2017-18 ")
        )

ggsave(here("figs","Highest Achieving Schools.png"))


math.local %>%
        arrange(desc(`2018`)) %>%
        filter(
                `County Code` == "27",
                `School Code` != "0000000",
                `2016` != 0,
                !is.na(`2016`),
                Charter == "N",
                frpm >= .7,
                ELpercent >= .5
        ) %>%
        head(10) %>%
        ggplot() +
        geom_col(aes(x=fct_reorder(usename, `2018`), y= `2018`),  fill = "dark blue") +
        coord_flip() +
        theme_hc() +
        labs(y="Percent Meeting and Exceeding Math Standard",
             x="School and its District",
             title = paste0("Percent Meeting or Exceeding in Math \nfor 2017-18 with above 70% FRPM and 50% EL")
        )

ggsave(here("figs","Monterey Highest Achieving Schools with High FRPM and EL.png"))


math.local %>%
        arrange(desc(`2018`)) %>%
        filter(
                `County Code` == "27",
                `School Code` != "0000000",
                `2016` != 0,
                !is.na(`2016`),
                Charter == "N",
                # frpm >= .5,
                # ELpercent >= .5
        ) %>%
        head(10) %>%
        ggplot() +
        geom_col(aes(x=fct_reorder(usename, `2018`), y= `2018`),  fill = "dark blue") +
        coord_flip() +
        theme_hc() +
        labs(y="Percent Meeting and Exceeding Math Standard",
             x="School and its District",
             title = paste0("Percent Meeting or Exceeding in Math \nfor 2017-18 ")
        )

ggsave(here("figs","Monterey Highest Achieving Schools.png"))



math.local %>%
        arrange(desc(`2018`)) %>%
        filter(
                `County Code` == "27",
                `School Code` != "0000000",
                `2016` != 0,
                !is.na(`2016`),
                Charter == "N",
                highgrade == 12
        ) %>%
        head(5) %>%
        ggplot() +
        geom_col(aes(x=fct_reorder(usename, `2018`), y= `2018`),  fill = "dark blue") +
        coord_flip() +
        theme_hc() +
        labs(y="Percent Meeting and Exceeding Math Standard",
             x="School and its District",
             title = paste0("Percent Meeting or Exceeding in Math \nfor 2017-18 ")
        )

ggsave(here("figs","Monterey Highest Achieving High Schools.png"))


math.local %>%
        arrange(desc(`2018`)) %>%
        filter(
                `County Code` == "27",
                `School Code` == "0000000",
                !is.na(`District Name`)
   #             `2016` != 0,
    #            !is.na(`2016`),
     #           Charter == "N",
                # frpm >= .5,
                # ELpercent >= .5
        ) %>%
        head(10) %>%
        ggplot() +
        geom_col(aes(x=fct_reorder(usename, `2018`), y= `2018`),  fill = "dark blue") +
        coord_flip() +
        theme_hc() +
        labs(y="Percent Meeting and Exceeding Math Standard",
             x="District",
             title = paste0("Percent Meeting or Exceeding in Math \nfor 2017-18 ")
        )

ggsave(here("figs","Monterey Highest Achieving Districts.png"))



### County Ranking ---- 

counties <- math.local %>%
        filter(`District Code` == "00000") %>% 
        mutate(county = str_c(`County Name`, ": ")) %>%
        select(county, `2018`) %>%
        arrange(desc(`2018`))

clipr::write_clip(counties)


### County State comparison line ----

math.local %>%
        filter(`County Code` %in% c("00", "27"),
               `District Code`== "00000") %>%
        select(`2016`,`2017`,`2018`,`County Name`) %>%
        gather(key = "year", value = "value" ,-`County Name`) %>%
        mutate(usename = case_when(year == "2018" ~ `County Name`,
                                   TRUE ~ "")) %>%
        ggplot(aes(x=year, y=value/100, group= `County Name`, color = `County Name`, label=usename))+
        geom_line() +
        geom_text(color="black", size=3, vjust=1.5) +
        theme_hc() +
        scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0,0.4)) +
        scale_colour_manual(values =c("red", "black")) +
        labs(y="Percent Meeting and Exceeding Math Standard",
             x="",
             title = paste0("Percent Meeting or Exceeding in Math for 2017-18 ")
        ) +
        theme(legend.position="none")

ggsave(here("figs","Monterey State linegraph.png"), height = 4)
