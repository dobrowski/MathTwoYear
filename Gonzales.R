


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


sbac2018 <- read_delim( here("data", "sb_ca2018_all_27_csv_v3.txt"), delim = ",") %>% select(`County Code`,`District Code`, `School Code`, `Test Year`, `Subgroup ID`, `Test Id`, Grade, `Percentage Standard Met and Above`, `Students Tested` )


subgroups <- read_delim(here("data", "subgroups.txt"), delim = ",", col_names = FALSE) %>%
        mutate(subs = as.factor(str_trim(X2)))





i <- "Gonzales"

gonz <- sbac2018 %>%
        mutate(cds = str_c( str_pad(  as.character(`County Code`) , width = 2, side = "left", pad = "0"  ) ,
                            str_pad(  as.character(`District Code`), width = 5, side = "left", pad = "0"  ) ,
                            str_pad( as.character(`School Code`), width = 7, side = "left", pad = "0"  )  )
        ) %>%
        mutate(`Students Tested` = as.numeric(`Students Tested`),
               MetPlus = as.numeric(`Percentage Standard Met and Above`),
               subs = as.factor(str_trim(`Subgroup ID`))
               ) %>%
        left_join(school.EL.FRPM, by = c("cds" = "cds")) %>% 
        filter(
               Grade == 13,
               str_detect(DistrictName, "Gonz"),
               ) %>%
        filter(`Test Id` == "2") %>%  #1 = ELA and 2 = Math
        na.omit() %>%
        left_join(subgroups) %>%
        mutate(X5 = paste0(X4,": ",X3)) %>%
        arrange(X4,X3)



####  GRaph -----

xlabels <- gonz %>%
 #       filter(AggregateLevel %in% c("T","D2" ) ) %>%
        group_by(`Subgroup ID`) %>%
        arrange(MetPlus) %>%
        filter(row_number() == 1) %>%
        ungroup() %>%
        arrange(Subgroup) %>%
        select(nlabel) %>%
        mutate(nlabel = fct_inorder(nlabel)) 

xlabs <- paste(levels(xlabels$nlabel), sep ="")


xlabs <- unique(gonz$X5)

 ggplot(gonz, aes(  X5   ,  fct_rev(SchoolName),   fill = MetPlus )) + 
        geom_tile(colour = "white") +
        geom_text(aes(label= percent( MetPlus/100)), size = 3) +
 #        facet_grid(~X4, scales = "free") +
        scale_x_discrete(labels = xlabs) +
        theme_hc() +
        scale_fill_gradient( high = "light yellow", low = "blue" )+
        theme(
                legend.position = "none",
              #  axis.ticks.x = element_blank(),
                strip.background = element_rect(fill = "black"),
                strip.text = element_text(colour = 'white'),
                axis.text.x = element_text(angle = 45, hjust = 1)
        ) +
        labs(x="Subgroup",
             y="School",
             title = paste0(i," Math Met or Exceeded by Subgroup and School in 2017-18"), 
             subtitle="", 
             fill="")

ggsave(here("figs", paste0(i," Math Met or Exceeded by Subgroup and School in 2017-18.png") ), height = 7, width = 14)


