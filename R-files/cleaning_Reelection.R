# Reelection Reform and Removing Rival in Mexico

# Adee Weller 

# Clear all
rm(list=ls())


# packages
pkgs <- c("tmap", "dplyr", "plm", "fixest", "kableExtra", "stargazer", "ggplot2", "modelsummary", "ggthemes", "did", "fwildclusterboot", "bacondecomp", "spdep", "xtable", "broom", "purrr", "lfe", "did2s", "didimputation", "glue", "ggiplot")

for (pkg in pkgs) {
  if (! (pkg %in% rownames(installed.packages()))) { install.packages(pkg) }
  lapply(pkg, library, character.only=T )
}



# load data
panel <- read.csv('C:\\Users\\adeew\\OneDrive\\Documents\\Reelect_Ref\\data\\Updated_panel_10_27_22.csv')


#### For cleaning Data #####

####### Comment out when needed #########
####### This is currently already cleaned, do not need to run this section ######

# get unique state names
states <- unique(panel$State)

# states

# # using data from Ch (2022)

treated_2015 <- c('Baja California Sur', 'Campeche', 'Chiapas', 'Colima', 'Guanajuato', 'Guerrero', 'Jalisco', 'Mexico', 'Michoacan', 'Morelos', 'Nuevo Leon', 'Queretaro', 'San Luis Potosi', 'Tabasco', 'Yucatan')

treated_2016 <- c('Aguascalientes', 'Baja California', 'Chihuahua', 'Durango', 'Oaxaca', 'Quintana Roo', 'Sinaloa', 'Tamaulipas', 'Zacatecas')

treated_2017 <- 'Coahuila'

treated_2018 <- c('Puebla', 'Sonora')

# Control Units
never_treated <- c('Hidalgo', 'Nayarit', 'Tlaxcala', 'Veracruz')


treat_stag <- rep(NA, length(panel$State))

for (i in 1:length(panel$State)) {
  if (panel$State[i] %in% treated_2015) {
    treat_stag[i] <- 2015
  } else if (panel$State[i] %in% treated_2016) {
    treat_stag[i] <- 2016
  } else if (panel$State[i] %in% treated_2017) {
    treat_stag[i] <- 2017
  } else if (panel$State[i] %in% treated_2018) {
    treat_stag[i] <- 2018
  } else {
    treat_stag[i] <- 0
  }
}

head(treat_stag)

panel$treat_stag <- treat_stag

write.csv(panel, 'C:\\Users\\adeew\\OneDrive\\Documents\\Reelect_Ref\\data\\Updated_panel_10_03_22.csv')

# add to panel
panel <- cbind(panel, treat_stag)


Group <- rep(1, length(panel$State))

for (i in 1:length(panel$State)) {
  if (panel$treat_stag[i] == 2015) {
    Group[i] <- 2
  } else if (panel$treat_stag[i] == 2016) {
    Group[i] <- 3
  } else if (panel$treat_stag[i] == 2017) {
    Group[i] <- 4
  } else Group[i] <- 5
}

head(Group)

# add to panel
panel <- cbind(panel, Group)


# Adding time periods

time_periods <- rep(NA, length(panel$State))

years <- unique(panel$Year)

for (i in 1:length(panel$State)){
    if (panel$treat_stag[i] == 2015){
        if (panel$Year[i] == 2013){
            time_periods[i] <- -2
        } else if (panel$Year[i] == 2014){
            time_periods[i] <- -1
        } else if (panel$Year[i] == 2015){
            time_periods[i] <- 0
        } else if (panel$Year[i] == 2016){
            time_periods[i] <- 1
        } else if (panel$Year[i] == 2017){
            time_periods[i] <- 2
        } else if (panel$Year[i] == 2018){
            time_periods[i] <- 3
        } else if (panel$Year[i] == 2019){
            time_periods[i] <- 4
        } else if (panel$Year[i] == 2020){
            time_periods[i] <- 5
        } else if (panel$Year[i] == 2021){
            time_periods[i] <- 6
        }
    } else if (panel$treat_stag[i] == 2016){
        if (panel$Year[i] == 2013){
            time_periods[i] <- -3
        } else if (panel$Year[i] == 2014){
            time_periods[i] <- -2
        } else if (panel$Year[i] == 2015){
            time_periods[i] <- -1
        } else if (panel$Year[i] == 2016){
            time_periods[i] <- 0
        } else if (panel$Year[i] == 2017){
            time_periods[i] <- 1
        } else if (panel$Year[i] == 2018){
            time_periods[i] <- 2
        } else if (panel$Year[i] == 2019){
            time_periods[i] <- 3
        } else if (panel$Year[i] == 2020){
            time_periods[i] <- 4
        } else if (panel$Year[i] == 2021){
            time_periods[i] <- 5
        }
    } else if (panel$treat_stag[i] == 2017){
        if (panel$Year[i] == 2013){
            time_periods[i] <- -4
        } else if (panel$Year[i] == 2014){
            time_periods[i] <- -3
        } else if (panel$Year[i] == 2015){
            time_periods[i] <- -2
        } else if (panel$Year[i] == 2016){
            time_periods[i] <- -1
        } else if (panel$Year[i] == 2017){
            time_periods[i] <- 0
        } else if (panel$Year[i] == 2018){
            time_periods[i] <- 1
        } else if (panel$Year[i] == 2019){
            time_periods[i] <- 2
        } else if (panel$Year[i] == 2020){
            time_periods[i] <- 3
        } else if (panel$Year[i] == 2021){
            time_periods[i] <- 4
        }
    } else if (panel$treat_stag[i] == 2018){
        if (panel$Year[i] == 2013){
            time_periods[i] <- -5
        } else if (panel$Year[i] == 2014){
            time_periods[i] <- -4
        } else if (panel$Year[i] == 2015){
            time_periods[i] <- -3
        } else if (panel$Year[i] == 2016){
            time_periods[i] <- -2
        } else if (panel$Year[i] == 2017){
            time_periods[i] <- -1
        } else if (panel$Year[i] == 2018){
            time_periods[i] <- 0
        } else if (panel$Year[i] == 2019){
            time_periods[i] <- 1
        } else if (panel$Year[i] == 2020){
            time_periods[i] <- 2
        } else if (panel$Year[i] == 2021){
            time_periods[i] <- 3
        }
    } else {
        time_periods[i] <- 0
    }

}


# add to panel
panel$time_periods <- time_periods

panel$time_periods

write.csv(panel, 'balanced_10_26_22.csv')

# treated indicator
panel <- panel %>%
  mutate(Treated_ind = ifelse(Year >= treat_stag, 1, 0))

panel$Treated_ind

# Find values that are causing -Inf

new_panel$hhi[which(new_panel$hhi == Inf)] <- NA

# remove duplicates

### turn on if needed ###
panel2 <- panel %>% distinct(Municipality, Year, .keep_all = TRUE)

# panel <- panel2


###### End of Cleaning Section #######

