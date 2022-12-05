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




####### Identifying predictors of Treatment #######


# regress covariates on treatment assigment
es <- plm(Group ~ total_alt + Upper_misalign + state_misalign + Pres_misalign  + MajorHighway + MajorPort + Airports + Railline + Oilline + Intlborder + Shoreline + MajCity + population + hom_rate + secuestro + narcomenudeo + electorales + groups_active_alt_2015 + pri_mayor + nCarteles_2010 + ACLED_conflict + groups_active_alt_2015 + Attack_count, 
          data=panel, model="within", effect="twoways",
          index=c("Municipality","Year"))


# output table with scientific notation
es_df <- summary(es)

# table
modelsummary(es, 
  output = "latex", 
  fmt = "%.4e",
  coef_rename = c("total_alt" = "Total Alternation",' Upper_misalign' = 'Misalignment of state-federal', 'state_misalign' = "State misalginment",'Pres_misalign'= "Federal misalignment", 'MajorHighway' =  "Major highway", 'population' = "Population", 'hom_rate' = "Homicide Rate", 'secuestro' = "Kidnapping", 'narcomenudeo' = "Small-scale drug sales", 'electorales' ="Electoral crimes", 'groups_active_alt_2015'= "Groups active (2015)", 'pri_mayor'= "PRI mayor", 'nCarteles_2010'= "Number of Cartels (2010)", 'ACLED_conflict'= "ACLED conflict", 'Attack_count'= "Attack Count")
  title = 'Regression of Covariates on Assignment of Treatment (Municipal)')


# since treatment is assigned on the state level, lets look at that

###### potential ways to see determinants of receiving treatment

panel$Aguacate_sembrada_tonelada <- as.numeric(panel$Aguacate_sembrada_tonelada)
panel$Aguacate_valor_miles_de_pesos <- as.numeric(panel$Aguacate_valor_miles_de_pesos)
panel$Limon_sembrada <- as.numeric(panel$Limon_sembrada)
panel$Limon_valor_prod <- as.numeric(panel$Limon_valor_prod)



state_panel <- panel %>%
  group_by(ADM1_PCODE, Year) %>%  
  summarise_at(vars(COUNT, MajorHighway, MajorPort, Airports, Railline, Oilline, Intlborder, Shoreline, Poppies, MajCity, Attack, Attack_count, Incumbent, population, homicidio, secuestro, narcomenudeo, electorales, hom_rate, Aguacate_sembrada_tonelada, Aguacate_valor_miles_de_pesos, Limon_sembrada, Limon_valor_prod, treat_stag, total_alt, party_alt, groups_active_alt_2015, Upper_misalign,  state_misalign, Pres_misalign,  IRCO, val_index, ACLED_conflict, val_index, nCarteles_2010), 
    list(name = mean))

head(state_panel)

names(state_panel) 

names_state <- c("State", "Year", "COUNT", "MajorHighway", "MajorPort", "Airports", "Railline", "Oilline", "Intlborder", "Shoreline", "Poppies", "MajCity", "Attack", "Attack_count", "Incumbent", "population", "homicidio", "secuestro", "narcomenudeo", "electorales", "hom_rate", "Aguacate_sembrada_tonelada", "Aguacate_valor_miles_de_pesos", "Limon_sembrada", "Limon_valor_prod", "treat_stag", "total_alt", "party_alt", "groups_active_alt_2015", "Upper_misalign", "state_misalign", "Pres_misalign", "IRCO", "val_index", "ACLED_conflict","nCarteles_2010")

length(names(state_panel))
length(names_state)

names(state_panel) <- names_state


# regress covariates on treatment assigment
es2 <- plm(treat_stag ~ total_alt + Upper_misalign + state_misalign + Pres_misalign  + MajorHighway + MajorPort + Airports + Railline + Oilline + Intlborder + Shoreline + MajCity + population + hom_rate + secuestro + narcomenudeo + electorales + groups_active_alt_2015 +  nCarteles_2010 + ACLED_conflict + groups_active_alt_2015 + Attack_count, 
          data=state_panel, model="within", effect="twoways",
          index=c("State","Year"))

# output table with scientific notation
summary(es2)

models_es <- list(es, es2)

modelsummary(models_es, 
  output = "latex", 
  fmt = "%.4e",
  #coef_rename = c("total_alt" = "Total Alternation",' Upper_misalign' = 'Misalignment of state-federal', 'state_misalign' = "State misalginment",'Pres_misalign'= "Federal misalignment", 'MajorHighway' =  "Major highway", 'population' = "Population", 'hom_rate' = "Homicide Rate", 'secuestro' = "Kidnapping", 'narcomenudeo' = "Small-scale drug sales", 'electorales' ="Electoral crimes", 'groups_active_alt_2015'= "Groups active (2015)", 'pri_mayor'= "PRI mayor", 'nCarteles_2010'= "Number of Cartels (2010)", 'ACLED_conflict'= "ACLED conflict", 'Attack_count'= "Attack Count")
  estimate = "{estimate} [{p.value}]",
    statistic = NULL,
  title = 'Regression of Covariates on Assignment of Treatment (Municipal)')

# plot settings

theme_set(theme_clean() + theme(plot.background = element_blank()))

# plot

# remove

panel_yr <- panel %>% group_by(Year, treat_stag) %>% summarize(Attack = mean(Attack))


# red: #E41A1C
# blue: #377EB8
# green: #4DAF4A
# purple: #984EA3
# orange: #FD7D1A


p1 <- ggplot(panel_yr, aes(x = Year, y = Attack, group = treat_stag, color = factor(treat_stag))) + 
  geom_line(size = 2) + 
  geom_line(alpha = 1/8, color = "grey") + 
  labs(x = "", y = "Average Number of Attacks", color = "Treatment group", labels = c("Control", "2015", "2016", "2017","2018")) + 
    geom_vline(xintercept = 2015, color = '#377EB8', size = 0.8) + 
    geom_vline(xintercept = 2016, color = '#4DAF4A', size = 0.8) + 
    geom_vline(xintercept = 2017, color = '#984EA3', size = 0.8) + 
    geom_vline(xintercept = 2018, color = '#FD7D1A', size = 0.8) + 
    scale_color_brewer(palette = 'Set1') + 
    theme(legend.position = 'bottom',
        #legend.title = element_blank(), 
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12))  +
    theme(plot.title = element_text(hjust = 0.5, size=12))
    # ggtitle("Averge Attacks with all groups being eventually treated")+

p1 + scale_x_continuous(breaks = c(2013, 2014, 2015,2016,2017,2018,2019,2020, 2021),limits = c(2013,2021))




### Estimating GATTS #####

# estimate group-time average treatment effects using att_gt method

example_attgt2 <- att_gt(yname = "Attack_count",
                        tname = "Year",
                        idname = "id_number",
                        gname = "treat_stag",
                        #allow_unbalanced_panel = TRUE,
                        # this is how you condition on parallel trends
                        # xformla = ~1,
                        data = panel,
                        control_group = 'notyettreated'#, 
                        # clustervars = 'Municipality'
                        )

summary(example_attgt2)

# visualizations
ggdid(example_attgt2)

agg.es2 <- aggte(example_attgt2, type = "dynamic")
summary(agg.es2)

ggdid(agg.es2)

agg.gs2 <- aggte(example_attgt2, type = "group")
summary(agg.gs2)

ggdid(agg.gs2)

agg.ct2 <- aggte(example_attgt2, type = "calendar")
summary(agg.ct2)

ggdid(agg.ct2)





#####################################
######## Main table estimates #######
#####################################


# full sample
res_total <- feols(Attack_count ~ total_alt + state_misalign + Pres_misalign + pri_mayor + sunab(treat_stag, Year) 
    | Year, panel, cluster = ~State)

# bootstrap SEs
boot_att <- fwildclusterboot::boot_aggregate(res_total, agg = 'period', B = 9999, clustid = 'State')

aggregate(res_total, agg = "ATT")

boot_att2 <- fwildclusterboot::boot_aggregate(res_total, agg = 'ATT', B = 9999, clustid = 'State')

boot_att2


# visualizations and Aggregations
etable(list(res_total), keep = 'Year')

coefplot(list(res_total), keep = 'Year')

iplot(res_total)

ggiplot(res_total, 
  geom_style = 'ribbon')


summary(res_total, agg = "ATT")
summary(res_total, agg = "period")


modelsummary(res_total, 
  agg = 'ATT',
  output = "latex")

modelsummary(res_total, 
  agg = 'cohort',
  output = "latex")





# writing a function for competition interaction

# names of competition variables
competition <- c("nCarteles_2010", "groups_active_alt_2015","IRCO","ACLED_conflict", "PEI_crim_index", "val_index")

aggregation <- c("period", "ATT")



sunab_feols <- function(dv, cv, aggregation) {
    # all elements must be strings!

    # for just  the full sample, with no het. effects of competition, cv = '1'
  
  # estimate effects
  reg_formula <- as.formula(paste(dv, ' ~ total_alt + state_misalign + Pres_misalign + pri_mayor + sunab(treat_stag, Year)*', cv, '| Year'))

  sunab_est <- feols(reg_formula, data = panel, cluster = ~State)

  # wild-cluster bootstrap
  sunab_boot <- fwildclusterboot::boot_aggregate(sunab_est, agg = aggregation, B = 9999, clustid = 'State')

  return(sunab_boot)

}


c1 <- sunab_feols("Attack_count", competition[1], aggregation[1])
c2 <- sunab_feols("Attack_count", competition[2], aggregation[1])
# no bootstrappping for 3
c3 <- feols(Attack_count ~ total_alt + state_misalign + Pres_misalign +  pri_mayor + sunab(treat_stag, Year)*IRCO |  Year, panel, cluster = ~State)
c4 <- sunab_feols("Attack_count", competition[4], aggregation[1])
c5 <- sunab_feols("Attack_count", competition[5], aggregation[1])
c6 <- sunab_feols("Attack_count", competition[6], aggregation[1])



# total atts
att_c1 <- sunab_feols("Attack_count", competition[1], aggregation[2])
att_c2 <- sunab_feols("Attack_count", competition[2], aggregation[2])
att_c3 <- aggregate(c3, agg = "ATT")
att_c4 <- sunab_feols("Attack_count", competition[4], aggregation[2])
att_c5 <- sunab_feols("Attack_count", competition[5], aggregation[2])
att_c6 <- sunab_feols("Attack_count", competition[6], aggregation[2])


# make total ATTs into a table

names <- c("Full", "Cartels (2010)", "MCO (2015)", "IRCG (2020)", "ACLED", "PEI", "Value")

table_atts <- data.frame(rbind(boot_att2,att_c1,att_c2,att_c3,att_c4,att_c5,att_c6))

table_atts <- cbind(names, table_atts)

rownames(table_atts) <- NULL 
colnames(table_atts) <- c("Model", "Estimate", "P-value", "conf.low", "conf.high")



# this is not very helpful!
ggplot(table_atts) +
    geom_point(aes(x = Estimate, y = Model, color = Model)) +
    geom_errorbarh(aes(x = Estimate, y = Model, xmin = conf.low, xmax = conf.high, ymin = Model, ymax = Model, color = Model)) +
    geom_hline(yintercept = 0) +
    theme_bw() +    
    xlab('Estimate') +
    ylab('Model') + 
    geom_vline(xintercept = 0, color = 'dark gray')


# now, without IRCG
table_atts2 <- subset(table_atts, table_atts$Model != 'IRCG (2020)')

ggplot(table_atts2) +
    geom_point(aes(x = Estimate, y = Model, color = Model)) +
    geom_errorbarh(aes(x = Estimate, y = Model, xmin = conf.low, xmax = conf.high, ymin = Model, ymax = Model, color = Model)) +
    geom_hline(yintercept = 0) +
    theme_bw() +    
    xlab('Estimate') +
    ylab('Model') + 
    geom_vline(xintercept = 0, color = 'dark gray')






##### Period treatment effects


# make table

Period <- rep(c(-5,-4,-3,-2,0,1,2,3,4,5,6),6)

model <- c(rep('Full',11),rep('Cartels (2010)',11),rep('MCO (2015)',11), rep('ACLED',11), rep('PEI',11), rep('Value',11))

# boot_att_c3,
booted_atts <- data.frame(rbind(boot_att, c1, c2,   c4, c5, c6))

booted_atts <- cbind(model, Period, booted_atts)

names(booted_atts) <- c('Model', 'Period', 'estimate', 'p.value', 'conf.low', 'conf.high')

# add pvalues, conf intervals for C3 (not bootstrapped)
c3_est <- data.frame(coef(c3))
c3_se <- data.frame(se(c3))
c3_pv <- data.frame(pvalue(c3))
c3_period <- c(NA, NA, NA, NA, -5, -4, -3, -2, 0,1, 2,3,4,5,6, NA)
c3_mod <- rep('IRCG', length(c3_se))

c3_df <- data.frame(c3_mod,c3_period, c3_est, c3_se, c3_pv)

names(c3_df) <- c('Model', 'Period', 'estimate', 'se', 'p.value')

# make CIs
alp <- 0.05
cval <- qnorm(1-alp/2)

c3_df <- c3_df %>%
    mutate(conf.low = estimate - cval*p.value,
           conf.high = estimate + cval*p.value)

# remove NAs
c3_df <- na.omit(c3_df)
c3_df <- c3_df[,-4]

# add to main df
head(booted_atts)

booted_top <- booted_atts[1:33,]
booted_bottom <- booted_atts[34:66,]

booted_atts <- rbind(booted_top, c3_df, booted_bottom)



# visualizations


# this is not very helpful!
ggplot(booted_atts) +
    geom_point(aes(x = Period, y = estimate, color = Model)) +
    geom_errorbar(aes(x = Period, y = estimate, xmin = Period, xmax = Period, ymin = conf.low, ymax = conf.high, color = Model)) +
    geom_hline(yintercept = 0) +
    theme_bw() +    
    xlab('Time to Treatment') +
    ylab('Estimated Coefficient')

# now, without IRCG
booted_atts2 <- subset(booted_atts, booted_atts$Model != 'IRCG')
booted_atts2 <- booted_atts2[-23,]

dodge <- position_dodge(width=0.5) 

ggplot(booted_atts2) +
    geom_point(aes(x = Period, y = estimate, color = Model), position = dodge) +
    geom_errorbar(aes(x = Period, y = estimate, xmin = Period, xmax = Period, ymin = conf.low, ymax = conf.high, color = Model), position = dodge) +
    geom_hline(yintercept = 0) +
    theme_bw() +    
    xlab('Time to Treatment') +
    ylab('Estimated Coefficient')


# just the full model

full_booted <- subset(booted_atts2, booted_atts2$Model == 'Full')

ggplot(full_booted) +
    geom_point(aes(x = Period, y = estimate, color = Model)) +
    geom_errorbar(aes(x = Period, y = estimate, xmin = Period, xmax = Period, ymin = conf.low, ymax = conf.high, color = Model)) +
    geom_hline(yintercept = 0) +
    theme_bw() +
    xlab('Time to Treatment') +
    ylab('Estimated Coefficient') + 
    scale_color_manual(values = c('Full' = '#2e8540'))

# produce table


######### ADEE #########

    #### DON'T FORGET TO PUT THE CODE FOR THE MAIN TABLE HERE ######

##### THANKS --- FROM PAST ADEE #######





#############################################
#### Number of candidates as outcome ########
#############################################

ncan_tot <- sunab_feols("n_can", '1', aggregation[1])

ncan_c1 <- sunab_feols("n_can", competition[1], aggregation[1])
ncan_c2 <- sunab_feols("n_can", competition[2], aggregation[1])
ncan_c3 <- feols(n_can ~ total_alt + state_misalign + Pres_misalign +  pri_mayor + sunab(treat_stag, Year)*IRCO |  Year, panel, cluster = ~State)
ncan_c4 <- sunab_feols("n_can", competition[4], aggregation[1])
ncan_c5 <- sunab_feols("n_can", competition[5], aggregation[1])
ncan_c6 <- sunab_feols("n_can", competition[6], aggregation[1])

# ATTs
bootncan_tot <- sunab_feols("n_can", '1', aggregation[2])
ncan_att_c1 <- sunab_feols("n_can", competition[1], aggregation[2])
ncan_att_c2 <- sunab_feols("n_can", competition[2], aggregation[2])
ncan_att_c3 <- aggregate(ncan_c3, agg = "ATT")
ncan_att_c4 <- sunab_feols("n_can", competition[4], aggregation[2])
ncan_att_c5 <- sunab_feols("n_can", competition[5], aggregation[2])
ncan_att_c6 <- sunab_feols("n_can", competition[6], aggregation[2])

# visualize ATTs


ncan_table_atts <- data.frame(rbind(bootncan_tot,ncan_att_c1,ncan_att_c2,ncan_att_c3,ncan_att_c4,ncan_att_c5,ncan_att_c6))

ncan_table_atts <- cbind(names, ncan_table_atts)

rownames(ncan_table_atts) <- NULL 
colnames(ncan_table_atts) <- c("Model", "Estimate", "P-value", "conf.low", "conf.high")



# this is not very helpful!
ggplot(ncan_table_atts) +
    geom_point(aes(x = Estimate, y = Model, color = Model)) +
    geom_errorbarh(aes(x = Estimate, y = Model, xmin = conf.low, xmax = conf.high, ymin = Model, ymax = Model, color = Model)) +
    geom_hline(yintercept = 0) +
    theme_bw() +    
    xlab('Estimate') +
    ylab('Model') + 
    geom_vline(xintercept = 0, color = 'dark gray')


# now, without IRCG
ncan_table_atts2 <- subset(ncan_table_atts, ncan_table_atts$Model != 'IRCG (2020)')

ggplot(ncan_table_atts2) +
    geom_point(aes(x = Estimate, y = Model, color = Model)) +
    geom_errorbarh(aes(x = Estimate, y = Model, xmin = conf.low, xmax = conf.high, ymin = Model, ymax = Model, color = Model)) +
    geom_hline(yintercept = 0) +
    theme_bw() +    
    xlab('Estimate') +
    ylab('Model') + 
    geom_vline(xintercept = 0, color = 'dark gray')



# table of dynamic booted ests

booted_atts_ncan <- data.frame(rbind(bootncan_tot,ncan_att_c1,ncan_att_c2,ncan_att_c4,ncan_att_c5,ncan_att_c6))

booted_atts_ncan <- cbind(model, Period, booted_atts_ncan)

names(booted_atts_ncan) <- c('Model', 'Period', 'estimate', 'p.value', 'conf.low', 'conf.high')


# add pvalues, conf intervals for C3 (not bootstrapped)

c3_est_ncan <- data.frame(coef(ncan_c3))
c3_se_ncan <- data.frame(se(ncan_c3))
c3_pv_ncan <- data.frame(pvalue(ncan_c3))
c3_period_ncan <- c(NA, NA, NA, NA,-5, -4, -3, -2, 0,1, 2,3,4,5,6, NA)
c3_mod_ncan <- rep('IRCG', length(c3_se_ncan))

c3_df_ncan <- data.frame(c3_mod_ncan,c3_period_ncan, c3_est_ncan, c3_se_ncan, c3_pv_ncan)

names(c3_df_ncan) <- c('Model', 'Period', 'estimate', 'se', 'p.value')

c3_df_ncan <- c3_df_ncan %>%
    mutate(conf.low = estimate - cval*p.value,
           conf.high = estimate + cval*p.value)


# remove NAs

c3_df_ncan <- na.omit(c3_df_ncan)
c3_df_ncan <- c3_df_ncan[,-4]

# add to main df
booted_top_ncan <- booted_atts_ncan[1:33,]
booted_bottom_ncan <- booted_atts_ncan[34:66,]

booted_atts_ncan2 <- rbind(booted_top_ncan, c3_df_ncan, booted_bottom_ncan)


# visualization
# without IRCG
booted_atts2_ncan <- subset(booted_atts_ncan, booted_atts_ncan$Model != 'IRCG')
booted_atts2_ncan <- booted_atts2_ncan[-23,]

booted_atts2_ncan <- subset(booted_atts_ncan, booted_atts_ncan$Period != -2)

dodge <- position_dodge(width=0.5) 

ggplot(booted_atts2_ncan) +
    geom_point(aes(x = Period, y = estimate, color = Model), position = dodge) +
    geom_errorbar(aes(x = Period, y = estimate, xmin = Period, xmax = Period, ymin = conf.low, ymax = conf.high, color = Model), position = dodge) +
    geom_hline(yintercept = 0) +
    theme_bw() +    
    xlab('Time to Treatment') +
    ylab('Estimated Coefficient')


# just the full model

full_booted_ncan <- subset(booted_atts2_ncan, booted_atts2_ncan$Model == 'Full' | booted_atts2_ncan$Model == 'MCO (2015)')

ggplot(full_booted_ncan) +
    geom_point(aes(x = Period, y = estimate, color = Model), position = dodge) +
    geom_errorbar(aes(x = Period, y = estimate, xmin = Period, xmax = Period, ymin = conf.low, ymax = conf.high, color = Model), position = dodge) +
    geom_hline(yintercept = 0) +
    theme_bw() +    
    xlab('Time to Treatment') +
    ylab('Estimated Coefficient')






#################################################
#### Vote Concentration (HHI) as outcome ########
#################################################

hhi_tot <- sunab_feols("hhi", '1', aggregation[1])

hhi_c1 <- sunab_feols("hhi", competition[1], aggregation[1])
hhi_c2 <- sunab_feols("hhi", competition[2], aggregation[1])
hhi_c3 <- feols(hhi ~ total_alt + state_misalign + Pres_misalign +  pri_mayor + sunab(treat_stag, Year)*IRCO |  Year, panel, cluster = ~State)
hhi_c4 <- sunab_feols("hhi", competition[4], aggregation[1])
hhi_c5 <- sunab_feols("hhi", competition[5], aggregation[1])
hhi_c6 <- sunab_feols("hhi", competition[6], aggregation[1])

# ATTs
boothhi_tot <- sunab_feols("hhi", '1', aggregation[2])
hhi_att_c1 <- sunab_feols("hhi", competition[1], aggregation[2])
hhi_att_c2 <- sunab_feols("hhi", competition[2], aggregation[2])
hhi_att_c3 <- aggregate(hhi_c3, agg = "ATT")
hhi_att_c4 <- sunab_feols("hhi", competition[4], aggregation[2])
hhi_att_c5 <- sunab_feols("hhi", competition[5], aggregation[2])
hhi_att_c6 <- sunab_feols("hhi", competition[6], aggregation[2])

# visualize ATTs
hhi_table_atts <- data.frame(rbind(boothhi_tot,hhi_att_c1,hhi_att_c2,hhi_att_c3,hhi_att_c4,hhi_att_c5,hhi_att_c6))

hhi_table_atts <- cbind(names, hhi_table_atts)

rownames(hhi_table_atts) <- NULL 
colnames(hhi_table_atts) <- c("Model", "Estimate", "P-value", "conf.low", "conf.high")



# this is not very helpful!
ggplot(hhi_table_atts) +
    geom_point(aes(x = Estimate, y = Model, color = Model)) +
    geom_errorbarh(aes(x = Estimate, y = Model, xmin = conf.low, xmax = conf.high, ymin = Model, ymax = Model, color = Model)) +
    geom_hline(yintercept = 0) +
    theme_bw() +    
    xlab('Estimate') +
    ylab('Model') + 
    geom_vline(xintercept = 0, color = 'dark gray')


# now, without IRCG
hhi_table_atts2 <- subset(hhi_table_atts, hhi_table_atts$Model != 'IRCG (2020)')

ggplot(hhi_table_atts2) +
    geom_point(aes(x = Estimate, y = Model, color = Model)) +
    geom_errorbarh(aes(x = Estimate, y = Model, xmin = conf.low, xmax = conf.high, ymin = Model, ymax = Model, color = Model)) +
    geom_hline(yintercept = 0) +
    theme_bw() +    
    xlab('Estimate') +
    ylab('Model') + 
    geom_vline(xintercept = 0, color = 'dark gray')



# table of dynamic booted ests

booted_atts_hhi <- data.frame(rbind(boothhi_tot,hhi_att_c1,hhi_att_c2,hhi_att_c4,hhi_att_c5,hhi_att_c6))

booted_atts_hhi <- cbind(model, Period, booted_atts_hhi)

names(booted_atts_hhi) <- c('Model', 'Period', 'estimate', 'p.value', 'conf.low', 'conf.high')


# add pvalues, conf intervals for C3 (not bootstrapped)

c3_est_hhi <- data.frame(coef(hhi_c3))
c3_se_hhi <- data.frame(se(hhi_c3))
c3_pv_hhi <- data.frame(pvalue(hhi_c3))
c3_period_hhi <- c(NA, NA, NA, NA,-5, -4, -3, 0,1, 2,3,4,5,6, NA)
c3_mod_hhi <- rep('IRCG', length(c3_se_hhi))

c3_df_hhi <- data.frame(c3_mod_hhi,c3_period_hhi, c3_est_hhi, c3_se_hhi, c3_pv_hhi)

names(c3_df_hhi) <- c('Model', 'Period', 'estimate', 'se', 'p.value')

c3_df_hhi <- c3_df_hhi %>%
    mutate(conf.low = estimate - cval*p.value,
           conf.high = estimate + cval*p.value)

# remove NAs
c3_df_hhi <- na.omit(c3_df_hhi)
c3_df_hhi <- c3_df_hhi[,-4]

# add to main df
booted_top_hhi <- booted_atts_hhi[1:33,]
booted_bottom_hhi <- booted_atts_hhi[34:66,]

booted_atts_hhi2 <- rbind(booted_top_hhi, c3_df_hhi, booted_bottom_hhi)


# visualization
# without IRCG
booted_atts2_hhi <- subset(booted_atts_hhi, booted_atts_hhi$Model != 'IRCG')
booted_atts2_hhi <- booted_atts2_hhi[-23,]

booted_atts2_hhi <- subset(booted_atts_hhi, booted_atts_hhi$Period != -2)

dodge <- position_dodge(width=0.5) 

ggplot(booted_atts2_hhi) +
    geom_point(aes(x = Period, y = estimate, color = Model), position = dodge) +
    geom_errorbar(aes(x = Period, y = estimate, xmin = Period, xmax = Period, ymin = conf.low, ymax = conf.high, color = Model), position = dodge) +
    geom_hline(yintercept = 0) +
    theme_bw() +    
    xlab('Time to Treatment') +
    ylab('Estimated Coefficient')


# just the full model

full_booted_hhi <- subset(booted_atts2_hhi, booted_atts2_hhi$Model == 'Full' | booted_atts2_hhi$Model == 'MCO (2015)')

ggplot(full_booted_hhi) +
    geom_point(aes(x = Period, y = estimate, color = Model), position = dodge) +
    geom_errorbar(aes(x = Period, y = estimate, xmin = Period, xmax = Period, ymin = conf.low, ymax = conf.high, color = Model), position = dodge) +
    geom_hline(yintercept = 0) +
    theme_bw() +    
    xlab('Time to Treatment') +
    ylab('Estimated Coefficient')




#################################################
#### Electoral Crimes as outcome ########
#################################################

    # remove 2015

panel2 <- panel %>%
            filter(Year != 2013,
                   Year != 2014,
                   treat_stag != 2015)



sunab_feols2 <- function(dv, cv, aggregation) {
  # estimate effects
  reg_formula <- as.formula(paste(dv, ' ~ total_alt + state_misalign + Pres_misalign + pri_mayor + sunab(treat_stag, Year)*', cv, '| Year'))

  sunab_est <- feols(reg_formula, data = panel2, cluster = ~State)

  # wild-cluster bootstrap
  sunab_boot <- fwildclusterboot::boot_aggregate(sunab_est, agg = aggregation, B = 9999, clustid = 'State')

  return(sunab_boot)

}


ele_tot <- sunab_feols2("electorales", '1', aggregation[1])

ele_c1 <- sunab_feols2("electorales", competition[1], aggregation[1])
ele_c2 <- sunab_feols2("electorales", competition[2], aggregation[1])
ele_c3 <- feols(electorales ~ total_alt + state_misalign + Pres_misalign +  pri_mayor + sunab(treat_stag, Year)*IRCO |  Year, panel2, cluster = ~State)
ele_c4 <- sunab_feols2("electorales", competition[4], aggregation[1])
ele_c5 <- sunab_feols2("electorales", competition[5], aggregation[1])
ele_c6 <- sunab_feols2("electorales", competition[6], aggregation[1])

# ATTs
bootele_tot <- sunab_feols2("electorales", '1', aggregation[2])
ele_att_c1 <- sunab_feols2("electorales", competition[1], aggregation[2])
ele_att_c2 <- sunab_feols2("electorales", competition[2], aggregation[2])
ele_att_c3 <- aggregate(ele_c3, agg = "ATT")
ele_att_c4 <- sunab_feols2("electorales", competition[4], aggregation[2])
ele_att_c5 <- sunab_feols2("electorales", competition[5], aggregation[2])
ele_att_c6 <- sunab_feols2("electorales", competition[6], aggregation[2])

# visualize ATTs
ele_table_atts <- data.frame(rbind(bootele_tot,ele_att_c1,ele_att_c2,ele_att_c3,ele_att_c4,ele_att_c5,ele_att_c6))

ele_table_atts <- cbind(names, ele_table_atts)

rownames(ele_table_atts) <- NULL 
colnames(ele_table_atts) <- c("Model", "Estimate", "P-value", "conf.low", "conf.high")



# this is not very helpful!
ggplot(ele_table_atts) +
    geom_point(aes(x = Estimate, y = Model, color = Model)) +
    geom_errorbarh(aes(x = Estimate, y = Model, xmin = conf.low, xmax = conf.high, ymin = Model, ymax = Model, color = Model)) +
    geom_hline(yintercept = 0) +
    theme_bw() +    
    xlab('Estimate') +
    ylab('Model') + 
    geom_vline(xintercept = 0, color = 'dark gray')


# now, without IRCG
ele_table_atts2 <- subset(ele_table_atts, ele_table_atts$Model != 'IRCG (2020)')

ggplot(ele_table_atts2) +
    geom_point(aes(x = Estimate, y = Model, color = Model)) +
    geom_errorbarh(aes(x = Estimate, y = Model, xmin = conf.low, xmax = conf.high, ymin = Model, ymax = Model, color = Model)) +
    geom_hline(yintercept = 0) +
    theme_bw() +    
    xlab('Estimate') +
    ylab('Model') + 
    geom_vline(xintercept = 0, color = 'dark gray')



# table of dynamic booted ests

booted_atts_ele <- data.frame(rbind(bootele_tot,ele_att_c1,ele_att_c2,ele_att_c4,ele_att_c5,ele_att_c6))

booted_atts_ele <- cbind(model, Period, booted_atts_ele)

names(booted_atts_ele) <- c('Model', 'Period', 'estimate', 'p.value', 'conf.low', 'conf.high')


# add pvalues, conf intervals for C3 (not bootstrapped)

c3_est_ele <- data.frame(coef(ele_c3))
c3_se_ele <- data.frame(se(ele_c3))
c3_pv_ele <- data.frame(pvalue(ele_c3))
c3_period_ele <- c(NA, NA, NA, NA, -3,-2, 0,1, 2,3,4,5, NA)
c3_mod_ele <- rep('IRCG', length(c3_se_ele))

c3_df_ele <- data.frame(c3_mod_ele,c3_period_ele, c3_est_ele, c3_se_ele, c3_pv_ele)

names(c3_df_ele) <- c('Model', 'Period', 'estimate', 'se', 'p.value')

c3_df_ele <- c3_df_ele %>%
    mutate(conf.low = estimate - cval*p.value,
           conf.high = estimate + cval*p.value)

# remove NAs
c3_df_ele <- na.omit(c3_df_ele)
c3_df_ele <- c3_df_ele[,-4]

# add to main df
booted_top_ele <- booted_atts_ele[1:33,]
booted_bottom_ele <- booted_atts_ele[34:66,]

booted_atts_ele2 <- rbind(booted_top_ele, c3_df_ele, booted_bottom_ele)


# visualization
# without IRCG
booted_atts2_ele <- subset(booted_atts_ele, booted_atts_ele$Model != 'IRCG')
booted_atts2_ele <- booted_atts2_ele[-23,]

booted_atts2_ele <- subset(booted_atts_ele, booted_atts_ele$Period != -2)

dodge <- position_dodge(width=0.5) 

ggplot(booted_atts2_ele) +
    geom_point(aes(x = Period, y = estimate, color = Model), position = dodge) +
    geom_errorbar(aes(x = Period, y = estimate, xmin = Period, xmax = Period, ymin = conf.low, ymax = conf.high, color = Model), position = dodge) +
    geom_hline(yintercept = 0) +
    theme_bw() +    
    xlab('Time to Treatment') +
    ylab('Estimated Coefficient')


# just the full model

full_booted_ele <- subset(booted_atts2_ele, booted_atts2_ele$Model == 'Full' | booted_atts2_ele$Model == 'MCO (2015)')

ggplot(full_booted_ele) +
    geom_point(aes(x = Period, y = estimate, color = Model), position = dodge) +
    geom_errorbar(aes(x = Period, y = estimate, xmin = Period, xmax = Period, ymin = conf.low, ymax = conf.high, color = Model), position = dodge) +
    geom_hline(yintercept = 0) +
    theme_bw() +    
    xlab('Time to Treatment') +
    ylab('Estimated Coefficient')

