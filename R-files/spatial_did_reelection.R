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





###############################################################
#### Spatial Elements ####
###############################################################

### Moran I's

# load map
mexico <- st_read(dsn ="C:\\Users\\adeew\\OneDrive\\Documents\\Reelect_Ref\\data\\mex_admbnda_govmex_20210618_SHP\\mex_admbnda_adm2_govmex_20210618.shp")


# rewrite code for each municipality to match shape file
shape_mex <- as.data.frame(mexico)

# add total attacks
municip_attacks <- panel2 %>%
  group_by(ADM2_PCODE) %>%
    summarize(total_attacks_mun = sum(Attack_count))

municip_attacks <- na.omit(as.data.frame(municip_attacks))

### make the correct number

codes <- municip_attacks$ADM2_PCODE
class(codes)
codes <- as.character(codes)

tes <- ifelse(nchar(codes)>4,paste("MX",codes,sep = ""),paste("MX0",codes,sep = ""))

municip_attacks$ADM2_PCODE <- tes

mex_shape2 <- merge(mexico, municip_attacks, by = "ADM2_PCODE")

names(mex_shape2)


# define neighboring polygons -- defined by a touching point
nb <- poly2nb(mex_shape2, queen = TRUE)
nb[1]


# assign weights to the neighbors
lw <- nb2listw(nb, style="W", zero.policy=TRUE)

summary(lw$weights)

lw$weights[1]

# compute the weighted neighbor values
inc.lag <- lag.listw(lw, mex_shape2$total_attacks_mun)

inc.lag

# plot
plot(inc.lag ~ mex_shape2$total_attacks_mun, pch=16, asp=1, xlab="Attacks", ylab="Average Attacks in Cluster")
M1 <- lm(inc.lag ~ mex_shape2$total_attacks_mun)
abline(M1, col="blue")



# slope of the line is Moran's I coefficient
coef(M1)[2]

# positive slope indicates that as the number of attacks that a polygon experiences increases, so does the number of attacks of its neighbors

# Hypothesis test
MT <- moran.test(mex_shape2$total_attacks_mun, lw, zero.policy = TRUE, alternative="greater")

MT

# Monte Carlo Simulation for p-value
MC <- moran.mc(mex_shape2$total_attacks_mun, lw, zero.policy = TRUE, nsim=999, alternative="greater")

MC

# make into a table

Moran_I_stats <- as.data.frame(as.numeric(MT$estimate))

se_moran <- sqrt(Moran_I_stats[3,1]/2014)

Moran_I_stats <- rbind(Moran_I_stats, se_moran)

Moran_I_stats <- rbind(Moran_I_stats, MC$p.value)

names_moran <- rbind("Moran I Statistic", "Expectation", "Variance", "SE", "P-value")

Moran_I_stats <- cbind(names_moran, Moran_I_stats)

names(Moran_I_stats) <- c("Value","Estimate")

library(xtable)

xtable(t(Moran_I_stats), digits = 5)

t(Moran_I_stats)


### Add other elements

## Now we can find moran's i for number of candidates in a municipality given nearby attacks, and concentration of votes

# add total attacks
voting <- panel2 %>%
  group_by(ADM2_PCODE) %>%
    summarize(mean_ncand = mean(n_can, na.rm= TRUE), mean_hhi = mean(hhi, na.rm= TRUE), mean_ele = mean(electorales, na.rm= TRUE))

voting <- na.omit(as.data.frame(voting))
head(voting)

### make the correct number

codes2 <- voting$ADM2_PCODE
codes2 <- as.character(codes2)

tes2 <- ifelse(nchar(codes2)>4,paste("MX",codes2,sep = ""),paste("MX0",codes2,sep = ""))

voting$ADM2_PCODE <- tes2

mex_shape_vote <- merge(mexico, voting, by = "ADM2_PCODE")

head(mex_shape_vote)

# # produce figure
# tm_shape(mex_shape_vote) + 
#   tm_polygons("mean_hhi", palette="-RdYlGn", style="pretty", n=10, title = "Mean HHI") 

# tm_shape(mex_shape_vote) + 
#   tm_polygons("mean_ncand", palette="-RdYlGn", style="pretty", n=10, title= "Number of Candidates")  +
#   tm_layout(legend.position = c('right','top'))

## add attacks
mex_shape_vote2 <- merge(mex_shape_vote, municip_attacks, by = "ADM2_PCODE")


# plot

# define neighboring polygons -- defined by a touching point
nb2 <- poly2nb(mex_shape_vote2, queen = TRUE)

# assign weights to the neighbors
lw2 <- nb2listw(nb2, style="W", zero.policy=TRUE)

# compute the weighted neighbor values
inc.lag2 <- lag.listw(lw2, mex_shape_vote2$total_attacks_mun)

inc.lag2

# plot
plot(inc.lag2 ~ mex_shape_vote2$mean_hhi, pch=16, asp=1, xlab="Mean HHI", ylab="Average Attacks in Cluster")
M1_2 <- lm(inc.lag2 ~ mex_shape_vote2$mean_hhi)
abline(M1_2, col="blue")

plot(inc.lag2 ~ mex_shape_vote2$mean_ncand, pch=16, asp=1, xlab="Mean Number of Candidates", ylab="Average Attacks in Cluster")
M1_2 <- lm(inc.lag2 ~ mex_shape_vote2$mean_ncand)
abline(M1_2, col="blue")

plot(inc.lag2 ~ mex_shape_vote2$mean_ele, pch=16, asp=1, xlab="Mean Number of Electoral Crimes", ylab="Average Attacks in Cluster")
M1_2 <- lm(inc.lag2 ~ mex_shape_vote2$mean_ele)
abline(M1_2, col="blue")

# make a table of HHI, n cand and ele


# Hypothesis test
MT1 <- moran.test(mex_shape_vote2$mean_ncand, lw2, zero.policy = TRUE, alternative="greater")

MT1

# Monte Carlo Simulation for p-value
MC1 <- moran.mc(mex_shape_vote2$mean_ncand, lw2, zero.policy = TRUE, nsim=999, alternative="greater")

MC1

MT2 <- moran.test(mex_shape_vote2$mean_hhi, lw2, zero.policy = TRUE, alternative="greater")
MC2 <- moran.mc(mex_shape_vote2$mean_hhi, lw2, zero.policy = TRUE, nsim=999, alternative="greater")

MT3 <- moran.test(mex_shape_vote2$mean_ele, lw2, zero.policy = TRUE, alternative="greater")
MC3 <- moran.mc(mex_shape_vote2$mean_ele, lw2, zero.policy = TRUE, nsim=999, alternative="greater")


# make into a table

Moran_I_stats1 <- as.data.frame(rbind(as.numeric(MT1$estimate), as.numeric(MT2$estimate), as.numeric(MT3$estimate)))

se_moran1 <- rep(0, 3)
se_moran1[1] <- sqrt(Moran_I_stats1[1,3]/2014)
se_moran1[2] <- sqrt(Moran_I_stats1[2,3]/2014)
se_moran1[3] <- sqrt(Moran_I_stats1[3,3]/2014)

Moran_I_stats1 <- cbind(Moran_I_stats1, se_moran1)

p_vals_MC <- rbind(MC1$p.value, MC2$p.value, MC3$p.value)

Moran_I_stats1 <- cbind(Moran_I_stats1, p_vals_MC)

names_moran <- rbind("Moran I Statistic", "Expectation", "Variance", "SE", "P-value")

names(Moran_I_stats1) <- names_moran

# stargazer(t(Moran_I_stats1))

# put it in the table by hand cause I got confused







###########################################
########## Looking at Spillovers ##########
###########################################


###### Sun and Abraham (2020) estimator with Spillovers #######

# rename to avoid repeats
panel <- panel %>%
    mutate(Year_a = Year,
           max_yr_va = max_yr_v)



# total sample
res_sunab_tot_sp <- feols(Attack_count ~ total_alt + state_misalign + Pres_misalign+pri_mayor  + sunab(max_yr_va, Year_a) + sunab(treat_stag, Year)
    | Municipality + Year, panel, cluster = ~State)


res_sunab2_sp <- feols(Attack_count ~ total_alt + state_misalign + Pres_misalign+pri_mayor  + sunab(treat_stag, Year)  + sunab(max_yr_va, Year_a)
    | Municipality + Year, panel, cluster = ~State)


etable(res_sunab_tot_sp)
etable(res_sunab2_sp)



# until I find a better way of doing this, this copied and pasted from the two etable outputs 


times <- c(-5,-4,-3,-2,0,1,2,3,4,5,6)

sunab_ests <- c(-0.0089,-0.0133,0.0053,0.0044,-0.0022,-0.0053,-0.0054,-0.0116,-0.0048,0.0051,0.0079)

sunab_ses <-c(0.0067,0.0069,0.0069,0.0045,0.0044,0.0043,0.0061,0.0174,0.0085,0.0128,0.0199)

sunab_ests_sp <- c(0.0034,-0.036, 0.0025,-0.0001,0.0213,-0.0227,0.0158,0.0123,-0.0009,0.0049,0.0026)

sunab_ses_sp <- c(0.0077,0.0039,0.0050,0.003,0.0066,0.0073,0.0056,0.0084,0.0069,0.0085,0.0138)

spillover <- c(rep("Direct",11),rep("Indirect",11))
estimates <- c(sunab_ests,sunab_ests_sp)
ses <- c(sunab_ses,sunab_ses_sp)
times2 <- rep(times, 2)

sunab_df <- data.frame(times, sunab_ests,sunab_ses, sunab_ests_sp, sunab_ses_sp)

sunab_df2 <- data.frame(times2, estimates, ses, spillover)

sunab_df

# make CIs
alp <- 0.05
cval <- qnorm(1-alp/2)


sunab_df2 <- sunab_df2 %>%
    mutate(ci_lower = estimates - cval*ses,
           ci_upper = estimates + cval*ses)

sunab_df2


sunab_df2$spillover <- as.factor(sunab_df2$spillover)

dodge <- position_dodge(width=0.5) 

ggplot(sunab_df2) +
    geom_point(aes(x = times2, y = estimates, color = factor(spillover)), position = dodge) + 
  geom_errorbar(aes(x = times2, y = estimates, xmin = times2, xmax = times2, ymin = ci_lower, ymax = ci_upper, color = spillover), position = dodge) +
    geom_hline(yintercept = 0, linetytpe = 'dotted') +
    scale_fill_manual(labels=c('Total Treament', 'Indirect')) + 
    theme_bw() +
    theme(legend.title = element_blank())  +
    xlab('Time to Treatment') +
    ylab('Estimated Coefficient')


################################################################
###### Competitive #####
################################################################


# total sample
sunab_c1 <- feols(Attack_count ~ total_alt + state_misalign + Pres_misalign+pri_mayor   + sunab(max_yr_va, Year_a)*groups_active_alt_2015 + sunab(treat_stag, Year)*groups_active_alt_2015
    | Municipality + Year, panel, cluster = ~State)


sunab_c1a <- feols(Attack_count ~ total_alt + state_misalign + Pres_misalign+pri_mayor  + sunab(treat_stag, Year)*groups_active_alt_2015  + sunab(max_yr_va, Year_a)*groups_active_alt_2015
    | Municipality + Year, panel, cluster = ~State)


etable(sunab_c1)
etable(sunab_c1a)



# until I find a better way of doing this, this copied and pasted from the two etable outputs 


times <- c(-5,-4,-3,-2,0,1,2,3,4,5,6)

sunab_ests_c1 <- c(-0.007,-0.0096,0.0047,-0.0036,0.0005,-0.0057,-0.007,-0.0195,-0.0082,-0.0028,-0.0056)

sunab_ses_c1 <-c(0.0041,0.0036,0.005,0.0028,0.0036,0.0024,0.0042,0.01,0.0063,0.0046,0.0106)

sunab_ests_sp_c1 <- c(0.0003,0.0008, 0.0003,-0.0005,0.0066,0.0062,0.0127,0.0056,0.0021,-0.0066,-0.0036)

sunab_ses_sp_c1 <- c(0.0040,0.0038,0.0039,0.0023,0.0045,0.0038,0.0034,0.0049,0.0034,0.0069,0.0110)

spillover <- c(rep("Direct",11),rep("Indirect",11))
comp_type <- rep('MCO (2015)',11)
estimates <- c(sunab_ests_c1,sunab_ests_sp_c1)
ses <- c(sunab_ses_c1,sunab_ses_sp_c1)
times2 <- rep(times, 2)


sunab_df2_c1 <- data.frame(times2, estimates, ses, spillover, comp_type)

sunab_df2_c1 <- sunab_df2_c1 %>%
    mutate(ci_lower = estimates - cval*ses,
           ci_upper = estimates + cval*ses)

sunab_df2_c1


sunab_df2_c1$spillover <- as.factor(sunab_df2_c1$spillover)

ggplot(sunab_df2_c1) +
    geom_point(aes(x = times2, y = estimates, color = factor(spillover)), position = dodge) + 
  geom_errorbar(aes(x = times2, y = estimates, xmin = times2, xmax = times2, ymin = ci_lower, ymax = ci_upper, color = spillover), position = dodge) +
    geom_hline(yintercept = 0, linetytpe = 'dotted') +
    scale_fill_manual(labels=c('Total Treament', 'Indirect')) + 
    theme_bw() +
    theme(legend.title = element_blank())  +
    xlab('Time to Treatment') +
    ylab('Estimated Coefficient')





# total sample
sunab_c2 <- feols(Attack_count ~ total_alt + state_misalign + Pres_misalign+pri_mayor   + sunab(max_yr_va, Year_a)*nCarteles_2010 + sunab(treat_stag, Year)*nCarteles_2010
    | Municipality + Year, panel, cluster = ~State)


sunab_c2a <- feols(Attack_count ~ total_alt + state_misalign + Pres_misalign+pri_mayor  + sunab(treat_stag, Year)*nCarteles_2010  + sunab(max_yr_va, Year_a)*nCarteles_2010
    | Municipality + Year, panel, cluster = ~State)


etable(sunab_c2)
etable(sunab_c2a)



# until I find a better way of doing this, this copied and pasted from the two etable outputs 


times <- c(-5,-4,-3,-2,0,1,2,3,4,5,6)

sunab_ests_c2 <- c(-0.0107,-0.0141,0.0044,0.0031,-0.0026,-0.0047,-0.0069,-0.0176,-0.0074,-0.0020,-0.0071)

sunab_ses_c2 <-c(0.0053,0.0052,0.0053,0.0032,0.0034,0.0031,0.0051,0.0117,0.0063,0.0075,0.0138)

sunab_ests_sp_c2 <- c(0.0025,-0.0042, 0.0001,-0.0001,0.0133,0.0133,0.01,0.0101,0.003,-0.0006,-0.0054)

sunab_ses_sp_c2 <- c(0.0064,0.0025,0.0036,0.0021,0.0041,0.0041,0.0038,0.0065,0.0042,0.0049,0.0079)

spillover <- c(rep("Direct",11),rep("Indirect",11))
comp_type <- rep('Cartels (2010)',11)
estimates <- c(sunab_ests_c2,sunab_ests_sp_c2)
ses <- c(sunab_ses_c2,sunab_ses_sp_c2)
times2 <- rep(times, 2)


sunab_df2_c2 <- data.frame(times2, estimates, ses, spillover, comp_type)

sunab_df2_c2 <- sunab_df2_c2 %>%
    mutate(ci_lower = estimates - cval*ses,
           ci_upper = estimates + cval*ses)

sunab_df2_c2


sunab_df2_c2$spillover <- as.factor(sunab_df2_c2$spillover)

ggplot(sunab_df2_c2) +
    geom_point(aes(x = times2, y = estimates, color = factor(spillover)), position = dodge) + 
  geom_errorbar(aes(x = times2, y = estimates, xmin = times2, xmax = times2, ymin = ci_lower, ymax = ci_upper, color = spillover), position = dodge) +
    geom_hline(yintercept = 0, linetytpe = 'dotted') +
    scale_fill_manual(labels=c('Total Treament', 'Indirect')) + 
    theme_bw() +
    theme(legend.title = element_blank())  +
    xlab('Time to Treatment') +
    ylab('Estimated Coefficient')


#  combine into one figure

df_sunab <- rbind(sunab_df2_c1, sunab_df2_c2)

df_sunab

dodge <- position_dodge(width=0.5) 

ggplot(df_sunab) +
    geom_point(aes(x = times2, y = estimates, color = spillover), position = dodge) + 
  geom_errorbar(aes(x = times2, y = estimates, xmin = times2, xmax = times2, ymin = ci_lower, ymax = ci_upper, color = comp_type), position = dodge) +
    geom_hline(yintercept = 0, linetytpe = 'dotted') +
    scale_fill_manual(labels=c('Total Treament', 'Indirect', 'MCO (2015)', 'Restrepo (2012)')) + 
    theme_bw() +
    theme(legend.title = element_blank())  +
    xlab('Time to Treatment') +
    ylab('Estimated Coefficient')





### produce table



# list(res_total, res_sunab_c1, res_sunab_c2, res_sunab_c3, res_sunab_c4, res_sunab_c5, res_sunab_c6)

modelsummary(res_sunab2_sp, 
    agg = "period",
    output = "latex",
    stars = TRUE,
    estimate = "{estimate}{stars}", 
    statistic = "[{conf.low}, {conf.high}]",
    #coef_omit = "^(?!Year*×*)",       
    #coef_rename = c("total_alt" = "Total alternation", "state_misalign" = 'State aisalignment', 'Pres_misalign' = 'Federal misalignment', 'pri_mayor' = 'PRI mayor', "Year::-5" = "Period: -5", "Year::-4" = "Period: -4", "Year::-3" = "Period: -3", "Year::-2" = "Period: -2", "Year::0" = "Period: 0", "Year::1" = "Period: 1", "Year::2" = "Period: 2", "Year::3" = "Period: 3", "Year::4" = "Period: 4", "Year::5" = "Period: 5", "Year::6" = "Period: 6", 'nCarteles_2010' = 'Restrepo (2010)', 'groups_active_alt_2015' = 'MCO (2015)', 'IRCO' = 'IRCG (2020)', 'ACLED_conflict' = 'ACLED', 'PEI_crim_index' = 'PEI Survey', 'val_index' = 'Value Index'), 
    notes = "+ p $<$ 0.1, * p $<$ 0.05, ** p $<$ 0.01, *** p $<$ 0.001",
    title = "Staggered (Di)DiD Estimates for Attacks") %>% kable_styling(latex_options = "scale_down")









################################################################
###### Number of candidates
################################################################

ncan_dir <- feols(n_can ~ total_alt + state_misalign + Pres_misalign+pri_mayor  +  sunab(treat_stag, Year)
    | Municipality + Year, panel, cluster = ~State)


ncan2_indir <- feols(n_can ~ total_alt + state_misalign + Pres_misalign+pri_mayor  + sunab(max_yr_va, Year_a)
    | Municipality + Year, panel, cluster = ~State)


etable(ncan_dir)
etable(ncan2_indir)

summary(ncan_dir, agg = 'cohort')



# until I find a better way of doing this, this copied and pasted from the two etable outputs 


times <- c(-5,-4,-3,-2,0,1,2,3,4,5,6)

sunab_ests <- c(0.8258,9.064,-3.603,-0.9731,-1.551,7.781,1.758,6.393, NA,-5.272, NA)

sunab_ses <-c(0.5775, 0.4827,162778.1,191735.7,98198.9,25910.2,191735.7,0.5793,NA, 191735.7, NA)

sunab_ests_sp <- c(-0.5940,-0.2497,-0.2723,-0.0331,-0.1384,-0.1390,0.1947,-0.3571,0.2659,0.0826,-0.0159)

sunab_ses_sp <- c(0.2160,0.6272,0.2600,0.2980,0.2851,0.3345,0.2286,0.2053,0.4636,0.3000,0.2545)

length(sunab_ses_sp)


spillover <- c(rep("Direct",11),rep("Indirect",11))
estimates <- c(sunab_ests,sunab_ests_sp)
ses <- c(sunab_ses,sunab_ses_sp)
times2 <- rep(times, 2)

sunab_df2_ncan <- data.frame(times2, estimates, ses, spillover)

sunab_df2_ncan


sunab_df2_ncan <- sunab_df2_ncan %>%
    mutate(ci_lower = estimates - cval*ses,
           ci_upper = estimates + cval*ses)

sunab_df2_ncan


sunab_df2_ncan$spillover <- as.factor(sunab_df2_ncan$spillover)

ggplot(sunab_df2_ncan) +
    geom_point(aes(x = times2, y = estimates, color = factor(spillover))) + 
  #geom_errorbar(aes(x = times2, y = estimates, xmin = times2, xmax = times2, ymin = ci_lower, ymax = ci_upper, color = spillover)) +
    geom_hline(yintercept = 0, linetytpe = 'dotted') +
    scale_fill_manual(labels=c('Total Treament', 'Indirect')) + 
    theme_bw() +
    theme(legend.title = element_blank())  +
    xlab('Time to Treatment') +
    ylab('Estimated Coefficient')




# total sample

sunab_c2_ncan <- feols(n_can ~ total_alt + state_misalign + Pres_misalign+pri_mayor   + sunab(max_yr_va, Year_a)*nCarteles_2010 + sunab(treat_stag, Year)*nCarteles_2010
    | Municipality + Year, panel, cluster = ~State)


sunab_c2a_ncan <- feols(n_can ~ total_alt + state_misalign + Pres_misalign+pri_mayor  + sunab(treat_stag, Year)*nCarteles_2010  + sunab(max_yr_va, Year_a)*nCarteles_2010
    | Municipality + Year, panel, cluster = ~State)


etable(sunab_c2_ncan)
etable(sunab_c2a_ncan)



# until I find a better way of doing this, this copied and pasted from the two etable outputs 


times <- c(-5,-4,-3,-2,0,1,2,3,4,5,6)

sunab_ests_c2 <- c(0.6337,6.126, -4.471, -3.564,-2.058,5.626, -0.9220,4.263, NA, -8.463, NA)

sunab_ses_c2 <-c(0.4665,0.2560,108410.3,175618.7,49734.0,15965.3,130057.3,NA,0.3837,175618.8,NA)

sunab_ests_sp_c2 <- c(-0.4197, 0.0170,-0.2338,0.0015, -0.1693,-0.0807 , 0.1786,-0.3704,0.2201,0.0248,-0.2188)

sunab_ses_sp_c2 <- c(0.1714,0.4444,0.1941,0.1964,0.1828,0.2482,0.1737,0.1509,0.3634,0.1980,0.3151)

length(sunab_ses_c2)

spillover <- c(rep("Direct",11),rep("Indirect",11))
comp_type <- rep('Cartels (2010)',22)
estimates <- c(sunab_ests_c2,sunab_ests_sp_c2)
ses <- c(sunab_ses_c2,sunab_ses_sp_c2)
times2 <- rep(times, 2)


sunab_df2_c2 <- data.frame(times2, estimates, ses, spillover, comp_type)

sunab_df2_c2 <- sunab_df2_c2 %>%
    mutate(ci_lower = estimates - cval*ses,
           ci_upper = estimates + cval*ses)

sunab_df2_c2


sunab_df2_c2$spillover <- as.factor(sunab_df2_c2$spillover)

ggplot(sunab_df2_c2) +
    geom_point(aes(x = times2, y = estimates, color = factor(spillover))) + 
  #geom_errorbar(aes(x = times2, y = estimates, xmin = times2, xmax = times2, ymin = ci_lower, ymax = ci_upper, color = spillover)) +
    geom_hline(yintercept = 0, linetytpe = 'dotted') +
    scale_fill_manual(labels=c('Total Treament', 'Indirect')) + 
    theme_bw() +
    theme(legend.title = element_blank())  +
    xlab('Time to Treatment') +
    ylab('Estimated Coefficient')


#  combine into one figure

df_sunab_ncan <- rbind(sunab_df2_c1, sunab_df2_c2)

df_sunab_ncan

ggplot(df_sunab_ncan) +
    geom_point(aes(x = times2, y = estimates, color = comp_type), position = dodge) + 
  #geom_errorbar(aes(x = times2, y = estimates, xmin = times2, xmax = times2, ymin = ci_lower, ymax = ci_upper, color = comp_type), position = dodge) +
    geom_hline(yintercept = 0, linetytpe = 'dotted') +
    scale_fill_manual(labels=c('Total Treament', 'Indirect', 'MCO (2015)', 'Restrepo (2012)')) + 
    theme_bw() +
    theme(legend.title = element_blank())  +
    xlab('Time to Treatment') +
    ylab('Estimated Coefficient')




################################################################
###### hhi
################################################################
head(panel)

# total sample
hhi <- feols(hhi ~ total_alt + state_misalign + Pres_misalign+pri_mayor   + sunab(max_yr_va, Year_a) + sunab(treat_stag, Year)
    | Municipality + Year, panel, cluster = ~State)


hhi2 <- feols(hhi ~ total_alt + state_misalign + Pres_misalign+pri_mayor  + sunab(treat_stag, Year)  + sunab(max_yr_va, Year_a)
    | Municipality + Year, panel, cluster = ~State)


etable(hhi)
etable(hhi2)




# until I find a better way of doing this, this copied and pasted from the two etable outputs 


times <- c(-5,-4,-3,-2,0,1,2,3,4,5,6)

sunab_ests <- c(-0.0582,-0.1413,-0.5138,NA,-0.2837,0.0502, -0.6330,-0.1437,NA,-0.5876,NA)

sunab_ses <- c(0.0321,0.0276,NA,26085.1,15766.7,4153.8,30738.1,0.0766,NA,30738.1,NA)

sunab_ests_sp <- c(0.0311,0.0212,0.0010,-0.0509,-0.0500, -0.0413,-0.0137,-0.0399,0.0420 , -0.0031,0.0274)


sunab_ses_sp<-c(0.0159, 0.0277,0.0189,0.0477,0.0438,0.0512,0.0127,0.0411,0.0431,0.0167,0.0314)


length(sunab_ests)


spillover <- c(rep("Direct",11),rep("Indirect",11))
estimates <- c(sunab_ests,sunab_ests_sp)
ses <- c(sunab_ses,sunab_ses_sp)
times2 <- rep(times, 2)


sunab_df2_ncan <- data.frame(times2, estimates, ses, spillover)

sunab_df2_ncan


sunab_df2_ncan <- sunab_df2_ncan %>%
    mutate(ci_lower = estimates - cval*ses,
           ci_upper = estimates + cval*ses)

sunab_df2_ncan


sunab_df2_ncan$spillover <- as.factor(sunab_df2_ncan$spillover)

ggplot(sunab_df2_ncan) +
    geom_point(aes(x = times2, y = estimates, color = factor(spillover))) + 
  #geom_errorbar(aes(x = times2, y = estimates, xmin = times2, xmax = times2, ymin = ci_lower, ymax = ci_upper, color = spillover)) +
    geom_hline(yintercept = 0, linetytpe = 'dotted') +
    scale_fill_manual(labels=c('Total Treament', 'Indirect')) + 
    theme_bw() +
    theme(legend.title = element_blank())  +
    xlab('Time to Treatment') +
    ylab('Estimated Coefficient')




##############################################
    ######## Spatial DiD ########
##############################################



#### spatial DiD


# control for valerage attacks in neighboring municipalities for previous year

panel2 <- panel

# need to make panel balanced

group_2013 <- subset(panel2, panel2$Year == 2013)
un_mun_13 <- unique(group_2013$ADM2_PCODE)
panel_bal <- subset(panel2, panel2$ADM2_PCODE %in% un_mun_13)


group_2021 <- subset(panel_bal, panel_bal$Year == 2021)
un_mun_21 <- unique(group_2021$ADM2_PCODE)
panel_bal2 <- subset(panel_bal, panel_bal$ADM2_PCODE %in% un_mun_21)


# panel is now balanced

#write.csv(panel_bal2, 'balanced_update1_10_26_22.csv')



# get coordinates of municipalites

# try with just one year

# add total attacks
municip_attacks_13 <- panel2 %>%
  filter(Year == 2013) %>%
  group_by(ADM2_PCODE) %>%
    summarize(total_attacks_mun = as.numeric(sum(Attack_count)))


municip_attacks_13 <- na.omit(as.data.frame(municip_attacks_13))

### make the correct number

codes_13 <- municip_attacks_13$ADM2_PCODE
class(codes_13)
codes_13 <- as.character(codes_13)

tes_13 <- ifelse(nchar(codes_13)>4,paste("MX",codes_13,sep = ""),paste("MX0",codes_13,sep = ""))

municip_attacks_13$ADM2_PCODE <- tes_13

mex_shape2_13 <- merge(mexico, municip_attacks_13, by = "ADM2_PCODE")

head(mex_shape2_13)

# define neighboring polygons -- defined by a touching point
nb_mex_13 <- poly2nb(mex_shape2_13, queen = TRUE)
nb_mex_13[1]

# number of neighbors for each municipality
card(nb_mex_13)


# assign weights to the neighbors
lw_mex_13 <- nb2listw(nb_mex_13, style="W", zero.policy=TRUE)
lw_mex_13$weights[2]


# compute the weighted neighbor values
inc.lag_mex13 <- lag.listw(lw_mex_13, mex_shape2_13$total_attacks_mun)

names_mex_13 <- unique(mex_shape2_13$ADM2_PCODE)


inc.lag_mex13


# now that this works, write a function and loop to do this for every year

neighborhood_lags <- function(panel, shapefile) {
    neighborhood_lags <- NULL
    un_years <- unique(panel$Year)
    for (i in 1:length(un_years)){
        Year_i <- un_years[i]
        # add total attacks for that year
        municip_attacks_i <- panel %>%
            filter(Year == Year_i) %>%
            group_by(ADM2_PCODE) %>%
            summarize(total_attacks_mun = as.numeric(sum(Attack_count)))
        municip_attacks_i <- na.omit(as.data.frame(municip_attacks_i))
        # clean names
        codes_i <- municip_attacks_i$ADM2_PCODE
        codes_i <- as.character(codes_i)
        tes_i <- ifelse(nchar(codes_i)>4,paste("MX",codes_i,sep = ""),paste("MX0",codes_i,sep = ""))
        municip_attacks_i$ADM2_PCODE <- tes_i
        mex_shape2_i <- merge(shapefile, municip_attacks_i, by = "ADM2_PCODE")
        # define neighboring polygons
        nb_mex_i <- poly2nb(mex_shape2_i, queen = TRUE)
        # assign weights to the neighbors
        lw_mex_i <- nb2listw(nb_mex_i, style="W", zero.policy=TRUE)
        # compute the weighted neighbor values
        inc.lag_mex_i <- lag.listw(lw_mex_i, mex_shape2_i$total_attacks_mun)
        neighborhood_weights <- inc.lag_mex_i
        year <- rep(Year_i, length(inc.lag_mex_i))
        id <- unique(mex_shape2_i$ADM2_PCODE)
        neighborhood <- data.frame(year, id, neighborhood_weights)
        neighborhood_lags <- rbind(neighborhood_lags, neighborhood)
        left <- length(un_years)-i
        out <- paste0('Completed loop for ', i, ', ', left, ' more to go.')
        print(out)
    }
    return(neighborhood_lags)
}



# make lagged attacks by year (This is going to take a *LONG* time to run -- like 15 minutes. Keep an eye out for check points and consider doing manually)
weights_n <- neighborhood_lags(panel_bal, mexico)

# this makes a 16926x3 matrix
head(weights_n)
dim(weights_n)

# merge this with panel data
neighs <- weights_n

names(neighs)

neighs$id <- sub("^MX", "", neighs$id)
neighs$id <- sub("^0", "", neighs$id)

neighs$id <- as.integer(neighs$id)

class(panel_bal$ADM2_PCODE)
class(neighs$id)

panel_spat <- left_join(panel_bal, neighs, by = c('Year' = 'year', 'ADM2_PCODE' = 'id'))


# save and write out
write.csv(panel_spat, "balanced_panel_10_25_22.csv")


head(panel_spat)
dim(panel_spat)
dim(panel_bal2)


# test loop

small_spat1 <- subset(panel_spat, panel_spat$ADM2_PCODE == un_mun_spat[1])

df_small1 <- small_spat1 %>% dplyr::select(Year, ADM2_PCODE, Treated_neighbor)


df_small1

treat_NR_tot1 <- NULL

for (i in 1:length(df_small1$Treated_neighbor)){
    unique_TN <- df_small1[!duplicated(df_small1$Treated_neighbor), ]
    UTN <- subset(unique_TN, unique_TN$Treated_neighbor == 1)
    UTN_yr <- UTN$Year
    UTN_yr <- ifelse(length(UTN_yr) == 0, 0, UTN_yr)
}
Treat_neigh_yr <- rep(0, length(small_spat1$Treated_neighbor))
total_yr <- rep(UTN_yr, length(small_spat1$Treated_neighbor))
for (j in 1:length(small_spat1$Treated_neighbor)){
    if (small_spat1$Year[j] >= UTN_yr){
        Treat_neigh_yr[j] <- UTN_yr
    }
}

df_small1 <- cbind(df_small1, Treat_neigh_yr, total_yr)

treat_NR_tot1 <- rbind(treat_NR_tot1, df_small1)

head(panel_bal2)

small_spat2 <- subset(panel_spat, panel_spat$ADM2_PCODE == un_mun_spat[2])

max(df_small2$Treat_neigh_yr)

df_small2 <- small_spat2 %>% dplyr::select(Year, ADM2_PCODE, Treated_neighbor)


df_small2

treat_NR_tot2 <- NULL


for (i in 1:length(df_small2$Treated_neighbor)){
    unique_TN <- df_small2[!duplicated(df_small2$Treated_neighbor), ]
    UTN <- subset(unique_TN, unique_TN$Treated_neighbor == 1)
    UTN_yr <- UTN$Year
    UTN_yr <- ifelse(length(UTN_yr) == 0, 0, UTN_yr)
}
Treat_neigh_yr <- rep(0, length(small_spat2$Treated_neighbor))
total_yr <- rep(UTN_yr, length(small_spat2$Treated_neighbor))
for (j in 1:length(small_spat2$Treated_neighbor)){
    if (small_spat2$Year[j] >= UTN_yr){
        Treat_neigh_yr[j] <- UTN_yr
    }
}

df_small2 <- cbind(df_small2, Treat_neigh_yr, total_yr)

treat_NR_tot1 <- rbind(treat_NR_tot1, df_small2)








# add staggered spillover
un_mun_spat <- unique(na.omit(panel_bal2$ADM2_PCODE))

treat_NR_tot <- NULL

# this loop will take a long time
for (j in 1:length(un_mun_spat)){
    small_spat <- subset(panel_bal2, panel_bal2$ADM2_PCODE == un_mun_spat[j])
    df_small <- small_spat %>% dplyr::select(Year, ADM2_PCODE, Treated_neighbor)
    names(df_small) <- c('Year', 'ADM2_PCODE', 'Treated_neighbor')
    for (i in 1:length(df_small$Treated_neighbor)){
        unique_TN <- df_small[!duplicated(df_small$Treated_neighbor), ]
        UTN <- subset(unique_TN, unique_TN$Treated_neighbor == 1)
        UTN_yr <- UTN$Year
        UTN_yr <- ifelse(length(UTN_yr) == 0, 0, UTN_yr)
    }
    Treat_neigh_yr <- rep(0, length(df_small$Treated_neighbor))
    total_neigh_yr <- rep(UTN_yr, length(df_small$Treated_neighbor))
    for (k in 1:length(df_small$Year)){
        if (df_small$Year[k] >= UTN_yr){
            Treat_neigh_yr[k] <- UTN_yr
        }
    }
    df_small <- cbind(df_small, Treat_neigh_yr)
    treat_NR_tot <- rbind(treat_NR_tot, df_small)
}

tail(treat_NR_tot)
dim(treat_NR_tot)

### add to main dataframe


#panel_spat <- panel

panel_bal2 <- left_join(panel_bal2, treat_NR_tot, by = c('Year', 'ADM2_PCODE', 'Treated_neighbor'))



head(panel_bal2)

table(panel_bal2$Treat_neigh_yr)


# which ones are 2021

# fix manually 
which(panel_bal2$Treat_neigh_yr==2021)

length(names(panel_bal2))

panel_bal2[16269,88] <- 2018
panel_bal2[16286,88] <- 2018
panel_bal2[16304,88] <- 2018
panel_bal2[16333,88] <- 2018
panel_bal2[16412,88] <- 2018


### now create indicator of first year *neighbors* are treated

# write a function to make a vector for the length of a subset

total_max_TN_yr <- NULL

for (i in 1:length(un_mun_spat)){
    small_df <- subset(panel_bal2, panel_bal2$ADM2_PCODE == un_mun_spat[i])
    max_TN_yr <- max(small_df$Treat_neigh_yr)
    max_TN_yr <- as.vector(rep(max_TN_yr, 9))
    total_max_TN_yr <- rbind(total_max_TN_yr, max_TN_yr)
}
    


heb <- panel_bal2 %>%
  group_by(ADM2_PCODE) %>%
  slice(which.max(Treat_neigh_yr))

heb <- data.frame(heb)

head(heb)

dim(heb)
table(heb$Treat_neigh_yr)

total_year_df <- NULL

for (i in 1:length(heb$Treat_neigh_yr)){
    ad2_code <- heb$ADM2_PCODE[i]
    max_yr <- heb$Treat_neigh_yr[i]
    ad2_code_v <- rep(ad2_code, 9)
    max_yr_v <- rep(max_yr, 9)
    Year <- seq(2013,2021,1)
    sd_df <- cbind(ad2_code_v, Year, max_yr_v)
    total_year_df <- rbind(total_year_df, sd_df)
}

head(total_year_df)
head(panel_bal2)

total_year_df <- data.frame(total_year_df)

dim(total_year_df)
dim(panel_bal2)

head(panel_bal2$ADM2_PCODE)

total_year_df <- total_year_df %>%
    arrange(Year)

head(total_year_df$ad2_code_v)


panel_bal3 <- left_join(panel_bal2, total_year_df, by = c('ADM2_PCODE' = 'ad2_code_v', 'Year'), copy = TRUE)

head(panel_bal3)

dim(panel_bal3)



panel_bal3$Treat_neigh_yr <- as.integer(panel_bal3$Treat_neigh_yr)

panel_bal3$Treat_neigh_yr[is.na(panel_bal3$Treat_neigh_yr)] <- 0

#### try spatial staggered DiD

time_periods_neigh <- rep(NA, length(panel_bal3$State))

head(panel_bal3)

bal3 <- panel_bal3 %>%
    mutate(rel_TN_periods = Year - max_yr_v)


bal3 <- panel_bal3 %>%
    mutate(rel_TN_periods = ifelse(rel_TN_periods > 7, 0, rel_TN_periods))

head(bal3)

table(bal3$rel_TN_periods)

panel_bal3 <- bal3

