#### Data setup ####
base <- here::here()
setwd(paste0(base, "/data/"))
load("full.RData")
rm(dfr)
library(dplyr)
library(likert)
library(xlsx)
library(MASS)
library(brms)
library(sjPlot)
library(psych)

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
    library(grid)
    
    # Make a list from the ... arguments and plotlist
    plots <- c(list(...), plotlist)
    
    numPlots = length(plots)
    
    # If layout is NULL, then use 'cols' to determine layout
    if (is.null(layout)) {
        # Make the panel
        # ncol: Number of columns of plots
        # nrow: Number of rows needed, calculated from # of cols
        layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                         ncol = cols, nrow = ceiling(numPlots/cols))
    }
    
    if (numPlots==1) {
        print(plots[[1]])
        
    } else {
        # Set up the page
        grid.newpage()
        pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
        
        # Make each plot, in the correct location
        for (i in 1:numPlots) {
            # Get the i,j matrix positions of the regions that contain this subplot
            matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
            
            print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                            layout.pos.col = matchidx$col))
        }
    }
}

common_ops <-  list(theme(plot.title = element_text(size=8),
                          axis.text = element_text(size = 8),
                          axis.title = element_text(size = 8),
                          strip.text = element_text(size = 8),
                          legend.position="bottom",
                          #   panel.grid.major=element_blank(),
                          #   panel.grid.minor=element_blank(),
                          panel.border=element_blank(),
                          axis.line=element_line(),
                          text=element_text()))
#### Descriptive statistics ####
#### Primary outcome by treatment ####

tab1_M <- df %>%
  filter(!is.na(M_trust)) %>% 
  group_by(treatment) %>% 
  summarise(
    M_trust_cat1 = paste0(length(id[M_trust == 1]) , " (", round((length(id[M_trust == 1])/length(id))*100, 2)," %)"),
    M_trust_cat2 = paste0(length(id[M_trust == 2]) , " (", round((length(id[M_trust == 2])/length(id))*100, 2)," %)"),
    M_trust_cat3 = paste0(length(id[M_trust == 3]) , " (", round((length(id[M_trust == 3])/length(id))*100, 2)," %)"),
    M_trust_cat4 = paste0(length(id[M_trust == 4]) , " (", round((length(id[M_trust == 4])/length(id))*100, 2)," %)"),
    M_trust_cat5 = paste0(length(id[M_trust == 5]) , " (", round((length(id[M_trust == 5])/length(id))*100, 2)," %)"),
    n = length(M_trust)
  )

write.xlsx(as.data.frame(tab1_M), "Table1.xlsx", sheetName = "tab1_M")

tab1_S <- df %>%
  filter(!is.na(S_trust)) %>% 
  group_by(treatment) %>% 
  summarise(
    S_trust_cat1 = paste0(length(id[S_trust == 1]) , " (", round((length(id[S_trust == 1])/length(id))*100, 2)," %)"),
    S_trust_cat2 = paste0(length(id[S_trust == 2]) , " (", round((length(id[S_trust == 2])/length(id))*100, 2)," %)"),
    S_trust_cat3 = paste0(length(id[S_trust == 3]) , " (", round((length(id[S_trust == 3])/length(id))*100, 2)," %)"),
    S_trust_cat4 = paste0(length(id[S_trust == 4]) , " (", round((length(id[S_trust == 4])/length(id))*100, 2)," %)"),
    S_trust_cat5 = paste0(length(id[S_trust == 5]) , " (", round((length(id[S_trust == 5])/length(id))*100, 2)," %)"),
    n = length(S_trust)
  )

write.xlsx(as.data.frame(tab1_S), "Table1.xlsx", sheetName = "tab1_S", append = T)

#### Secondary outcomes by treatment ####
####Perceived source trustworthiness, Perceived source expertise, Perceived content credibility ####
# Cronbach's alpha (internal consistency)

df %>% 
  select(M_dep, M_hon, M_rel, M_sin, M_trust) %>% 
  mutate_all(funs(as.numeric)) %>% 
  alpha(na.rm = T)
df %>% 
  select(M_exp, M_expd, M_kno, M_qua, M_ski) %>% 
  mutate_all(funs(as.numeric)) %>% 
  alpha(na.rm = T)
df %>% 
  select(M_acc, M_bel, M_fac, M_tru, M_trust) %>% 
  mutate_all(funs(as.numeric)) %>% 
  alpha(na.rm = T)

M_tab2 <- df %>% 
  group_by(treatment) %>% 
  filter(!is.na(M_trust_scale)) %>% 
  summarise(
    mean_trust = round(mean(M_trust_scale), 2),
    sd_trust = round(sd(M_trust_scale), 2),
    median_trust = round(median(M_trust_scale), 2),
    mean_exp = round(mean(M_exp_scale), 2),
    sd_exp = round(sd(M_exp_scale), 2),
    median_exp = round(median(M_exp_scale), 2),
    mean_contcred = round(mean(M_contcred_scale), 2),
    sd_contcred = round(sd(M_contcred_scale), 2),
    median_contcred = round(median(M_contcred_scale), 2),
    n = length(M_trust_scale)
  )

write.xlsx(as.data.frame(M_tab2), "tab2.xlsx", sheetName = "M_tab2")

# Cronbach's alpha (internal consistency)
df %>% 
  select(S_dep, S_hon, S_rel, S_sin, S_trust) %>% 
  mutate_all(funs(as.numeric)) %>% 
  alpha(na.rm = T)
df %>% 
  select(S_exp, S_expd, S_kno, S_qua, S_ski) %>% 
  mutate_all(funs(as.numeric)) %>% 
  alpha(na.rm = T)
df %>% 
  select(S_acc, S_bel, S_fac, S_tru, S_trust) %>% 
  mutate_all(funs(as.numeric)) %>% 
  alpha(na.rm = T)

S_tab2 <- df %>% 
  group_by(treatment) %>% 
  filter(!is.na(S_trust_scale)) %>% 
  summarise(
    mean_trust = round(mean(S_trust_scale), 2),
    sd_trust = round(sd(S_trust_scale), 2),
    median_trust = round(median(S_trust_scale), 2),
    mean_exp = round(mean(S_exp_scale), 2),
    sd_exp = round(sd(S_exp_scale), 2),
    median_exp = round(median(S_exp_scale), 2),
    mean_contcred = round(mean(S_contcred_scale), 2),
    sd_contcred = round(sd(S_contcred_scale), 2),
    median_contcred = round(median(S_contcred_scale), 2),
    n = length(S_trust_scale)
  )

write.xlsx(as.data.frame(S_tab2), "tab2.xlsx", sheetName = "S_tab2", append = T)

#### Visualisation of primary outcome variables ####
## Violin plots

x1 <- ggplot(df, aes(x = treatment, y = as.numeric(M_trust), fill = treatment)) +
  geom_violin(alpha = 0.7)  +
  geom_boxplot(width = 0.1) +
  geom_jitter(shape = 1, width = 0.1, height = 0.1) +
  scale_y_continuous(breaks = c(seq(1,5,1)), limits = c(0.5,5.5)) +
  ylab("Trust in Media") +
  xlab("Treatment") +
  theme_bw() +
  common_ops +
  scale_fill_grey() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  guides(fill = F)

x2 <- ggplot(df, aes(x = treatment, y = as.numeric(S_trust), fill = treatment)) +
  geom_violin(alpha = 0.7)  +
  geom_boxplot(width = 0.1) +
  geom_jitter(shape = 1, width = 0.1, height = 0.1) +
  scale_y_continuous(breaks = c(seq(1,5,1)), limits = c(0.5,5.5)) +
  ylab("Trust in Science") +
  xlab("Treatment") +
  theme_bw() +
  common_ops +
  scale_fill_grey() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  guides(fill = F)


multiplot(x1, x2, cols = 2)

#### Inferential statistics ####
## Non-parametric MWU test of primary outcome by treatment ##
# H1
chisq.test(df$M_trust[!is.na(df$M_trust)], df$treatment[!is.na(df$M_trust)]) # treats outcome as nominal and tests for equality of proportions
wilcox.test(as.numeric(df$M_trust[df$treatment == "No Quote"]), as.numeric(df$M_trust[df$treatment == "Quote"]), alternative = "greater", paired = F, conf.int = T, conf.level = 0.95)

# H2
chisq.test(df$S_trust[!is.na(df$S_trust)],  df$treatment[!is.na(df$S_trust)])
wilcox.test(as.numeric(df$S_trust[df$treatment == "No Quote"]), as.numeric(df$S_trust[df$treatment == "Quote"]), alternative = "greater", paired = F, conf.int = T, conf.level = 0.95)


## Ordered logistic regression ##
df$StudyArea <- relevel(df$StudyArea, "Other")
# With control variables #
# H1
df_r_con <- df %>% 
  dplyr::select(M_trust, treatment, Age, Gender, StudyArea, contribution)
M2_M_ol <- polr(M_trust ~ ., data = df_r_con, method = "logistic", Hess = T) # similar to ologit in Stata
summary(M2_M_ol)

# one-sided tests (post-estimation) https://stackoverflow.com/questions/8089797/inference-about-slope-coefficient-in-r
coef<- summary(M2_M_ol)$coefficients[1,][[1]] # extracts treatment coefficient
se <- summary(M2_M_ol)$coefficients[1,][[2]] # extracts standard error, and t-value
d_f <- summary(M2_M_ol)$edf # extracts effective degrees of freedom used by model
m <- 0 # value to which coefficients should be compared

paste0("H0: ", round(coef,3), " >= ", m, ", p-value: ", round(pnorm((coef - m)/se, lower.tail = T), 3), " (H0: ", round(coef,3), " = 0, p-value: ", round(pnorm((coef - m)/se, lower.tail = T)*2, 3), ")", " [assumes normal distribution]") # one-sided, H0: coef >= 0, Ha: coef < 0 (Normal-distribution) (Stata olit-output)

# H2
df_r_con <- df %>% 
  dplyr::select(S_trust, treatment, Age, Gender, StudyArea, contribution)
M2_S_ol <- polr(S_trust ~ ., data = df_r_con, method = "logistic", Hess = T) # similar to ologit in Stata
summary(M2_S_ol)

# one-sided tests (post-estimation) https://stackoverflow.com/questions/8089797/inference-about-slope-coefficient-in-r
coef<- summary(M2_S_ol)$coefficients[1,][[1]] # extracts treatment coefficient
se <- summary(M2_S_ol)$coefficients[1,][[2]] # extracts standard error, and t-value
d_f <- summary(M2_S_ol)$edf # extracts effective degrees of freedom used by model
m <- 0 # value to which coefficients should be compared

paste0("H0: ", round(coef,3), " >= ", m, ", p-value: ", round(pnorm((coef - m)/se, lower.tail = T), 3), " (H0: ", round(coef,3), " = 0, p-value: ", round(pnorm((coef - m)/se, lower.tail = T)*2, 3), ")", " [assumes normal distribution]") # one-sided, H0: coef >= 0, Ha: coef < 0 (Normal-distribution) (Stata olit-output)


#### Bayesian ordered logistic regression ####
theme_set(theme_minimal())
get_prior(M_trust ~ treatment + Age + Gender + StudyArea + contribution, data = df, family = cumulative(link = "logit"))
get_prior(S_trust ~ treatment + Age + Gender + StudyArea + contribution, data = df, family = cumulative(link = "logit"))

## The following treatment priors are flat (Gelman et al. 2008) ##
levels(df$treatment) <- c("No Quote", "Quote")
priors2 <- c(
  set_prior("uniform(-10,10)", class = "b", lb = -10, ub = 10), 
  set_prior("cauchy(0,10)", class = "Intercept") 
)

# H1 (prior2)
B1_M_bf <- brm(M_trust ~ treatment + Age + Gender + StudyArea + contribution, data = df, family = cumulative(link = "logit"),
               prior = priors2, sample_prior = T)
summary(B1_M_bf)
WAIC(B1_M_bf)
hypothesis(B1_M_bf, "treatmentQuote < 0") 
hypothesis(B1_M_bf, "treatmentQuote = 0") 

# H2
B1_S_bf <- brm(S_trust ~ treatment + Age + Gender + StudyArea + contribution, data = df, family = cumulative(link = "logit"),
            prior = priors2, sample_prior = T, control = list(adapt_delta = 0.95))
summary(B1_S_bf)
WAIC(B1_S_bf) 
hypothesis(B1_S_bf, "treatmentQuote < 0")
hypothesis(B1_S_bf, "treatmentQuote = 0")


## The following treatment priors are more informative: increased belief that there is no treatment effect ##
priors2 <- c(
  set_prior("cauchy(0,1/sqrt(2))", class = "b", coef = "treatmentQuote"),
  set_prior("cauchy(0,10)", class = "Intercept") 
)

# H1
B1_M_bi <- brm(M_trust ~ treatment + Age + Gender + StudyArea + contribution, data = df, family = cumulative(link = "logit"),
            prior = priors2, sample_prior = T, control = list(max_treedepth = 15))
summary(B1_M_bi)
WAIC(B1_M_bi) 
hypothesis(B1_M_bi, "treatmentQuote < 0")
hypothesis(B1_M_bi, "treatmentQuote = 0")

# H2
B1_S_bi <- brm(S_trust ~ treatment + Age + Gender + StudyArea + contribution, data = df, family = cumulative(link = "logit"),
            prior = priors2, sample_prior = T, control = list(max_treedepth = 15))
summary(B1_S_bi)
WAIC(B1_S_bi) 
hypothesis(B1_S_bi, "treatmentQuote < 0")
hypothesis(B1_S_bi, "treatmentQuote = 0")

#### Secondary analyses ####
#### Visualisation of secondary outcomes ####
M_violin_ts <- ggplot(df, aes(x = treatment, y = as.numeric(M_trust_scale), fill = treatment)) +
  geom_violin(alpha = 0.7)  +
  geom_boxplot(width = 0.1) +
  geom_jitter(shape = 1) +
  theme_bw() +
  ylab("Trust (scale) in\nmedia source") +
  xlab("Treatment") +
  common_ops +
  scale_fill_grey() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  guides(fill = F)

S_violin_ts <- ggplot(df, aes(x = treatment, y = as.numeric(S_trust_scale), fill = treatment)) +
  geom_violin(alpha = 0.7) +
  geom_boxplot(width = 0.1) +
  geom_jitter(shape = 1) +
  theme_bw() +
  ylab("Trust (scale) in\nscience source") +
  xlab("Treatment") +
  common_ops +
  scale_fill_grey() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  guides(fill = F)

M_violin_es <- ggplot(df, aes(x = treatment, y = as.numeric(M_exp_scale), fill = treatment)) +
  geom_violin(alpha = 0.7)  +
  geom_boxplot(width = 0.1) +
  geom_jitter(shape = 1) +
  theme_bw() +
  common_ops +  
  ylab("Expertise of\nmedia source") +
  xlab("Treatment") +
  scale_fill_grey() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  guides(fill = F)

S_violin_es <- ggplot(df, aes(x = treatment, y = as.numeric(S_exp_scale), fill = treatment)) +
  geom_violin(alpha = 0.7) +
  geom_boxplot(width = 0.1) +
  geom_jitter(shape = 1) +
  theme_bw() +
  ylab("Expertise of\nscience source") +
  xlab("Treatment") +
    common_ops +
  scale_fill_grey() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  guides(fill = F)

M_violin_cs <- ggplot(df, aes(x = treatment, y = as.numeric(M_contcred_scale), fill = treatment)) +
  geom_violin(alpha = 0.7)  +
  geom_boxplot(width = 0.1) +
  geom_jitter(shape = 1) +
  theme_bw() +
  common_ops +
  ylab("Content credibility\nof media article") +
  xlab("Treatment") +
  scale_fill_grey() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  guides(fill = F)

S_violin_cs <- ggplot(df, aes(x = treatment, y = as.numeric(S_contcred_scale), fill = treatment)) +
  geom_violin(alpha = 0.7) +
  geom_boxplot(width = 0.1) +
  geom_jitter(shape = 1) +
  ylab("Content credibility\nof science article") +
  xlab("Treatment") +
  theme_bw() +
  common_ops +
  scale_fill_grey() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  guides(fill = F)

multiplot(M_violin_ts, M_violin_es, M_violin_cs, S_violin_ts, S_violin_es, S_violin_cs, cols = 2)

# Inferential statistics on secondary outcomes (OLS Regression)
df_r_con <- df %>% 
  dplyr::select(M_trust_scale, treatment, Age, Gender, StudyArea, contribution)
M_trust_scale_lm_cv <- lm(M_trust_scale ~ treatment + Age + Gender + relevel(StudyArea, "Other") + contribution, data = df_r_con)
df_r_con <- df %>% 
  dplyr::select(M_exp_scale, treatment, Age, Gender, StudyArea, contribution)
M_exp_scale_lm_cv <- lm(M_exp_scale ~ treatment + Age + Gender + relevel(StudyArea, "Other") + contribution, data = df_r_con)
df_r_con <- df %>% 
  dplyr::select(M_contcred_scale, treatment, Age, Gender, StudyArea, contribution)
M_contcred_scale_lm_cv <- lm(M_contcred_scale ~ treatment + Age + Gender + relevel(StudyArea, "Other") + contribution, data = df_r_con)

sjt.lm(M_trust_scale_lm_cv, M_exp_scale_lm_cv, M_contcred_scale_lm_cv, robust = T, show.ci = T, show.fstat = T, show.aic = T)


df_r_con <- df %>% 
  dplyr::select(S_trust_scale, treatment, Age, Gender, StudyArea, contribution)
S_trust_scale_lm_cv <- lm(S_trust_scale ~ treatment + Age + Gender + relevel(StudyArea, "Other") + contribution, data = df_r_con)
df_r_con <- df %>% 
  dplyr::select(S_exp_scale, treatment, Age, Gender, StudyArea, contribution)
S_exp_scale_lm_cv <- lm(S_exp_scale ~ treatment + Age + Gender + relevel(StudyArea, "Other") + contribution, data = df_r_con)
df_r_con <- df %>% 
  dplyr::select(S_contcred_scale, treatment, Age, Gender, StudyArea, contribution)
S_contcred_scale_lm_cv <- lm(S_contcred_scale ~ treatment + Age + Gender + relevel(StudyArea, "Other") + contribution, data = df_r_con)

sjt.lm(S_trust_scale_lm_cv, S_exp_scale_lm_cv, S_contcred_scale_lm_cv, robust = T, show.ci = T, show.fstat = T, show.aic = T)

#### Order effects ####
wilcox.test(as.numeric(df$M_trust[df$ZO_order == 0]), as.numeric(df$M_trust[df$ZO_order == 1]))
wilcox.test(as.numeric(df$S_trust[df$ZO_order == 0]), as.numeric(df$S_trust[df$ZO_order == 1]))
wilcox.test(as.numeric(df$M_trust[df$ZO_order == 0 & df$treatment == "No Quote"]), as.numeric(df$M_trust[df$ZO_order == 1 & df$treatment == "No Quote"]))
wilcox.test(as.numeric(df$S_trust[df$ZO_order == 0 & df$treatment == "No Quote"]), as.numeric(df$S_trust[df$ZO_order == 1 & df$treatment == "No Quote"]))
wilcox.test(as.numeric(df$M_trust[df$ZO_order == 0 & df$treatment == "Quote"]), as.numeric(df$M_trust[df$ZO_order == 1 & df$treatment == "Quote"]))
wilcox.test(as.numeric(df$S_trust[df$ZO_order == 0 & df$treatment == "Quote"]), as.numeric(df$S_trust[df$ZO_order == 1 & df$treatment == "Quote"]))

#### Additional analyses (not in pre analysis protocol) ####
## Difference in trust perception between science and media (WS) ##
wilcox.test(as.numeric(df$M_trust[!is.na(df$M_trust) & !is.na(df$S_trust)]),  as.numeric(df$S_trust[!is.na(df$M_trust) & !is.na(df$S_trust)], paired = T))

#### OrderxTreatment interaction ####
summary(polr(S_trust~ZO_order*treatment + Age + Gender + StudyArea + contribution, df,method = "logistic", Hess = T))
summary(polr(M_trust~ZO_order*treatment + Age + Gender + StudyArea + contribution, df,method = "logistic", Hess = T))

#### Calculate Cohen's d ####
cohens_d <- function(x, y) {
  lx <- length(x)- 1
  ly <- length(y)- 1
  md  <- abs(mean(x) - mean(y))        ## mean difference (numerator)
  csd <- lx * var(x) + ly * var(y)
  csd <- csd/(lx + ly)
  csd <- sqrt(csd)                     ## common sd computation
  
  cd  <- md/csd                        ## cohen's d
  return(cd)
}

cohens_d(as.numeric(df$M_trust[!is.na(df$M_trust) & df$treatment == "No Quote"]), as.numeric(df$M_trust[!is.na(df$M_trust) & df$treatment == "Quote"]))
cohens_d(as.numeric(df$S_trust[!is.na(df$S_trust) & df$treatment == "No Quote"]), as.numeric(df$S_trust[!is.na(df$S_trust) & df$treatment == "Quote"]))