base <- here::here()
setwd(paste0(base, "/raw/"))

load("raw.RData")
library(dplyr)
library(scales)
#### Remove subjets that did read either article in less than mean - 2*sd time ####
## Treatment text (120 sec)
df$TimeIchHabeDenTextGelesenUndVerstandenQuotenoIdOK <- ifelse(df$treatment == 0, NA, df$TimeIchHabeDenTextGelesenUndVerstandenQuotenoIdOK)
df$TimeWeiterQuotenoIdOK <- ifelse(df$treatment == 0, NA, df$TimeWeiterQuotenoIdOK) # since this variable is not important for subjects in the control, this is set to NA

df$tf_t_c <- ifelse( # mark subjects as "too fast" (conservative, when clicking "Have read and understood")
  df$treatment == 1 & (120 - df$TimeIchHabeDenTextGelesenUndVerstandenQuotenoIdOK < (mean(120 - df$TimeIchHabeDenTextGelesenUndVerstandenQuotenoIdOK, na.rm = T) - 2*sd(120 - df$TimeIchHabeDenTextGelesenUndVerstandenQuotenoIdOK, na.rm = T))), 1,0
)

df$tf_t_l <- ifelse( # mark subjects as "too fast" (liberal, when proceeding to next screen)
  df$treatment == 1 & (120 - df$TimeWeiterQuotenoIdOK < (mean(120 - df$TimeWeiterQuotenoIdOK, na.rm = T) - 2*sd(120 - df$TimeWeiterQuotenoIdOK, na.rm = T))), 1,0
)

## ZO text (300 sec)
df$TimeIchHabeDenTextAufmerksamGelesenZOOK <- ifelse(df$ZO_order == 1, df$TimeIchHabeDenTextAufmerksamGelesenZOOK, NA)
df$TimeWeiterZOOK <- ifelse(df$ZO_order == 1, df$TimeWeiterZOOK, NA)
df$TimeIchHabeDenTextAufmerksamGelesenZO2OK <- ifelse(df$ZO_order == 0, df$TimeIchHabeDenTextAufmerksamGelesenZO2OK, NA)
df$TimeWeiterZO2OK <- ifelse(df$ZO_order == 0, df$TimeWeiterZO2OK, NA)


df$tf_ZO_c <- ifelse( # mark subjects as "too fast" (conservative, when clicking "Have read and understood")
  df$treatment == 1 & (300 - df$TimeIchHabeDenTextAufmerksamGelesenZOOK < (mean(300 - df$TimeIchHabeDenTextAufmerksamGelesenZOOK, na.rm = T) - 2*sd(300 - df$TimeIchHabeDenTextAufmerksamGelesenZOOK, na.rm = T))), 1,0
)

df$tf_ZO_l <- ifelse( # mark subjects as "too fast" (liberal, when proceeding to next screen)
  df$treatment == 1 & (300 - df$TimeWeiterZOOK < (mean(300 - df$TimeWeiterZOOK, na.rm = T) - 2*sd(300 - df$TimeWeiterZOOK, na.rm = T))), 1,0
)

df$tf_ZO2_c <- ifelse( # mark subjects as "too fast" (conservative, when clicking "Have read and understood")
  df$treatment == 1 & (300 - df$TimeIchHabeDenTextAufmerksamGelesenZO2OK < (mean(300 - df$TimeIchHabeDenTextAufmerksamGelesenZO2OK, na.rm = T) - 2*sd(300 - df$TimeIchHabeDenTextAufmerksamGelesenZO2OK, na.rm = T))), 1,0
)

df$tf_ZO2_l <- ifelse( # mark subjects as "too fast" (liberal, when proceeding to next screen)
  df$treatment == 1 & (300 - df$TimeWeiterZO2OK < (mean(300 - df$TimeWeiterZO2OK, na.rm = T) - 2*sd(300 - df$TimeWeiterZO2OK, na.rm = T))), 1,0
)



## Source text (300 sec)
df$TimeIchHabeDenTextAufmerksamGelesenZOsourceOK <- ifelse(df$ZO_order == 0, df$TimeIchHabeDenTextAufmerksamGelesenZOsourceOK, NA)
df$TimeWeiterZOsourceOK <- ifelse(df$ZO_order == 0, df$TimeWeiterZOsourceOK, NA)
df$TimeIchHabeDenTextAufmerksamGelesenZOsource2OK <- ifelse(df$ZO_order == 1, df$TimeIchHabeDenTextAufmerksamGelesenZOsource2OK, NA)
df$TimeWeiterZOsource2OK <- ifelse(df$ZO_order == 1, df$TimeWeiterZOsource2OK, NA)

df$tf_ZOs_c <- ifelse( # mark subjects as "too fast" (conservative, when clicking "Have read and understood")
  df$treatment == 1 & (300 - df$TimeIchHabeDenTextAufmerksamGelesenZOsourceOK < (mean(300 - df$TimeIchHabeDenTextAufmerksamGelesenZOsourceOK, na.rm = T) - 2*sd(300 - df$TimeIchHabeDenTextAufmerksamGelesenZOsourceOK, na.rm = T))), 1,0
)

df$tf_ZOs_l <- ifelse( # mark subjects as "too fast" (liberal, when proceeding to next screen)
  df$treatment == 1 & (300 - df$TimeWeiterZOsourceOK < (mean(300 - df$TimeWeiterZOsourceOK, na.rm = T) - 2*sd(300 - df$TimeWeiterZOsourceOK, na.rm = T))), 1,0
)

df$tf_ZO2s_c <- ifelse( # mark subjects as "too fast" (conservative, when clicking "Have read and understood")
  df$treatment == 1 & (300 - df$TimeIchHabeDenTextAufmerksamGelesenZOsource2OK < (mean(300 - df$TimeIchHabeDenTextAufmerksamGelesenZOsource2OK, na.rm = T) - 2*sd(300 - df$TimeIchHabeDenTextAufmerksamGelesenZOsource2OK, na.rm = T))), 1,0
)

df$tf_ZO2s_l <- ifelse( # mark subjects as "too fast" (liberal, when proceeding to next screen)
  df$treatment == 1 & (300 - df$TimeWeiterZOsource2OK < (mean(300 - df$TimeWeiterZOsource2OK, na.rm = T) - 2*sd(300 - df$TimeWeiterZOsource2OK, na.rm = T))), 1,0
)

#### Exclusion of observation if too fast ####
dfr <- df %>% # puts those that would have to be excluded from the data frame in the dfr data frame
  filter(tf_ZO2s_l == 1 | df$tf_ZOs_l == 1 | df$tf_ZO2_l == 1 | df$tf_ZO_l == 1)

df <- df[!(df$id == dfr$id[1]),] # excludes those ids that are in dfr from df (still need to verify if it works for multiple TRUEs)
df <- df[!(df$id == dfr$id[2]),] # excludes those ids that are in dfr from df (still need to verify if it works for multiple TRUEs)

#### Code treatment as factor ####
df$treatment <- factor(df$treatment, levels = c(0,1), labels = c("no quote", "quote"))
#df$treatmentR <- rescale(df$treatment, binary.inputs = "-0.5,0.5")
#df$treatmentR <- as.factor(df$treatmentR)


#### Create M_trust and S_trust (dependent variables) as ordered factors
df$M_trust <- ifelse(
  df$session > 1 & df$ZO_order == 1, df$so_tru, ifelse(
    df$session > 1 & df$ZO_order == 0, df$so_tru3, ifelse(
      df$session == 1 & df$ZO_order == 0, df$so_tru3, NA
      )
  )
)
df$S_trust <- ifelse(
  df$session > 1 & df$ZO_order == 1, df$so_tru2, ifelse(
    df$session > 1 & df$ZO_order == 0, df$so_tru1, ifelse(
      df$session == 1 & df$ZO_order == 1, df$so_tru2, NA
    )
  )
)

df$M_trust <- factor(df$M_trust, levels = c(1:5), ordered = T)
df$S_trust <- factor(df$S_trust, levels = c(1:5), ordered = T)

#### Create M_dep and S_dep (dependent variables) as ordered factors
df$M_dep <- ifelse(
  df$session > 1 & df$ZO_order == 1, df$so_dep, ifelse(
    df$session > 1 & df$ZO_order == 0, df$so_dep3, ifelse(
      df$session == 1 & df$ZO_order == 0, df$so_dep3, NA
    )
  )
)
df$S_dep <- ifelse(
  df$session > 1 & df$ZO_order == 1, df$so_dep2, ifelse(
    df$session > 1 & df$ZO_order == 0, df$so_dep1, ifelse(
      df$session == 1 & df$ZO_order == 1, df$so_dep2, NA
    )
  )
)

df$M_dep <- factor(df$M_dep, levels = c(1:5), ordered = T)
df$S_dep <- factor(df$S_dep, levels = c(1:5), ordered = T)

#### Create M_hon and S_hon (dependent variables) as ordered factors
df$M_hon <- ifelse(
  df$session > 1 & df$ZO_order == 1, df$so_hon, ifelse(
    df$session > 1 & df$ZO_order == 0, df$so_hon3, ifelse(
      df$session == 1 & df$ZO_order == 0, df$so_hon3, NA
    )
  )
)
df$S_hon <- ifelse(
  df$session > 1 & df$ZO_order == 1, df$so_hon2, ifelse(
    df$session > 1 & df$ZO_order == 0, df$so_hon1, ifelse(
      df$session == 1 & df$ZO_order == 1, df$so_hon2, NA
    )
  )
)

df$M_hon <- factor(df$M_hon, levels = c(1:5), ordered = T)
df$S_hon <- factor(df$S_hon, levels = c(1:5), ordered = T)

#### Create M_rel and S_rel (dependent variables) as ordered factors
df$M_rel <- ifelse(
  df$session > 1 & df$ZO_order == 1, df$so_rel, ifelse(
    df$session > 1 & df$ZO_order == 0, df$so_rel3, ifelse(
      df$session == 1 & df$ZO_order == 0, df$so_rel3, NA
    )
  )
)
df$S_rel <- ifelse(
  df$session > 1 & df$ZO_order == 1, df$so_rel2, ifelse(
    df$session > 1 & df$ZO_order == 0, df$so_rel1, ifelse(
      df$session == 1 & df$ZO_order == 1, df$so_rel2, NA
    )
  )
)

df$M_rel <- factor(df$M_rel, levels = c(1:5), ordered = T)
df$S_rel <- factor(df$S_rel, levels = c(1:5), ordered = T)

#### Create M_sin and S_sin (dependent variables) as ordered factors
df$M_sin <- ifelse(
  df$session > 1 & df$ZO_order == 1, df$so_sin, ifelse(
    df$session > 1 & df$ZO_order == 0, df$so_sin3, ifelse(
      df$session == 1 & df$ZO_order == 0, df$so_sin3, NA
    )
  )
)
df$S_sin <- ifelse(
  df$session > 1 & df$ZO_order == 1, df$so_sin2, ifelse(
    df$session > 1 & df$ZO_order == 0, df$so_sin1, ifelse(
      df$session == 1 & df$ZO_order == 1, df$so_sin2, NA
    )
  )
)

df$M_sin <- factor(df$M_sin, levels = c(1:5), ordered = T)
df$S_sin <- factor(df$S_sin, levels = c(1:5), ordered = T)

#### Create M_exp and S_exp (dependent variables) as ordered factors
df$M_exp <- ifelse(
  df$session > 1 & df$ZO_order == 1, df$so_exp, ifelse(
    df$session > 1 & df$ZO_order == 0, df$so_exp3, ifelse(
      df$session == 1 & df$ZO_order == 0, df$so_exp3, NA
    )
  )
)
df$S_exp <- ifelse(
  df$session > 1 & df$ZO_order == 1, df$so_exp2, ifelse(
    df$session > 1 & df$ZO_order == 0, df$so_exp1, ifelse(
      df$session == 1 & df$ZO_order == 1, df$so_exp2, NA
    )
  )
)

df$M_exp <- factor(df$M_exp, levels = c(1:5), ordered = T)
df$S_exp <- factor(df$S_exp, levels = c(1:5), ordered = T)

#### Create M_expd and S_expd (dependent variables) as ordered factors
df$M_expd <- ifelse(
  df$session > 1 & df$ZO_order == 1, df$so_expd, ifelse(
    df$session > 1 & df$ZO_order == 0, df$so_expd3, ifelse(
      df$session == 1 & df$ZO_order == 0, df$so_expd3, NA
    )
  )
)
df$S_expd <- ifelse(
  df$session > 1 & df$ZO_order == 1, df$so_expd2, ifelse(
    df$session > 1 & df$ZO_order == 0, df$so_expd1, ifelse(
      df$session == 1 & df$ZO_order == 1, df$so_expd2, NA
    )
  )
)

df$M_expd <- factor(df$M_expd, levels = c(1:5), ordered = T)
df$S_expd <- factor(df$S_expd, levels = c(1:5), ordered = T)

#### Create M_kno and S_kno (dependent variables) as ordered factors
df$M_kno <- ifelse(
  df$session > 1 & df$ZO_order == 1, df$so_kno, ifelse(
    df$session > 1 & df$ZO_order == 0, df$so_kno3, ifelse(
      df$session == 1 & df$ZO_order == 0, df$so_kno3, NA
    )
  )
)
df$S_kno <- ifelse(
  df$session > 1 & df$ZO_order == 1, df$so_kno2, ifelse(
    df$session > 1 & df$ZO_order == 0, df$so_kno1, ifelse(
      df$session == 1 & df$ZO_order == 1, df$so_kno2, NA
    )
  )
)

df$M_kno <- factor(df$M_kno, levels = c(1:5), ordered = T)
df$S_kno <- factor(df$S_kno, levels = c(1:5), ordered = T)

#### Create M_qua and S_qua (dependent variables) as ordered factors
df$M_qua <- ifelse(
  df$session > 1 & df$ZO_order == 1, df$so_qua, ifelse(
    df$session > 1 & df$ZO_order == 0, df$so_qua3, ifelse(
      df$session == 1 & df$ZO_order == 0, df$so_qua3, NA
    )
  )
)
df$S_qua <- ifelse(
  df$session > 1 & df$ZO_order == 1, df$so_qua2, ifelse(
    df$session > 1 & df$ZO_order == 0, df$so_qua1, ifelse(
      df$session == 1 & df$ZO_order == 1, df$so_qua2, NA
    )
  )
)

df$M_qua <- factor(df$M_qua, levels = c(1:5), ordered = T)
df$S_qua <- factor(df$S_qua, levels = c(1:5), ordered = T)

#### Create M_ski and S_ski (dependent variables) as ordered factors
df$M_ski <- ifelse(
  df$session > 1 & df$ZO_order == 1, df$so_ski, ifelse(
    df$session > 1 & df$ZO_order == 0, df$so_ski3, ifelse(
      df$session == 1 & df$ZO_order == 0, df$so_ski3, NA
    )
  )
)
df$S_ski <- ifelse(
  df$session > 1 & df$ZO_order == 1, df$so_ski2, ifelse(
    df$session > 1 & df$ZO_order == 0, df$so_ski1, ifelse(
      df$session == 1 & df$ZO_order == 1, df$so_ski2, NA
    )
  )
)

df$M_ski <- factor(df$M_ski, levels = c(1:5), ordered = T)
df$S_ski <- factor(df$S_ski, levels = c(1:5), ordered = T)

#### Create M_acc and S_acc (dependent variables) as ordered factors
df$M_acc <- ifelse(
  df$session > 1 & df$ZO_order == 1, df$co_acc, ifelse(
    df$session > 1 & df$ZO_order == 0, df$co_acc3, ifelse(
      df$session == 1 & df$ZO_order == 0, df$co_acc3, NA
    )
  )
)
df$S_acc <- ifelse(
  df$session > 1 & df$ZO_order == 1, df$co_acc2, ifelse(
    df$session > 1 & df$ZO_order == 0, df$co_acc1, ifelse(
      df$session == 1 & df$ZO_order == 1, df$co_acc2, NA
    )
  )
)

df$M_acc <- factor(df$M_acc, levels = c(1:5), ordered = T)
df$S_acc <- factor(df$S_acc, levels = c(1:5), ordered = T)

#### Create M_bel and S_bel (dependent variables) as ordered factors
df$M_bel <- ifelse(
  df$session > 1 & df$ZO_order == 1, df$co_bel, ifelse(
    df$session > 1 & df$ZO_order == 0, df$co_bel3, ifelse(
      df$session == 1 & df$ZO_order == 0, df$co_bel3, NA
    )
  )
)
df$S_bel <- ifelse(
  df$session > 1 & df$ZO_order == 1, df$co_bel2, ifelse(
    df$session > 1 & df$ZO_order == 0, df$co_bel1, ifelse(
      df$session == 1 & df$ZO_order == 1, df$co_bel2, NA
    )
  )
)

df$M_bel <- factor(df$M_bel, levels = c(1:5), ordered = T)
df$S_bel <- factor(df$S_bel, levels = c(1:5), ordered = T)

#### Create M_fac and S_fac (dependent variables) as ordered factors
df$M_fac <- ifelse(
  df$session > 1 & df$ZO_order == 1, df$co_fac, ifelse(
    df$session > 1 & df$ZO_order == 0, df$co_fac3, ifelse(
      df$session == 1 & df$ZO_order == 0, df$co_fac3, NA
    )
  )
)
df$S_fac <- ifelse(
  df$session > 1 & df$ZO_order == 1, df$co_fac2, ifelse(
    df$session > 1 & df$ZO_order == 0, df$co_fac1, ifelse(
      df$session == 1 & df$ZO_order == 1, df$co_fac2, NA
    )
  )
)

df$M_fac <- factor(df$M_fac, levels = c(1:5), ordered = T)
df$S_fac <- factor(df$S_fac, levels = c(1:5), ordered = T)

#### Create M_tru and S_tru (dependent variables) as ordered factors
df$M_tru <- ifelse(
  df$session > 1 & df$ZO_order == 1, df$co_tru, ifelse(
    df$session > 1 & df$ZO_order == 0, df$co_tru3, ifelse(
      df$session == 1 & df$ZO_order == 0, df$co_tru3, NA
    )
  )
)
df$S_tru <- ifelse(
  df$session > 1 & df$ZO_order == 1, df$co_tru2, ifelse(
    df$session > 1 & df$ZO_order == 0, df$co_tru1, ifelse(
      df$session == 1 & df$ZO_order == 1, df$co_tru2, NA
    )
  )
)

df$M_tru <- factor(df$M_tru, levels = c(1:5), ordered = T)
df$S_tru <- factor(df$S_tru, levels = c(1:5), ordered = T)


####Perceived source trustworthiness scale ####
df$M_trust_scale <- as.numeric(df$M_dep) + as.numeric(df$M_hon) + as.numeric(df$M_rel) + as.numeric(df$M_sin) + as.numeric(df$M_trust)
df$S_trust_scale <- as.numeric(df$S_dep) + as.numeric(df$S_hon) + as.numeric(df$S_rel) + as.numeric(df$S_sin) + as.numeric(df$S_trust)


#### Perceived source expertise scale ####
df$M_exp_scale <- as.numeric(df$M_exp) + as.numeric(df$M_expd)  + as.numeric(df$M_kno) + as.numeric(df$M_qua) + as.numeric(df$M_ski)
df$S_exp_scale <- as.numeric(df$S_exp) + as.numeric(df$S_expd)  + as.numeric(df$S_kno) + as.numeric(df$S_qua) + as.numeric(df$S_ski)

#### Perceived content credibility ####
df$M_contcred_scale <- as.numeric(df$M_acc) + as.numeric(df$M_bel) + as.numeric(df$M_fac) + as.numeric(df$M_tru)
df$S_contcred_scale <- as.numeric(df$S_acc) + as.numeric(df$S_bel) + as.numeric(df$S_fac) + as.numeric(df$S_tru)


## Gender as factor ##
df$Gender <- factor(df$Gender, levels = c(0,1), labels = c("Female", "Male"))

## StudyArea as factor ##
df$StudyArea <- as.factor(ifelse(df$StudyArea == 1, "Economics", 
                                  ifelse(df$StudyArea == 2, "Law", 
                                         ifelse(df$StudyArea == 3, "Social Science",
                                                ifelse(df$StudyArea == 4, "Management",
                                                       ifelse(df$StudyArea == 5, "Philosophy",
                                                              ifelse(df$StudyArea == 6, "History",
                                                                     "Other")))))))


# Correcting two contribution amounts that were reported by participants to be a mistake
# In both cases, they wanted to contribute 0 instead of 8,. Clicked default by mistake. I also have to correct usedDefault
df$contribution[df$id == "61"] <- 0
df$contribution[df$id == "58"] <- 0

# Correcting two contribution amounts that were reported by participants to be a mistake
# clicked default by mistake
df$contribution[df$id == "95"] <- 3
df$contribution[df$id == "96"] <- 2

# Correcting two contribution amounts that were reported by participants to be a mistake
# clicked default by mistake
df$contribution[df$id == "126"] <- 2
df$contribution[df$id == "135"] <- 2

df$quote_perc <- ifelse(df$quote_perc == 0, NA, df$quote_perc)
levels(df$treatment) <- c("No Quote", "Quote")
#### Safe ####

write.xlsx(as.data.frame(df), paste0(base, "/data/" , "full.xlsx"))
save.image(paste0(base, "/data/" , "full.RData"))
library(foreign)
write.dta(as.data.frame(df), paste0(base, "/data/" , "full.dta"))
