# /////////////////////////////////////////////////////////////////////////////
# =============================================================================
# SETUP
# =============================================================================
# /////////////////////////////////////////////////////////////////////////////

library(data.table)
library(mgcv)
library(ggplot2)
library(patchwork)

xdir = "~/Documents/GitHub/Epistorm-Mix/tutorials/data-analysis/data/"

# function for Age Breaks
# age.breaks = list("_by5_80" = c(seq(0,80, by = 5), 120))
set_age_breaks <- function(age_vec) {
  cut(age_vec, breaks = c(seq(0,80, by = 5), 120), include.lowest = T)
}

# /////////////////////////////////////////////////////////////////////////////
# =============================================================================
# RESPONDENTS DATA
# =============================================================================
# /////////////////////////////////////////////////////////////////////////////

# this is the participant datasets
resp_imp = read.csv(file.path(xdir, "imputed_resp.csv"))
setDT(resp_imp)
head(resp_imp)

# add age fct
resp_imp[, resp_age_by5_80 := set_age_breaks(resp_age)]

# weighted number of respondents by age
age_matrix_n = resp_imp[,
  .(n = sum(weight, na.rm = T)),
  by = resp_age
]
setorderv(age_matrix_n, cols = "resp_age")
age_matrix_n

## weighted number of respondents by age group
age_matrix_n_fct = resp_imp[,
  .(n = sum(weight, na.rm = T)),
  by = resp_age_by5_80
]
setorderv(age_matrix_n_fct, cols = "resp_age_by5_80")
age_matrix_n_fct

# /////////////////////////////////////////////////////////////////////////////
# =============================================================================
# CONTACT DATA
# =============================================================================
# /////////////////////////////////////////////////////////////////////////////

# this is the contact data
# -- CM UPDATES --
# there are some NAs here so I'm going to remove
# in either setting or by Age
# how are we supposed to to know what the contact
# matrix is with these included
# reduces the data from 15,000 to 12,000 -- not terrible
# ----------------
cont_imp = read.csv(file.path(xdir, "imputed_cont.csv"))
setDT(cont_imp)
head(cont_imp)

# add age fct
cont_imp[, cont_age_by5_80 := set_age_breaks(cont_age)]

#
cont_imp <- subset(cont_imp, !is.na(cont_setting) & !is.na(cont_age))
any(is.na(cont_imp))

# # ok so some of these are NA because they are customers at work
# but we can't include these in the contact matrix because we don't know their age
# subset(cont_imp, is.na(cont_age))
#
# # ok but this seems different
# subset(cont_imp, is.na(cont_setting))
# subset(cont_imp, cont_id == 27)

# /////////////////////////////////////////////////////////////////////////////
# =============================================================================
# MERGE DATASETS
# =============================================================================
# /////////////////////////////////////////////////////////////////////////////

## Merge datasets
merged.df = resp_imp[cont_imp, on = "resp_id"]
dim(cont_imp)
dim(merged.df)
head(merged.df)

## Binary variable to denote whether respondent reported contacts
## (or reported contacts in a particular setting)
## 0 = no contact (in this setting);
## 1 = reported at least 1 contact (in this setting)
settings = unique(merged.df$cont_setting)
settings

## Create dummy variables for individual settings
## this works the opposite that it should, so I'm switching it
for(x in settings){
  cat(x, "\n")
  rr <- paste0("cont_", tolower(as.character(x)))
  merged.df[, rr] = ifelse(merged.df$cont_setting == x, 1, 0)
}

head(merged.df)

# /////////////////////////////////////////////////////////////////////////////
# =============================================================================
# CHOOSE WHICH ONE YOU ARE LOOKING FOR
# =============================================================================
# /////////////////////////////////////////////////////////////////////////////

xenv = "Total"
xenv = "household"

if(xenv == "Total") {
  merged.df[,binary_col := 1]
} else {
  xcol = paste0("cont_", xenv)
  merged.df[,binary_col := get(xcol)]
}

head(merged.df)

# /////////////////////////////////////////////////////////////////////////////
# =============================================================================
# Single-year age-specific contact matrices
# =============================================================================
# /////////////////////////////////////////////////////////////////////////////

## prediction ages ##
predict.m = setDT(expand.grid(
  resp_age = c(0:max(merged.df$resp_age)),
  cont_age = c(0:max(merged.df$resp_age))))

# obtain the weighted sum of contacts per respondent-contact group
age_matrix_s = merged.df[,
  .(weighted_sum = sum(weight * binary_col, na.rm = T)),
  by = .(cont_age, resp_age)
]

# combine weighted sum with the data for predictions
age_matrix_s = age_matrix_s[predict.m, on = c("cont_age", "resp_age")]

# change NAs to 0s
age_matrix_s$weighted_sum[is.na(age_matrix_s$weighted_sum)] = 0

# combine total weighted contact sum with weighted N of people
age_matrix_s = age_matrix_s[age_matrix_n, on = c("resp_age")]

# average number of contacts for the matrix
age_matrix_s[, ncont := weighted_sum/n]

# GAM (estimating smoothing)
model = gam(ncont ~ te(as.numeric(resp_age), as.numeric(cont_age),
                       k = c(10,10), # k=10 fits our data best
                       bs = c("cc","cc"), # cc = cyclic cubic spline
                                          # (best suited for our data)
                       m = c(0,0)),
            family = nb(),
            data = age_matrix_s)

## For work contacts, the cc smoothing spline does not provide sufficient flexibility.
## Consider using thin plate (tp) regression spline that allows for more flexibility
#  in modeling complex multi-dimensional and non-linear relationships.

# predictions (predicting smoothed contact matrices)
age_matrix_s$pred = predict(model, newdata = age_matrix_s, type = "response")

# PLOT
p1 <- ggplot(age_matrix_s) + # needs to be a data frame (long format of the data)
  geom_tile(aes(x = resp_age, y = cont_age, fill = pred)) +
  ggtitle(xenv) + # Main title
  xlab("Respondent Age") + # X-axis title
  ylab("Contact Age") + # Y-axis title
  scale_fill_viridis_c() +
  scale_y_continuous(breaks = c(0,20,40,60,80)) +
  scale_x_continuous(breaks = c(0,20,40,60,80)) +
  coord_fixed(ratio = 1) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, size = 16),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent", color = "black"),
        plot.background = element_rect(fill = "transparent", color = NA)#,
  )

# /////////////////////////////////////////////////////////////////////////////
# =============================================================================
# Single-year age-factor contact matrices
# =============================================================================
# /////////////////////////////////////////////////////////////////////////////

## all age group combinations ##
## in case some combinations are missing from the sample ##
predict.m_fct = setDT(
  expand.grid(resp_age_by5_80 = levels(merged.df$resp_age_by5_80),
              cont_age_by5_80 = levels(merged.df$cont_age_by5_80))
)

# obtain the weighted sum of contacts per respondent-contact group
age_matrix_s_fct = merged.df[,
  .(weighted_sum = sum(weight * binary_col, na.rm = T)),
  by = .(cont_age_by5_80, resp_age_by5_80)
]

# combine weighted sum with the data for predictions
age_matrix_s_fct = age_matrix_s_fct[
  predict.m_fct,
  on = c("cont_age_by5_80", "resp_age_by5_80")]

# change NAs to 0s
age_matrix_s_fct$weighted_sum[is.na(age_matrix_s_fct$weighted_sum)] = 0

# combine total weighted contact sum with weighted N of people
age_matrix_s_fct = age_matrix_s_fct[age_matrix_n_fct, on = c("resp_age_by5_80")]

# average number of contacts for the matrix
age_matrix_s_fct[, ncont := weighted_sum/n]

age_matrix_s_fct

# PLOT
p2  <- ggplot(age_matrix_s_fct) + # should be a data frame (long format)
  geom_tile(aes(x = resp_age_by5_80, y = cont_age_by5_80, fill = ncont)) +
  ggtitle(xenv) + # Main title
  xlab("Respondent Age") + # X-axis title
  ylab("Contact Age") + # Y-axis title
  # scale_fill_gradientn(colors = c("#005f87", "#87afd7", "lightblue")
  # ) +
  scale_fill_viridis_c() +
  coord_fixed(ratio = 1) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, size = 16),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent", color = "black"),
        plot.background = element_rect(fill = "transparent", color = NA)
  )


age_matrix_s_fct = merged.df[,
                             .(weighted_sum = sum(weight * binary_col, na.rm = T)),
                             by = .(cont_age_by5_80, resp_age_by5_80)
]

#
head(age_matrix_s_fct)
subset(age_matrix_s_fct, resp_age_by5_80 == '[0,5]')



# /////////////////////////////////////////////////////////////////////////////
# =============================================================================
# Age-grp plot
# =============================================================================
# /////////////////////////////////////////////////////////////////////////////

# obtain the weighted sum of contacts per respondent-contact group
age_matrix_s_fct_pp = merged.df[,
  .(ind_weighted_sum = sum(weight * binary_col, na.rm = T)),
  by = .(resp_age_by5_80, resp_id)
]

age_matrix_s_fct_pp

age_matrix_s_fct_2 = age_matrix_s_fct_pp[,
  .(weighted_avg = mean(ind_weighted_sum, na.rm = T),
    weighted_sd = sd(ind_weighted_sum, na.rm = T)),
  by = .(resp_age_by5_80)
]
setorderv(age_matrix_s_fct_2, "resp_age_by5_80")

age_matrix_s_fct_2

# also plot the total contacts by age group
age_matrix_s_fct_sum = age_matrix_s_fct[,
  .(ncont_sum = sum(ncont)),
  by = resp_age_by5_80
]

# you could get this by age group
p3 <- ggplot(age_matrix_s_fct_2) +
  geom_col(aes(x = resp_age_by5_80, y = weighted_avg)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, size = 16),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent", color = "black"),
        plot.background = element_rect(fill = "transparent", color = NA))

p3

# /////////////////////////////////////////////////////////////////////////////
# =============================================================================
# Joined plot
# =============================================================================
# /////////////////////////////////////////////////////////////////////////////

p2 + p3

