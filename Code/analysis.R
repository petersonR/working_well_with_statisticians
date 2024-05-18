################################################### -
## Title: Working well with statisticians - survey analysis
## Code Author: Ryan Peterson
## Date Created: Tue Jun 13 14:10:16 2023
################################################### -

library(tidyverse)
library(here)
library(RColorBrewer)
library(kableExtra)

#### data cleaning ####

df <- readxl::read_xlsx(here("DataRaw/working_well_w_statisticians_survey.xlsx"), 1)

# some initial renaming
df <- df %>% 
  rename(`DE becomes unresponsive after analysis is complete,\nbut before following through with publication` = 
    `DE becomes unresponsive after analysis is complete, but before following through with publication `,
  `The DE asks you what you think\nabout a particular issue in the analysis` = 
    `The DE asks you what you think about a particular issue in the analysis`,
  `The DE does not include your name\nin an abstract despite you requesting it` = 
    `The DE does not include your name in an abstract despite you requesting it`, 
  `The DE includes your name in an\nabstract without your permission` = 
    `The DE includes your name in an abstract without your permission`,
  `The DE asks you for last-minute help\nwith a grant, and the grant is very well-written` =
    `The DE asks you for last-minute help with a grant, and the grant is very well-written`, 
  `DE has clear priorities and expectations;\nknows what is needed and when` = 
    `DE has clear priorities and expectations; knows what is needed and when`, 
  `DE shows empathy and inclusiveness (ability\nto connect at a human level)` = 
    `DE shows empathy and inclusiveness (ability to connect at a human level)`, 
  `The DE reaches out for support shortly before\na grant deadline, and the grant is not well-written` = 
    `The DE reaches out for support shortly before a grant deadline, and the grant is not well-written`, 
  `DE is an effective teacher, communicator` = 
    `DE is an effective teacher, communi-cator`, 
  `DE shows poor or chaotic communication during projects` = 
    `DE shows poor or chaotic communi-cation during projects`
  )

# Initial descriptive stats
table(df$`What setting do you primarily work in?`)
table(df$`What is your current position?`, useNA = "always")

# Bin groups
df$grp <- ifelse(
  df$`What setting do you primarily work in?` == "Academic", 
  ifelse(df$`What is your current position?` == "Faculty", 
         "Faculty", "Academic non-faculty"), 
  "Non-academic"
)

table(df$grp, useNA = "always")
df$grp <- relevel(factor(df$grp), "Faculty")

df$experience <- as.numeric(df$`Approximately how many years have you engaged in statistical collaboration or consultation? (Please include all experience, including across different roles or settings)`)

# EDA for experience
# hist(df$experience)
summary(df$experience)
sd(df$experience)

df$grp_granular <- df$`What setting do you primarily work in?`
df$position <- df$`What is your current position?`

df %>% 
  select(grp_granular, position, grp, experience) %>% 
  gtsummary::tbl_summary(
    missing = "ifany", missing_text = "Not provided", 
    label = list(
         grp_granular ~ "Primary work setting (reported)",
         position ~ "Current position (reported)",
         grp ~ "Analysis group",
         experience ~ "Experience (years)"))


## Clean question data

# Question 6
df <- df %>% 
  mutate_at(vars(`DE is good at big-picture thinking`:`DE values team science`), 
            .funs = function(x) as.numeric(as.character(factor(x, c("Not needed", "Useful to have, but not critical", "Very important to have", "Absolutely must have"), labels = 0:3))))

# Question 8
df <- df %>% 
  mutate_at(vars(`DE is not productive`:`DE does not have a clear research plan or objective`), 
            .funs = function(x) as.numeric(as.character(factor(x, c("Not problematic", "Consider avoiding DE", "Avoid DE if possible", "Avoid DE at all costs"), labels = 0:3))))

# Question 10 

df <- df %>% 
  mutate_at(vars(`Your peer tells you they had a bad experience with the DE`:`The DE dismisses opinions of others on the team`, `Your gut tells you the DE will be good to work with`), 
            .funs = function(x) as.numeric(as.character(factor(x, c("Definitely a bad sign", "Possibly a bad sign", "No big deal, indeterminate", "Possibly a good sign", "Definitely a good sign"), labels = 0:4))))

#### question 6; positives ####

pos_qual_df <- df %>% 
  select(`DE is good at big-picture thinking`:`DE values team science`) %>% 
  sapply(function(x) {
    sd_ci <- misty::ci.sd(x, output = FALSE)
    c(
      n_0 = sum(x == 0),
      n_1 = sum(x == 1),
      n_2 = sum(x == 2),
      n_3 = sum(x == 3),
      p_0 = mean(x == 0),
      p_1 = mean(x == 1),
      p_2 = mean(x == 2),
      p_3 = mean(x == 3),
      mean_score = mean(x),
      sd_score = sd(x),
      sd_lb = sd_ci$result$low,
      sd_ub = sd_ci$result$upp,
      coefvar_score = sd(x) / mean(x)
    )
  }) %>% 
  t() %>% as.data.frame()  %>% 
  rownames_to_column("quality") %>% 
  arrange(mean_score)

## ORM modeling

pos_fits <- lapply(pos_qual_df$quality, function(x) {
  f <- as.formula(paste0("`", x, "` ~ grp +  I(experience/10)"))
  fit <- rms::orm(f, data = df)
  b <- coef(fit)
  b <- b[grepl("grp|exp", names(b))]
  se <- sqrt(diag(fit$var))
  se <- se[grepl("grp|exp", names(se))]
  data.frame(Est = b, se = se, z = b/se, p = 2*pnorm(-abs(b/se)))
})

names(pos_fits) <- pos_qual_df$quality

#### question 8; negatives ####

neg_qual_df <- df %>% 
  select(`DE is not productive`:`DE does not have a clear research plan or objective`) %>% 
  sapply(function(x) c(n_0 = sum(x==0), n_1 = sum(x==1), 
                       n_2 = sum(x==2), n_3 = sum(x==3), 
                       p_0 = mean(x==0), p_1 = mean(x==1), 
                       p_2 = mean(x==2), p_3 = mean(x==3), 
                       mean_score = mean(x), sd_score = sd(x), coefvar_score = sd(x)/mean(x))) %>% 
  t() %>% as.data.frame()  %>% 
  rownames_to_column("quality") %>% 
  arrange(mean_score)

## ORM modeling
neg_fits <- lapply(neg_qual_df$quality, function(x) {
  f <- as.formula(paste0("`", x, "` ~ grp +  I(experience/10)"))
  fit <- rms::orm(f, data = df)
  b <- coef(fit)
  b <- b[grepl("grp|exp", names(b))]
  se <- sqrt(diag(fit$var))
  se <- se[grepl("grp|exp", names(se))]
  data.frame(Est = b, se = se, z = b/se, p = 2*pnorm(-abs(b/se)))
})

names(neg_fits) <- neg_qual_df$quality
neg_fits

#### question 10; redflags ####

rf_qual_df <- df %>% 
  select(`Your peer tells you they had a bad experience with the DE`:`The DE dismisses opinions of others on the team`,
         `Your gut tells you the DE will be good to work with`) %>% 
  sapply(function(x) c(n_0 = sum(x==0), n_1 = sum(x==1), 
                       n_2 = sum(x==2), n_3 = sum(x==3), n_4 = sum(x==4),
                       p_0 = mean(x==0), p_1 = mean(x==1), 
                       p_2 = mean(x==2), p_3 = mean(x==3), p_4 = mean(x==4),
                       mean_score = mean(x), sd_score = sd(x), coefvar_score = sd(x)/mean(x))) %>% 
  t() %>% as.data.frame()  %>% 
  rownames_to_column("quality") %>% 
  arrange(desc(mean_score))

## ORM modeling
rf_fits <- lapply(rf_qual_df$quality, function(x) {
  f <- as.formula(paste0("`", x, "` ~ grp + I(experience/10)"))
  fit <- rms::orm(f, data = df)
  b <- coef(fit)
  b <- b[grepl("grp|exp", names(b))]
  se <- sqrt(diag(fit$var))
  se <- se[grepl("grp|exp", names(se))]
  data.frame(Est = b, se = se, z = b/se, p = 2*pnorm(-abs(b/se)))
})

names(rf_fits) <- rf_qual_df$quality
rf_fits

#### Summary of model results ####

pretty_pos_fits <- lapply(pos_fits, function(fit) {
  fit %>% 
    mutate(OR = exp(Est), 
           ci_lb = exp(Est - 1.96 * se), 
           ci_ub = exp(Est + 1.96*se))
}) %>% 
  bind_rows()

pretty_pos_fits$Quality = rep(names(pos_fits), each = 3)
pretty_pos_fits$Covariate <- factor(
  c("Academic staff", "Non-academic", "Experience"),
  levels =c("Academic staff", "Non-academic", "Experience"),
  ordered = TRUE
)
table(pretty_pos_fits$Covariate)

pretty_neg_fits <- lapply(neg_fits, function(fit) {
  fit %>% 
    mutate(OR = exp(Est), 
           ci_lb = exp(Est - 1.96 * se), 
           ci_ub = exp(Est + 1.96*se))
}) %>% 
  bind_rows()

pretty_neg_fits$Quality = rep(names(neg_fits), each = 3)
pretty_neg_fits$Covariate <- factor(
  c("Academic staff", "Non-academic", "Experience"),
  levels =c("Academic staff", "Non-academic", "Experience"),
  ordered = TRUE
)
table(pretty_neg_fits$Covariate)

pretty_rf_fits <- lapply(rf_fits, function(fit) {
  fit %>% 
    mutate(OR = exp(Est), 
           ci_lb = exp(Est - 1.96 * se), 
           ci_ub = exp(Est + 1.96*se))
}) %>% 
  bind_rows()

pretty_rf_fits$Quality = rep(names(rf_fits), each = 3)
pretty_rf_fits$Covariate <- factor(
  c("Academic staff", "Non-academic", "Experience"),
  levels =c("Academic staff", "Non-academic", "Experience"),
  ordered = TRUE
)
table(pretty_rf_fits$Covariate)

