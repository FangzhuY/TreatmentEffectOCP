rm(list=ls())

library(nnet)
library(readxl)
library(foreign)
library(haven)
library(tidyverse)
library(dplyr)
library(pracma)
library(patchwork)
library(ggplot2)
library(kableExtra)

#------------------------------------------------------------------------------#
# Load the processed data
#------------------------------------------------------------------------------#

load("Working_data/data_final.RData")
data <- data %>% filter(age>=20 & age<=42)


#------------------------------------------------------------------------------#
# Draw graphs for ideal and actual number of children (Figure 1)
#------------------------------------------------------------------------------#

# All population
data_graph1 <- data.frame(
  Year = factor(rep(c(2014, 2018), each = 2)),
  Type = factor(rep(c("Ideal", "Realized"), times = 2)),
  Number = c(round(mean(data$ideal[data$year==2014]),2), 
             round(mean(data$N[data$year==2014]),2), 
             round(mean(data$ideal[data$year==2018]),2), 
             round(mean(data$N[data$year==2018]),2))
)


# Urban and rural
data_graph2 <- data.frame(
  Year = factor(rep(c(2014, 2018), each = 4)),
  Type = factor(rep(c("Ideal", "Realized"), times = 4)),
  Urban = factor(rep(c("Urban", "Urban", "Rural", "Rural"), times = 2)),  # Urban=1 for original data, Urban=0 for new data
  Number = c(round(mean(data$ideal[data$year==2014 & data$urban==1]),2), 
             round(mean(data$N[data$year==2014 & data$urban==1]),2), 
             round(mean(data$ideal[data$year==2014 & data$urban==0]),2), 
             round(mean(data$N[data$year==2014 & data$urban==0]),2), 
             round(mean(data$ideal[data$year==2018 & data$urban==1]),2), 
             round(mean(data$N[data$year==2018 & data$urban==1]),2), 
             round(mean(data$ideal[data$year==2018 & data$urban==0]),2), 
             round(mean(data$N[data$year==2018 & data$urban==0]),2))
)

# Han and minority
data_graph3 <- data.frame(
  Year = factor(rep(c(2014, 2018), each = 4)),
  Type = factor(rep(c("Ideal", "Realized"), times = 4)),
  Han = factor(rep(c("Han", "Han", "Minority", "Minority"), times = 2)),  # Urban=1 for original data, Urban=0 for new data
  Number = c(round(mean(data$ideal[data$year==2014 & data$han==1]),2), 
             round(mean(data$N[data$year==2014 & data$han==1]),2), 
             round(mean(data$ideal[data$year==2014 & data$han==0]),2), 
             round(mean(data$N[data$year==2014 & data$han==0]),2), 
             round(mean(data$ideal[data$year==2018 & data$han==1]),2), 
             round(mean(data$N[data$year==2018 & data$han==1]),2), 
             round(mean(data$ideal[data$year==2018 & data$han==0]),2), 
             round(mean(data$N[data$year==2018 & data$han==0]),2))
)

# Education
data_graph4 <- data.frame(
  Year = factor(rep(c(2014, 2018), each = 6)),
  Type = factor(rep(c("Ideal", "Realized"), times = 6)),
  Educ = factor(rep(c("Less than high", "Less than high", 
                      "High school", "High school",
                      "College & higher", "College & higher"), times = 2)), 
  Number = c(round(mean(data$ideal[data$year==2014 & data$educ1=="Less than High School"]),2), 
             round(mean(data$N[data$year==2014 & data$educ1=="Less than High School"]),2), 
             round(mean(data$ideal[data$year==2014 & data$educ1=="High School Graduate"]),2), 
             round(mean(data$N[data$year==2014 & data$educ1=="High School Graduate"]),2), 
             round(mean(data$ideal[data$year==2014 & data$educ1=="College and Higher"]),2), 
             round(mean(data$N[data$year==2014 & data$educ1=="College and Higher"]),2), 
             round(mean(data$ideal[data$year==2018 & data$educ1=="Less than High School"]),2), 
             round(mean(data$N[data$year==2018 & data$educ1=="Less than High School"]),2), 
             round(mean(data$ideal[data$year==2018 & data$educ1=="High School Graduate"]),2), 
             round(mean(data$N[data$year==2018 & data$educ1=="High School Graduate"]),2), 
             round(mean(data$ideal[data$year==2018 & data$educ1=="College and Higher"]),2), 
             round(mean(data$N[data$year==2018 & data$educ1=="College and Higher"]),2))
)

# Employment
data_graph5 <- data.frame(
  Year = factor(rep(c(2014, 2018), each = 6)),
  Type = factor(rep(c("Ideal", "Realized"), times = 6)),
  Employ = factor(rep(c("Unemployed", "Unemployed", 
                      "Self-employed", "Self-employed",
                      "Agriculture", "Agriculture"), times = 2)), 
  Number = c(round(mean(data$ideal[data$year==2014 & data$jobclass=="unemployed" & !is.na(data$jobclass)]),2), 
             round(mean(data$N[data$year==2014 & data$jobclass=="unemployed" & !is.na(data$jobclass)]),2), 
             round(mean(data$ideal[data$year==2014 & data$jobclass=="self-employed" & !is.na(data$jobclass)]),2), 
             round(mean(data$N[data$year==2014 & data$jobclass=="self-employed" & !is.na(data$jobclass)]),2), 
             round(mean(data$ideal[data$year==2014 & data$jobclass=="agriculture" & !is.na(data$jobclass)]),2), 
             round(mean(data$N[data$year==2014 & data$jobclass=="agriculture" & !is.na(data$jobclass)]),2), 
             round(mean(data$ideal[data$year==2018 & data$jobclass=="unemployed" & !is.na(data$jobclass)]),2), 
             round(mean(data$N[data$year==2018 & data$jobclass=="unemployed" & !is.na(data$jobclass)]),2), 
             round(mean(data$ideal[data$year==2018 & data$jobclass=="self-employed" & !is.na(data$jobclass)]),2), 
             round(mean(data$N[data$year==2018 & data$jobclass=="self-employed" & !is.na(data$jobclass)]),2), 
             round(mean(data$ideal[data$year==2018 & data$jobclass=="agriculture" & !is.na(data$jobclass)]),2), 
             round(mean(data$N[data$year==2018 & data$jobclass=="agriculture" & !is.na(data$jobclass)]),2))
)

# Employer type
data_graph6 <- data.frame(
  Year = factor(rep(c(2014, 2018), each = 4)),
  Type = factor(rep(c("Ideal", "Realized"), times = 4)),
  Employer = factor(rep(c("Government", "Government", 
                          "Private", "Private"), times = 2)), 
  Number = c(round(mean(data$ideal[data$year==2014 & data$employer=="government" & !is.na(data$employer)]),2), 
             round(mean(data$N[data$year==2014 & data$employer=="government" & !is.na(data$employer)]),2), 
             round(mean(data$ideal[data$year==2014 & data$employer=="private" & !is.na(data$employer)]),2), 
             round(mean(data$N[data$year==2014 & data$employer=="private" & !is.na(data$employer)]),2), 
             round(mean(data$ideal[data$year==2018 & data$employer=="government" & !is.na(data$employer)]),2), 
             round(mean(data$N[data$year==2018 & data$employer=="government" & !is.na(data$employer)]),2), 
             round(mean(data$ideal[data$year==2018 & data$employer=="private" & !is.na(data$employer)]),2), 
             round(mean(data$N[data$year==2018 & data$employer=="private" & !is.na(data$employer)]),2))
)

########################### Create the bar chart ###########################

# --------------------- Shared Theme ---------------------
my_theme <- theme_minimal(base_size = 22) +
  theme(
    text = element_text(face = "bold"),
    plot.title = element_text(size = 20, face = "bold"),
    plot.subtitle = element_text(size = 14),
    axis.title = element_text(size = 15),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 18),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    legend.position = "bottom",
    plot.margin = margin(10, 10, 20, 10)
  )

# --------------------- Helper: Faceted Plot ---------------------
make_faceted_plot <- function(data, facet_var) {
  ggplot(data, aes(x = Year, y = Number, fill = Type)) +
    geom_bar(stat = "identity", position = "dodge", color = "black") +
    geom_point(aes(shape = Type), position = position_dodge(width = 0.9), size = 3) +
    geom_text(aes(label = Number), 
              position = position_dodge(width = 0.9), 
              vjust = -0.5, size = 5, fontface = "bold") +
    labs(x = "Year", y = "Number of Children") +
    scale_fill_manual(values = c("Ideal" = "gray100", "Realized" = "gray100")) +
    facet_wrap(as.formula(paste("~", facet_var)), nrow = 1) +
    # expand_limits(y = max(data$Number, na.rm = TRUE) + 0.5) +  # room above bars
    coord_cartesian(clip = "off") +  # allow labels to extend
    my_theme
}

# --------------------- Plot 1: No facet ---------------------
base_plot1 <- ggplot(data_graph1, aes(x = Year, y = Number, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  geom_point(aes(shape = Type), position = position_dodge(width = 0.9), size = 3) +
  geom_text(aes(label = Number), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5, size = 5, fontface = "bold") +
  labs(x = "Year", y = "Number of Children") +
  scale_fill_manual(values = c("Ideal" = "gray100", "Realized" = "gray100")) +
  # expand_limits(y = max(data_graph1$Number, na.rm = TRUE) + 0.5) +
  coord_cartesian(clip = "off") +
  my_theme

# --------------------- Faceted Plots ---------------------
base_plot2 <- make_faceted_plot(data_graph2, "Urban")
base_plot3 <- make_faceted_plot(data_graph3, "Han")
base_plot4 <- make_faceted_plot(data_graph4, "Educ")
base_plot5 <- make_faceted_plot(data_graph5, "Employ")
base_plot6 <- make_faceted_plot(data_graph6, "Employer")

# --------------------- Add Titles ---------------------
plot1 <- base_plot1 + ggtitle("All Population")
plot2 <- base_plot2 + ggtitle("Urban vs Rural")
plot3 <- base_plot3 + ggtitle("Han vs Minority")
plot4 <- base_plot4 + ggtitle("Education")
plot5 <- base_plot5 + ggtitle("Employment")
plot6 <- base_plot6 + ggtitle("Employer")

# --------------------- Combine ---------------------
# composite_plot <- (plot1 | plot2 | plot3) / (plot4 | plot5) / plot6
composite_plot <- (plot1 | plot2) / (plot3 | plot4) / (plot5 | plot6)

# --------------------- Save ---------------------
ggsave("Output/sum_stats_bar_chart_fixed.pdf", plot = composite_plot,
       device = "pdf", width = 15, height = 20, units = "in", dpi = 300)



#------------------------------------------------------------------------------#
# Get averages for other variables (Table 1)
#------------------------------------------------------------------------------#

length(unique(data$pid)) # 5841
dim(data)[1] # 8000

data2 <- data %>% group_by(pid) %>% filter(row_number()==1) %>% ungroup()

# Create a helper function
summarize_var <- function(x) {
  c(Mean = mean(x, na.rm = TRUE),
    SD = sd(x, na.rm = TRUE),
    Min = min(x, na.rm = TRUE),
    Max = max(x, na.rm = TRUE),
    Obs = sum(!is.na(x)))
}

# ----- Age -----
age_2014 <- summarize_var(data$age[data$year == 2014])
age_2018 <- summarize_var(data$age[data$year == 2018])

# ----- Fertility -----
ideal_2014 <- summarize_var(data$ideal[data$year == 2014])
ideal_2018 <- summarize_var(data$ideal[data$year == 2018])
N_2014     <- summarize_var(data$N[data$year == 2014])
N_2018     <- summarize_var(data$N[data$year == 2018])

# ----- Urban & Han -----
urban <- summarize_var(data2$urban)
han   <- summarize_var(data2$han)

# ----- Education -----
data2$edu1 <- as.numeric(data2$educ1=="Less than High School")
data2$edu2 <- as.numeric(data2$educ1=="High School Graduate")
data2$edu3 <- as.numeric(data2$educ1=="College and Higher")

edu1 <- summarize_var(data2$edu1)
edu2 <- summarize_var(data2$edu2)
edu3 <- summarize_var(data2$edu3)

# ----- Job -----
data$unemploy    <- as.numeric(data$jobclass=="unemployed")
data$employ_self <- as.numeric(data$jobclass=="self-employed")
data$agri        <- as.numeric(data$jobclass=="agriculture")
data$employed    <- as.numeric(data$jobclass=="employed")

unemploy <- summarize_var(data$unemploy)
selfemp  <- summarize_var(data$employ_self)
agri     <- summarize_var(data$agri)
employed <- summarize_var(data$employed)

# ----- employer ----- 
data$private     = as.numeric(data$employer=="private")
data$government  = as.numeric(data$employer=="government")
data$foreign     = as.numeric(data$employer=="foreign")
data$other       = as.numeric(data$employer=="other")

private     <- summarize_var(data$private)
government  <- summarize_var(data$government)
foreign     <- summarize_var(data$foreign)
other       <- summarize_var(data$other)

# Combine into a table
table1 <- rbind(
  `Age in year 2014` = age_2014,
  `Age in year 2018` = age_2018,
  `Urban residence` = urban,
  `Han Chinese` = han,
  `Educational level` = NA,
  `- Less than high school` = edu1,
  `- High school graduate` = edu2,
  `- College and higher` = edu3,
  `Fertility` = NA,
  `- Ideal number of children in 2014` = ideal_2014,
  `- Ideal number of children in 2018` = ideal_2018,
  `- Actual number of children in 2014` = N_2014,
  `- Actual number of children in 2018` = N_2018,
  `Job status` = NA,
  `- Unemployed` = unemploy,
  `- Self-employed` = selfemp,
  `- Agriculture` = agri,
  `- Employed` = employed,
  `-- Private` = private,
  `-- Government` = government,
  `-- Foreign` = foreign,
  `-- Other` = other
)

# Turn into data.frame
table1_df <- as.data.frame(table1)
table1_df$Variables <- rownames(table1_df)
table1_df <- table1_df %>% select(Variables, everything())

# Format with kable (LaTeX style)
kable(table1_df, format = "latex", booktabs = TRUE, digits = 4,
      caption = "Table 1: Summary Statistics for Key Variables") %>%
  kable_styling(latex_options = c("hold_position", "striped")) %>%
  column_spec(1, bold = TRUE) %>%
  collapse_rows(columns = 1, valign = "top")

# EOF #

