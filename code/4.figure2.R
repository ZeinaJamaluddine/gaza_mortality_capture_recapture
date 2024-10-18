#..........................................................................................
###      Gaza-CAPTURE-RECAPTURE ANALYSIS Figure 2    ###
#..........................................................................................

#..........................................................................................

# Load required libraries
library(tidyverse)
library(gridExtra)
library(lubridate)
library(viridis)
library(cowplot)
library(extrafont)  
library(scales)  
loadfonts(device = "win")
common_font_size <- 10

# Define specific colors
hospital_color <- alpha(viridis(4)[2], 0.5)
survey_color <- alpha(viridis(4)[3], 0.5)
social_media_color <- alpha(viridis(4)[4], 0.5)

# Color palette
color_palette <- c("Hospital" = hospital_color, 
                   "Survey" = survey_color, 
                   "Social Media" = social_media_color)

add_space <- function(plot) {
  plot + theme(
    plot.margin = unit(c(0.5, 0.5, 0.5, 1.5), "cm"),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10))
  )
}

# Common theme for all plots
common_theme <- theme_minimal() +
  theme(
    text = element_text(family = "Arial"),
    axis.text = element_text(size = common_font_size),
    axis.title = element_text(size = common_font_size),
    legend.title = element_blank(),
    legend.text = element_text(size = common_font_size)
  )

# Monthly distribution plot
monthly_plot <- df %>%
  mutate(date = make_date(year_death, month_death, 1)) %>%
  group_by(date) %>%
  summarise(
    Hospital = sum(list1),
    Survey = sum(list2),
    `Social Media` = sum(list3)
  ) %>%
  pivot_longer(cols = c(Hospital, Survey, `Social Media`), names_to = "Source", values_to = "Count") %>%
  group_by(Source) %>%
  mutate(
    Percentage = Count / sum(Count) * 100,
    Source = factor(Source, levels = c("Hospital", "Survey", "Social Media")),
    Month = factor(format(date, "%b"), 
                   levels = c("Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun"))
  ) %>%
  ggplot(aes(x = Month, y = Percentage, fill = Source)) +
  geom_col(position = position_dodge(width = 0.9), show.legend = TRUE) +
  geom_text(aes(label = sprintf("%.1f%%", Percentage)), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5, 
            size = 2.5) +
  scale_fill_manual(values = color_palette) +
  scale_y_continuous(labels = comma) +
  labs(x = "Month", y = "Percentage") +
  common_theme +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top",
    legend.direction = "horizontal"
  )

monthly_plot <- add_space(monthly_plot)

# Age distribution plot without NA
age_plot <- df %>%
  mutate(age_group = case_when(
    is.na(age) ~ NA_character_, 
    age >= 80 ~ "80+",
    TRUE ~ paste0(5 * floor(age / 5), "-", 5 * floor(age / 5) + 4)
  )) %>%
  filter(!is.na(age_group)) %>% 
  mutate(age_group = factor(age_group, levels = c("0-4", "5-9", "10-14", "15-19", "20-24", 
                                                  "25-29", "30-34", "35-39", "40-44", 
                                                  "45-49", "50-54", "55-59", "60-64", 
                                                  "65-69", "70-74", "75-79", "80+"))) %>%
  
  group_by(age_group) %>%
  summarise(
    Hospital=sum(list1),
    Survey=sum(list2),
    `Social Media`=sum(list3)
  ) %>%
  pivot_longer(cols=c(Hospital, Survey, `Social Media`), names_to="Source", values_to="Count") %>%
  group_by(Source) %>%
  mutate(
    Percentage=Count/sum(Count)*100,
    Source=factor(Source, levels=c("Hospital","Survey","Social Media"))
  ) %>%
  ggplot(aes(x=age_group,y=Percentage,fill=Source)) +
  geom_col(position=position_dodge(width=0.9), show.legend=TRUE) +
  geom_text(aes(label=sprintf("%.1f%%", Percentage)), 
            position=position_dodge(width=0.9), 
            vjust=-0.5, 
            size=2,
            angle=90) +
  scale_fill_manual(values=color_palette) +
  scale_y_continuous(labels=comma) +
  labs(x="Age groups (years)", y="Percentage") +
  common_theme +
  theme(axis.text.x=element_text(angle=45,hjust=1))

age_plot <- add_space(age_plot)

# Gender proportion plot
gender_color_palette <- c(
  "Hospital.female"=alpha(hospital_color,0.3),
  "Hospital.male"=alpha(hospital_color,0.5),
  "Survey.female"=alpha(survey_color,0.3),
  "Survey.male"=alpha(survey_color,0.5),
  "Social Media.female"=alpha(social_media_color,0.3),
  "Social Media.male"=alpha(social_media_color,0.5)
)

gender_plot <- df %>%
  group_by(gender) %>%
  summarise(
    Hospital=sum(list1),
    Survey=sum(list2),
    `Social Media`=sum(list3)
  ) %>%
  pivot_longer(cols=c(Hospital, Survey, `Social Media`), names_to="Source", values_to="Count") %>%
  group_by(Source) %>%
  mutate(
    Proportion=Count/sum(Count),
    Source=factor(Source, levels=c("Hospital","Survey","Social Media")),
    gender=factor(gender, levels=c("female","male"))
  ) %>%
  filter(!is.na(gender)) %>%
  ggplot(aes(x=Source,y=Proportion*100,fill=interaction(Source,gender))) +
  geom_col(position="stack", show.legend=TRUE) +
  geom_text(aes(label=paste0(ifelse(gender=="female","female: ","male: "), 
                             round(Proportion*100,1),"% n=",comma(Count))),
            position=position_stack(vjust=0.5), 
            size=3,
            angle=0,
            hjust=0.5,
            color="grey30") +
  scale_fill_manual(values=gender_color_palette) +
  labs(x="List",y="Percentage") +
  common_theme

gender_plot <- add_space(gender_plot)

# Combine plots 
combined_plot <- plot_grid(
  monthly_plot,
  plot_grid(age_plot + theme(legend.position="none"),
            gender_plot + theme(legend.position="none"),
            ncol=1,
            rel_heights=c(1,0.7)),
  ncol=1,
  rel_heights=c(1.2,1.7)
)

final_plot <- plot_grid(combined_plot,ncol=1,rel_heights=c(1))
print(combined_plot)

# Save the combined plot 

ggsave("C:/Users/Zeina Jamaluddine/OneDrive - London School of Hygiene and Tropical Medicine/gaza-capture recapture/github/output/2.figure2.pdf",  plot = combined_plot, width=200,height=247,units="mm", device = cairo_pdf, bg = "white")
