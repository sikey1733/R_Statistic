
# Разделение данных на группы
gr_t_sleep_female <- s_c_p %>% 
  select(Gender, `Total Sleep Hours`) %>% 
  filter(Gender == "Female") 

gr_t_sleep_male <- s_c_p %>% 
  select(Gender, `Total Sleep Hours`) %>% 
  filter(Gender == "Male") 

# Обьединение групп
combined_data <- rbind(
  data.frame(Gender = "Female", `Total_Sleep_Hours` = 
               gr_t_sleep_female$`Total Sleep Hours`),
  data.frame(Gender = "Male", `Total_Sleep_Hours` =
               gr_t_sleep_male$`Total Sleep Hours`)
)

# Проверка данных на нормальность для женщин
shapiro.test(gr_t_sleep_female$`Total Sleep Hours`)

# Построение гистограммы для проверки на нормальность распределения данных
ggplot(data = gr_t_sleep_female, aes(x = `Total Sleep Hours`)) +
  geom_histogram(binwidth = 0.2, fill = "skyblue", color = "black", alpha = 0.7) +
  geom_density(aes(y = after_stat(density) * 0.2 * nrow(gr_t_sleep_female)), 
               color = "red", linewidth = 1) +
  labs(title = "Распределение общего количества часов сна у женщин",
       x = "Total Sleep Hours",
       y = "Frequency") +
  theme_minimal()

# Проверка данных на нормальность для мужчин
shapiro.test(gr_t_sleep_male$`Total Sleep Hours`)

# Построение гистограммы для проверки на нормальность распределения данных
ggplot(data = gr_t_sleep_male, aes(x = `Total Sleep Hours`)) +
  geom_histogram(binwidth = 0.2, fill = "skyblue", color = "black", alpha = 0.7) +
  geom_density(aes(y = after_stat(count) * 0.2), color = "red", linewidth = 1) +
  labs(title = "Распределение общего количества часов сна у женщин",
       x = "Total Sleep Hours",
       y = "Frequency") +
  theme_minimal()

# Тест Манна-Уитни
wilcox.test(gr_t_sleep_female$`Total Sleep Hours`,
            gr_t_sleep_male$`Total Sleep Hours`)

# Построение boxplot
ggplot(data = combined_data, 
       aes(x = Gender, 
           y = `Total_Sleep_Hours`,
           fill = Gender)) +
  geom_boxplot(width = 0.5) +
  labs(title = "Сравнение общего количества часов сна по полу",
       x = "Пол",
       y = "Время сна",
       fill = "Пол") +
  theme_minimal()


model_extended <- lm(`Total Sleep Hours` ~ `Stress Level` + `Screen Time Before Bed (mins)` + 
                       `Exercise (mins/day)` + `Caffeine Intake (mg)` + `Work Hours (hrs/day)` +
                       `Sleep Quality` + `Mood Score` + `Age`, 
                     data = s_c_p)
summary(model_extended)

library(car)
vif(model_extended)