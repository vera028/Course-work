
library(readxl)      
library(dplyr)       
library(purrr)       
library(lubridate)   
library(stringr)     
library(ggplot2)     
library(fixest)      
library(stargazer)   
library(corrplot)    
library(forcats)     
library(knitr)       
library(htmltools)   
library(modelsummary)

# загрузка файла 
setwd("C:/Users/faser/OneDrive/Рабочий стол")
wb <- "data_real_estate.xlsx"

# первичная обработка данных

# лист "Проекты"
projects_raw <- read_excel(wb, sheet = "Проекты")

projects <- projects_raw %>%
  select(
    БЦ                  = `Проект`,              
    Локация_проект      = `Локация проекта`,   
    Класс_проект        = `Класс проекта`,       
    Договор_проект      = `Договор проекта`,    
    Расстояние_проект   = `Минут до метро`,
    РНВ_проект          = `РНВ проекта`          
  ) %>%
  mutate(
    # очистка строк и приведение типов
    БЦ                 = str_trim(as.character(БЦ)),
    Локация_проект     = str_trim(as.character(Локация_проект)),
    Класс_проект       = str_trim(str_replace_all(as.character(Класс_проект), "А", "A")),
    Договор_проект     = str_trim(as.character(Договор_проект)),
    Расстояние_проект  = as.numeric(Расстояние_проект),
    РНВ_проект         = as.numeric(РНВ_проект)
  )

# листы в формате мм.гггг
sheet_names <- excel_sheets(wb)
sheet_names <- sheet_names[!(sheet_names %in% c("Проекты"))]

# читаем данные с 99 строки
read_monthly_sheet <- function(sheet) {
  df <- read_excel(wb, sheet = sheet, skip = 98, col_types = "text")
  
    expected_names <- c("Дата_сбора","ID","БЦ","Девелопер","Корпус","Номер",
                      "Этаж","Площадь","Диапазон_площади","Стоимость",
                      "Цена","Динамика","Отделка","блок_этаж_здание",
                      "Локация","Деловой_район","Договор","Стадия_строительства",
                      "Станция_метро","Расстояние_до_метро","РНВ","Класс","Источник")
  
  ncols <- ncol(df)
  if (ncols >= 23) {
    df <- df[, 1:23]
    names(df) <- expected_names
  } else {
    names(df)[1:ncol(df)] <- expected_names[1:ncol(df)]
  }
  
  df$Source_Sheet <- sheet
  return(df)
}

# объединяем в одну таблицу
lots_raw <- map_dfr(sheet_names, read_monthly_sheet)

#очистка
lots <- lots_raw %>%
  mutate(
    Месяц = my(Source_Sheet),
    Площадь = as.numeric(Площадь),
    Стоимость = as.numeric(Стоимость),
    Цена = as.numeric(Цена),
    Этаж = as.numeric(Этаж),
    Расстояние_до_метро = as.numeric(Расстояние_до_метро),
    РНВ = as.numeric(РНВ),
    БЦ = str_trim(as.character(БЦ)),
    Стадия_строительства = str_trim(as.character(Стадия_строительства)),
    Класс = str_trim(as.character(Класс)),
    Договор = str_trim(as.character(Договор)),
    Локация = str_trim(as.character(Локация)),
    Деловой_район = str_trim(as.character(Деловой_район)),
    Девелопер = str_trim(as.character(Девелопер))
  ) %>%
  select(-Source_Sheet) %>%
  filter(!is.na(Цена), !is.na(Площадь), !str_detect(ID, "Общий итог"),
         Цена > 0, Площадь > 0, Стоимость > 0,
         !is.na(Этаж), !is.na(Месяц))

lots <- lots %>%
  mutate(
    Класс = str_to_upper(Класс)
  )
# лбьединения с проектами 
lots <- left_join(lots, projects, by = "БЦ")

lots <- lots %>%
  mutate(
    Локация             = if_else(is.na(Локация) | Локация == "", Локация_проект, Локация),
    Класс               = if_else(is.na(Класс) | Класс == "", Класс_проект, Класс),
    Договор             = if_else(is.na(Договор) | Договор == "", Договор_проект, Договор),
    Расстояние_до_метро = if_else(is.na(Расстояние_до_метро), Расстояние_проект, Расстояние_до_метро),
    РНВ                 = if_else(is.na(РНВ), РНВ_проект, РНВ)
  ) %>%
  select(-ends_with("_проект"))

# создаем переменные и факторы
lots <- lots %>%
  mutate(
    log_Цена    = log(Цена),
    log_Площадь = log(Площадь),
    До_сдачи    = РНВ - year(Месяц),
    Этаж_группа = case_when(
      Этаж <= 5  ~ "низкий",
      Этаж <= 15 ~ "средний",
      TRUE       ~ "высокий"
    ),
    
    # стадия строительства
    Стадия_укр = case_when(
      str_detect(str_to_lower(Стадия_строительства), "котлован|подготовительные") ~ "начальная",
      str_detect(str_to_lower(Стадия_строительства), "нижних|средних|верхних|монолитные") ~ "средняя",
      str_detect(str_to_lower(Стадия_строительства), "фасадные|отделочные") ~ "финальная",
      TRUE ~ NA_character_
    ),
    
    # Числовая стадия (1–6)
    Стадия_num = case_when(
      str_detect(str_to_lower(Стадия_строительства), "котлован|подготовительные") ~ 1L,
      str_detect(str_to_lower(Стадия_строительства), "нижних")                     ~ 2L,
      str_detect(str_to_lower(Стадия_строительства), "средних")                    ~ 3L,
      str_detect(str_to_lower(Стадия_строительства), "верхних")                    ~ 4L,
      str_detect(str_to_lower(Стадия_строительства), "фасадные")                   ~ 5L,
      str_detect(str_to_lower(Стадия_строительства), "отделочные")                 ~ 6L,
      TRUE ~ NA_integer_
    ),
    
    # Радиальный пояс
    Пояс = case_when(
      str_detect(Локация, "СК-ТТК") ~ "Центр (СК-ТТК)",
      str_detect(Локация, "ТТК-МКАД") ~ "Срединный (ТТК-МКАД)",
      str_detect(Локация, "За МКАД") ~ "Периферия (За МКАД)",
      TRUE ~ "Прочее"
    ),
    
    Класс   = factor(Класс),
    Договор = factor(Договор),
    Деловой_район = factor(Деловой_район),
    Девелопер = factor(Девелопер),
    Локация = factor(Локация),
    Пояс    = factor(Пояс, levels = c("Центр (СК-ТТК)", "Срединный (ТТК-МКАД)", "Периферия (За МКАД)")),
    Этаж_группа = factor(Этаж_группа, levels = c("низкий","средний","высокий")),
    Стадия_укр  = factor(Стадия_укр, levels = c("начальная","средняя","финальная"))
  ) %>%
  
  # удаляем выбросы
  filter(!is.na(Стадия_num), !is.na(До_сдачи), До_сдачи >= 0)
lots <- lots %>% filter(До_сдачи <= 10)
low_price  <- quantile(lots$Цена, 0.01, na.rm = TRUE)
high_price <- quantile(lots$Цена, 0.99, na.rm = TRUE)

lots <- lots %>%
  filter(Цена >= low_price & Цена <= high_price)

area_q01 <- quantile(lots$Площадь, 0.01, na.rm = TRUE)
area_q99 <- quantile(lots$Площадь, 0.99, na.rm = TRUE)
lots <- lots %>%
  filter(Площадь >= area_q01 & Площадь <= area_q99)

lots <- lots %>%
  filter(Класс %in% c("A", "B+"))

# переменные взаимодействия 
lots <- lots %>%
  mutate(
    d_ДДУ          = as.integer(Договор == "ДДУ"),
    d_ТТК_МКАД     = as.integer(Пояс == "Срединный (ТТК-МКАД)"),
    d_СК_ТТК       = as.integer(Пояс == "Центр (СК-ТТК)"),
    ДДУ_x_До_сдачи = d_ДДУ * До_сдачи,
    Метро_x_Стадия = Расстояние_до_метро * Стадия_num,
    Пл_x_Стадия    = log_Площадь * Стадия_num,
    Ст_x_ТТК_МКАД  = Стадия_num * d_ТТК_МКАД,
    Ст_x_СК_ТТК    = Стадия_num * d_СК_ТТК
  )

dim(lots)

# модели
library(plm)
pdata <- suppressWarnings(pdata.frame(lots, index = c("БЦ", "Месяц")))

fe <- plm(log_Цена ~ log_Площадь + Этаж_группа + Договор + Расстояние_до_метро + Стадия_укр,
          data = pdata, model = "within", effect = "individual")

re <- plm(log_Цена ~ log_Площадь + Этаж_группа + Договор + Расстояние_до_метро + Стадия_укр,
          data = pdata, model = "random")

phtest(fe, re)

model_pooled <- feols(log_Цена ~ log_Площадь + Этаж_группа +
                        Класс + Договор + Расстояние_до_метро +
                        Стадия_укр + Деловой_район + Девелопер | Месяц,
                      data = lots, cluster = ~БЦ)

# Панельная модель с фиксированными эффектами по проектам (БЦ)
model_fe <- feols(log_Цена ~ log_Площадь + Этаж_группа +
                    Договор + Расстояние_до_метро +
                    Стадия_укр | БЦ + Месяц, data = lots, cluster = ~БЦ)

model_H1 <- feols(log_Цена ~ log_Площадь + Этаж_группа + Класс +
                    Договор * До_сдачи * Девелопер +
                    Расстояние_до_метро  +
                    Стадия_укр + Деловой_район | Месяц,
                  data = lots, cluster = ~БЦ)

model_H2 <- feols(log_Цена ~ log_Площадь + Этаж_группа + Класс +
                    Договор + Расстояние_до_метро  +
                    Стадия_num + Метро_x_Стадия +
                    Деловой_район + Девелопер | Месяц,
                  data = lots, cluster = ~БЦ)

model_H3 <- feols(log_Цена ~ log_Площадь + Этаж_группа + Класс +
                    Договор + Расстояние_до_метро  +
                    Стадия_num + Пл_x_Стадия +
                    Деловой_район + Девелопер | Месяц,
                  data = lots, cluster = ~БЦ)

model_H4 <- feols(log_Цена ~ log_Площадь + Этаж_группа + Класс +
                    Договор + Расстояние_до_метро  +
                    Локация + Стадия_num + Ст_x_ТТК_МКАД + Ст_x_СК_ТТК +
                    Девелопер | Месяц,
                  data = lots, cluster = ~БЦ)

lots <- lots %>%
  group_by(ID) %>%
  arrange(Месяц) %>%
  mutate(log_Цена_lag = dplyr::lag(log_Цена, 1)) %>%
  ungroup()

model_H5 <- feols(log_Цена ~ log_Цена_lag + log_Цена_lag:Стадия_укр +
                    log_Площадь + Этаж_группа + Класс +
                    Договор + Расстояние_до_метро  +
                    Стадия_укр + Деловой_район + Девелопер | Месяц,
                  data = lots, cluster = ~БЦ)

summary(model_pooled)
summary(model_fe)
summary(model_H1)
summary(model_H2)
summary(model_H3)
summary(model_H4)
summary(model_H5)

# AIC BIC ТЕСТЫ
AIC(model_pooled, model_H1, model_H2, model_H3, model_H4)
BIC(model_pooled, model_H1, model_H2, model_H3, model_H4)

#graphs
lots %>%
  group_by(Класс, Деловой_район) %>%
  summarise(Средняя_цена = mean(Цена, na.rm = TRUE),
            Медиана = median(Цена, na.rm = TRUE), n = n()) %>%
  arrange(desc(Средняя_цена))

lots %>% filter(is.na(Класс)) %>% distinct(БЦ, Деловой_район)

# Корреляционная матрица 
numeric_vars <- lots %>% select(Цена, Площадь, Этаж, Расстояние_до_метро, РНВ)
corrplot(cor(numeric_vars, use = "complete.obs"), method = "number")


# Распределение цены
ggplot(lots, aes(x = Цена / 1000)) +
  geom_histogram(aes(y = after_stat(density)), bins = 60,
                 fill = "steelblue", alpha = 0.7, color = "white") +
  geom_density(color = "firebrick", linewidth = 1.2) +
  geom_vline(aes(xintercept = median(Цена / 1000, na.rm = TRUE)),
             linetype = "dashed", color = "darkorange", linewidth = 1) +
  labs(title = "Распределение цены кв. м. строящихся БЦ",
       subtitle=paste0("N = ",format(nrow(lots),big.mark=" ")," лотов"),
       x = "Цена, тыс. руб./м²", y = "Плотность")

stage_num_trend <- lots %>%
  filter(Класс %in% c("A", "B+"), !is.na(Стадия_num), Стадия_num >= 1, Стадия_num <= 6) %>%
  group_by(Стадия_num, Класс) %>%
  summarise(
    med_price = median(Цена/1000, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  filter(n >= 30)

ggplot(stage_num_trend, aes(x = Стадия_num, y = med_price, color = Класс, group = Класс)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  scale_x_continuous(breaks = 1:6, 
                     labels = c("1\nКотлован/\nПодгот.", "2\nНижние\nэтажи", "3\nСредние\nэтажи", 
                                "4\nВерхние\nэтажи", "5\nФасадные", "6\nОтделочные")) +
  labs(
    title = "Медианная цена по детализированным стадиям строительства",
    x = "Стадия строительства (по номерам)",
    y = "Цена, тыс. руб./м²",
    color = "Класс"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Динамика медианной цены
dynamics <- lots %>%
  group_by(Месяц) %>%
  summarise(медиана=median(Цена)/1000, N=n(), .groups="drop") %>%
  arrange(Месяц) %>%
  mutate(рост=round((медиана/first(медиана)-1)*100,1))

p2 <- ggplot(dynamics, aes(x=Месяц, y=медиана)) +
  geom_ribbon(aes(ymin=min(медиана)*0.97, ymax=медиана), fill="steelblue", alpha=0.1) +
  geom_line(color="steelblue", lwd=1.6) +
  geom_point(size=2.5, shape=21, fill="white", color="steelblue", stroke=1.5) +
  scale_x_date(date_labels="%m.%Y", date_breaks="3 months") +
  annotate("text", x=max(dynamics$Месяц), y=max(dynamics$медиана),
           label=paste0("+",last(dynamics$рост),"% за период"),
           hjust=1.1, vjust=-0.5, color="steelblue", size=3.5, fontface="bold") +
  labs(title="Динамика медианной цены кв. м. (февр. 2024 — март 2026)",
       x=NULL, y="Медианная Цена, тыс. руб./м²")
print(p2)

# Премии деловых районов из базовой модели
coefs_base <- as.data.frame(coef(model_pooled)) %>%
  tibble::rownames_to_column("Переменная") %>%
  rename(Коэф = `coef(model_pooled)`) %>%
  filter(str_detect(Переменная,"Деловой_район")) %>%
  mutate(
    Район = str_remove(Переменная,"Деловой_район"),
    Премия = round((exp(Коэф)-1)*100, 1),
    Цвет   = ifelse(Премия>0, "Позитивная","Негативная")
  ) %>%
  arrange(desc(Премия))

p3 <- ggplot(coefs_base, aes(x=reorder(Район,Премия), y=Премия, fill=Цвет)) +
  geom_col(alpha=0.85, width=0.7) +
  geom_text(aes(label=paste0(ifelse(Премия>0,"+",""),Премия,"%"),
                hjust=ifelse(Премия>0,-0.1,1.1)), size=3.3) +
  coord_flip() +
  scale_fill_manual(values=c("Позитивная"="steelblue","Негативная"="red")) +
  labs(title="Ценовые премии деловых районов",
       subtitle="Относительно базового района; ОЛС с ФЭ по месяцу",
       x=NULL, y="Премия к цене, %") 
print(p3)


# График цены отдельно для класса A и B+ ( до 3 лет)
ggplot(lots %>% filter(Класс %in% c("A", "B+")),
       aes(x = До_сдачи, y = Цена/1000, color = Договор)) +
  geom_smooth(method = "loess", se = FALSE, span = 0.8) +
  facet_wrap(~Класс, scales = "free_y") +
  coord_cartesian(xlim = c(0, 3)) +
  labs(
    title = "Цена в зависимости от срока до ввода, по классам (0-3 года)",
    x = "Лет до ввода",
    y = "Цена, тыс. руб./м²",
    color = "Договор"
  ) +
  theme_minimal()


# H1 - убывание премии ДДУ
c_ddu   <- coef(model_H1)["ДоговорДДУ"]
c_int   <- coef(model_H1)["ДоговорДДУ:До_сдачи"]

срок_seq <- seq(0, 3, 0.1)
h1_line <- data.frame(
  срок = срок_seq,
  eff  = (exp(c_ddu + c_int * срок_seq) - 1) * 100
)
h1_pts <- data.frame(срок=c(0,1,2,3)) %>%
  mutate(eff=(exp(c_ddu+c_int*срок)-1)*100)

p4 <- ggplot(h1_line, aes(x=срок, y=eff)) +
  geom_ribbon(aes(ymin=pmin(eff,0), ymax=pmax(eff,0)),
              fill="steelblue", alpha=0.12) +
  geom_line(color="steelblue", lwd=2) +
  geom_hline(yintercept=0, lty=2, color="gray", alpha=0.6) +
  geom_point(data=h1_pts, aes(x=срок,y=eff),
             size=4, shape=21, fill="white", color="steelblue", stroke=2) +
  geom_label(data=h1_pts,
             aes(x=срок, y=eff,
                 label=paste0(ifelse(eff>0,"+",""),round(eff,1),"%")),
             nudge_y=2.5, size=3.2, color="steelblue", fill="white",
             label.size=0.3) +
  labs(title="Рис. 4. H1: Убывание премии ДДУ к дате ввода",
       subtitle=paste0("ДДУ × До_сдачи = ",round(c_int,3),
                       " (p<0.001); ДДУ база: ДКПБВ"),
       x="Лет до ввода в эксплуатацию",
       y="Премия ДДУ над ДКПБВ, %") 
print(p4)

# Этаж и класс - A и B+
plot_df <- lots %>%
  filter(!is.na(Класс), Класс %in% c("A", "B+")) %>%
  group_by(Этаж_группа, Класс) %>%
  summarise(медиана = median(Цена) / 1000, N = n(), .groups = "drop")

ggplot(plot_df, aes(x = Этаж_группа, y = медиана, fill = Класс)) +
  geom_col(position = "dodge", alpha = 0.85, width = 0.6) +
  geom_text(aes(label = round(медиана)),
            position = position_dodge(0.6), vjust = -0.4, size = 3.3) +
  scale_fill_manual(values = c("A" = "steelblue", "B+" = "brown")) +
  labs(title = "Медианная цена по классу и группе этажа",
       x = "Группа этажа", y = "Медианная цена, тыс. руб./м²",
       fill = "Класс")


#Метро — нелинейный эффект 
metro_df <- lots %>%
  mutate(метро_бин = round(Расстояние_до_метро)) %>%
  group_by(метро_бин) %>%
  summarise(медиана=median(Цена)/1000, N=n(), .groups="drop") %>%
  filter(N >= 30, метро_бин <= 25)

p6 <- ggplot(metro_df, aes(x=метро_бин, y=медиана)) +
  geom_smooth(method="lm", formula=y~x+I(x^2),
              color="steelblue", fill="steelblue", alpha=0.12, lwd=1.5) +
  geom_point(aes(size=N), color="steelblue", alpha=0.75) +
  scale_size_continuous(range=c(2,8), name="Число лотов") +
  labs(title="Нелинейная зависимость цены от расстояния до метро",
       subtitle="Медианная Цена по 1-минутным бинам; квадратичный тренд",
       x="Расстояние до метро, мин.", y="Медианная цена, тыс. руб./м²") 
print(p6)

# Robustness check
m24  <- feols(log_Цена ~ log_Площадь + Этаж_группа + Класс +
                Договор + Расстояние_до_метро +
                Стадия_укр + Деловой_район + Девелопер | Месяц,
              data = filter(lots, year(Месяц) == 2024), cluster = ~БЦ)

m25p <- feols(log_Цена ~ log_Площадь + Этаж_группа + Класс +
                Договор + Расстояние_до_метро +
                Стадия_укр + Деловой_район + Девелопер | Месяц,
              data = filter(lots, year(Месяц) >= 2025), cluster = ~БЦ)

rob_vars <- c("log_Площадь", "Этаж_группасредний", "Этаж_группавысокий",
              "КлассA", "ДоговорДДУ", "Расстояние_до_метро",
              "Стадия_укрсредняя", "Стадия_укрфинальная")

rob_df <- bind_rows(
  data.frame(Период = "Полная", Переменная = rob_vars,
             Коэф = coef(model_pooled)[rob_vars],
             SE   = se(model_pooled)[rob_vars]),
  data.frame(Период = "2024",   Переменная = rob_vars,
             Коэф = coef(m24)[rob_vars],
             SE   = se(m24)[rob_vars]),
  data.frame(Период = "2025-2026", Переменная = rob_vars,
             Коэф = coef(m25p)[rob_vars],
             SE   = se(m25p)[rob_vars])
) %>%
  mutate(
    lo = Коэф - 1.96 * SE,
    hi = Коэф + 1.96 * SE,
    Период = factor(Период, levels = c("2024", "Полная", "2025-2026")),
    Переменная = dplyr::recode(Переменная,
                               "log_Площадь"             = "log(Площадь)",
                               "Этаж_группасредний"      = "Этаж: средний",
                               "Этаж_группавысокий"      = "Этаж: высокий",
                               "КлассA"                  = "Класс A",
                               "ДоговорДДУ"              = "Договор ДДУ",
                               "Расстояние_до_метро"     = "Расстояние до метро",
                               "Стадия_укрсредняя"       = "Стадия: средняя",
                               "Стадия_укрфинальная"     = "Стадия: финальная"
    )
  ) %>%
  na.omit()

# 4. График
ggplot(rob_df, aes(x = Переменная, y = Коэф, color = Период,
                   ymin = lo, ymax = hi)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", alpha = 0.6) +
  geom_pointrange(position = position_dodge(0.5), size = 0.8, linewidth = 1) +
  scale_color_manual(
    values = c("2024" = "#D4A017",         
               "Полная" = "#3A6EA5",        
               "2025-2026" = "#2E8B57")     
  ) +
  coord_flip() +
  labs(
    title = "Проверка робустности: коэффициенты по подпериодам",
    subtitle = "95% доверительные интервалы; кластеризация по БЦ",
    x = NULL,
    y = "Коэффициент (log-цена)",
    color = "Период"
  ) 

# таблицы
lots %>%
  group_by(Стадия_укр) %>%
  summarise(
    Средняя = mean(Цена/1000, na.rm = TRUE),
    Медиана = median(Цена/1000, na.rm = TRUE),
    Стандартное_отклонение = sd(Цена/1000, na.rm = TRUE),
    N = n()
  ) %>%
  knitr::kable(digits = 1, caption = "Описательная статистика цены по стадиям")

library(gt)
model_list <- list(
  "Базовая" = model_pooled,
  "FE проекта" = model_fe,
  "H1: ДДУ×Срок×Дев" = model_H1,
  "H2: Метро×Стадия" = model_H2,
  "H3: Площадь×Стадия" = model_H3,
  "H4: Стадия×Пояс" = model_H4,
  "H5: Авторегрессия" = model_H5
)
coef_subset <- c(
  "log_Площадь", "Этаж_группасредний", "Этаж_группавысокий",
  "КлассB+", "ДоговорДДУ", "Расстояние_до_метро",
  "Стадия_укрсредняя", "Стадия_укрфинальная",
  "До_сдачи",
  "ДоговорДДУ:До_сдачи", "ДоговорДКПБВ:До_сдачи",
  "Метро_x_Стадия", "Пл_x_Стадия",
  "Ст_x_ТТК_МКАД", "Ст_x_СК_ТТК",
  "log_Цена_lag", "log_Цена_lag:Стадия_укрсредняя", "log_Цена_lag:Стадия_укрфинальная"
)

tab_gt <- modelsummary(
  model_list,
  output = "gt",
  stars = TRUE,
  coef_keep = coef_subset,
  gof_map = c("nobs", "r.squared", "adj.r.squared"),
  title = "Результаты гедонистических моделей (ключевые коэффициенты)"
)
print(tab_gt)
gtsave(tab_gt, "table_models.png", vwidth = 1600, vheight = 900)

library(skimr)
skim(lots)
