project_path <- "p01_She-cession"
source(paste(project_path, "helper.R", sep = "/"))
source(paste(project_path, "pl_th_01.R", sep = "/"))

##=======================================================##
## >> Section No.1 <<
## >> Load data << 
##=======================================================##
dt00 <- load_data("lfs_workers_month_en.rds", project_path)


##=======================================================##
## >> Section No.2 <<
## >> Handling << 
##=======================================================##
dt10 <- 
  dt00 %>% 
  filter(
    empst %in% c("Workers"),
    age %in% c("15++"),
    year >= 1990,
    date <= as.Date("2021-05-01")
  ) %>% 
  mutate(
    value = value / 1000000
  )

dt11 <- 
  dt00 %>% 
  filter(
    empst %in% c("Workers"),
    ! age %in% c("15++", "15-64"),
    year >= 1990,
    date <= as.Date("2021-05-01")
  ) %>% 
  mutate(
    value = value / 1000000
  )

dt11_women <- 
  dt11 %>% 
  filter(gender == "Women")

dt11_men <- 
  dt11 %>% 
  filter(gender == "Men")


##=======================================================##
## >> Section No.3 <<
## >> Plot << 
##=======================================================##
p1111 <-
  dt10 %>% 
  ggplot(aes(x = date, y = value)) +
  pl_th +
  geoms_all

p1112 <-
  dt11_women %>% 
  ggplot(aes(x = date, y = value)) +
  pl_th +
  scale_y_continuous(
    breaks = seq(1, 100, 0.5)
  ) +
  geoms_gen

p1113 <-
  dt11_men %>% 
  ggplot(aes(x = date, y = value)) +
  pl_th +
  scale_y_continuous(
    breaks = seq(1, 100, 1)
  ) +
  geoms_gen



p1114 <- 
  dt10 %>% 
  filter(year >= 2015) %>% 
  ggplot(aes(x = date, y = value)) +
  pl_th +
  scale_x_date(date_breaks = "2 year", date_labels = "%Y") +
  scale_y_continuous(
    breaks = seq(1, 100, 0.5)
  ) +
  geoms_year


p1115 <- 
  dt11_women %>% 
  filter(year >= 2015) %>% 
  ggplot(aes(x = date, y = value)) +
  pl_th +
  scale_x_date(date_breaks = "2 year", date_labels = "%Y") +
  scale_y_continuous(
    breaks = seq(1, 100, 0.25)
  ) +
  geoms_year


p1116 <- 
  dt11_men %>% 
  filter(year >= 2015) %>% 
  ggplot(aes(x = date, y = value)) +
  pl_th +
  scale_x_date(date_breaks = "2 year", date_labels = "%Y") +
  scale_y_continuous(
    breaks = seq(1, 100, 0.25)
  ) +
  geoms_year


##=======================================================##
## >> Section No.4 <<
## >> save << 
##=======================================================##
save_graph(p1111, "1111_lfs_workers_trend", png = F, svg = T)
save_graph(p1112, "1112_lfs_workers_trend_women", png = F, svg = T)
save_graph(p1113, "1113_lfs_workers_trend_men", png = F, svg = T)

save_graph(p1114, "1114_lfs_workers_trend_focus", png = F, svg = T)
save_graph(p1115, "1115_lfs_workers_trend_focus_women", png = F, svg = T)
save_graph(p1116, "1116_lfs_workers_trend_focus_men", png = F, svg = T)

